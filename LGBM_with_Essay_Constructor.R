library(moments)
library(ggplot2)
library(caTools)
library(lightgbm)
library(readr)
library(caret)

train_scores <- read.csv("/kaggle/input/linking-writing-processes-to-writing-quality/train_scores.csv")
train_logs <- read.csv("/kaggle/input/linking-writing-processes-to-writing-quality/train_logs.csv")

sample_submission <- read.csv("/kaggle/input/linking-writing-processes-to-writing-quality/sample_submission.csv")
test_logs <- read.csv("/kaggle/input/linking-writing-processes-to-writing-quality/test_logs.csv")
test_scores <- data.frame(id = unique(test_logs$id))

data_cleaning <-  function(logs) {
  
    #Simplify Move Activities
    tryCatch ({
        logs[substr(logs$activity, 1, 1) == "M", ]$text_change <- strsplit(logs[substr(logs$activity, 1, 1) == "M", ]$activity, " From ")[[1]][2]
        logs[substr(logs$activity, 1, 1) == "M", ]$activity <- "Move"
    }, error = function(e) {
        cat("Error reformatting MOVE activities:\n", conditionMessage(e), "\n")
    })

    #Convert Activity to Factor
    logs$activity <- as.factor(logs$activity)
    str(logs$activity)

    #Combine Event Columns
    logs <- logs[, names(logs) != "up_event"]
    names(logs)[names(logs) == "down_event"] <- "event"

    #Simplify Event Values
    logs$event <- gsub("(?i)(F[0-9]{1,2}|.*Media.*|.*Audio.*|.*Pause.*).*", "DeviceFeatureChangeKey", logs$event)
    logs$event <- gsub("(?i)(Scroll.*|Meta|Dead|Process|OS|AltGraph|Mode.*)", "SpecialProcessKey", logs$event)
    logs$event <- gsub("(?i)(Clear|Cancel)", "Escape", logs$event)
    logs$event <- gsub("^[0-9]$", "DigitKey", logs$event)
    logs$event <- gsub("(?i).*Arrow.*", "ArrowKey", logs$event)
    logs$event <- gsub("(?i)(Home|End|^Page.*)", "TextJumpKey", logs$event)

    #Compile list of allowed event values
    event_labels <- c("^[A-Za-z]$", "^[[:punct:]]$", "click", "Shift", "Space", "Backspace", "Enter", "Tab", "Caps", "Control", "Delete", "Insert", "Escape", "Print", "RareKey", "NumLock", "Alt", "ContextMenu", "ArrowKey", "TextJumpKey", "DeviceFeatureChangeKey", "SpecialProcessKey", "DigitKey", "Unidentified")
    valid_events <- unique(logs$event)[apply(sapply(event_labels, function(x) grepl(x, unique(logs$event))), 1, any)]

    #Replace remaining event values with "Unidentified"
    cat("\n", "\n", "INVALID VALUES: ")
    invalid_events <- setdiff(unique(logs$event), valid_events)
    print(table(invalid_events))
    logs$event[logs$event %in% invalid_events] <- "Unidentified"

    #Replace text_change values with "q" for essay constructor
    cat("\n", "\n", "INVALID TEXT_CHANGE VALUES: ")
    print(table(logs$text_change[logs$text_change %in% invalid_events]))
    logs$text_change[logs$text_change %in% invalid_events] <- "q"

    #Recording valid inputs for essay constructor
    text_change_inputs <- unique(logs[logs$activity == "Input", ]$event)
    text_change_inputs <- gsub("Space", " ", text_change_inputs)
    text_change_inputs <- gsub("Enter", "\n", text_change_inputs)
    text_change_inputs <- text_change_inputs[text_change_inputs != "Unidentified"]

    cat("\n", "RARE VALUES: ")
    print(names(which(table(logs$event) < 25)))
    logs$event[logs$event %in% names(which(table(logs$event) < 25))] <- "RareKey"

    cat("\n", "REMAINING VALUES: ")
    print(unique(logs$event))
    cat("Total 'Event' Values: ", length(unique(logs$event)))

    #Convert event variable to factor and check
    logs$event <- as.factor(logs$event)
    str(logs$event)


    #Fixing text_change errors for essay constructor
    cat("\n", "VALID TEXT_CHANGE VALUES: ")
    print(text_change_inputs)
    text_change_correction <- logs[logs$activity == "Replace" | logs$activity == "Paste", ]$text_change
    split_value <- c()
  
    if (length(text_change_correction) > 0) {
        
        for (i in 1:length(text_change_correction)) {
        split_value <- strsplit(text_change_correction[i], "")[[1]]
            
            if (length(split_value[!split_value %in% text_change_inputs]) > 0) {
                print(paste("Iteration: ", i))
                print(split_value[!split_value %in% text_change_inputs])
            }
            
        split_value[!split_value %in% text_change_inputs] <- "q"
        text_change_correction[i] <- paste(split_value, collapse = "")       
        }
    }
                                                  
    logs[logs$activity == "Replace" | logs$activity == "Paste", ]$text_change <- text_change_correction
    return(logs)
}

train_logs <- data_cleaning(train_logs)

essay_constructor <- function(x) {
    operations <- x[, c("activity", "cursor_position", "event", "text_change")]
    essay <- character(0)
    expected_length <- 0
    error_messages <- character(0)
  
    # Capitalize Inputs from Shift Events 
    for (i in 1:nrow(operations)) {
        if (operations$activity[i] == "Nonproduction" & operations$event[i] == "Shift" & i != nrow(operations)) {
            if (operations$activity[i + 1] == "Input" & operations$text_change[i + 1] == "q") {
                operations$text_change[i + 1] <- "Q"
            }
        }
    }
  
    # Initial Processing Loop
    for (i in 1:nrow(operations)) {
        starting_position <- 0
        input <- character(0)
    
        if (operations$activity[i] == "Remove/Cut") {
            
            expected_length <- expected_length - nchar(operations$text_change[i])
            essay <- essay[-((operations$cursor_position[i] + 1):(operations$cursor_position[i] + (nchar(operations$text_change[i]))))]
            
        } else if (operations$activity[i] == "Input") {
            
            input <- strsplit(operations$text_change[i], "")[[1]]
            starting_position <- operations$cursor_position[i] - length(input) + 1

        } else if (operations$activity[i] == "Paste") {
            
            input <- strsplit(operations$text_change[i], "")[[1]]
            starting_position <- operations$cursor_position[i] - length(input) + 1

        } else if (operations$activity[i] == "Move") {

            move_indexes <- strsplit(operations$text_change[i], " To ")[[1]]
            initial_location <- as.numeric(strsplit(substr(move_indexes[1], 2, nchar(move_indexes[1]) - 1), ", ")[[1]])
            new_location <- as.numeric(strsplit(substr(move_indexes[2], 2, nchar(move_indexes[2]) - 1), ", ")[[1]])

            if (initial_location[2] - initial_location[1] != new_location[2] - new_location[1]) {
                error_messages <- c(error_messages, paste("Move Error in Row ", i, ":  initial location range: ", initial_location[2] - initial_location[1], "  -  new location range: ", new_location[2] - new_location[1]))
            }

            input <- essay[(initial_location[1] + 1):(initial_location[2] + 1)]
            essay <- essay[-((initial_location[1] + 1):(initial_location[2] + 1))]
            expected_length <- expected_length - length(input)
            starting_position <- new_location[1] + 1

        } else if (operations$activity[i] == "Replace") {  
            
            value <- strsplit(operations$text_change[i], " => ")[[1]]
            input <- strsplit(value[2], "")[[1]]
            starting_position <- operations$cursor_position[i] - length(input) + 1
            
            if ((starting_position + nchar(value[1]) - 1) > length(essay) + 1) {  
                error_messages <- c(error_messages, paste("Error with 'Replace' starting_position: ", starting_position, "  End of value[1]: ", (starting_position + nchar(value[1]) - 1), "  Essay Length: ", length(essay)))
                starting_position <- length(essay) - nchar(value[1]) + 2
            
            } else if (starting_position <= 0) {
                c(error_messages, paste("Error with 'Replace' starting_position (before the essay)): ", starting_position))
                starting_position <- 1
            }

            essay <- essay[-((starting_position):(starting_position + (nchar(value[1]) - 1)))]
            expected_length <- expected_length - nchar(value[1])
            
        }
    
      # Input Processing Loop
        if (operations$activity[i] != "Remove/Cut" & operations$activity[i] != "Nonproduction") {
            if (starting_position == (length(essay) + 1)) {
                essay <- c(essay, input)
            } else if (starting_position == 1) {
                essay <- c(input, essay)
            } else if (starting_position > 1 & starting_position <= length(essay)) {
                essay <- c(essay[1:starting_position - 1], input, essay[(starting_position):length(essay)])
            } else if (starting_position > length(essay) + 1) {
                correction <- starting_position - (length(essay) + 1)
                error_messages <- c(error_messages, paste("Starting Position Past End of Essay in Row:", i, "  Starting Position: ", starting_position, "  Length of Essay: ", length(essay), "@'s added: ", correction))
                essay <- c(essay, input, strsplit(strrep("@", correction), "")[[1]])
                expected_length <- expected_length + correction
            } else if (starting_position < 1) {
                essay <- c(input, essay)
                error_messages <- c(error_messages, paste("Starting Position Error in Row:", i, "  Starting Position: ", starting_position, "  Length of Essay: ", length(essay)))
            }
            expected_length <- expected_length + length(input)
        }
    
        ifelse(expected_length != length(essay), error_messages <- c(error_messages, paste("Length Error in Row:", i, "  Expected Length: ", expected_length, " Actual Length: ", length(essay))), next)
    
    }
  
    #essay <- paste(essay, collapse = "")
    return(list(essay, error_messages))
}


get_essay <- function(logs, scores) {
    essay_df <- data.frame(id = scores$id)
    essay_errors <- character(0)
    error_rate <- 0
  
    for (i in 1:nrow(essay_df)) {
        essay <- essay_constructor(logs[logs$id == scores$id[i],])
        na_amount <- sum(is.na(essay[[1]]))
        error_char_amount <- length(essay[[1]][essay[[1]] == "@"])
        essay_df$essay[i] <- paste(essay[[1]], collapse = "")
        
        if (length(essay[[2]]) > 0 | na_amount > 0 | error_char_amount > 0) {
            essay_errors <- c(essay_errors, paste("Submission #: ", i, "   Id: ", essay_df$id[i], "  NA Values: ", na_amount, "  @-Values: ", error_char_amount), essay[[2]])
            error_rate <- error_rate + 1
        }
    }
    
    print(essay_errors)
    print(paste("Total Number of Essays with Errors: ", error_rate))
    scores <- merge(essay_df, scores, by = "id")
    return(scores)
    
}

train_scores <- get_essay(train_logs, train_scores)

create_essay_features <- function(scores) {
  scores$total_char <- nchar(scores$essay)
  scores$total_words <- sapply(strsplit(scores$essay, "\\s+"), length)
  scores$total_sentences <- sapply(strsplit(scores$essay, "[.!?]"), length)
  scores$total_paragraphs <- sapply(strsplit(scores$essay, "\\n+"), length)
  scores$avg_word_length <- scores$total_char / scores$total_words
  scores$avg_sentence_length <- scores$total_words / scores$total_sentences
  scores$avg_paragraph_length <- scores$total_sentences / scores$total_paragraphs
  scores$word_paragraph_ratio <- scores$total_words / scores$total_paragraphs
  scores$one_letter_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) == 1)) / scores$total_words
  scores$two_letter_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) == 2)) / scores$total_words
  scores$three_letter_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) == 3)) / scores$total_words
  scores$four_letter_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) == 4)) / scores$total_words
  scores$five_letter_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) == 5)) / scores$total_words
  scores$six_letter_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) == 6)) / scores$total_words
  scores$seven_letter_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) == 7)) / scores$total_words
  scores$eight_plus_word_ratio <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(nchar(x) > 7)) / scores$total_words
  scores$comma_count_2 <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(grepl(",", x)))
  scores$comma_ratio <- scores$comma_count / scores$total_words
  scores$comma_sentence_ratio <- scores$comma_count / scores$total_sentences
  scores$punctuation_count_2 <- sapply(strsplit(scores$essay, "\\s+"), function(x) sum(grepl("[[:punct:]]", x)))
  scores$punctuation_ratio <- scores$punctuation_count / scores$total_words
  scores$punctuation_sentence_ratio <- scores$punctuation_count / scores$total_sentences
  scores$clause_count <- sapply(strsplit(scores$essay, "[.!?,:;(){}[]]"), function(x) sum(grepl(",", x)))
  scores$clause_ratio <- scores$clause_count / scores$total_sentences
  scores$two_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 2)) / scores$total_sentences
  scores$three_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 3)) / scores$total_sentences
  scores$four_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 4)) / scores$total_sentences
  scores$five_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 5)) / scores$total_sentences
  scores$six_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 6)) / scores$total_sentences
  scores$seven_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 7)) / scores$total_sentences
  scores$eight_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 8)) / scores$total_sentences
  scores$nine_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 9)) / scores$total_sentences
  scores$ten_plus_word_sentence_ratio <- sapply(strsplit(scores$essay, "[.!?]"), function(x) sum(sapply(strsplit(x, "\\s+"), length) == 10)) / scores$total_sentences
  scores$unique_word_count <- sapply(strsplit(scores$essay, "\\s+"), function(x) length(unique(x)))
  return(scores)
}

train_scores <- create_essay_features(train_scores)
cor(train_scores[, sapply(train_scores, is.numeric)])

create_features <- function(logs, scores) {

  total_events <- function(logs, scores) {
    result <- aggregate(event_id ~ id, data = logs, FUN = max)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "event_id"] <- "total_events"
    return(result)
  }
  
  event_diversity <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) length(unique(x)))
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "event_diversity"
    return(result)
  }
  
  normalized_event_diversity <- function(scores) {
    scores$normalized_event_diversity <- scores$event_diversity / scores$total_events
    return(scores)
  }
  
  submitted_words <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = function(x) tail(x, 1))
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "submitted_words"
    return(result)
  }
  
  submission_complexity <- function(scores) {
    scores$submission_complexity <- scores$submitted_words / scores$total_events
    return(scores)
  }
  
  max_words <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = max)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "max_words"
    return(result)
  }
  
  word_reduction <- function(scores) {
    scores$word_reduction <- scores$max_words - scores$submitted_words
    return(scores)
  }
  
  word_count_mean <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = mean)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "word_count_mean"
    return(result)
  }
  
  word_count_sd <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = sd)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "word_count_sd"
    return(result)
  }
  
  word_count_skew <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = skewness)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "word_count_skew"
    return(result)
  }
  
  word_count_kurt <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = kurtosis)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "word_count_kurt"
    return(result)
  }
  
  word_count_median <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = median)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "word_count_median"
    return(result)
  }
  
  word_count_IQR <- function(logs, scores) {
    result <- aggregate(word_count ~ id, data = logs, FUN = IQR)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "word_count"] <- "word_count_IQR"
    return(result)
  }
  
  submission_time <- function(logs, scores) {
    result <- aggregate(up_time ~ id, data = logs, FUN = max)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "up_time"] <- "submission_time"
    return(result)
  }
  
  first_input <- function(logs, scores) {
    result <- aggregate(down_time ~ id, data = logs[logs$activity == "Input", ], FUN = min)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "down_time"] <- "first_input"
    return(result)
  }
  
  writing_time <- function(scores) {
    scores$writing_time <- scores$submission_time - scores$first_input
    return(scores)
  }
  
  submit_word_rate <- function(scores) {
    scores$submit_word_rate <- scores$submitted_words / scores$writing_time
    return(scores)
  }
  
  max_word_rate <- function(scores) {
    scores$max_word_rate <- scores$max_words / scores$writing_time
    return(scores)
  }
  
  normalized_cursor_position <- function(logs, scores) {
    logs <- merge(scores[ , c("id", "submitted_words")], logs, by = "id", all.x = TRUE)
    logs$normalized_cursor_position <- logs$cursor_position / logs$submitted_words
    return(logs)
  }
  
  cursor_position_mean <- function(logs, scores) {
    result <- aggregate(cursor_position ~ id, data = logs, FUN = mean)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "cursor_position"] <- "cursor_position_mean"
    return(result)
  }
  
  cursor_position_sd <- function(logs, scores) {
    result <- aggregate(cursor_position ~ id, data = logs, FUN = sd)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "cursor_position"] <- "cursor_position_sd"
    return(result)
  }
  
  cursor_position_skew <- function(logs, scores) {
    result <- aggregate(cursor_position ~ id, data = logs, FUN = skewness)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "cursor_position"] <- "cursor_position_skew"
    return(result)
  }
  
  cursor_position_kurt <- function(logs, scores) {
    result <- aggregate(cursor_position ~ id, data = logs, FUN = kurtosis)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "cursor_position"] <- "cursor_position_kurt"
    return(result)
  }
  
  cursor_position_median <- function(logs, scores) {
    result <- aggregate(cursor_position ~ id, data = logs, FUN = median)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "cursor_position"] <- "cursor_position_median"
    return(result)
  }
  
  cursor_position_IQR <- function(logs, scores) {
    result <- aggregate(cursor_position ~ id, data = logs, FUN = IQR)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "cursor_position"] <- "cursor_position_IQR"
    return(result)
  }
  
  cursor_position_max <- function(logs, scores) {
    result <- aggregate(cursor_position ~ id, data = logs, FUN = max)
    result <- merge(result, scores, by = "id", all.x = TRUE)
    names(result)[names(result) == "cursor_position"] <- "cursor_position_max"
    return(result)
  }
  
  word_reduction <- function(scores) {
    scores$word_reduction <- scores$max_words - scores$submitted_words
    return(scores)
  }
  
  input_count <- function(logs, scores) {
    result <- aggregate(activity ~ id, data = logs, FUN = function(x) sum(x == "Input"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "activity"] <- "input_count"
    return(result)
  }
  
  cut_count <- function(logs, scores) {
    result <- aggregate(activity ~ id, data = logs, FUN = function(x) sum(x == "Remove/Cut"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "activity"] <- "cut_count"
    return(result)
  }
  
  paste_count <- function(logs, scores) {
    result <- aggregate(activity ~ id, data = logs, FUN = function(x) sum(x == "Paste"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "activity"] <- "paste_count"
    return(result)
  }
  
  move_count <- function(logs, scores) {
    result <- aggregate(activity ~ id, data = logs, FUN = function(x) sum(x == "Move"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "activity"] <- "move_count"
    return(result)
  }
  
  replace_count <- function(logs, scores) {
    result <- aggregate(activity ~ id, data = logs, FUN = function(x) sum(x == "Replace"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "activity"] <- "replace_count"
    return(result)
  }
  
  nonproduction_count <- function(logs, scores) {
    result <- aggregate(activity ~ id, data = logs, FUN = function(x) sum(x == "Nonproduction"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "activity"] <- "nonproduction_count"
    return(result)
  }
  
  input_frequency <- function(scores) {
    scores$input_frequency <- scores$input_count / scores$total_events
    return(scores)
  }
  
  nonproduction_frequency <- function(scores) {
    scores$nonproduction_frequency <- scores$nonproduction_count / scores$total_events
    return(scores)
  }
  
  cut_frequency <- function(scores) {
    scores$cut_frequency <- scores$cut_count / scores$total_events
    return(scores)
  }
  
  cut_input_ratio <- function(scores) {
    scores$cut_input_ratio <- scores$cut_count / scores$input_count
    return(scores)
  }
  
  input_rate <- function(scores) {
    scores$input_rate <- scores$input_count / scores$writing_time
    return(scores)
  }
  
  input_productivity <- function(scores) {
    scores$input_productivity <- scores$input_count / scores$submitted_words
    return(scores)
  }
  
  period_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == "."))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "period_count"
    return(result)
  }
  
  comma_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == ","))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "comma_count"
    return(result)
  }
  
  punctuation_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == "." | x == "," | x == "!" | x == "?" | x == ":" | x == ";"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "punctuation_count"
    return(result)
  }
  
  space_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == "Space"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "space_count"
    return(result)
  }
  
  enter_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == "Enter"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "enter_count"
    return(result)
  }
  
  deletion_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == "Delete" | x == "Backspace" | x == "c"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "deletion_count"
    return(result)
  }
  
  shift_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == "Shift"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "shift_count"
    return(result)
  }
  
  tab_count <- function(logs, scores) {
    result <- aggregate(event ~ id, data = logs, FUN = function(x) sum(x == "Tab"))
    result <- merge(scores, result, by = "id", all.x = TRUE)
    names(result)[names(result) == "event"] <- "tab_count"
    return(result)
  }
  
  punctuation_frequency <- function(scores) {
    scores$punctuation_frequency <- scores$punctuation_count / scores$submitted_words
    return(scores)
  }
  
  deletion_frequency <- function(scores) {
    scores$deletion_frequency <- scores$deletion_count / scores$submitted_words
    return(scores)
  }
  
  shift_frequency <- function(scores) {
    scores$shift_frequency <- scores$shift_count / scores$submitted_words
    return(scores)
  }
  
  tab_frequency <- function(scores) {
    scores$tab_frequency <- scores$tab_count / scores$submitted_words
    return(scores)
  }
  
  enter_frequency <- function(scores) {
    scores$enter_frequency <- scores$enter_count / scores$submitted_words
    return(scores)
  }
  
  period_frequency <- function(scores) {
    scores$period_frequency <- scores$period_count / scores$submitted_words
    return(scores)
  }
  
  comma_frequency <- function(scores) {
    scores$comma_frequency <- scores$comma_count / scores$submitted_words
    return(scores)
  }
  
  space_frequency <- function(scores) {
    scores$space_frequency <- scores$space_count / scores$submitted_words
    return(scores)
  }
  
  scores <- total_events(logs, scores)
  scores <- event_diversity(logs, scores)
  scores <- normalized_event_diversity(scores)
  scores <- submitted_words(logs, scores)
  scores <- submission_complexity(scores)
  scores <- max_words(logs, scores)
  scores <- word_reduction(scores)
  scores <- word_count_mean(logs, scores)
  scores <- word_count_sd(logs, scores)
  scores <- word_count_median(logs, scores)
  scores <- word_count_IQR(logs, scores)
  scores <- word_count_skew(logs, scores)
  scores <- word_count_kurt(logs, scores)
  scores <- submission_time(logs, scores)
  scores <- first_input(logs, scores)
  scores <- writing_time(scores)
  scores <- submit_word_rate(scores)
  scores <- max_word_rate(scores)
  logs <- normalized_cursor_position(logs, scores)
  scores <- cursor_position_mean(logs, scores)
  scores <- cursor_position_sd(logs, scores)
  scores <- cursor_position_median(logs, scores)
  scores <- cursor_position_IQR(logs, scores)
  scores <- cursor_position_skew(logs, scores)
  scores <- cursor_position_kurt(logs, scores)
  scores <- cursor_position_max(logs, scores)
  scores <- input_count(logs, scores)
  scores <- cut_count(logs, scores)
  scores <- paste_count(logs, scores)
  scores <- move_count(logs, scores)
  scores <- nonproduction_count(logs, scores)
  scores <- replace_count(logs, scores)
  scores <- input_frequency(scores)
  scores <- nonproduction_frequency(scores)
  scores <- cut_frequency(scores)
  scores <- cut_input_ratio(scores)
  scores <- input_rate(scores)
  scores <- input_productivity(scores)
  scores <- period_count(logs, scores)
  scores <- comma_count(logs, scores)
  scores <- punctuation_count(logs, scores)
  scores <- space_count(logs, scores)
  scores <- enter_count(logs, scores)
  scores <- deletion_count(logs, scores)
  scores <- shift_count(logs, scores)
  scores <- tab_count(logs, scores)
  scores <- punctuation_frequency(scores)
  scores <- deletion_frequency(scores)
  scores <- shift_frequency(scores)
  scores <- tab_frequency(scores)
  scores <- enter_frequency(scores)
  scores <- period_frequency(scores)
  scores <- comma_frequency(scores)
  scores <- space_frequency(scores)
  
  return(scores)
}

train_scores <- create_features(train_logs, train_scores)
cor(train_scores[, sapply(train_scores, is.numeric)])

# Convert data to lightgbm dataset format
train_data <- lightgbm::lgb.Dataset(data = as.matrix(train_scores[, !(names(train_scores) %in% c("score", "id", "essay"))]), label = train_scores$score)

# Set up LGBM parameters
train_params <- list(
    objective = "regression",
    metric = "rmse",
    boosting_type = "gbdt",
    num_leaves = 14,
    learning_rate = 0.05,
    feature_fraction = 0.83,
    nthread = 6
)

# Cross-Validation
set.seed(123)
cv_results <- lightgbm::lgb.cv(
    params = train_params,
    data = train_data,
    nrounds = 250,
    stratified = TRUE,
    shuffle = TRUE,
    nfold = 8
)
print(cv_results)

# Train LGBM Model
classifier_LGBM <- lightgbm::lgb.train(
    params = train_params,
    data = train_data,
    nrounds = cv_results$best_iter
)

# Plot feature importance
importance <- lgb.importance(classifier_LGBM, percentage = TRUE)
print(importance[1:30])
lgb.plot.importance(importance, top_n = 30, measure = "Gain")

test_logs <- data_cleaning(test_logs)
test_scores <- get_essay(test_logs, test_scores)
test_scores <- create_essay_features(test_scores)
test_scores <- create_features(test_logs, test_scores)

cat("\n", "\n", "TRAIN_SCORES VARIABLES:", "\n")
print(names(train_scores))

cat("\n", "\n", "TEST_SCORES VARIABLES:", "\n")
print(names(test_scores))

print(classifier_LGBM)

test_scores[test_scores == "NaN"] <- 0
test_scores[test_scores == "Inf"] <- 0
test_scores[is.na(test_scores)] <- 0

test_predictions <- predict(classifier_LGBM, as.matrix(test_scores[, names(test_scores) != "id" & names(test_scores) != "essay"]))
sample_submission <- data.frame(id = test_scores$id, score = test_predictions)

write_csv(sample_submission, "submission.csv")
