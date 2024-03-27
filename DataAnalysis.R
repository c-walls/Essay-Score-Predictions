library(ggplot2)
library(moments)

###  UNIVARIATE ANALYSIS  ###

###Score###
ggplot(train_scores, aes(x = score)) +
  geom_bar(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "Score", y = "Count", title = "Scores Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

score_distribution <- table(train_scores$score)
View(score_distribution)
View(round(prop.table(score_distribution)*100, 2))

train_scores$score_numeric <- as.numeric(as.character(train_scores$score))
nrow(train_scores[train_scores$score != train_scores$score_numeric, ])

summary(train_scores$score_numeric)
sd(train_scores$score_numeric)
skewness(train_scores$score_numeric)
kurtosis(train_scores$score_numeric)


###Total Events###
ggplot(train_scores, aes(x = total_events)) +
  geom_histogram(binwidth = 250, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "total_events", y = "Count", title = "Histogram of Total Events per ID") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_scores, aes(x = total_events)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "total_events", y = "", title = "Boxplot of Total Events per ID") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$total_events)
sd(train_scores$total_events)
skewness(train_scores$total_events)
kurtosis(train_scores$total_events)


###Normalized Event Diversity###
ggplot(train_scores, aes(x = normalized_event_diversity)) +
  geom_histogram(binwidth = .0005, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "normalized_event_diversity", y = "Count", title = "Histogram of Normalized Event Diversity per ID") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_scores, aes(x = normalized_event_diversity)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "normalized_event_diversity", y = "", title = "Boxplot of Normalized Event Diversity per ID") +
  theme(plot.title = element_text(hjust = 0.5))

###Submission Time###
ggplot(train_scores, aes(x = submission_time)) +
  geom_histogram(binwidth = 1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "submission_time", y = "Count", title = "Histogram of Submission Time per ID") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_scores, aes(x = submission_time)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "submission_time", y = "", title = "Boxplot of Submission Time per ID") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$submission_time)
sd(train_scores$submission_time)
skewness(train_scores$submission_time)
kurtosis(train_scores$submission_time)

###First Action###
ggplot(train_scores, aes(x = first_action)) +
  geom_histogram(binwidth = .5, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "first_action", y = "Count", title = "Histogram of First Action per ID") +
  theme(plot.title = element_text(hjust = 0.5))

###Writing Time###
ggplot(train_scores, aes(x = writing_time)) +
  geom_histogram(binwidth = 1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "writing_time", y = "Count", title = "Histogram of Writing Time per ID") +
  theme(plot.title = element_text(hjust = 0.5))


###Action Time###
ggplot(train_logs, aes(x = action_time)) +
  geom_histogram(binwidth = 25, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "action_time", y = "Count", title = "Histogram of Action Time") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_logs, aes(x = action_time)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "action_time", y = "", title = "Boxplot of Action Time") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_logs, aes(x = action_time)) +
  geom_density(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "action_time", y = "", title = "Density Plot of Action Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_sqrt()

summary(train_logs$action_time)
sd(train_logs$action_time)
skewness(train_logs$action_time)
kurtosis(train_logs$action_time)

##Faceted Action Time - Initial Logs##
ggplot(initial_logs, aes(x = action_time)) +
  geom_histogram(binwidth = 25, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "action_time", y = "Count", title = "Histogram of Action Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~id, scales = "free")


##initial_logs varaible will be used to facet data by id when analyzing some raw data
initial_logs <- train_logs[train_logs$id == unique(train_logs$id)[1:6], ]


##Cursor Position##
ggplot(train_logs, aes(x = cursor_position)) +
  geom_histogram(binwidth = 50, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position", y = "Count", title = "Histogram of Cursor Position") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_logs, aes(x = cursor_position)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position", y = " ", title = "Boxplot of Cursor Position") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_logs$cursor_position)
sd(train_logs$cursor_position)
skewness(train_logs$cursor_position)
kurtosis(train_logs$cursor_position)

##Faceted Cursor Position - Initial Logs##
ggplot(initial_logs, aes(x = cursor_position)) +
  geom_histogram(binwidth = 50, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position", y = "Count", title = "Histogram of Cursor Position") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~id, scales = "free")

##Normalized Cursor Position##
ggplot(train_logs, aes(x = normalized_cursor_position)) +
  geom_histogram(binwidth = 0.1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "normalized_cursor_position", y = "Count", title = "Histogram of Normalized Cursor Position") +
  theme(plot.title = element_text(hjust = 0.5))




###Word Count###
ggplot(train_logs, aes(x = word_count)) +
  geom_histogram(binwidth = 25, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "word_count", y = "Count", title = "Histogram of Word Count") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_logs, aes(x = word_count)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "word_count", y = " ", title = "Boxplot of Word Count") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_logs$word_count)
sd(train_logs$word_count)
skewness(train_logs$word_count)
kurtosis(train_logs$word_count)

##Faceted Word Count - Initial Logs##
ggplot(initial_logs, aes(x = word_count)) +
  geom_histogram(binwidth = 25, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "word_count", y = "Count", title = "Histogram of Word Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~id, scales = "free")


###Submitted Words###
ggplot(train_scores, aes(x = submitted_words)) +
  geom_histogram(binwidth = 25, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "submitted_words", y = "Count", title = "Histogram of Submitted Words per ID") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_scores, aes(x = submitted_words)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "submitted_words", y = "", title = "Boxplot of Submitted Words per ID") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$submitted_words)
sd(train_scores$submitted_words)
skewness(train_scores$submitted_words)
kurtosis(train_scores$submitted_words)




##Visualize extra cursor position metrics by histogram##
ggplot(train_scores, aes(x = cursor_position_sd)) +
  geom_histogram(binwidth = 0.1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position_sd", y = "Count", title = "Histogram of Cursor Position SD") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$cursor_position_sd)
sd(train_scores$cursor_position_sd)
skewness(train_scores$cursor_position_sd)
kurtosis(train_scores$cursor_position_sd)

ggplot(train_scores, aes(x = cursor_position_mean)) +
  geom_histogram(binwidth = 0.1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position_mean", y = "Count", title = "Histogram of Cursor Position Mean") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$cursor_position_mean)
sd(train_scores$cursor_position_mean)
skewness(train_scores$cursor_position_mean)
kurtosis(train_scores$cursor_position_mean)

ggplot(train_scores, aes(x = cursor_position_skewness)) +
  geom_histogram(binwidth = 0.1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position_skewness", y = "Count", title = "Histogram of Cursor Position Skewness") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$cursor_position_skewness)
sd(train_scores$cursor_position_skewness)
skewness(train_scores$cursor_position_skewness)
kurtosis(train_scores$cursor_position_skewness)

ggplot(train_scores, aes(x = cursor_position_kurtosis)) +
  geom_histogram(binwidth = 0.1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position_kurtosis", y = "Count", title = "Histogram of Cursor Position Kurtosis") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$cursor_position_kurtosis)
sd(train_scores$cursor_position_kurtosis)
skewness(train_scores$cursor_position_kurtosis)
kurtosis(train_scores$cursor_position_kurtosis)

ggplot(train_scores, aes(x = cursor_position_median)) +
  geom_histogram(binwidth = 0.1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position_median", y = "Count", title = "Histogram of Cursor Position Median") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$cursor_position_median)
sd(train_scores$cursor_position_median)
skewness(train_scores$cursor_position_median)
kurtosis(train_scores$cursor_position_median)

ggplot(train_scores, aes(x = cursor_position_IQR)) +
  geom_histogram(binwidth = 0.1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position_IQR", y = "Count", title = "Histogram of Cursor Position IQR") +
  theme(plot.title = element_text(hjust = 0.5))

summary(train_scores$cursor_position_IQR)
sd(train_scores$cursor_position_IQR)
skewness(train_scores$cursor_position_IQR)
kurtosis(train_scores$cursor_position_IQR)

ggplot(train_scores, aes(x = cursor_position_max)) +
  geom_histogram(binwidth = 300, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position_max", y = "Count", title = "Histogram of cursor") +
  theme(plot.title = element_text(hjust = 0.5))

##Activity
ggplot(train_logs, aes(x = activity)) +
  geom_bar(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "activity", y = "Count", title = "Barplot of Activity") +
  theme(plot.title = element_text(hjust = 0.5))

activity_frequency <- merge(table(train_logs$activity), round(prop.table(table(train_logs$activity))*100, 2), by = "Var1")
View(activity_frequency)

##Event
ggplot(train_logs, aes(x = event)) +
  geom_bar(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "event", y = "Count", title = "Barplot of Event") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_y_sqrt()

event_frequency <- merge(table(train_logs$event), round(prop.table(table(train_logs$event))*100, 4), by = "Var1")
View(event_frequency)

#Event Diversity
ggplot(train_scores, aes(x = event_diversity)) +
  geom_histogram(binwidth = 1, color = "#767B7D", fill = "#66BBDD") +
  labs(x = "event_diversity", y = "Count", title = "Histogram of Event Diversity") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_scores, aes(x = event_diversity)) +
  geom_boxplot(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "event_diversity", y = "Count", title = "Boxplot of Event Diversity") +
  theme(plot.title = element_text(hjust = 0.5))


summary(train_scores$event_diversity)
sd(train_scores$event_diversity)
skewness(train_scores$event_diversity)
kurtosis(train_scores$event_diversity)

### BIVARIATE ANALYSIS ###
#Numerical correlation table

correlation_matrix <- as.data.frame(as.table(cor(train_scores[, c(2:11, 13)])))

#Visualize correlation matrix

ggplot(correlation_matrix, aes(Var1, Var2, fill = Freq, label = round(Freq, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black", size = 3) +
  scale_fill_gradient(low = "#FFFFFF", high = "#66BBDD") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix",
       x = "Variables",
       y = "Variables")



###Score vs Total Events###

ggplot(train_scores, aes(x = total_events, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "total_events", y = "score", title = "Score vs Total Events") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Submission Time###

ggplot(train_scores, aes(x = submission_time, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "submission_time", y = "score", title = "Score vs Submission Time") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Writing Time###

ggplot(train_scores, aes(x = writing_time, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "writing_time", y = "score", title = "Score vs Writing Time") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs First Action###

ggplot(train_scores, aes(x = first_action, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "first_action", y = "score", title = "Score vs First Action") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Action Time###

ggplot(train_logs, aes(x = action_time, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "action_time", y = "score", title = "Score vs Action Time") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Cursor Position###

ggplot(train_logs, aes(x = cursor_position, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "cursor_position", y = "score", title = "Score vs Cursor Position") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Submitted Words###

ggplot(train_scores, aes(x = submitted_words, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "submitted_words", y = "score", title = "Score vs Submitted Words") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Event Diversity###
ggplot(train_scores, aes(x = event_diversity, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "event_diversity", y = "score", title = "Score vs Event Diversity") +
  theme(plot.title = element_text(hjust = 0.5))

###Cursor Position vs. Event ID###
ggplot(initial_logs, aes(x = event_id, y = cursor_position)) +
  geom_line(color = "#767B7D", fill = "#66BBDD") +
  labs(x = "cursor_position", y = "event_id", title = "Cursor Position vs. Event ID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~id, scales = "free")

###Score vs Cursor Position Mean###
ggplot(train_scores, aes(x = cursor_position_mean, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "cursor_position_mean", y = "score", title = "Score vs Cursor Position Mean") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Cursor Position Median###
ggplot(train_scores, aes(x = cursor_position_median, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "cursor_position_median", y = "score", title = "Score vs Cursor Position Median") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Cursor Position IQR###
ggplot(train_scores, aes(x = cursor_position_IQR, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "cursor_position_IQR", y = "score", title = "Score vs Cursor Position IQR") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Cursor Position SD###
ggplot(train_scores, aes(x = cursor_position_sd, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "cursor_position_SD", y = "score", title = "Score vs Cursor Position SD") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Cursor Position Skewness###
ggplot(train_scores, aes(x = cursor_position_skewness, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "cursor_position_skewness", y = "score", title = "Score vs Cursor Position Skewness") +
  theme(plot.title = element_text(hjust = 0.5))

###Score vs Cursor Position Kurtosis###
ggplot(train_scores, aes(x = cursor_position_kurtosis, y = score)) +
  geom_point(color = "#767B7D") +
  labs(x = "cursor_position_kurtosis", y = "score", title = "Score vs Cursor Position Kurtosis") +
  theme(plot.title = element_text(hjust = 0.5))

###Activity vs Event###
ggplot(train_logs, aes(x = event, fill = activity)) +
  geom_bar(position = "fill") +
  labs(x = "event", y = "Count", title = "Activity vs Event") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

contengency_table <- as.data.frame(table(train_logs$event, train_logs$activity))

ggplot(contengency_table, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "#FFFFFF", high = "#66BBDD")
  labs(x = "Event", y = "Activity", title = "Heatmap: Activity vs Event") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

###Action_time vs Event###
ggplot(train_logs, aes(x = event, y = action_time)) +
  geom_boxplot() +
  labs(x = "event", y = "action_time", title = "Action_time vs Event") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_y_sqrt()

###Action_time vs Activity###
ggplot(train_logs, aes(x = activity, y = action_time)) +
  geom_boxplot() +
  labs(x = "activity", y = "action_time", title = "Action_time vs Activity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()