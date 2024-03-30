# Essay-Score-Predictions

This repository contains a comprehensive data analysis report, visualizations, and predictive models developed for a Kaggle competition. The goal of the analysis was to understand the correlation between typing behavior and writing quality, using a dataset compiled by Vanderbilt University's Learning Agency Lab.

## Dataset Overview

The dataset includes typing event logs and essay scores from 2471 participants, totaling almost 8.5 million rows of observations. The letter keystrokes were all anonymized as "q" in the logs in order to ensure that predictive models were based on writing behavior rather than writing output. The main variables of interest are:

- `down_time` and `up_time`: Timestamps for each typing event.
- `action_time`: The duration of the event
- `activity`: The type of activity (e.g., input, move, paste).
- `event`: The specific keystroke or mouse event.
- `text_change`: The alteration in the text due to the event.
- `cursor_position`: The location of the text cursor at event completion.
- `word_count`: The count of words in the essay at the time of event completion.
- `score`: The score assigned to the essay, used as the dependent variable.

## Data Analysis

The analysis consists of several stages:

1. **Data Preprocessing**: Cleaning the data, converting data types, and reformatting in preperation for modeling.
2. **Exploratory Data Analysis (EDA)**: Univariate and bivariate analysis, including visualizations of distributions and correlations.
3. **Feature Engineering**: Deriving metrics from the data, and selecting the most relevant for modeling.
4. **Model Selection**: Choosing appropriate models for predicting essay scores based on typing behavior.
5. **Model Evaluation**: Assessing the performance of predictive models using various metrics.

## Tools and Libraries Used

- **R**
- **RStudio**
- **Google Colab**
- **ggplot2**: An R library for creating visualizations.
- **moments**: An R library for calculating descriptive statistics.
- **caret**: An R library for machine learning and predictive modeling.
- **LightGBM**: A gradient boosting framework that uses tree-based learning algorithms.
- **RandomForest**: An ensemble learning method that constructs multiple decision trees for prediction.

## Predictive Models

I developed several predictive models to estimate essay scores of the participants based on their keystroke data.

- Random Forest using hyperparameter tuning (RMSE: )
- LightGBM using cross validation and feature selection (RMSE: )
- LightGBM using cross validation, feature selection, and a custom essay reconstructor algorithm (RMSE: )
