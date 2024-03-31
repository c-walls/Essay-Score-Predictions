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

1. **Data Preprocessing**: Cleaning the data, converting data types, and reformatting in preparation for modeling.
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

I developed several predictive models to estimate essay scores of the participants based on their keystroke data. All models were evaluated based on the RMSE of the predictions that they generated. A few of my best models are listed below and have been included in this repository:

- LightGBM using cross validation and feature selection (RMSE: **0.613** on the Kaggle Leaderboard test set / **0.639** on cross-validation)
- LightGBM using cross validation, feature selection, and a custom essay reconstructor function with related features (RMSE: **0.615** on cross-validation)
- Random Forest using hyperparameter tuning (RMSE: **0.633** on the Kaggle Leaderboard test set)

The winning submission for this Kaggle competition had a 0.559 RMSE. Many of the top leaderboard teams utilized a custom essay reconstructor function similar to the one that I built for my LGBM_CV+FS+EssayReconstructor.ipynb model. While this essay reconstructor function and its corresponding features significantly improved my cross-validation RMSE, It caused an unknown error on the leaderboard test set and I ran out of time before resolving the issue. Other techniques used by top-scoring teams, which I plan to incorporate in future competitions, include ensembling, TF-IDF vectors to quantify the significance of certain words within the reconstructed essays, and the inclusion of external data in the training set. 
