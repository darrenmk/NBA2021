# NBA2021
Raw data and code to replicate the data analysis, figures, and statistics for Kenney et al. (submitted) "Visual-vestibular integration is preserved with healthy aging in a simple acceleration detection task"

React4.m reads the raw data and formats it, outputting two variables: React4Data.csv which is the data for each subject, and CDFs.mat which contain the CDFs required to plot the Race Model Inequality in Figure 2.
React4.R reads in React4Data.csv and CDFs.mat, and reproduces the statistics and figures.
