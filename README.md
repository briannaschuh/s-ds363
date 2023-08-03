# Multivariate Exploratory Analysis on the Premier League

Welcome to the **Multivariate Exploratory Analysis on the Premier League** repository! This project focuses on performing an in-depth analysis of player statistics from FIFA 20 to gain insights into the categorization of soccer players based on various attributes. This project was completed as a requirement for a Yale course, S&DS 363: Multivariate Statistical Methods.

## Table of Contents
- [Introduction](#introduction)
- [Design and Primary Questions](#design-and-primary-questions)
- [Dataset](#dataset)
- [Results and Conclusions](#results-and-conclusions)
- [Points for Further Analysis](#points-for-further-analysis)
- [Files](#files)
- [License](#license)

## Introduction
FIFA 20 is a popular soccer simulation video game that mirrors real-world gameplay. This project explores player statistics from FIFA 20 to analyze and categorize soccer players based on attributes such as overall skill, wage, mentality composure, and movement agility. These player statisitcs are based off the respective soccer player's real-life gameplay in the year prior.

## Design and Primary Questions
The primary objective of this analysis is to determine if soccer players can be categorized by their positions using key attributes. The project investigates differences in overall skill, wage, mentality composure, and movement agility across forward, midfielder, and defender positions. Multivariate analysis of variance (MANOVA), discriminant analysis, and pricipal component analysis (PCA) are the statistical methods used in this project.


## Dataset
The analysis is performed on a subset of the FIFA 20 dataset, focusing on Premier League clubs during the 2019 – 2020 season. The dataset includes a range of player statistics, from age and height to attacking and defensive skills.

## Results and Conclusions
The analysis reveals significant differences in mean wage, mentality composure, and movement agility based on player positions. However, the attempts to categorize players through these variables were not successful, which suggests that the relationship between these variables are not easily defined, and thus more complex models will need to be used for further research. 

## Points for Further Analysis
While the initial analysis provides valuable insights, future investigations could delve deeper into the impact of variables such as age on movement agility and mentality composure. Exploring interactions among variables and employing more sophisticated models, like Principal Component Regression, could lead to better results of player categorization.

## Files
- [premier-league-scratchwork.Rmd](premier-league-scratchwork.Rmd) - R Markdown document containing project scratchwork and exploratory analysis.
- [final-project-script.R](final-project-script.R) - R script with the final analysis code and modeling.
Both files contain the same R code. The only difference is that first file is an R Markdown and the second file is an R script.

## License
This project is licensed under the [MIT License](LICENSE).

---

*Disclaimer: This project is based on data available up to 2021. The analysis and conclusions might not reflect more recent developments.*
