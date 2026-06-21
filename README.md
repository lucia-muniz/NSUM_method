# Analysis of Gender Inequalities in Caregiving Using the Network Scale-Up Method (NSUM)

[cite_start]This repository contains the complete codebase, data processing pipelines, and predictive modeling framework developed for my Bachelor's Thesis (Trabajo Fin de Grado) in Statistics and Business at **Universidad Carlos III de Madrid (UC3M)**.

## 📌 Project Overview
[cite_start]Traditional surveys measuring gender inequality in domestic tasks and caregiving workloads often suffer from **social desirability bias**, where respondents tend to over- or under-report behaviors to match socially accepted norms[cite: 3, 42]. [cite_start]This research leverages the **Network Scale-Up Method (NSUM)**—an indirect survey estimation technique—within the framework of the multidisciplinary research project **CuidaNSUM**[cite: 4, 43]. 

[cite_start]Instead of asking respondents about their own personal routines, NSUM asks how many individuals within their personal network exhibit specific traits or behaviors[cite: 4, 47]. [cite_start]By mathematically adjusting these responses according to the estimated size of each respondent's personal network, we can infer subpopulation dimensions with significantly reduced bias[cite: 5, 47].

[cite_start]Additionally, this project builds a robust comparative framework between traditional (direct) and indirect question formulations using classical frequentist estimators and advanced supervised **Machine Learning** techniques[cite: 7].

---

## 🛠️ Methodology & Statistical Models

### 1. NSUM Frequentist Estimation
[cite_start]The implementation relies on frequentist models to estimate hidden subpopulation proportions[cite: 124]. Specifically, we utilize the **Mean of Ratios (MoR)** estimator to evaluate specific caregiving and work-life balance indicators:
* [cite_start]**Mean of Ratios (MoR):** Equalizes the weight given to each individual in the sample, reducing the distortion caused by respondents with exceptionally large network sizes[cite: 144, 145].
$$\hat{N}_{MoR\_u} = N \cdot \frac{1}{n} \sum_{i=1}^{n} \frac{y_{iu}}{\hat{d}_i}$$

### 2. Machine Learning Predictive Pipeline
[cite_start]To analyze the structural divergence and predictive relationship between direct and indirect questionnaire responses, we framed the data as a classification challenge[cite: 7, 252]. [cite_start]We model direct response behaviors (e.g., whether an individual experiences a specific caregiving conflict) using the indirect metrics harvested from their social network[cite: 251, 252]:
* [cite_start]**Lasso Regularization (Logistic Regression):** Implemented to handle high-dimensional survey features, eliminate multicollinearity, and perform embedded variable selection[cite: 7, 269].
* [cite_start]**Random Forest:** Employed to capture non-linear relationships and interactions between network indicators, evaluated via *Mean Decrease Accuracy* and *Mean Decrease Gini*[cite: 7, 294, 443].
* [cite_start]**Support Vector Machines (SVM):** Tuned with class weighting and dynamic decision threshold shifts to overcome high class imbalance[cite: 266].
