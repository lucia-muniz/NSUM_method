# Analysis of Gender Inequalities in Caregiving Using the Network Scale-Up Method (NSUM)

This repository contains the complete codebase, data processing pipelines, and predictive modeling framework developed for my Bachelor's Thesis (Trabajo Fin de Grado) in Statistics and Business at **Universidad Carlos III de Madrid (UC3M)**.

##  Project Overview
Traditional surveys measuring gender inequality in domestic tasks and caregiving workloads often suffer from **social desirability bias**, where respondents tend to over- or under-report behaviors to match socially accepted norms. This research leverages the **Network Scale-Up Method (NSUM)**—an indirect survey estimation technique—within the framework of the multidisciplinary research project **CuidaNSUM**. 

Instead of asking respondents about their own personal routines, NSUM asks how many individuals within their personal network exhibit specific traits or behaviors. By mathematically adjusting these responses according to the estimated size of each respondent's personal network, we can infer subpopulation dimensions with significantly reduced bias.

Additionally, this project builds a robust comparative framework between traditional (direct) and indirect question formulations using classical frequentist estimators and advanced supervised **Machine Learning** techniques.

---

##  Methodology & Statistical Models

### 1. NSUM Frequentist Estimation
The implementation relies on frequentist models to estimate hidden subpopulation proportions. Specifically, we utilize the **Mean of Ratios (MoR)** estimator to evaluate specific caregiving and work-life balance indicators:
**Mean of Ratios (MoR):** Equalizes the weight given to each individual in the sample, reducing the distortion caused by respondents with exceptionally large network sizes.
$$\hat{N}_{MoR\_u} = N \cdot \frac{1}{n} \sum_{i=1}^{n} \frac{y_{iu}}{\hat{d}_i}$$

### 2. Statistical Inference, Correlations & Visualizations
To validate the reliability of the NSUM estimates and uncover structural patterns, the analysis integrates rigorous statistical testing and custom data visualizations:
* **Hypothesis Testing & Distribution Analysis:** Non-parametric methods—such as the **Wilcoxon Signed-Rank Test**—are applied to evaluate significant differences between genders regarding social perceptions and responsibilities. The distribution and density of response behaviors are thoroughly checked via **Q-Q Plots** and normality diagnostics.
* **Correlation Framework:** **Spearman's Rank Correlation** is utilized to assess the relationships between the respondents' network sizes and indirect query metrics, mapping out how social circles influence the visibility of caregiving tasks.
* **Stratified Graphics & Bias Detection:** The repository features data visualization scripts using `ggplot2` to generate stratified scatterplots and breakdown charts (e.g., analyzing indirect distributions like `Q17` through `Q34` against criteria variables like `Q45` or `Q48`). These plots visually isolate the divergence between direct and indirect questionnaire layers, highlighting the impact of social desirability bias.

### 3. Machine Learning Predictive Pipeline
To analyze the structural divergence and predictive relationship between direct and indirect questionnaire responses, we framed the data as a classification challenge. We model direct response behaviors (e.g., whether an individual experiences a specific caregiving conflict) using the indirect metrics harvested from their social network:
**Lasso Regularization (Logistic Regression):** Implemented to handle high-dimensional survey features, eliminate multicollinearity, and perform embedded variable selection.
**Random Forest:** Employed to capture non-linear relationships and interactions between network indicators, evaluated via *Mean Decrease Accuracy* and *Mean Decrease Gini*.
**Support Vector Machines (SVM):** Tuned with class weighting and dynamic decision threshold shifts to overcome high class imbalance. Employed to capture non-linear relationships and interactions between network indicators, evaluated via *Permutation Importance*.
