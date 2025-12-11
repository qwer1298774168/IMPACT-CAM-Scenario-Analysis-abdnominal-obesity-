# IMPACT-CAM-Scenario-Analysis-abdnominal-obesity-
This describes how to use IMPACT-CAM to simulate a predictive model estimating the number of cases and prevalence of CVD, dementia, and disability among Chinese adults aged 65 and above from 2020 to 2050 under different obesity-intervention scenarios.


“Function code updated” comprises all functions required in IMPACT-CAM, including initialization of Markov states, annual transition probabilities (TPs), and computation of future disease-burden states through 2050.

Load_china: Imports the raw data and assigns them to each Markov state at the baseline year (2015).

RunModelMultithrd_China: Runs the model based on transition probabilities, UN population projections, and other relevant parameters.

Scenario analysis uses the functions within Function code updated to run code files under different scenarios. By adjusting the TPs in these scripts, one can obtain the disease burden associated with 
