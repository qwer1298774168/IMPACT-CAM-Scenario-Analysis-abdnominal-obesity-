# IMPACT-CAM-Scenario-Analysis-abdnominal-obesity-

## Code
This describes how to use IMPACT-CAM to simulate a predictive model estimating the number of cases and prevalence of CVD, dementia, and disability among Chinese adults aged 65 and above from 2020 to 2050 under different obesity-intervention scenarios.


“Function code updated” comprises all functions required in IMPACT-CAM, including initialization of Markov states, annual transition probabilities (TPs), and computation of future disease-burden states through 2050.

Load_china: Imports the raw data and assigns them to each Markov state at the baseline year (2015).

RunModelMultithrd_China: Runs the model based on transition probabilities, UN population projections, and other relevant parameters.

Scenario analysis uses the functions within Function code updated to run code files under different scenarios. By adjusting the TPs in these scripts, one can obtain the disease burden associated with 


## Data
### IMPACT-CAM Initial Model Data (R data)

UN2022_update_UI.RDS: UN 2022 mortality projections, using the Constant-assumption forecast.

Prevalence for IMPACT-CAM eight states_20230401_60dementia.RDS: Baseline allocation of each Markov state, including initial state assignments and disease prevalence.

CHARLS - Incidence for IMPACT-CAM_20230403_Harm.RDS: Transition probabilities.

prop_BAM_HR_nweight_0317.RDS: Ratio of CVD to non-CVD mortality, used to decompose total mortality in each Markov state into CVD and non-CVD components.

Pop structure (China UN 2022 ref).RDS: UN-projected population data for 2015–2050.

### Adjusted Transition Probability Data (Scenario data)

AO_constant.csv: Prevalence under the Optimal scenario.

AO_intervened.csv: Prevalence under the Improved scenario.

AO_Lag0.csv: Included relative risks.
