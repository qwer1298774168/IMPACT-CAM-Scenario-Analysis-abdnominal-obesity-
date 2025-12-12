# IMPACT-CAM-Scenario-Analysis-abdnominal-obesity-

## Code
This describes how to use IMPACT-CAM to simulate a predictive model estimating the number of cases and prevalence of CVD, dementia, and disability among Chinese adults aged 65 and above from 2020 to 2050 under different obesity-intervention scenarios.


“Function code updated” comprises all functions required in IMPACT-CAM, including initialization of Markov states, annual transition probabilities (TPs), and computation of future disease-burden states through 2050.

Load_china: Imports the raw data and assigns them to each Markov state at the baseline year (2015).

RunModelMultithrd_China: Runs the model based on transition probabilities, UN population projections, and other relevant parameters.

Scenario analysis uses the functions within Function code updated to run code files under different scenarios. By adjusting the TPs in these scripts, one can obtain the disease burden associated with 


## Data
The raw data used in this study include CHALRS, CLHLS, GBD, WPP, and CHNS. All of these databases can be freely accessed by applying through their official websites. The input data for this study were derived from these databases using the methods described in the appendix.

China Health Aging and Retirement Longitudinal Study (CHALRS): https://charls.pku.edu.cn/en/

Chinese Longitudinal Healthy Longevity Study (CLHLS): https://opendata.pku.edu.cn/dataverse/CHADS

United Nations World Population Prospects 2022 (UN 2022): https://population.un.org/wpp/

Global Burden of Diseases (GBD): https://vizhub.healthdata.org/gbd-results/

China Health and Nutrition Survey (CHNS): https://dataverse.unc.edu/dataverse/chns
### IMPACT-CAM Initial Model Data (R data)

UN2022_update_UI.RDS: UN 2022 mortality projections, using the Constant-assumption forecast.

Prevalence for IMPACT-CAM eight states_20230401_60dementia.RDS: Baseline allocation of each Markov state, including initial state assignments and disease prevalence.

CHARLS - Incidence for IMPACT-CAM_20230403_Harm.RDS: Transition probabilities.

prop_CAM_HR_nweight_0317.RDS: Ratio of CVD to non-CVD mortality, used to decompose total mortality in each Markov state into CVD and non-CVD components.

Pop structure (China UN 2022 ref).RDS: UN-projected population data for 2015–2050.

Other datasets are primarily used for the internal validation of IMPACT-CAM and are not within the scope of this study.

### Adjusted Transition Probability Data (Scenario data)

AO_constant.csv: Prevalence under the Optimal scenario.

AO_intervened.csv: Prevalence under the Improved scenario.

AO_Lag0.csv: Included relative risks.
