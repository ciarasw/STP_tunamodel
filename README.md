# STP_tunamodel
Model to accompany "Harvest Control Rules for Tuna in the WCPFC: Implications for Indonesia, Philippines, and Vietnam" (Bailey, Willis, et al. 2023) for WWF Sustainable Tuna Partnership

Start with base model (Basic IN-PH-VN fishing.R). This runs the model under to generate an approximation of current real conditions. It needs to read in the parameter file tuna_setupLeMaRns.csv
The base model code generates two data files that are used as input to the harvest scenarios: Base_model_LHparams.RData (life history parameters) and base_model_N0.RData (starting biomasses per species)

Each scenario is run in its own code file (Scenario - X.R)
Comparison plots among scenarios are generated in Scenario Comparison scripts. The 3 skipjack sub-scenarios are plotted separately from other scenarios for ease of visualization

-CW 2023.09.25
