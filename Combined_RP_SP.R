# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_RP_SP",
  modelDescr      = "RP-SP model on mode choice data",
  indivID         = "id", 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package

database = read.csv("M3.csv",header=TRUE)
### for data dictionary, use ?apollo_modeChoiceData

### Create new variable with average income
#database$mean_income = mean(database$income)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #


apollo_beta=c(asc_car                 = 0,
              asc_bus                 = -5,
              asc_tw                 = -6,
              asc_metro                = -41,
              b_tt                   =1,
              b_transfers_bus         =1,
              b_cost                   =1,
              b_crowd                  =1,
              mu_RP                   = 1,
              mu_SP                   = 0.5)
### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","mu_RP")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (before applying scales): these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  #                           
  V[["metro"]] = asc_metro + b_tt * TT_METRO *WORK +b_cost* COST_METRO/INCOME
  V[["bus"]]  = asc_bus  + b_tt  * (IVTT_BUS  + OVTT_BUS)*WORK  + b_transfers_bus*TRANSFERS+ b_cost *COST_BUS/INCOME +b_crowd *CROWD 
  V[["car"]]  = asc_car + b_cost *COST_CAR/INCOME+TT_DRIVE*b_tt*WORK
  V[["tw"]]=     asc_tw + b_tt *TT_DRIVE*WORK +b_cost* COST_TW/INCOME

  
  ### Compute probabilities for the RP part of the data using MNL model
  mnl_settings_RP = list(
    alternatives  = c(metro=1, bus=2, car=3, tw=4), 
    avail         = list(metro=avail_metro, bus=avail_bus, car=avail_car, tw=avail_tw), 
    choiceVar     = CHALT, 
    utilities     = list(metro = mu_RP*V[["metro"]],
                         bus  = mu_RP*V[["bus"]],
                         car  = mu_RP*V[["car"]],
                         tw = mu_RP*V[["tw"]]),
                         #walk=  mu_RP* V[["walk"]]),
    rows          = (RP==1)
  
  )
  P[["RP"]] = apollo_mnl(mnl_settings_RP, functionality)
  
  ### Compute probabilities for the SP part of the data using MNL model
  mnl_settings_SP = list(
    alternatives  = c(metro=1, bus=2, car=3, tw=4), 
    avail         = list(metro=avail_metro, bus=avail_bus, car=avail_car, tw=avail_tw), 
    choiceVar     = CHALT, 
    utilities     = list(
                         metro = mu_SP*V[["metro"]],
                         bus  = mu_SP*V[["bus"]],
                         car  = mu_SP*V[["car"]],
                         tw=  mu_SP* V[["tw"]]),
                         #walk  = mu_SP*V[["walk"]]),
                         
                         
    rows          = (SP==1)
  )
  
  P[["SP"]] = apollo_mnl(mnl_settings_SP, functionality)
  
  ### Combined model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #
# 
# apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
# apollo_sink()

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #

### RP elasticities

## Use the estimated model to make predictions
predictions_base = apollo_prediction(model,
                                     apollo_probabilities,
                                     apollo_inputs,
                                     prediction_settings=list(modelComponent = "RP"))

### Look at a summary of the predicted choice probabilities
summary(predictions_base)

## Now imagine the cost for bus increases by 10%
database$IVTT_BUS =0.85 *database$IVTT_BUS
# database$COST_CAR=1.02* database$COST_CAR

## Rerun predictions with the new data, and save into a separate matrix
apollo_inputs=apollo_validateInputs()
predictions_new = apollo_prediction(model,
                                    apollo_probabilities,
                                    apollo_inputs,
                                    prediction_settings=list(modelComponent = "RP"))

### Look at a summary of the predicted choice probabilities
summary(predictions_new)

## Return to original data
database$IVTT_BUS =(1/0.85)*database$IVTT_BUS
# database$COST_CAR=(1/1.02)* database$COST_CAR

### Compute own elasticity for rail:
#log(sum(predictions_new[,6],na.rm=TRUE)/sum(predictions_base[,6],na.rm=TRUE))/log(1.01)

### SP elasticities

### Use the estimated model to make predictions
# apollo_inputs=apollo_validateInputs()
# predictions_base = apollo_prediction(model, 
#                                      apollo_probabilities, 
#                                      apollo_inputs,
#                                      prediction_settings=list(modelComponent = "SP"))
# 
# ### Look at a summary of the predicted choice probabilities
# summary(predictions_base)

### Now imagine the cost for rail increases by 10%
#database$cost_rail = 1.01*database$cost_rail

### Rerun predictions with the new data, and save into a separate matrix
# apollo_inputs=apollo_validateInputs()
# predictions_new = apollo_prediction(model, 
#                                     apollo_probabilities, 
#                                     apollo_inputs,
#                                     prediction_settings=list(modelComponent = "SP"))
# 
# ### Look at a summary of the predicted choice probabilities
# summary(predictions_new)

### Return to original data
#database$cost_rail = 1/1.01*database$cost_rail

### Compute own elasticity for rail:
#log(sum(predictions_new[,6],na.rm=TRUE)/sum(predictions_base[,6],na.rm=TRUE))/log(1.01)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()