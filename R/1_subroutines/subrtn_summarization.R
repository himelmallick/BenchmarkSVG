
#######################################
##SUMMARISE REPLICATE X METHOD RESULTS#
#######################################

summary_scenario <- function(Output_replicate){
  
  
  ########################
  ##CREATE SCENARIO LIST##
  ########################
  
  Output_replicate$scenario= paste(Output_replicate$pattern, Output_replicate$zero_inflation, sep = "_")
  Output_replicate= data.frame(Method = unique(Output_replicate$methods),
                               scenario = unique(Output_replicate$scenario),
                               Sensitivity = as.numeric(Output_replicate$Sensitivity),
                               Specificity = as.numeric(Output_replicate$Specificity),
                               F1_score = as.numeric(Output_replicate$F1_score),
                               FDR = as.numeric(Output_replicate$FDR),
                               AUC = as.numeric(Output_replicate$AUC),
                               MCC = as.numeric(Output_replicate$MCC),
                               Time = as.numeric(Output_replicate$Time)
                               # replicate_count = length(unique(Output_replicate$scenario))
  )
  
  
  ########################
  ##SUMMARY BY SCENARIO###
  ########################
  
  
  df_grp_region = Output_replicate %>% group_by(Output_replicate$scenario, Output_replicate$Method) %>%
    summarise(Sensitivity = median(Sensitivity), 
              Specificity = median(Specificity),
              F1_score = median(F1_score),
              FDR = median(FDR),
              AUC = median(AUC),
              MCC = median(MCC),
              Time = median(Time),
              replicate_count = length(scenario),
              .groups = 'drop')
  
  View(df_grp_region)
  
  
  return(df_grp_region)
}
