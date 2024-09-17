
train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.
  
  # Parameters:
  #cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  #outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).
  
  ## This script contains a bare minimum working example
  #set.seed(1) # not useful here because logistic regression deterministic
  
  # Combine cleaned_df and outcome_df
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")
  
  # Logistic regression model
  model <- logistf (new_child ~ gender_bg*(age+migration+education+field_edu+occupation+income+income_log+urban+urban_delta+owner+health+dist_fr_parents+next_child+numb_child+first_birth+marriage_dur+partner+partner_delta+partner_dur+partner_type+partner_type_delta+rela_satisfied+housework+relig1+sm_fa), data = model_df) 

  # Save the model
  saveRDS(model, "model.rds")
}


train_cleaned <- clean_df(train)

# training and saving the model
train_save_model(train_cleaned, outcome)
