


#install.packages(c("dplyr","data.table","tidyr"), repos="https://cran.r-project.org")
#library(c("dplyr","data.table","tidyr"))

#library(data.table) 


#overall steps:
#1. Read in data
#a. read in training data and outcome
#b. strongly advised to save datafiles separate from local repository so they are not accidently made public through a push function to GitHub
#c. The code to read-in your data is the only code you don't need to document through your repository

#data = read.csv('Z:/PreFer/training_data/PreFer_train_data.csv', 
#                header=TRUE,sep=",")

#outcome = read.csv('Z:/PreFer/training_data/PreFer_train_outcome.csv', 
#                   header=TRUE,sep=",")


#train <- data.table::fread("Z:/PreFer/training_data/PreFer_train_data.csv",
  #                         keepLeadingZeros = TRUE, # if FALSE adds zeroes to some dates
   #                        data.table = FALSE)

#outcome <- data.table::fread("Z:/PreFer/training_data/PreFer_train_outcome.csv",
    #                         data.table = FALSE) 


#2. Preprocess data
# total <- merge(data,outcome,by="nomem_encr")
#total <- total[total$outcome_available==1,]
#myvars <- c("nomem_encr", "new_child","gender_bg","migration_background_bg","age_bg","partner_2020","cf20m128")
#newdata <- total[myvars]


clean_df <- function(df, background_df = NULL) {
  # Process the input data to feed the model
  
  # Age as a categorical variable
  df$age <- NA
  df[(!is.na(df$age_bg) & df$age_bg<=20),]$age <- "_20"
  df[(!is.na(df$age_bg) & (df$age_bg>=21 & df$age_bg<=23)),]$age <- "21_23"
  df[!is.na(df$age_bg) & (df$age_bg>=24 & df$age_bg<=26),]$age <- "24_26"
  df[!is.na(df$age_bg) & (df$age_bg>=27 & df$age_bg<=29),]$age <- "27_29"
  df[!is.na(df$age_bg) & (df$age_bg>=30 & df$age_bg<=31),]$age <- "30_31"
  df[!is.na(df$age_bg) & (df$age_bg>=32 & df$age_bg<=35),]$age <- "32_35"
  df[!is.na(df$age_bg) & (df$age_bg>=36 & df$age_bg<=40),]$age <- "36_40"
  df[!is.na(df$age_bg) & (df$age_bg>=41),]$age <- "41+"
  
  # Migration as a categorical variable
  df$migration_background_bg[is.na(df$migration_background_bg)] <- 999
  df$migration <- "NA"
  df[df$migration_background_bg==0,]$migration <- "Dutch"
  df[df$migration_background_bg==101,]$migration <- "gen_1_west"
  df[df$migration_background_bg==102,]$migration <- "gen_1_non_west"
  df[df$migration_background_bg==201,]$migration <- "gen_2_west"
  df[df$migration_background_bg==202,]$migration <- "gen_2_non_west"
  
  # Get education level
  df$education <- NA
  df[(df$oplmet_2020==1 | df$oplmet_2020==8 | df$oplmet_2020==9) & !is.na(df$oplmet_2020),]$education <- "1_prim_8_9"
  df[df$oplmet_2020==2 & !is.na(df$oplmet_2020),]$education <- "2_sec_intermed"
  df[df$oplmet_2020==3 & !is.na(df$oplmet_2020),]$education <- "3_sec_high"
  df[df$oplmet_2020==4 & !is.na(df$oplmet_2020),]$education <- "4_voc_intermed"
  df[df$oplmet_2020==5 & !is.na(df$oplmet_2020),]$education <- "5_voc_high"
  df[df$oplmet_2020==6 & !is.na(df$oplmet_2020),]$education <- "6_uni"
  df[df$oplmet_2020==7 | is.na(df$oplmet_2020),]$education <- "7_other_missing"
  
  # Get education field
  df$field_edu <- "NA"
  df[!is.na(df$cw20m011) & df$cw20m011==1,]$field_edu <- "general"
  df[!is.na(df$cw20m012) & df$cw20m012==1,]$field_edu <- "teaching"
  df[!is.na(df$cw20m013) & (df$cw20m013==1),]$field_edu <- "art_humanities"
  df[!is.na(df$cw20m014) & (df$cw20m014==1),]$field_edu <- "art_humanities"
  df[!is.na(df$cw20m015) & df$cw20m015==1,]$field_edu <- "social"
  df[!is.na(df$cw20m016) & df$cw20m016==1,]$field_edu <- "economics"
  df[!is.na(df$cw20m017) & df$cw20m017==1,]$field_edu <- "law"
  df[!is.na(df$cw20m018) & df$cw20m018==1,]$field_edu <- "math"
  df[!is.na(df$cw20m019) & (df$cw20m019==1),]$field_edu <- "technology_agri"
  df[!is.na(df$cw20m020) & (df$cw20m020==1),]$field_edu <- "technology_agri"
  df[!is.na(df$cw20m025) & (df$cw20m025==1),]$field_edu <- "technology_agri"
  df[!is.na(df$cw20m021) & df$cw20m021==1,]$field_edu <- "1_health"
  df[!is.na(df$cw20m022) & df$cw20m022==1,]$field_edu <- "personal_care"
  df[!is.na(df$cw20m023) & df$cw20m023==1,]$field_edu <- "catering"
  df[!is.na(df$cw20m024) & (df$cw20m024==1),]$field_edu <- "transport_public_order"
  df[!is.na(df$cw20m026) & (df$cw20m026==1),]$field_edu <- "transport_public_order"
  df[!is.na(df$cw20m027) & df$cw20m027==1,]$field_edu <- "other"
  
  # Get occupation
  df$occupation <- "NA"
  df[!is.na(df$ci20m383) & (df$ci20m383==1 | df$ci20m383==2),]$occupation <- "employed"
  df[!is.na(df$ci20m383) & (df$ci20m383==3),]$occupation <- "self-employed"
  df[!is.na(df$ci20m383) & (df$ci20m383==7),]$occupation <- "student"
  df[!is.na(df$ci20m383) & (df$ci20m383==4 | df$ci20m383==5  | df$ci20m383==6 |df$ci20m383==8 |  df$ci20m383==9 | df$ci20m383==10 | df$ci20m383==11 | df$ci20m383==12 | df$ci20m383==13),]$occupation <- "not_employed"
  
  # Get income, imput missing values with mean income
  df$income <- df$nettohh_f_2020
  df[is.na(df$nettohh_f_2020),]$income <-  mean(df$nettohh_f_2020, na.rm=TRUE)
  
  # Add how satisfied individuals are with their finances
  df$satisfied_own_finance <- "NA"
  df[!is.na(df$ci20m006) & (df$ci20m006==0 | df$ci20m006==1 | df$ci20m006==2 | df$ci20m006==3),]$satisfied_own_finance <- "0123_satisfied"
  df[!is.na(df$ci20m006) & (df$ci20m006==4 | df$ci20m006==5),]$satisfied_own_finance <- "45_satisfied"
  df[!is.na(df$ci20m006) & (df$ci20m006==6),]$satisfied_own_finance <- "6_satisfied"
  df[!is.na(df$ci20m006) & (df$ci20m006==7),]$satisfied_own_finance <- "7_satisfied"
  df[!is.na(df$ci20m006) & (df$ci20m006==8),]$satisfied_own_finance <- "8_satisfied"
  df[!is.na(df$ci20m006) & (df$ci20m006==9 | df$ci20m006==10),]$satisfied_own_finance <- "910_satisfied"
  
  # Add whether the respondent is an owner of its current dwelling
  df$owner <- "NA"
  df[!is.na(df$cd20m003) & (df$cd20m003==1 |df$cd20m003==2 | df$cd20m003==4),]$owner <- "1_no"
  df[!is.na(df$cd20m003) & df$cd20m003==3,]$owner <- "2_yes"
  
  # Generate relationship satisfaction
  df$rela_satisfied <- "8_NA"
  df[!is.na(df$cf20m166) & df$cf20m166<=4,]$rela_satisfied <- "7_single_dissatisfied"
  df[!is.na(df$cf20m166) & df$cf20m166>=5 & df$cf20m166<=7,]$rela_satisfied <- "6_single_neutral"
  df[!is.na(df$cf20m166) & df$cf20m166>=8,]$rela_satisfied <- "4_single_very_satisfied"
  df[!is.na(df$cf20m180) & df$cf20m180<=6,]$rela_satisfied <- "3_relation_less_satisfied"
  df[!is.na(df$cf20m180) & df$cf20m180>=7 & df$cf20m180<=8,]$rela_satisfied <- "2_relation_satisfied"
  df[!is.na(df$cf20m180) & df$cf20m180>=9,]$rela_satisfied <- "1_relation_very_satisfied"
  
  # Generate health
  df$health <- "NA"
  df[!is.na(df$ch20m004) & (df$ch20m004==1 | df$ch20m004==2),]$health <- "12_poor_moderate"
  df[!is.na(df$ch20m004) & df$ch20m004==3,]$health <- "3_good"
  df[!is.na(df$ch20m004) & df$ch20m004==4,]$health <- "4_very_good"
  df[!is.na(df$ch20m004) & df$ch20m004==5,]$health <- "5_excellent"
  
  # Generate religiousness
  df$relig <- "5_NA"
  df[!is.na(df$cr20m041) & (df$cr20m041==1 | df$cr20m041==2 | df$cr20m041==3) ,]$relig <- "4_>once_week"
  df[!is.na(df$cr20m041) & (df$cr20m041==4) ,]$relig <- ">3_once_month"
  df[!is.na(df$cr20m041) & (df$cr20m041==5) ,]$relig <- ">2_few_per_year"
  df[!is.na(df$cr20m041) & (df$cr20m041==6) ,]$relig <- "1_never"
  
  # Generate personality
  df[is.na(df$cp20l026),]$cp20l026 <- "999"
  df[is.na(df$cp20l023),]$cp20l023 <- "999"
  df$personality <- "NA"
  df[(df$cp20l026==1 | df$cp20l026==2 | df$cp20l026==3) & (df$cp20l023==1 | df$cp20l023==2 | df$cp20l023==3),]$personality <- "less_social_less_stressed"
  df[(df$cp20l026==4 | df$cp20l026==5) & (df$cp20l023==1 | df$cp20l023==2 | df$cp20l023==3),]$personality <- "more_social_less_stressed"
  df[(df$cp20l026==1 | df$cp20l026==2 | df$cp20l026==3) & (df$cp20l023==4 | df$cp20l023==5),]$personality <- "less_social_more_stressed"
  df[(df$cp20l026==4 | df$cp20l026==5) & (df$cp20l023==4 | df$cp20l023==5),]$personality <- "more_social_more_stressed"
  
  # Generate living arrangement
  df$urban <- "6_NA"
  df[!is.na(df$sted_2020) & df$sted_2020==1,]$urban <- "1_extreme_urban"
  df[!is.na(df$sted_2020) & df$sted_2020==2,]$urban <- "2_very_urban"
  df[!is.na(df$sted_2020) & df$sted_2020==3,]$urban <- "3_mod_urban"
  df[!is.na(df$sted_2020) & df$sted_2020==4,]$urban <- "4_slight_urban"
  df[!is.na(df$sted_2020) & df$sted_2020==5,]$urban <- "5_not_urban"
  
  # Generate help from parents
  df[is.na(df$cf20m134),]$cf20m134 <- "999"
  df[is.na(df$cf20m133),]$cf20m133 <- "999"
  df$help_fr_parents <- "NA"
  df[(df$cf20m134==3 | df$cf20m133==3),]$help_fr_parents <- "several_times"
  df[(df$cf20m134==2 | df$cf20m133==2) & (df$cf20m134!=3 & df$cf20m133!=3),]$help_fr_parents <- "once_twice"
  df[(df$cf20m134==1 | df$cf20m133==1) & (df$cf20m134!=2 & df$cf20m133!=2 & df$cf20m134!=3 & df$cf20m133!=3),]$help_fr_parents <- "never"
  
  # Generate fertility intentions 
  df$cf20m130[is.na(df$cf20m130)] <- 999
  df$cf20m128[is.na(df$cf20m128)] <- 999
  df$next_child <- "NA"
  df[df$cf20m130<=1,]$next_child <- "<1_year"
  df[df$cf20m130>=2 & df$cf20m130<=3,]$next_child <- "2_3_year"
  df[df$cf20m130>=4,]$next_child <- ">4_year"
  df[df$cf20m130==999,]$next_child <- "NA"
  df[df$cf20m128==2,]$next_child <- "no"
  
  # First birth
  df$cf20m456[is.na(df$cf20m456)] <- 999
  df$cf20m454[is.na(df$cf20m454)] <- 999
  df$first_birth <- "NA"
  df[df$cf20m456>=2019 & df$cf20m456<=2020,]$first_birth <- "2019_2020"
  df[df$cf20m456>=2017 & df$cf20m456<=2018,]$first_birth <- "2017_2018"
  df[df$cf20m456>=2014 & df$cf20m456<=2016,]$first_birth <- "2014_2016"
  df[df$cf20m456>=2010 & df$cf20m456<=2013,]$first_birth <- "2010_2013"
  df[df$cf20m456<=2009,]$first_birth <- "<2009"
  df[df$cf20m456==999,]$first_birth <- "NA"
  df[df$cf20m454==2,]$first_birth <- "no_children"
  
  # Generate partner duration
  df$cf20m029[is.na(df$cf20m029)] <- 999
  df$partner_dur <- "NA"
  df[df$cf20m029>=2018 & df$cf20m029<=2020,]$partner_dur <- "2018_2020"
  df[df$cf20m029>=2015 & df$cf20m029<=2017,]$partner_dur <- "2015_2017"
  df[df$cf20m029>=2011 & df$cf20m029<=2014,]$partner_dur <- "2011_2014"
  df[df$cf20m029>=2006 & df$cf20m029<=2010,]$partner_dur <- "2006_2010"
  df[df$cf20m029<=2005,]$partner_dur <- "_2005"
  df[df$cf20m029==999,]$partner_dur <- "NA"
  
  # Generate marriage duration
  df$cf20m031[is.na(df$cf20m031)] <- 999
  df$marriage_dur <- "NA"
  df[df$cf20m031>=2018 & df$cf20m031<=2020,]$marriage_dur <- "2018_2020"
  df[df$cf20m031>=2014 & df$cf20m031<=2017,]$marriage_dur <- "2014_2017"
  df[df$cf20m031>=2009 & df$cf20m031<=2013,]$marriage_dur <- "2009_2013"
  df[df$cf20m031<=2008,]$marriage_dur <- "_2008"
  df[df$cf20m031==999,]$marriage_dur <- "NA"
  
  
  keepcols = c('nomem_encr', # ID variable required for predictions,
               'age', 
               'gender_bg',
               'migration',
               'education',
               'field_edu',
               'occupation',
               'income',
               'satisfied_own_finance',
               'owner',
               'rela_satisfied',
               'health',
               'relig',
               'personality',
               'urban',
               'help_fr_parents',
               'next_child',
               'first_birth',
               'partner_dur',
               'marriage_dur')  
  
  # Keeping data with variables selected
  df <- df[ , keepcols ]
  
  # turning gender into factor
  df$gender_bg<- as.factor(df$gender_bg) 
  
  return(df)
}



#####################

#train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.
  
  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).
  
  ## This script contains a bare minimum working example
 # set.seed(1) # not useful here because logistic regression deterministic
  
  # Combine cleaned_df and outcome_df
  #model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")
  
  # Logistic regression model
  #model <- glm(new_child ~ gender_bg*(age+migration+education+occupation+satisfied_own_finance+owner+income+rela_satisfied+health+relig+personality+urban+help_fr_parents+next_child+first_birth+partner_dur+marriage_dur+field_edu), family = "binomial", data = model_df) 
  
  # Save the model
  #saveRDS(model, "model.rds")
#}


###############

#setwd("Z:/PreFer/") 

#train_cleaned <- clean_df(train)

# training and saving the model
#train_save_model(train_cleaned, outcome)


###############################
#setwd("Z:/PreFer/other_data/") 


#fake <- data.table::fread("PreFer_fake_data.csv",
#                          keepLeadingZeros = TRUE, # if FALSE adds zeroes to some dates
#                          data.table = FALSE)

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  # Generate predictions using the saved model and the input dataframe.
  
  # The predict_outcomes function accepts a dataframe as an argument
  # and returns a new dataframe with two columns: nomem_encr and
  # prediction. The nomem_encr column in the new dataframe replicates the
  # corresponding column from the input dataframe The prediction
  # column contains predictions for each corresponding nomem_encr. Each
  # prediction is represented as a binary value: '0' indicates that the
  # individual did not have a child during 2021-2023, while '1' implies that
  # they did.
  
  # Parameters:
  # df (dataframe): The data dataframe for which predictions are to be made.
  # background_df (dataframe): The background data dataframe for which predictions are to be made.
  # model_path (str): The path to the saved model file (which is the output of training.R).
  
  # Returns:
  # dataframe: A dataframe containing the identifiers and their corresponding predictions.
  
  ## This script contains a bare minimum working example
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }
  
  # Load the model
  model <- readRDS(model_path)
  
  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)
  
  # Exclude the variable nomem_encr if this variable is NOT in your model
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  
  # Generate predictions from model
  predictions <- predict(model, 
                         subset(df, select = vars_without_id), 
                         type = "response") 
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return( df_predict )
}



###################################

