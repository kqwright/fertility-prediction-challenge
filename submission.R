
library(data.table)
library(data.table)
library(stats)


#overall steps:
#1. Read in data
#a. read in training data and outcome
#b. strongly advised to save datafiles separate from local repository so they are not accidently made public through a push function to GitHub



#2. Preprocess data
#total <- merge(data,outcome,by="nomem_encr")
#total <- total[total$outcome_available==1,]
myvars <- c("nomem_encr", "new_child","gender_bg","migration_background_bg","age_bg","partner_2020","cf20m128", "oplmet_2020", "cr20m041", "cr20m042", "ch20m004", "ch20m219", "sted_2020", "nettohh_f_2020", "cw20m011", "cw20m012", "cw20m013", "cw20m014", "cw20m015", "cw20m016", "cw20m017", "cw20m018", "cw20m019", "cw20m020", "cw20m021", "cw20m022", "cw20m023", "cw20m024", "cw20m025", "cw20m026", "cw20m027", "ci20m383", "cf20m180", "cf20m166", "cf20m031", "ci20m006", "cd20m003", "cf18k184", "cp20l029", "cp20l030", "cp20l031", "cp20l032", "cp20l033", "cp20l034", "cp20l035", "cp20l036", "cp20l037", "cp20l038", "cp20l039", "cp20l040", "cp20l041", "cp20l042", "cp20l043", "cp20l044", "cp20l045", "cp20l046", "cp20l047", "cp20l048", "cp20l049", "cp20l050", "cp20l051", "cp20l052", "cp20l053", "cp20l054", "cp20l055", "cp20l056", "cp20l057", "cp20l058", "cp20l059", "cp20l060", "cp20l061", "cp20l062", "cp20l063", "cp20l064", "cp20l065", "cp20l066", "cp20l067", "cp20l068", "cp20l069")
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
  df$migration <- NA
  df[df$migration_background_bg==0,]$migration <- "Dutch"
  df[df$migration_background_bg==101,]$migration <- "gen_1_west"
  df[df$migration_background_bg==102,]$migration <- "gen_1_non_west"
  df[df$migration_background_bg==201,]$migration <- "gen_2_west"
  df[df$migration_background_bg==202,]$migration <- "gen_2_non_west"
  
#Get education level
  df$oplcat_2020 [is.na(df$oplcat_2020)] <- 999 
  df$education <- NA
  df[df$oplcat_2020==1 & !is.na(df$oplcat_2020),]$education <- "1_primary"
  df[df$oplcat_2020==2 & !is.na(df$oplcat_2020),]$education <- "2_sec_intermed"
  df[df$oplcat_2020==3 & !is.na(df$oplcat_2020),]$education <- "3_sec_senior"
  df[df$oplcat_2020==4 & !is.na(df$oplcat_2020),]$education <- "4_voc_intermed"
  df[df$oplcat_2020==5 & !is.na(df$oplcat_2020),]$education <- "5_voc_senior"
  df[df$oplcat_2020==6 & !is.na(df$oplcat_2020),]$education <- "6_uni"
  df[df$oplcat_2020==999 | is.na(df$oplcat_2020),]$education <- "other_missing"
  
# Get education field
  df$field_edu <- NA
  df[!is.na(df$cw20m011) & df$cw20m011==1,]$field_edu <- "other"
  df[!is.na(df$cw20m012) & df$cw20m012==1,]$field_edu <- "teaching"
  df[!is.na(df$cw20m013) & df$cw20m013==1,]$field_edu <- "other"
  df[!is.na(df$cw20m014) & df$cw20m014==1,]$field_edu <- "other"
  df[!is.na(df$cw20m015) & df$cw20m015==1,]$field_edu <- "other"
  df[!is.na(df$cw20m016) & df$cw20m016==1,]$field_edu <- "other"
  df[!is.na(df$cw20m017) & df$cw20m017==1,]$field_edu <- "other"
  df[!is.na(df$cw20m018) & df$cw20m018==1,]$field_edu <- "other"
  df[!is.na(df$cw20m019) & df$cw20m019==1,]$field_edu <- "other"
  df[!is.na(df$cw20m020) & df$cw20m020==1,]$field_edu <- "other"
  df[!is.na(df$cw20m025) & df$cw20m025==1,]$field_edu <- "other"
  df[!is.na(df$cw20m021) & df$cw20m021==1,]$field_edu <- "1_health"
  df[!is.na(df$cw20m022) & df$cw20m022==1,]$field_edu <- "other"
  df[!is.na(df$cw20m023) & df$cw20m023==1,]$field_edu <- "other"
  df[!is.na(df$cw20m024) & df$cw20m024==1,]$field_edu <- "other"
  df[!is.na(df$cw20m026) & df$cw20m026==1,]$field_edu <- "other"
  df[!is.na(df$cw20m027) & df$cw20m027==1,]$field_edu <- "other"
  
  
  #Get employment
  df$ci20m383[is.na(df$ci20m383)] <- 999
  df$occupation <- NA
  df[!is.na(df$ci20m383) & (df$ci20m383==1 | df$ci20m383==2),]$occupation <- "employed"
  df[!is.na(df$ci20m383) & (df$ci20m383==3),]$occupation <- "self-employed"
  df[!is.na(df$ci20m383) & (df$ci20m383==7),]$occupation <- "student"
  df[!is.na(df$ci20m383) & (df$ci20m383==4 | df$ci20m383==5  | df$ci20m383==6 |df$ci20m383==8 |  df$ci20m383==9 | df$ci20m383==10 | df$ci20m383==11 | df$ci20m383==12 | df$ci20m383==13),]$occupation <- "not_employed"
  df[!is.na(df$ci20m383) | (df$ci20m383==999),]$occupation <- "missing"

  
  # Get income, input missing values with mean income
  df$income <- df$nettohh_f_2020
  df[is.na(df$nettohh_f_2020),]$income <-  mean(df$nettohh_f_2020, na.rm=TRUE)
  income_log <- log(df$income)
  
  
  # Urban
  df$sted_2020[is.na(df$sted_2020)] <- 999  
  df$sted_2019[is.na(df$sted_2019)] <- 999  
  df$urban <- "NA"
  df[!is.na(df$sted_2020) & df$sted_2020==1,]$urban <- "1_extremely urban"
  df[!is.na(df$sted_2020) & df$sted_2020==2,]$urban <- "2_very urban"
  df[!is.na(df$sted_2020) & df$sted_2020==3,]$urban <- "3_moderately urban"
  df[!is.na(df$sted_2020) & df$sted_2020==4,]$urban <- "4_slightly urban"
  df[!is.na(df$sted_2020) & df$sted_2020==5,]$urban <- "5_slightly urban"
  df[!is.na(df$sted_2020) | df$sted_2020==999,]$urban <- "missing"
  
  #GENERATE URBAN DELTA 
  df$urban_delta <- 0
  df[df$sted_2020!=df$sted_2019,]$urban_delta <- 1 
  
  # Add how satisfied individuals are with their finances
  df$ci20m006[is.na(df$ci20m006)] <- 999 
   df$satisfied_own_finance <- "NA"
   df[!is.na(df$ci20m006) & (df$ci20m006==0 | df$ci20m006==1 | df$ci20m006==2 | df$ci20m006==3),]$satisfied_own_finance <- "0123_satisfied"
   df[!is.na(df$ci20m006) & (df$ci20m006==4 | df$ci20m006==5),]$satisfied_own_finance <- "45_satisfied"
   df[!is.na(df$ci20m006) & (df$ci20m006==6),]$satisfied_own_finance <- "6_satisfied"
   df[!is.na(df$ci20m006) & (df$ci20m006==7),]$satisfied_own_finance <- "7_satisfied"
   df[!is.na(df$ci20m006) & (df$ci20m006==8),]$satisfied_own_finance <- "8_satisfied"
   df[!is.na(df$ci20m006) & (df$ci20m006==9 | df$ci20m006==10),]$satisfied_own_finance <- "910_satisfied"
   df[!is.na(df$ci20m006) | df$ci20m006==999,]$satisfied_own_finance <- "missing"
   
   # Add whether the respondent is an owner of its current dwelling
   df$cd20m003[is.na(df$cd20m003)] <- 999 
  df$owner <- "NA"
   df[!is.na(df$cd20m003) & (df$cd20m003==1 |df$cd20m003==2 | df$cd20m003==4),]$owner <- "1_no"
   df[!is.na(df$cd20m003) & df$cd20m003==3,]$owner <- "2_yes"
   df[!is.na(df$cd20m003) | df$cd20m003==999,]$owner <- "missing"
  

  # Generate health
  df$ch20m004[is.na(df$ch20m004 )] <- 999
  df$health <- "NA"
  df[!is.na(df$ch20m004) & (df$ch20m004==1 | df$ch20m004==2),]$health <- "12_poor_moderate"
  df[!is.na(df$ch20m004) & df$ch20m004==3,]$health <- "3_good"
  df[!is.na(df$ch20m004) & df$ch20m004==4,]$health <- "4_very_good"
  df[!is.na(df$ch20m004) & df$ch20m004==5,]$health <- "5_excellent"
  df[!is.na(df$ch20m004) | df$ch20m004==999,]$health <- "missing"
  
  df$ch20m004[is.na(df$ch20m219 )] <- 999  
  df$gyno <- "NA"
  df[!is.na(df$ch20m219) & df$ch20m219==0,]$gyno <- "0_no"
  df[!is.na(df$ch20m219) & df$ch20m219==1,]$gyno <- "1_yes"
  df[!is.na(df$ch20m219) | df$ch20m219==999,]$gyno <- "missing"
  
  
  
  # Generate help from parents
  df$cf20m398[is.na(df$cf20m398 )] <- 999
  df$dist_fr_parents <- "NA"
  df$dist_fr_parents <- df$cf20m398
  df[(df$cf20m398==999),]$dist_fr_parents <- "missing"

  
  # Generate fertility intentions 
  #128: Do you think you will have [more] children in the future? yes, no, i don't know
  #129: How many [more] children do you think you will have in the future? continuous
  #130: Within how many years do you hope to have your [first/next] child?
  df$cf20m130[is.na(df$cf20m130)] <- 999
  df$cf20m128[is.na(df$cf20m128)] <- 999
  df$next_child <- "NA"
  df[df$cf20m130<=1,]$next_child <- "<1_year"
  df[df$cf20m130>=2 & df$cf20m130<=3,]$next_child <- "2_3_year"
  df[df$cf20m130>=4,]$next_child <- ">4_year"
  df[df$cf20m128=3 & df$cf20m130==999,]$next_child <- "Wants more, unknown time frame"
  df[df$cf20m130==999,]$next_child <- "missing"
  df[df$cf20m128==2,]$next_child <- "no"
  
  
  #Number of desired children
  df$cf20m129[is.na(df$cf20m129)] <- 999
  df$numb_child <- "NA"
  df[!is.na(df$cf20m129) & df$cf20m129==1,]$numb_child <- "1"
  df[!is.na(df$cf20m129) & df$cf20m129==2,]$numb_child <- "2"
  df[!is.na(df$cf20m129) & df$cf20m129>=3,]$numb_child <- "3 or more"
  df[!is.na(df$cf20m129) | df$cf20m129==999,]$numb_child <- "missing"
  
   
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
  
  
  # Generate marriage duration
  df$cf20m031[is.na(df$cf20m031)] <- 999
  df$marriage_dur <- "NA"
  df[df$cf20m031>=2018 & df$cf20m031<=2020,]$marriage_dur <- "2018_2020"
  df[df$cf20m031>=2014 & df$cf20m031<=2017,]$marriage_dur <- "2014_2017"
  df[df$cf20m031>=2009 & df$cf20m031<=2013,]$marriage_dur <- "2009_2013"
  df[df$cf20m031<=2008,]$marriage_dur <- "_2008"
  df[df$cf20m031==999,]$marriage_dur <- "Missing"

  # Currently have a partner
  df$cf20m024[is.na(df$cf20m024)] <- 999
  df$partner <- "NA"
  df[!is.na(df$cf20m024) & df$cf20m024==1,]$partner <- "1_yes"
  df[!is.na(df$cf20m024) & df$cf20m024==2,]$partner <- "2_no"
  df[!is.na(df$cf20m024) | df$cf20m024==999,]$partner <- "missing"
  
  #GENERATE partner delta
  df$partner_delta <- 0
  df[df$cf20m024!=df$cf19l024,]$partner_delta <- 1 
  
  # Generate partner duration
  df$cf20m028[is.na(df$cf20m028)] <- 999
  df$partner_dur <- "NA"
  df[df$cf20m028 >=2018 & df$cf20m028 <=2020,]$partner_dur <- "2018_2020"
  df[df$cf20m028 >=2015 & df$cf20m028 <=2017,]$partner_dur <- "2015_2017"
  df[df$cf20m028 >=2011 & df$cf20m028 <=2014,]$partner_dur <- "2011_2014"
  df[df$cf20m028 >=2006 & df$cf20m028 <=2010,]$partner_dur <- "2006_2010"
  df[df$cf20m028 <=2005 &  df$cf20m028 >999,]$partner_dur <- "_2005"
  df[df$cf20m028 ==999,]$partner_dur <- "Missing"
  

  # Generate partner living type
  df$woonvorm_2020[is.na(df$woonvorm_2020)] <- 999
  df$partner_type <- "NA"
  df[!is.na(df$woonvorm_2020) & df$woonvorm_2020==1,]$partner_type <- "1_single"
  df[!is.na(df$woonvorm_2020) & df$woonvorm_2020==2,]$partner_type <- "2_coh_no_child"
  df[!is.na(df$woonvorm_2020) & df$woonvorm_2020==3,]$partner_type <- "3_coh_w_child"
  df[!is.na(df$woonvorm_2020) & df$woonvorm_2020==4,]$partner_type <- "4_single_w_child"
  df[!is.na(df$woonvorm_2020) & df$woonvorm_2020==5,]$partner_type <- "5_other"
  df[!is.na(df$woonvorm_2020) | df$woonvorm_2020==999,]$partner_type <- "missing"
  

  #GENERATE partner living type delta
  df$partner_type_delta <- 0
  df[df$woonvorm_2020!=df$woonvorm_2019,]$partner_type_delta <- 1
  
  # Generate relationship satisfaction
  df$cf20m180[is.na(df$cf20m180)] <-999
  df$cf20m166[is.na(df$cf20m166)] <-999
  df$rela_satisfied <- "8_NA"
  df[!is.na(df$cf20m166) & df$cf20m166<=4,]$rela_satisfied <- "7_single_dissatisfied"
  df[!is.na(df$cf20m166) & df$cf20m166>=5 & df$cf20m166<=7,]$rela_satisfied <- "6_single_neutral"
  df[!is.na(df$cf20m166) & df$cf20m166>=8,]$rela_satisfied <- "4_single_very_satisfied"
  df[!is.na(df$cf20m180) & df$cf20m180<=6,]$rela_satisfied <- "3_relation_less_satisfied"
  df[!is.na(df$cf20m180) & df$cf20m180>=7 & df$cf20m180<=8,]$rela_satisfied <- "2_relation_satisfied"
  df[!is.na(df$cf20m180) & df$cf20m180>=9,]$rela_satisfied <- "1_relation_very_satisfied"
  df[!is.na(df$cf20m166) | df$cf20m166==999, !is.na(df$cf20m180) | df$cf20m180==999]$rela_satisfied  <- "missing"
  
  #Add housework   
  df$cf18k184[is.na(df$cf20m180)] <-999
  df$housework <- "NA"
  df[!is.na(df$cf18k184) & df$cf18k184==1,]$housework <- "1_practically_never"
  df[!is.na(df$cf18k184) & df$cf18k184==2,]$housework <- "2_occasionally"
  df[!is.na(df$cf18k184) & df$cf18k184==3,]$housework <- "3_often"
  df[!is.na(df$cf18k184) & df$cf18k184==4,]$housework <- "4_not applicabler"
  df[!is.na(df$cf18k184) | df$cf18k184==999,]$housework <- "missing"
  

  # Generate religiousness factor
  relig_df = subset(train, select = c(cr20m041, cr20m042))
  na.omit(relig_df)
  relig_df <- subset(relig_df, 
                     !(is.na(cr20m041) |
                         is.na(cr20m042) )
                                 )
  
  data_relig <- factanal(relig_df, factors = 1, scores="regression")
  relig_fa <-data_relig$scores

  
  # Generate social media factor
  sm_df = subset(train, select = c(cs20m267, cs20m277, cs20m280, cs20m281))
  na.omit(sm_df)
  sm_df <- subset(sm_df, 
                     !(is.na(cs20m267) |
                         is.na(cs20m277) |
                         is.na(cs20m280) |
                         is.na(cs20m281) )
  )
  
  data_sm <- factanal(sm_df, factors = 1, scores="regression")
  sm_fa <-sm_df$scores

  
  #Generate personality factors, big five
  new_df = subset(train, select = c(cp20l029, cp20l030, cp20l031, cp20l032, cp20l033, cp20l034, cp20l035, cp20l036, cp20l037, cp20l038, cp20l039, 
                                    cp20l040, cp20l041, cp20l042, cp20l043, cp20l044, cp20l045, cp20l046, cp20l047, cp20l048, cp20l049, 
                                    cp20l050, cp20l051, cp20l052, cp20l053, cp20l054, cp20l055, cp20l056, cp20l057, cp20l058, cp20l059, 
                                    cp20l060, cp20l061, cp20l062, cp20l063, cp20l064, cp20l065, cp20l066, cp20l067, cp20l068, cp20l069))
    #Omit rows with missing values on any of the personality variables
  na.omit(new_df)
  new_df <- subset(new_df, 
                   !(is.na(cp20l029) |
                       is.na(cp20l030) |
                       is.na(cp20l031) |
                       is.na(cp20l032) |
                       is.na(cp20l033) |
                       is.na(cp20l034) |
                       is.na(cp20l035) |
                       is.na(cp20l036) |
                       is.na(cp20l037) |
                       is.na(cp20l038) |
                       is.na(cp20l039) |
                       is.na(cp20l040) |
                       is.na(cp20l041) |
                       is.na(cp20l042) |
                       is.na(cp20l043) |
                       is.na(cp20l044) |
                       is.na(cp20l045) |
                       is.na(cp20l046) |
                       is.na(cp20l047) |
                       is.na(cp20l048) |
                       is.na(cp20l049) |
                       is.na(cp20l050) |
                       is.na(cp20l051) |
                       is.na(cp20l052) |
                       is.na(cp20l053) |
                       is.na(cp20l054) |
                       is.na(cp20l055) |
                       is.na(cp20l056) |
                       is.na(cp20l057) |
                       is.na(cp20l058) |
                       is.na(cp20l059) |
                       is.na(cp20l060) |
                       is.na(cp20l061) |
                       is.na(cp20l062) |
                       is.na(cp20l063) |
                       is.na(cp20l064) |
                       is.na(cp20l065) |
                       is.na(cp20l066) |
                       is.na(cp20l067) |
                       is.na(cp20l068) |
                       is.na(cp20l069) )
  )
  
  

  
  
  #Factor analysis (confirmatory, as based on the expectation of five personality traits)
  data_fa <- factanal(new_df, factors = 5, scores="regression")
  
  personality_factor<-data_fa$scores
  factor1 <- personality_factor[1]
  factor2 <- personality_factor[2]
  factor3 <- personality_factor[3]
  factor4 <- personality_factor[4]
  factor5 <- personality_factor[5]

  #Jessica's interpretation of the five factors, based on prior expectation/theory: 
  #Factor 1 Neurotism
  #Factor 2 Extraversion
  #Factor 3 Agreeableness
  #Factor 4 Openness to experience
  #Factor 5 Conscientiousness
  
  #Based on the p-value, this number of factors does not capture all dimensions in the data; however a different number (3-6) of factors does not seem to improve it;

  # Source: https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Multivariate-approaches/Factor-Analysis/A-Simple-Example-of-Factor-Analysis-in-R/index.html
  
  
  keepcols = c('nomem_encr', # ID variable required for predictions,
               'age', 
               'gender_bg',
               'migration',
               'education',
               'field_edu',
               'occupation',
               'income',
               'income_log',
               'owner',
               'satisfied_own_finance',
               'urban',
               'urban_delta',
               'health',
               'gyno',
               'relig_fa',
               'sm_fa',
               'dist_fr_parents',
               'rela_satisfied',
               'marriage_dur',
               'next_child',
               'numb_child',
               'first_birth',
               'partner',
               'partner_delta',   
               'partner_dur',
               'housework',
               'partner_type',
               'partner_type_delta',
               'factor1', 
               'factor2', 
               'factor3', 
               'factor4', 
               'factor5' 
                            )  
  
  # Keeping data with variables selected
  df <- df[ , keepcols]
  
  # turning gender into factor
  df$gender_bg<- as.factor(df$gender_bg) 
  
  return(df)
}



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

