# Description of Final submission for NextFinBaby team
Some notes about our updated model: we had intended to remove several variables, including satisfaction with own finances (because it was quite correlated with partner) and help from parents (to be replaced with distance from parents), however, the submission through github refused to recognise the model.rds file once these variables were removed. We also intended to add several variables we were unable to add: several indicator variables that were unable to be processed via the R code. These include whether the sted value changed from 2019 to 2020, whether the partner status changed from 2019 to 2020, and whether the woonvorm value changed from 2019 to 2020 (all yes=1 no change=0).

Instead, we took the original model (which we were unable to modify because of the above-mentioned issue with the model.rds file; original submission listed below) and added the following variables:

•	Distance from parents, in kilometres
•	Number of desired children, (1, 2, 3 or more, missing)
•	Currently has a partner (yes, no, missing)
•	Cohabitation type with partner (Single, (un)married cohab w/o children, (un)married cohab w/children, Single w/children, other, missing)
•	Difference of opinion about household work (practically never, occasionally, often, missing)

We also tried to add two factor variables to capture the big five personality characteristics by creating a factor out of variables cp20l29-cp20l69 and of social media use by combining variables cs20m267, cs20m277, cs20m280, and cs20m281; however, the revised script was unable to pull these variables from the prefer csv. The model remained the same – a logistic model with all variables interacted by gender. Please see below for the original model description and justification


# Original description:

We are a group of fertility experts who wanted to provide a model with predictors driven by theoretical relevance. We included predictors that reflect demographic, economic, social, and psychological factors shown to be important for childbearing in the literature, particularly in the near future. The model includes interactions between gender and all other predictors in the model. These interactions were motivated because a) theoretically, many variables tend to have different relationships with fertility for women and men, and b) it improved the predictive accuracy of the model.

A binary logistic regression was run on the outcome variable ‘new_child’ (0,1) with the following, manually selected 19 predictors:

•	Gender (male; female)
•	Age (_20; 21_23; 24_26; 27_29; 30_31; 32_35; 36_40; 41+)
•	Time since first birth “Did you ever have any children?” & “Birth year first child” (<2009; 2010_2013; 2014_2016; 2017_2018; 2019_2020; no_children; missing)
•	Migration background (Dutch; gen_1_non_west; gen_1_west; gen_2_non_west; gen_2_west; missing)
•	Education level (prim; sec_intermed; sec_high; voc_intermed; voc_high; uni; other_missing)
•	Education field (health; art_humanities; catering; economics; general; law; math; other; personal_care; social; teaching; technology_agri; transport_public_order; missing)
•	Occupation (employed; self-employed; not_employed; student; missing)
•	Income “Net household income in Euros”: (Missingness was imputed using the mean income value)
•	Financial satisfaction “How satisfied are you with your financial situation?” (0123_satisfied; 45_satisfied; 6_satisfied; 7_satisfied; 8_satisfied; 910_satisfied, missing)
•	Home ownership “Are you a tenant, subtenant, or (co-)owner of your current dwelling?” (no_owner, yes_owner; missing)
•	Urban character of place of residence (extreme_urban; very_urban; mod_urban; slight_urban; not_urban; missing)
•	Partner info and duration “In what year did you start living together with your partner?” (_2005; 2006_2010; 2011_2014; 2015_2017; 2018_2020; missing)
•	Marriage info and duration “In what year did you marry?” (_2008; 2009_2013; 2014_2017; 2018_2020; missing)
•	Relationship satisfaction “How satisfied are you with your current relationship?” & ”How satisfied are you with your situation as a single?” (relation_very_satisfied; relation_satisfied; relation_less_satisfied; single_very_satisfied; single_neutral; single_dissatisfied; missing)
•	Help from own parents “Did you receive any help from your mother over the past 3 months in tending to the children, such as child- or babysitting, caring or transport?” & “Did you receive any help from your father over the past 3 months in tending to the children, such as child- or babysitting, caring or transport?” (never; once_twice; several_times; missing)
•	Religiousness ”Aside from special occasions such as weddings and funerals, how often do you attend religious gatherings nowadays?” (never; few_per_year; >once_month; >once_week; missing)
•	Personality “Am interested in people“ & “Get stressed out easily” (less_social_less_stressed; less_social_more_stressed; more_social_less_stressed; more_social_more_stressed; missing)
•	Subjective health “How would you describe your health, generally speaking?” (poor_moderate; good; very_good; excellent; missing)
•	Fertility intentions “Within how many years do you hope to have your [first/next] child?” & “Do you think you will have [more] children in the future?” (<1_year; 2_3_year; >4_year; no (more) children; missing)

