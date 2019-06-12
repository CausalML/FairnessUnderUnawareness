# Missing Data 
get_obs_state_missing <- function(data){
  is.na(data$state_code)
}

get_obs_county_missing <- function(data){
  is.na(data$county_name)
}

get_obs_tract_missing <- function(data){
  is.na(data$census_tract_number)
}

get_obs_race_empty <- function(data){
  data$race == ""
}


remove_obs <- function(data, fun_obs){
  obs = fun_obs(data)
  filter(data, !obs)
}

# create race
make_race_fat <- function(data){
  data$race = ""
  data$race[data$applicant_ethnicity_name == "Hispanic or Latino"] = "Hispanic"
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & (!is.na(data$applicant_race_name_2))] = "mult_race"
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & 
              (data$applicant_race_name_1 == "American Indian or Alaska Native") &
              (is.na(data$applicant_race_name_2))] = "AIAN"
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & 
              ((data$applicant_race_name_1 == "Asian") | (data$applicant_race_name_1 == "Native Hawaiian or Other Pacific Islander")) &
              (is.na(data$applicant_race_name_2))] = "API"
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & 
              (data$applicant_race_name_1 == "Black or African American") &
              (is.na(data$applicant_race_name_2))] = "Black"
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & 
              (data$applicant_race_name_1 == "White") &
              (is.na(data$applicant_race_name_2))] = "White"
  
  select(data, -one_of(c("applicant_ethnicity_name", "applicant_race_name_1", "applicant_race_name_2")))
}
make_race_ecological <- function(data){
  data$race = ""
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & 
              ((data$applicant_race_name_1 == "Asian") | (data$applicant_race_name_1 == "Native Hawaiian or Other Pacific Islander")) &
              (is.na(data$applicant_race_name_2))] = "API"
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & 
              (data$applicant_race_name_1 == "Black or African American") &
              (is.na(data$applicant_race_name_2))] = "Black"
  
  data$race[(data$applicant_ethnicity_name == "Not Hispanic or Latino") & 
              (data$applicant_race_name_1 == "White") &
              (is.na(data$applicant_race_name_2))] = "White"
  
  select(data, -one_of(c("applicant_ethnicity_name", "applicant_race_name_1", "applicant_race_name_2")))
}

