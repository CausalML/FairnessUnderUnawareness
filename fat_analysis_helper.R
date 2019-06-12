# estimating race based on proxy probabilities
est_race_threshold_rule <- function(data, t){
  race_name = c("White", "Black", "AIAN", "API", "Hispanic")
  threshold_compare = select(data, one_of(race_name)) >= t
  temp = apply(threshold_compare, MARGIN = 1, FUN = which) # identify which index has prob larger than threshold
  # e.g., if for the ith obs, White has prob larger than threshold, then the ith obs in the resulting vector temp
  # would be 1, which is the index for White in the race_name vector 
  result = unlist(lapply(temp, function(x) if (length(x) > 0) names(x)[1] else NA)) # recover the race name for which 
  # the prob is larger than threshold; for some obs, no race has prob larger than t, so the estimated race is NA in this case
  # here we use names(x)[1] because when using t = 0.5, 6 observations have two race probs
  # both equal to 50%; we choose the first one 
  result
  
  
}
demo_disparity_race <- function(data, est_race){
  est_white_mean = mean(filter(data, est_race == "White")$outcome)
  est_black_mean = mean(filter(data, est_race == "Black")$outcome)
  est_hispanic_mean = mean(filter(data, est_race == "Hispanic")$outcome)
  est_api_mean = mean(filter(data, est_race == "API")$outcome)
  est_aian_mean = mean(filter(data, est_race == "AIAN")$outcome)
  
  prop = c(est_white_mean, est_black_mean, est_hispanic_mean, est_api_mean, est_aian_mean)
  names(prop) = c("White", "Black", "Hispanic", "API", "AIAN")
  
  white_black = est_white_mean - est_black_mean
  white_hispanic = est_white_mean - est_hispanic_mean
  white_AIAN = est_white_mean - est_aian_mean
  white_Asian = est_white_mean - est_api_mean
  
  disparity = c(white_black, white_hispanic, white_AIAN, white_Asian)
  names(disparity) = c("white-black", "white-hispanic", "white-AIAN", "white-API")
  
  list(est_prop = prop, est_disparity = disparity)
}
demo_disparity_weighted <- function(data){
  White_mean = sum(data$White * data$outcome)/sum(data$White)
  Black_mean = sum(data$Black * data$outcome)/sum(data$Black)
  Hispanic_mean = sum(data$Hispanic * data$outcome)/sum(data$Hispanic)
  AIAN_mean = sum(data$AIAN * data$outcome)/sum(data$AIAN)
  API_mean = sum(data$API * data$outcome)/sum(data$API)
  
  prop = c(White_mean, Black_mean, Hispanic_mean, API_mean, AIAN_mean)
  names(prop) = c("White", "Black", "Hispanic", "API", "AIAN")
  
  white_black = White_mean - Black_mean
  white_hispanic = White_mean - Hispanic_mean
  white_AIAN = White_mean - AIAN_mean
  white_Asian = White_mean - API_mean
  
  disparity = c(white_black, white_hispanic, white_AIAN, white_Asian)
  names(disparity) = c("white-black", "white-hispanic", "white-AIAN", "white-Asian")
  
  list(prop = prop, disparity = disparity)
}

# thresholded estimator estimation bias condition
evaluate_threshold_conditions <- function(data, t, advantage_group_name = "White", disadvantage_group_name = "Black"){
  
  advantage_group_prob = data[, advantage_group_name]
  disadvantage_group_prob = data[, disadvantage_group_name]
  
  cond_3 = mean(filter(data, (advantage_group_prob > t) & (race == advantage_group_name))$outcome) - mean(filter(data, (advantage_group_prob <= t) & (race == advantage_group_name))$outcome) 
  
  cond_4 = mean(filter(data, (disadvantage_group_prob <= t) & (race == disadvantage_group_name))$outcome) - mean(filter(data, (disadvantage_group_prob > t) & (race == disadvantage_group_name))$outcome)
  
  cond_1 = mean(filter(data, (advantage_group_prob > t) & (race == disadvantage_group_name))$outcome) - mean(filter(data, (advantage_group_prob > t) & (race == advantage_group_name))$outcome)
  
  cond_2 = mean(filter(data, (disadvantage_group_prob > t) & (race == disadvantage_group_name))$outcome) - mean(filter(data, (disadvantage_group_prob > t) & (race == advantage_group_name))$outcome) 
  
  result = c(cond_1, cond_2, cond_3, cond_4)
  names(result) = c("condition 1", "condition 2", "condition 3", "condition 4")
  result 
}

evaluate_theory_bias <- function(data, t, race_pair, self_name, other_name){
  result = data.frame(race_pair = race_pair, race = self_name, bias = 0, C1 = 0, C2 = 0, C3 = 0,
                      prop_A_u = 0, prop_hat_A_u = 0, prop_Au_hatAu = 0, prop_Anotu_hatAu = 0)
  
  self_prob = data[, self_name]
  other_prob = data[, other_name]
  
  N = nrow(data)
  
  A1 = mean(filter(data, (self_prob > t) & (race == other_name))$outcome) - mean(filter(data, (self_prob > t) & (race == self_name))$outcome)
  A2 = mean(filter(data, (self_prob <= t) & (race == self_name))$outcome) - mean(filter(data, (self_prob > t) & (race == self_name))$outcome)
  A3 = A1 - A2
  
  C1 = (
    nrow(filter(data, (self_prob > t) & (race == self_name)))/nrow(filter(data, (race == self_name)))
  )*(
    nrow(filter(data, (self_prob > t) & (race == other_name)))/nrow(filter(data, (self_prob > t)))
  )
  C2 = (
    nrow(filter(data, (self_prob <= t) & (race == self_name)))/nrow(filter(data, (race == self_name)))
  )*(
    nrow(filter(data, (self_prob > t) & (race == self_name)))/nrow(filter(data, (self_prob > t)))
  )
  C3 = (
    nrow(filter(data, (self_prob <= t) & (race == self_name)))/nrow(filter(data, (race == self_name)))
  )*(
    nrow(filter(data, (self_prob > t) & (race == other_name)))/nrow(filter(data, (self_prob > t)))
  )
  
  result[, "C1"] = C1; result[, "C2"] = C2; result[, "C3"] = C3;
  
  result[, "prop_A_u"] = nrow(filter(data, race == self_name))/N
  result[, "prop_hat_A_u"] = nrow(filter(data, self_prob > t))/N
  result[, "prop_Au_hatAu"] = nrow(filter(data, (self_prob > t) & (race == self_name)))/nrow(filter(data, self_prob > t))
  result[, "prop_Anotu_hatAu"] = 1 - result[, "prop_Au_hatAu"]
  result[, "bias"] = A1*C1 - A2*C2 + A3*C3
  
  result
}

compute_bias_influence <- function(race_pair, race, other_race, threshold = 0.5){
  bias = data.frame(race_pair = race_pair,
                    race = race,
                    bias = rep(0, 6), C1 = rep(0, 6), C2 = rep(0, 6), C3 = rep(0, 6), 
                    prop_A_u = rep(0, 6), prop_hat_A_u  = rep(0, 6), prop_Au_hatAu = rep(0, 6), 
                    prop_Anotu_hatAu = rep(0, 6))
  
  for (i in seq_along(race_pair)){
    bias[(bias$race_pair == race_pair[i]) & (bias$race == race[i]), ] =   
      evaluate_theory_bias(data = hmda, t = threshold, race_pair =  race_pair[i], self_name = race[i], other_name = other_race[i])
  }
  
  bias
}

make_covariance_race_outcome <- function(data, advantage_name = "White", disadvantage_name = "Hispanic"){
  data %>% 
    mutate(
      advantage_or_not = ifelse(race == advantage_name, 1, 0),
      disadvantage_or_not = ifelse(race == disadvantage_name, 1, 0)
    ) %>% 
    group_by(state_code, COUNTY, census_tract_number) %>% 
    summarise(
      cov_ad_outcome = cov(advantage_or_not, outcome),
      cov_disad_outcome = cov(disadvantage_or_not, outcome)
    ) %>% 
    right_join(data, by =c("state_code", "COUNTY", "census_tract_number"))
}