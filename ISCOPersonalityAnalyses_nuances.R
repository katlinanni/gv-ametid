require(tidyverse)
require(splitstackshape)
require(effectsize)
require(DT)

load("new_isco.RData")

persn = read.csv("selfRatings100NP.csv", header=T, sep = ";", dec =".") %>% 
  select(skood, gender, ageAtAgreement, questionnaireLanguage, 21:218) %>%
  rename(scode = skood, age = ageAtAgreement, language = questionnaireLanguage)

nuanceVars <- persn[, which(names(persn) == "neuroticism01"):which(names(persn) == "others29")]

# Deleting rows with >10 NA-s
persn$na_count <- rowSums(is.na(nuanceVars))
persn <- subset(persn, na_count <= 10)

# Replacing NA-s with the median of the column
cols_with_na <- colnames(persn)[apply(persn, 2, function(x) any(is.na(x)))]
for (col in cols_with_na) {
  persn[[col]][is.na(persn[[col]])] <- median(persn[[col]], na.rm = TRUE)
}

persn = left_join(new_isco, persn, "scode")  %>%
  filter(language == "et") %>%
  stratified("XXXX_code", 1000)

start_var <- which(names(persn) == "neuroticism01")
end_var <- which(names(persn) == "others29")
nuanceVars <- names(persn)[start_var:end_var]

# Scale the variables
for (var in nuanceVars) {
  persn[[var]] <- scale(persn[[var]])
}

# Residualize the scaled variables
for (var in nuanceVars) {
  model <- lm(persn[[var]] ~ persn$age + persn$gender, data = persn)
  persn[[var]] <- residuals(model)
}

# Eta-squared effect sizes
results <- list()
for(current_var in nuanceVars) {
  formula <- reformulate("XXXX_code", response = current_var)
  model <- aov(formula, data = persn)
  eta_val <- eta_squared(model, alternative = "two.sided")
  results[[current_var]] <- eta_val
}

results_df <- do.call(rbind, results)
item_names <- readxl::read_xlsx("item_names.xlsx")
results_df <- results_df %>%
  as_tibble(rownames = "Kood")
results_df <- left_join(results_df, item_names, by = "Kood")

nuanceVars <- colnames(persn[, which(names(persn) == "neuroticism01"):which(names(persn) == "others29")])

XXXX_means_nuances <- persn %>%
  group_by(XXXX_code) %>%
  summarise(across(all_of(nuanceVars), list(mean = mean, sd = sd, length = length))) %>%
  mutate(XXX_code = strtrim(XXXX_code, 3)) %>%
  left_join(persn %>%
              group_by(XXX_code) %>%
              summarise(across(all_of(nuanceVars), list(mean = mean, sd = sd, length = length))) %>%
              mutate(XX_code = strtrim(XXX_code, 2)), 
            by = "XXX_code") %>%
  left_join(persn %>%
              group_by(XX_code) %>%
              summarise(across(all_of(nuanceVars), list(mean = mean, sd = sd, length = length))) %>%
              mutate(X_code = strtrim(XX_code, 1)), 
            by = "XX_code") %>%
  left_join(persn %>%
              group_by(X_code) %>%
              summarise(across(all_of(nuanceVars), list(mean = mean, sd = sd, length = length))), 
            by = "X_code")

names(XXXX_means_nuances) = names(XXXX_means_nuances) %>% gsub("length","N",., fixed = T) %>% gsub(".y.y","_X",., fixed = T) %>% gsub(".x.x","_XX",., fixed = T) %>% gsub(".y","_XXX",., fixed = T) %>% gsub(".x","_XXXX",., fixed = T) 

XXXX_means_nuances = ISCO %>% select(XXXX_code, XXXX_name) %>% right_join(XXXX_means_nuances, by="XXXX_code")

## Function here for trialing. Later to be moved to the helpers file.
posteriors = function(m_data, var_data, m_prior, var_prior, n_data, n_prior) {
  m_posterior = (n_prior * m_prior + n_data * m_data) / (n_prior + n_data)
  var_posterior = (n_prior * var_prior + (n_data - 1) * var_data + (n_data * n_prior * (m_data - m_prior)^2) / (n_prior + n_data)) / (n_prior + n_data - 1)
  return(list(m_posterior = m_posterior, var_posterior = var_posterior))
}

## to understand try different sample sizes, moving mean from observed .5 closer to 0 and var from observed 1.1^2 close to 1
n = 100 ## data
k = 25 ## the sample size at which data and prior have equal weight
posteriors(.5, 1.1^2, 0, 1, (n/k)^2 * k, k)

tmp1 = XXXX_means_nuances %>% filter(neuroticism01_N_XXX >= 100) %>% select(contains("XXXX"), ends_with("XXX")) ## almost all observations
tmp2 = XXXX_means_nuances %>% filter(neuroticism01_N_XXX < 100 & neuroticism01_N_XX >= 100)  %>% select(contains("XXXX"), ends_with("_XX"))
tmp3 = XXXX_means_nuances %>% filter(neuroticism01_N_XX < 100 & neuroticism01_N_X >= 100)  %>% select(contains("XXXX"), ends_with("_X"))

k = 25
smoothed1 = posteriors(
  select(tmp1, ends_with("mean_XXXX")),select(tmp1, ends_with("sd_XXXX"))^2,
  select(tmp1, ends_with("mean_XXX")),select(tmp1, ends_with("sd_XXX"))^2,
  k * (select(tmp1, ends_with("_N_XXXX"))/k)^2, k) 

smoothed1_means = smoothed1$m_posterior %>% as_tibble %>%
  mutate(XXXX_code = tmp1$XXXX_code, XXXX_name = tmp1$XXXX_name)

smoothed1_sds = sqrt(smoothed1$var_posterior) %>% as_tibble %>%
  mutate(XXXX_code = tmp1$XXXX_code, XXXX_name = tmp1$XXXX_name)

smoothed2 = posteriors(
  select(tmp2, ends_with("mean_XXXX")),select(tmp2, ends_with("sd_XXXX"))^2,
  select(tmp2, ends_with("mean_XX")),select(tmp2, ends_with("sd_XX"))^2,
  k * (select(tmp2, ends_with("_N_XXXX"))/k)^2, k) 

smoothed2_means = smoothed2$m_posterior %>% as_tibble %>%
  mutate(XXXX_code = tmp2$XXXX_code, XXXX_name = tmp2$XXXX_name)

smoothed2_sds = sqrt(smoothed2$var_posterior) %>% as_tibble %>%
  mutate(XXXX_code = tmp2$XXXX_code, XXXX_name = tmp2$XXXX_name)

smoothed3 = posteriors(
  select(tmp3, ends_with("mean_XXXX")),select(tmp3, ends_with("sd_XXXX"))^2,
  select(tmp3, ends_with("mean_X")),select(tmp3, ends_with("sd_X"))^2,
  k * (select(tmp3, ends_with("_N_XXXX"))/k)^2, k) 

smoothed3_means = smoothed3$m_posterior %>% as_tibble %>%
  mutate(XXXX_code = tmp3$XXXX_code, XXXX_name = tmp3$XXXX_name)

smoothed3_sds = sqrt(smoothed3$var_posterior) %>% as_tibble %>%
  mutate(XXXX_code = tmp3$XXXX_code, XXXX_name = tmp3$XXXX_name)

names(smoothed1_means) = names(smoothed2_means) = names(smoothed3_means) = c(paste(nuanceVars, "mean",sep ="_"), "XXXX_code","XXXX_name")
names(smoothed1_sds) = names(smoothed2_sds) = names(smoothed3_sds) = c(paste(nuanceVars, "sd",sep ="_"), "XXXX_code","XXXX_name")

smoothed_means = rbind(smoothed1_means, smoothed2_means, smoothed3_means)
smoothed_sds = rbind(smoothed1_sds, smoothed2_sds, smoothed3_sds)

res_nuances = XXXX_means_nuances %>% left_join(smoothed_means, by="XXXX_code") %>% left_join(smoothed_sds, by="XXXX_code")


res_nuances %>% arrange(desc(extraversion12_mean)) %>% select(XXXX_name) %>% slice(1:10)
res_nuances %>% arrange(extraversion12_mean) %>% select(XXXX_name) %>% slice(1:10)

res_nuances %>% arrange(desc(neuroticism12_mean)) %>% select(XXXX_name) %>% slice(1:10)
res_nuances %>% arrange(neuroticism12_mean) %>% select(XXXX_name) %>% slice(1:10)
