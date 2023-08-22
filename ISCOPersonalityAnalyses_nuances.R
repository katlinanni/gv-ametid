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

