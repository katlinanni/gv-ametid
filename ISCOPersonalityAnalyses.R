require(tidyverse)
require(car)
require(splitstackshape)
require(effectsize)

B5 = read.csv("~/.gvpers/otherQuestionnaireResponses.csv") %>% select(Personskood, Persongender, ageAtAgreement, education, questionnaireLanguage) %>%
  rename(scode = Personskood, gender = Persongender, age = ageAtAgreement, language = questionnaireLanguage) %>%
 left_join(read_csv("~/.gvpers/B5_scores_selfreport.csv"), "scode") %>% 
  filter(language == "et") 

B5 = right_join(B5, new_isco, "scode") %>% as_tibble %>% filter(!is.na(N)) %>% rename(O = `O-`)
B5 = stratified(B5, "XXXX_code", 1000)
 
B5$N = scale(residuals(lm(N ~ gender + age, B5))) %>% as.numeric
B5$E = scale(residuals(lm(E ~ gender + age, B5))) %>% as.numeric
B5$O = scale(residuals(lm(B5$O ~ gender + age, B5)))*(-1) %>% as.numeric
B5$A = scale(residuals(lm(A ~ gender + age, B5))) %>% as.numeric
B5$C = scale(residuals(lm(C ~ gender + age, B5))) %>% as.numeric

## More fine-grained groups give bigger effects, so less variability in sample means for broader groups. 
## This means later Bayesian averaging will generally push means and variances in smaller samples towards the population means
eta_squared(aov(B5$O ~ XXXX_code, B5),alternative = "two.sided")
eta_squared(aov(B5$O ~ XXX_code, B5),alternative = "two.sided")
eta_squared(aov(B5$O ~ XX_code, B5),alternative = "two.sided")
eta_squared(aov(B5$O ~ X_code, B5),alternative = "two.sided")

new_means = B5 %>% group_by(new) %>% summarise_at(6:10, c(mean, sd, length))
XXXX_means = B5 %>% group_by(XXXX_code) %>% summarise_at(6:10, c(mean, sd, length)) %>% mutate(XXX_code = strtrim(XXXX_code,3)) %>%
  left_join(B5 %>% group_by(XXX_code) %>% summarise_at(6:10, c(mean, sd, length)) %>% mutate(XX_code = strtrim(XXX_code,2)), by = "XXX_code") %>% 
  left_join(B5 %>% group_by(XX_code) %>% summarise_at(6:10, c(mean, sd, length))%>% mutate(X_code = strtrim(XX_code,1)), by = "XX_code") %>% 
  left_join(B5 %>% group_by(X_code) %>% summarise_at(6:10, c(mean, sd, length)), by = "X_code")

names(XXXX_means) = names(XXXX_means) %>% gsub("fn1","mean",., fixed = T) %>%  gsub("fn2","sd",., fixed = T) %>% gsub("fn3","N",., fixed = T)  %>% 
  gsub(".y.y","_X",., fixed = T) %>% gsub(".x.x","_XX",., fixed = T) %>% gsub(".y","_XXX",., fixed = T) %>% gsub(".x","_XXXX",., fixed = T) 

XXXX_means = ISCO %>% select(XXXX_code, XXXX_name) %>% right_join(XXXX_means, by="XXXX_code")

XXXX_means %>% arrange(desc(N_mean_XXXX))

## Function here for trialing. Later to be moved to the helpers file.
poseriors = function(m_data, var_data, m_prior, var_prior, n_data, n_prior) {
  m_posterior = (n_prior * m_prior + n_data * m_data) / (n_prior + n_data)
  var_posterior = (n_prior * var_prior + (n_data - 1) * var_data + (n_data * n_prior * (m_data - m_prior)^2) / (n_prior + n_data)) / (n_prior + n_data - 1)
  return(list(m_posterior = m_posterior, var_posterior = var_posterior))
}

## to understand try different sample sizes, moving mean from observed .5 closer to 0 and var from observed 1.1^2 close to 1
n = 100 ## data
k = 25 ## the sample size at which data and prior have equal weight
posteriors(.5, 1.1^2, 0, 1, (n/k)^2 * k, k)

### just trying the analyses on real data, messy code ...

tmp = XXXX_means %>% filter(N_N_XXX >= 100) %>% select(contains("XXXX"), ends_with("XXX")) ## almost all observations
#XXXX_means %>% filter(N_N_XXX < 100 & N_N_XX >= 100)  %>% select(contains("XXXX"), ends_with("_XX"))
#XXXX_means %>% filter(N_N_XX < 100 & N_N_X >= 100)  %>% select(contains("XXXX"), ends_with("_X"))

k = 25 ## To understand, try also 100, 50, etc
plot(xlim=c(-1,1),ylim=c(-1,1),xlab="Posterior",ylab="Data",
  posteriors(
    m_data = tmp$N_mean_XXXX, var_data = tmp$N_sd_XXXX^2, 
    m_prior = tmp$N_mean_XXX, var_prior = tmp$N_sd_XXX^2, 
    n_data = (tmp$N_N_XXXX/k)^2 * k, n_prior = k )$m_posterior,
  tmp$N_mean_XXXX)
lines(c(-10,10), c(-10,10))

### Are means and variances correlated, suggesting scores with higher/lower Big Five trait levels are more selection?

cor(XXXX_means %>% select(N_mean_XXXX:O_mean_XXXX), XXXX_means %>% select(N_sd_XXXX:O_sd_XXXX), method="spearman")
