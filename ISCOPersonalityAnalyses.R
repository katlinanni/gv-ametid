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
eta_squared(aov(O ~ XXXX_code, B5),alternative = "two.sided")
eta_squared(aov(O ~ XXX_code, B5),alternative = "two.sided")
eta_squared(aov(O ~ XX_code, B5),alternative = "two.sided")
eta_squared(aov(O ~ X_code, B5),alternative = "two.sided")

B5 = B5 %>% mutate_at(c("N","A","E","C", "O"), scale)

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

tmp1 = XXXX_means %>% filter(N_N_XXX >= 100) %>% select(contains("XXXX"), ends_with("XXX")) ## almost all observations
tmp2 = XXXX_means %>% filter(N_N_XXX < 100 & N_N_XX >= 100)  %>% select(contains("XXXX"), ends_with("_XX"))
tmp3 = XXXX_means %>% filter(N_N_XX < 100 & N_N_X >= 100)  %>% select(contains("XXXX"), ends_with("_X"))

k = 25 ## To understand, try also 100, 50, etc
plot(xlim=c(-1,1),ylim=c(-1,1),xlab="Posterior",ylab="Data",
  posteriors(
    m_data = tmp1$A_mean_XXXX, var_data = tmp1$A_sd_XXXX^2, 
    m_prior = tmp1$A_mean_XXX, var_prior = tmp1$A_sd_XXX^2, 
    n_data = (tmp1$A_N_XXXX/k)^2 * k, n_prior = k )$m_posterior,
  tmp1$A_mean_XXXX)
lines(c(-10,10), c(-10,10))

### Let's smooth everyone for final results

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

names(smoothed1_means) = names(smoothed2_means) = names(smoothed3_means) = c("N_mean","A_mean","E_mean","C_mean", "O_mean", "XXXX_code","XXXX_name")
names(smoothed1_sds) = names(smoothed2_sds) = names(smoothed3_sds) = c("N_sd","A_sd","E_sd","C_sd", "O_sd", "XXXX_code","XXXX_name")

smoothed_means = rbind(smoothed1_means, smoothed2_means, smoothed3_means)
smoothed_sds = rbind(smoothed1_sds, smoothed2_sds, smoothed3_sds)

res = XXXX_means %>% left_join(smoothed_means, by="XXXX_code") %>% left_join(smoothed_sds, by="XXXX_code")


### Sanity check

cor(select(res, N_mean_XXXX:O_mean_XXXX), select(res, N_mean:O_mean))
cor(select(res, N_sd_XXXX:O_sd_XXXX), select(res, N_sd:O_sd))

plot(res$N_mean, res$N_mean_XXXX)
lines(c(-10,10),c(-10,10))

plot(res$N_sd, res$N_sd_XXXX)
lines(c(0,2),c(0,2))

###

res %>% arrange(desc(N_mean)) %>% select(XXXX_name, N_mean:O_sd, N_N_XXXX) %>% slice(1:20)

res %>% filter(XXXX_name %in% "Psychologists") %>% select(XXXX_name, N_mean:O_sd, N_N_XXXX) %>% slice(1:20)

require(DT)

res %>% select(XXXX_name, N_mean:O_sd, N_N_XXXX, -ends_with(".y")) %>% mutate_at(2:11, ~round(.,2)) %>% datatable %>% saveWidget("Means.html")

### Are means and variances correlated, suggesting scores with higher/lower Big Five trait levels are more selection?

cor(XXXX_means %>% select(N_mean_XXXX:O_mean_XXXX), XXXX_means %>% select(N_sd_XXXX:O_sd_XXXX), method="spearman")
cor(res %>% select(N_mean:O_mean), res %>% select(N_sd:O_sd), method="spearman")


### wolfram comparison

wolfram = read_xlsx("~/Downloads/wolfram.xlsx") %>% mutate(XXXX_code = as.character(strtrim(Occupation,4)))

wolfram = res %>% select(XXXX_code, XXXX_name, N_mean:O_sd, N_N_XXXX, -ends_with(".y")) %>% left_join(wolfram, by="XXXX_code")

cor(select(wolfram, N_mean:O_mean), select(wolfram, contains("Big 5")), use="pairwise") %>% round(2)
