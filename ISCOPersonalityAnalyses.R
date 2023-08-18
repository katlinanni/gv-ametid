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

new_means = B5 %>% group_by(new) %>% summarise_at(6:10, c(mean, sd, length))

XXXX_means = B5 %>% group_by(XXXX_code) %>% summarise_at(6:10, c(mean, sd, length)) %>% mutate(XXX_code = strtrim(XXXX_code,3)) %>%
  left_join(B5 %>% group_by(XXX_code) %>% summarise_at(6:10, c(mean, sd, length)) %>% mutate(XX_code = strtrim(XXX_code,3)), by = "XXX_code") %>% 
  left_join(B5 %>% group_by(XX_code) %>% summarise_at(6:10, c(mean, sd, length))%>% mutate(X_code = strtrim(XX_code,3)), by = "XX_code") %>% 
  left_join(B5 %>% group_by(X_code) %>% summarise_at(6:10, c(mean, sd, length)), by = "X_code")

XXXX_means %>% arrange(desc(O_fn1))

eta_squared(aov(B5$O ~ XXXX_code, B5),alternative = "two.sided")

XXXX_means %>% filter(N_fn3.x < 100 & N_fn3.x.x >= 100)
