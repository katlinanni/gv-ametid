require(tidyverse)
## assume the latest occupations are read in, in column new
B5 = read_csv("~/Private/gvpers/B5_scores_selfreport.csv")
BG = read_tsv("~/Private/gvpers/query-pt_allelse.tsv") %>% 
  select(c("Person skood","Person gender code", "PtAnswerset ageAtAgreement","PtAnswerset questionnaireLanguage name")) %>%
  `names<-`(c("scode","gender","age","language"))

B5 = left_join(B5, BG, "scode") %>% filter(language %in% "Eesti keeles") %>% left_join(new %>% select(c("scode","new")), by = "scode")
uh = table(new$new) %>% .[. > 200] %>% names
B5$N = scale(residuals(lm(N ~ gender + age, B5)))
B5$E = scale(residuals(lm(E ~ gender + age, B5)))
B5$`O-` = scale(residuals(lm(B5$`O-` ~ gender + age, B5)))
B5$A = scale(residuals(lm(A ~ gender + age, B5)))
B5$C = scale(residuals(lm(C ~ gender + age, B5)))
B5 = B5 %>% filter(new %in% uh)

tapply(scale(B5$N), B5$new, mean) %>% sort
tapply(scale(B5$E), B5$new, mean) %>% sort
tapply(scale(B5$`O-`)*(-1), B5$new, mean) %>% sort
tapply(scale(B5$A), B5$new, mean) %>% sort
tapply(scale(B5$C), B5$new, mean) %>% sort
tapply(scale(B5$`LS-`)*(-1), B5$new, mean) %>% sort
