require(tidyverse)

B5 = read_csv("B5_scores_selfreport.csv")
# see BG-fail on sul vist natuke teine
BG = read.csv("dataall.csv", header=T, sep = ";", dec =".") %>% select(c("person_skood","person_gender_code", "pt_answerset_age_at_agreement","pt_answerset_questionnaire_language_code")) %>%
  `names<-`(c("scode","gender","age","language"))
load("new_isco.rda")

B5 = left_join(B5, BG, "scode") %>% filter(language == "et") %>% left_join(new_isco %>% select(c("scode","ISCOcode_minor", "ISCOname_minor")), by = "scode")
uh = table(new_isco$ISCOname_minor) %>% .[. > 101] %>% names

B5$N = scale(residuals(lm(N ~ gender + age, B5)))
B5$E = scale(residuals(lm(E ~ gender + age, B5)))
B5$`O-` = scale(residuals(lm(B5$`O-` ~ gender + age, B5)))
B5$A = scale(residuals(lm(A ~ gender + age, B5)))
B5$C = scale(residuals(lm(C ~ gender + age, B5)))
B5 = B5 %>% filter(ISCOname_minor %in% uh)

count = vec_count(B5$ISCOcode_minor)
count = rename(count, ISCOcode_minor = key)

tapply(scale(B5$N), B5$ISCOname_minor, mean) %>% sort
tapply(scale(B5$E), B5$ISCOname_minor, mean) %>% sort
tapply(scale(B5$`O-`)*(-1), B5$ISCOname_minor, mean) %>% sort
tapply(scale(B5$A), B5$ISCOname_minor, mean) %>% sort
tapply(scale(B5$C), B5$ISCOname_minor, mean) %>% sort
tapply(scale(B5$`LS-`)*(-1), B5$ISCOname_minor, mean) %>% sort

means_N = tapply(scale(B5$N), B5$ISCOname_minor, function(x) c(mean(x), sd(x)))
means_E = tapply(scale(B5$E), B5$ISCOname_minor, function(x) c(mean(x), sd(x)))
means_O = tapply(B5$`O-`*(-1), B5$ISCOname_minor, function(x) c(mean(x), sd(x)))
means_A = tapply(scale(B5$A), B5$ISCOname_minor, function(x) c(mean(x), sd(x)))
means_C = tapply(scale(B5$C), B5$ISCOname_minor, function(x) c(mean(x), sd(x)))

# pole ilmselt oluline, tegin lihtsalt tabeli keskmiste ja SD-ga nii (kirja manuses)
#B5means = data.frame(means_N, means_E, means_O, means_A, means_C)
#B5means_sd = B5 %>% group_by(ISCOname_minor) %>% summarize(across(N:'O-', list(mean = mean, sd = sd)))
#B5means_sd = left_join(B5means_sd, ISCOminor, by = "ISCOname_minor")

# ANOVA
modelA = aov(A ~ ISCOname_minor, data = B5)
summary(modelA)
modelC = aov(C ~ ISCOname_minor, data = B5)
summary(modelC)
modelN = aov(N ~ ISCOname_minor, data = B5)
summary(modelN)
modelE = aov(E ~ ISCOname_minor, data = B5)
summary(modelE)
modelO = aov(`O-` ~ ISCOname_minor, data = B5)
summary(modelO)

posthoc = TukeyHSD(modelA)
adj_p_values = p.adjust(posthoc$p.value, method = "none")
# siin ei tulnud ühtegi väärtust, seega gruppidevahelisi erinevusi ei ole? Proovisin erinevate meetoditega, sh ka "none"-iga

# kõikide mudelitega on tegelikult Levene test < .001 ehk peaks kasutam Welch ANOVAT? Muud ANOVA eeldused peaks olema täidetud
leveneTest(modelA)
leveneTest(modelC)
...

# proovisin Welchi ANOVAT, aga lõpp-järeldus vist ikka sama?
model = oneway.test(A ~ ISCOname_minor, data = B5, var.equal = FALSE)
model$statistic
model$parameter
model$p.value
model$method

# Perform Games-Howell post-hoc test - kuna nii palju võrdlusi, siis raske hoomata...
posthoc2 = pairwise.t.test(B5$A, B5$ISCOname_minor, p.adjust.method = "none")

# Proovisin veel midagi sellist
library(ggplot2)
ggplot(B5, aes(x = reorder(ISCOname_minor, A), y = A)) +
  geom_boxplot() +
  xlab("ISCOname_minor") +
  ylab("A") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


