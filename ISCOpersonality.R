require(tidyverse)


B5 = read_csv("B5_scores_selfreport.csv")
# see BG-fail on sul vist natuke teine
BG = read.csv("dataall.csv", header=T, sep = ";", dec =".") %>% select(c("person_skood","person_gender_code", "pt_answerset_age_at_agreement","pt_answerset_questionnaire_language_code")) %>%
  `names<-`(c("scode","gender","age","language"))
load("new_isco.rda")

B5 = left_join(B5, BG, "scode") %>% filter(language == "et") %>% left_join(new_isco %>% select(c("scode","ISCOcode_minor", "ISCOname_minor", "ISCOcode", "ISCOname")), by = "scode")
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

# MANOVA
library(car)
manova_result <- manova(cbind(A, `O-`, C, E, N) ~ ISCOname_minor, data = B5)
summary(manova_result)

# Compute partial Eta-squared
ss_residual <- sum(diag(manova_result$residuals))
ss_effect <- sum(diag(manova_result$SS))
ss_total <- ss_effect + ss_residual
partial_eta_sq <- ss_effect / ss_total
cat("Partial Eta-squared:", partial_eta_sq, "\n")

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


# Proovisin homogeensuse analüüse (King et al., 2017 eeskujul ja ChatGPT abiga genereerisin koodi)
# hüpotees, et kitsamate ametinimetuste (ISCOcode) sees on isiksus homogeensem kui laiemate gruppide sees (ISCOcode_minor) - King et al., 2017 ei saanud kinnitust
A_homog <- lmer(A ~ 1 + (1 | ISCOcode) + (1 | ISCOcode_minor) + (1 | ISCOcode:ISCOcode_minor), data = B5)
var_components <- lme4::VarCorr(A_homog)
print(var_components)
var_ISCOcode <- var_components$ISCOcode
var_ISCOcodeminor <- var_components$ISCOcode_minor

total_variance <- attr(var_components, "sc")^2
icc_ISCOcode <- var_ISCOcode / total_variance
icc_ISCOcodeminor <- var_ISCOcodeminor / total_variance

O_homog <- lmer(`O-` ~ 1 + (1 | ISCOcode) + (1 | ISCOcode_minor) + (1 | ISCOcode:ISCOcode_minor), data = B5)
var_components <- lme4::VarCorr(O_homog)
print(var_components)

var_ISCOcode <- var_components$ISCOcode
var_ISCOcodeminor <- var_components$ISCOcode_minor

total_variance <- attr(var_components, "sc")^2
icc_ISCOcode <- var_ISCOcode / total_variance
icc_ISCOcodeminor <- var_ISCOcodeminor / total_variance

# pigem näib, et on vastupidi - mida suurem/laiem grupp (ehk ISCOcode_minor), seda suurem homogeensus/ICC? 
# O ICC näitajad märksa kõrgemad kui A
# aga seda peab veel mõtlema, ilmselt koodide jaotust muutma, aga jätan praegu (E, N, C mudelid ei konvergeerunud) 

