require(tidyverse)
library(dplyr)
require(vctrs)

B5 = read_csv("B5_scores_selfreport.csv")
# see BG-fail (dataall.csv) on sul vist natuke teine
BG <- read.csv("dataall.csv", header=T, sep = ";", dec =".") %>%
  dplyr::select(person_skood, person_gender_code, pt_answerset_age_at_agreement, pt_answerset_questionnaire_language_code) %>%
  rename(scode = person_skood, gender = person_gender_code, age = pt_answerset_age_at_agreement, language = pt_answerset_questionnaire_language_code)
load("new_isco.rda")

B5 = left_join(B5, BG, "scode") %>% filter(language == "et") %>% left_join(new_isco %>% dplyr::select(c("scode","ISCOcode_minor", "ISCOname_minor", "ISCOcode", "ISCOname")), by = "scode")
uh = table(new_isco$ISCOname_minor) %>% .[. > 101] %>% names

B5$N = scale(residuals(lm(N ~ gender + age, B5)))
B5$E = scale(residuals(lm(E ~ gender + age, B5)))
B5$`O-` = scale(residuals(lm(B5$`O-` ~ gender + age, B5)))
B5$A = scale(residuals(lm(A ~ gender + age, B5)))
B5$C = scale(residuals(lm(C ~ gender + age, B5)))

# removing outliers?
# Specify the columns to remove outliers from
columns <- c("O-", "A", "N", "E", "C")

# Set the threshold for outlier detection - 3? 4?
threshold <- 3

# Identify and remove outliers
B5 <- B5
for (col in columns) {
  B5 <- B5[abs(B5[[col]]) <= threshold, ]
}

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

# tabel (M ja SD)
B5means = data.frame(means_N, means_E, means_O, means_A, means_C)
B5means_sd = B5 %>% group_by(ISCOname_minor) %>% summarize(across(N:'O-', list(mean = mean, sd = sd)))

# MANOVA
library(car)
library(rstatix)
manova_result <- manova(cbind(A, `O-`, C, E, N) ~ ISCOname_minor, data = B5)
summary(manova_result)

# samas MANOVA eeldused on ilmselt täitmata
# nt univariate normality assumption
B5 %>%
  group_by(ISCOname_minor) %>%
  shapiro_test(E) %>%
  arrange(p)

B5 %>%
  group_by(ISCOname_minor) %>%
  shapiro_test(N) %>%
  arrange(p)

...

# korrelatsioonid ja homogeneity of covariances
B5 %>% cor_test(A, `O-`, C, E, N)
box_m(B5[, c("A", "O-", "C", "E", "N")], B5$ISCOname_minor)

# proovisin mitteparameetrilist MANOVAt, aga mälu vähe? Ehk siis ei saanud tulemust
install.packages("vegan")
library(vegan)

B5_matrix <- B5[,c("A", "O-", "C", "E", "N")]
B5$ISCOname_minor <- as.factor(B5$ISCOname_minor)
permanova_result <- adonis2(B5_matrix ~ B5$ISCOname_minor, permutations = 999)
permanova_result <- adonis2(B5_matrix ~ B5$ISCOname_minor, permutations = 100)

# ANOVA 
# kõikide mudelitega on Levene test < .001 ehk peaks kasutama ilmselt Welch ANOVAt 
leveneTest(modelA)
leveneTest(modelC)
leveneTest(modelN)
leveneTest(modelE)
leveneTest(modelO)

# ANOVA, Welch ANOVA ja Kruskal-Wallis
grouped.data <- B5 %>%
  gather(key = "variable", value = "value", A, `O-`, C, E, N) %>%
  group_by(variable)

grouped.data %>% anova_test(value ~ ISCOname_minor)
grouped.data %>% welch_anova_test(value ~ ISCOname_minor)
grouped.data %>% kruskal_test(value ~ ISCOname_minor)

# Games-Howell post-hoc
pwc <- B5 %>%
  gather(key = "variables", value = "value", A, `O-`, C, E, N) %>%
  group_by(variables) %>%
  games_howell_test(value ~ ISCOname_minor) %>%
  select(-conf.low, -conf.high) # Remove details
pwc

significant_rows <- pwc %>%
  filter(p.adj.signif != "ns")

# vaatasin plotte
library(ggplot2)
ggplot(B5, aes(x = reorder(ISCOname_minor, A), y = A)) +
  geom_boxplot() +
  xlab("ISCOname_minor") +
  ylab("A") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname_minor, E), y = E)) +
  geom_boxplot() +
  xlab("ISCOname_minor") +
  ylab("E") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname_minor, N), y = N)) +
  geom_boxplot() +
  xlab("ISCOname_minor") +
  ylab("N") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname_minor, `O-`), y = `O-`)) +
  geom_boxplot() +
  xlab("ISCOname_minor") +
  ylab("O-") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname_minor, C), y = C)) +
  geom_boxplot() +
  xlab("ISCOname_minor") +
  ylab("C") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Lineaarsed mudelid (Wolfram, 2023 eeskujul)

model_E <- lmer(E ~ (1 | ISCOname_minor), data = B5)
summary(model_E)
model_A <- lmer(A ~ (1 | ISCOname_minor), data = B5)
summary(model_A)
model_N <- lmer(N ~ (1 | ISCOname_minor), data = B5)
summary(model_N)
model_O <- lmer(`O-` ~ (1 | ISCOname_minor), data = B5)
summary(model_O)
model_C <- lmer(C ~ (1 | ISCOname_minor), data = B5)
summary(model_C)


### Ametite M ja SD seos - kaudselt homogeensuse analüüsimiseks?
# Do occupations with higher (or lower) average scores on a certain 
# personality trait also have more (or less) variability within those occupations on that trait?

occupational_group_stats <- B5 %>%
  group_by(ISCOname_minor) %>%
  summarize(mean_trait = mean(A),
            sd_trait = sd(A))
correlation_A <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'A' (between-occupation)", y = "Standard Deviation of 'A' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

occupational_group_stats <- B5 %>%
  group_by(ISCOname_minor) %>%
  summarize(mean_trait = mean(E),
            sd_trait = sd(E))
correlation_E <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'E' (between-occupation)", y = "Standard Deviation of 'E' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

occupational_group_stats <- B5 %>%
  group_by(ISCOname_minor) %>%
  summarize(mean_trait = mean(N),
            sd_trait = sd(N))
correlation_N <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

occupational_group_stats <- B5 %>%
  group_by(ISCOname_minor) %>%
  summarize(mean_trait = mean(C),
            sd_trait = sd(C))
correlation_C <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

occupational_group_stats <- B5 %>%
  group_by(ISCOname_minor) %>%
  summarize(mean_trait = mean(`O-`),
            sd_trait = sd(`O-`))
correlation_O <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'O' (between-occupation)", y = "Standard Deviation of 'O' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

# Proovisin homogeensuse analüüse (King et al., 2017 eeskujul ja ChatGPT abiga genereerisin koodi)
# hüpotees, et kitsamate ametinimetuste (ISCOcode) sees on isiksus homogeensem kui laiemate gruppide sees (ISCOcode_minor) - King et al., 2017 ei saanud kinnitust
library(lme4)
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

