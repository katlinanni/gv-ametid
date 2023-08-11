require(tidyverse)
library(dplyr)
require(vctrs)

B5 = read_csv("B5_scores_selfreport.csv")
BG <- read.csv("dataall.csv", header=T, sep = ";", dec =".") %>%
  dplyr::select(person_skood, person_gender_code, pt_answerset_age_at_agreement, pt_answerset_questionnaire_language_code) %>%
  rename(scode = person_skood, gender = person_gender_code, age = pt_answerset_age_at_agreement, language = pt_answerset_questionnaire_language_code)
load("new_isco.RData")

B5 = left_join(B5, BG, "scode") %>% filter(language == "et") %>% left_join(new_isco %>% dplyr::select(c("scode","ISCOcode_minor", "ISCOname_minor", "ISCOcode", "ISCOname")), by = "scode")
uh = table(new_isco$ISCOname) %>% .[. > 95] %>% names

B5$N = scale(residuals(lm(N ~ gender + age, B5)))
B5$E = scale(residuals(lm(E ~ gender + age, B5)))
B5$`O-` = scale(residuals(lm(B5$`O-` ~ gender + age, B5)))
B5$A = scale(residuals(lm(A ~ gender + age, B5)))
B5$C = scale(residuals(lm(C ~ gender + age, B5)))

B5 = B5 %>% filter(ISCOname %in% uh)

count = vec_count(B5$ISCOname)
count = rename(count, ISCOname = key)

tapply(scale(B5$N), B5$ISCOname, mean) %>% sort
tapply(scale(B5$E), B5$ISCOname, mean) %>% sort
tapply(scale(B5$`O-`)*(-1), B5$ISCOname, mean) %>% sort
tapply(scale(B5$A), B5$ISCOname, mean) %>% sort
tapply(scale(B5$C), B5$ISCOname, mean) %>% sort
#tapply(scale(B5$`LS-`)*(-1), B5$ISCOname, mean) %>% sort

means_N = tapply(scale(B5$N), B5$ISCOname, function(x) c(mean(x), sd(x)))
means_E = tapply(scale(B5$E), B5$ISCOname, function(x) c(mean(x), sd(x)))
means_O = tapply(B5$`O-`*(-1), B5$ISCOname, function(x) c(mean(x), sd(x)))
means_A = tapply(scale(B5$A), B5$ISCOname, function(x) c(mean(x), sd(x)))
means_C = tapply(scale(B5$C), B5$ISCOname, function(x) c(mean(x), sd(x)))

# tabel (M ja SD)
B5means = data.frame(means_N, means_E, means_O, means_A, means_C)
B5means_sd = B5 %>% group_by(ISCOname) %>% summarize(across(N:'O-', list(mean = mean, sd = sd)))

# vaatasin plotte
library(ggplot2)
ggplot(B5, aes(x = reorder(ISCOname, A), y = A)) +
  geom_boxplot() +
  xlab("ISCOname") +
  ylab("A") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname, E), y = E)) +
  geom_boxplot() +
  xlab("ISCOname") +
  ylab("E") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname, N), y = N)) +
  geom_boxplot() +
  xlab("ISCOname") +
  ylab("N") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname, `O-`), y = `O-`)) +
  geom_boxplot() +
  xlab("ISCOname") +
  ylab("O-") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(B5, aes(x = reorder(ISCOname, C), y = C)) +
  geom_boxplot() +
  xlab("ISCOname") +
  ylab("C") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Lineaarsed mudelid (Wolfram, 2023 eeskujul)

model_E <- lmer(E ~ (1 | ISCOname), data = B5)
summary(model_E)
model_A <- lmer(A ~ (1 | ISCOname), data = B5)
summary(model_A)
model_N <- lmer(N ~ (1 | ISCOname), data = B5)
summary(model_N)
model_O <- lmer(`O-` ~ (1 | ISCOname), data = B5)
summary(model_O)
model_C <- lmer(C ~ (1 | ISCOname), data = B5)
summary(model_C)


### Ametite M ja SD seos - kaudselt homogeensuse analüüsimiseks?
# Do occupations with higher (or lower) average scores on a certain 
# personality trait also have more (or less) variability within those occupations on that trait?

occupational_group_stats <- B5 %>%
  group_by(ISCOname) %>%
  summarize(mean_trait = mean(A),
            sd_trait = sd(A))
correlation_A <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'A' (between-occupation)", y = "Standard Deviation of 'A' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

occupational_group_stats <- B5 %>%
  group_by(ISCOname) %>%
  summarize(mean_trait = mean(E),
            sd_trait = sd(E))
correlation_E <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'E' (between-occupation)", y = "Standard Deviation of 'E' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

occupational_group_stats <- B5 %>%
  group_by(ISCOname) %>%
  summarize(mean_trait = mean(N),
            sd_trait = sd(N))
correlation_N <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'N' (between-occupation)", y = "Standard Deviation of 'E' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

occupational_group_stats <- B5 %>%
  group_by(ISCOname) %>%
  summarize(mean_trait = mean(C),
            sd_trait = sd(C))
correlation_C <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'C' (between-occupation)", y = "Standard Deviation of 'E' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

occupational_group_stats <- B5 %>%
  group_by(ISCOname) %>%
  summarize(mean_trait = mean(`O-`),
            sd_trait = sd(`O-`))
correlation_O <- cor(occupational_group_stats$mean_trait, occupational_group_stats$sd_trait)

ggplot(occupational_group_stats, aes(x = mean_trait, y = sd_trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean of 'O' (between-occupation)", y = "Standard Deviation of 'O' (within-occupation)",
       title = "Relationship between within-occupation variation and between-occupation mean")

# katsetused profiilide klastrite leidmiseks
library(FactoMineR)
library(factoextra)

pca <- PCA(B5[, c('E', 'A', 'C', 'N', 'O-')], graph = FALSE)
pca_df <- data.frame(ISCOcode = B5$ISCOcode, pca$ind$coord)

mean_scores <- pca_df %>%
  group_by(ISCOcode) %>%
  summarise(Dim1 = mean(Dim.1), Dim2 = mean(Dim.2))

ggplot(mean_scores, aes(x = Dim1, y = Dim2)) +
  geom_point() +
  geom_text(aes(label = ISCOcode), vjust = 1, hjust = 1) +
  labs(x = "Dimension 1", y = "Dimension 2", title = "PCA of Occupations Based on Personality Traits")

#heatmap
mean_scores <- B5 %>%
  group_by(ISCOcode) %>%
  summarise(
    C_mean = mean(C, na.rm = TRUE),
    A_mean = mean(A, na.rm = TRUE),
    O_mean = mean(`O-`*(-1), na.rm = TRUE),
    E_mean = mean(E, na.rm = TRUE),
    N_mean = mean(N, na.rm = TRUE)
  )


library(pheatmap)
rownames(mean_scores) <- mean_scores$ISCOcode
mean_scores_matrix <- as.matrix(mean_scores[, -1])

pheatmap(mean_scores_matrix, 
         main = "Mean Personality Scores by Occupation", 
         color = colorRampPalette(c("blue", "white", "red"))(50),  # Gradient of colors
         scale = "row", 
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean", 
         clustering_method = "complete",
         show_rownames = FALSE)


wss <- (nrow(mean_scores_matrix) - 1) * sum(apply(mean_scores_matrix, 2, var))
for (i in 2:15) {
  set.seed(123) # Setting seed for reproducibility
  wss[i] <- sum(kmeans(mean_scores_matrix, centers = i)$tot.withinss)
}

plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

set.seed(123) 
kmeans_result <- kmeans(mean_scores_matrix, centers = 4) 
kmeans_result$cluster
kmeans_result$centers

# visualising with PCA
pca_result <- prcomp(mean_scores_matrix)
pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$cluster <- kmeans_result$cluster

ggplot(data = pca_data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point() +
  labs(color = 'Cluster') +
  theme_minimal()

mean_scores$cluster <- kmeans_result$cluster
cluster_table <- mean_scores %>%
  select(ISCOcode, cluster) %>%
  arrange(cluster, ISCOcode) %>%
  group_by(cluster) %>%
  summarise(ISCOcode = paste(ISCOcode, collapse = ", "))

centers_long <- kmeans_result$centers %>%
  as.data.frame() %>%
  mutate(cluster = row_number()) %>%
  pivot_longer(cols = C_mean:N_mean, names_to = "trait", values_to = "mean_value")

ggplot(centers_long, aes(x = trait, y = mean_value, group = cluster, color = factor(cluster))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Cluster Centers across Personality Traits",
       x = "Personality Trait",
       y = "Mean Value",
       color = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Proovisin huvi pärast vaadata kahe ameti isiksuseprofiilide sarnasust (enamik katsetusi näitas, et sarnased ISCO-koodid ei pruugi olla väga sarnase profiiliga, all üks suurema sarnasusega)
# Extract mean personality scores for the two ISCOname values
isco1_values <- c(means_N[["Specialist Medical Practitioners"]][1], means_E[["Specialist Medical Practitioners"]][1], means_O[["Specialist Medical Practitioners"]][1], 
                  means_A[["Specialist Medical Practitioners"]][1], means_C[["Specialist Medical Practitioners"]][1])

isco2_values <- c(means_N[["Medical Doctors (unspecified)"]][1], 
                  means_E[["Medical Doctors (unspecified)"]][1], 
                  means_O[["Medical Doctors (unspecified)"]][1], 
                  means_A[["Medical Doctors (unspecified)"]][1], 
                  means_C[["Medical Doctors (unspecified)"]][1])


# Compute cosine similarity
cosine_similarity <- sum(isco1_values * isco2_values) / (sqrt(sum(isco1_values^2)) * sqrt(sum(isco2_values^2)))
cosine_similarity