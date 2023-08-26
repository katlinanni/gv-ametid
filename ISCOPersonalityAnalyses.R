require(tidyverse)
require(splitstackshape)
require(effectsize)
require(DT)

pVars = c("N","E","O","A","C")

B5 = read.csv("~/.gvpers/otherQuestionnaireResponses.csv") %>% 
  select(Personskood, Persongender, ageAtAgreement, education, questionnaireLanguage) %>%
  rename(scode = Personskood, gender = Persongender, age = ageAtAgreement, language = questionnaireLanguage) %>%
  left_join(read_csv("~/.gvpers/B5_scores_selfreport.csv"), "scode") %>%
  filter(language == "et") %>%
  right_join(new_isco, "scode") %>% 
  as_tibble %>% 
  filter(!is.na(N)) %>% 
  rename(O = `O-`) %>%
  stratified("XXXX_code", 1000) %>%
  mutate_at(pVars, ~scale(residuals(lm(. ~ gender + age, ,)))) %>%
  mutate(O = -1*O)

# B5 = B5 %>% mutate_at(c("N","A","E","C", "O"), scale) ## if no residualised for age and sex

## More fine-grained groups give bigger effects, so less variability in sample means for broader groups. 
## This means later Bayesian averaging will generally push means and variances in smaller samples towards the population means
eta_squared(aov(O ~ XXXX_code, B5),alternative = "two.sided")
eta_squared(aov(O ~ XXX_code, B5),alternative = "two.sided")
eta_squared(aov(O ~ XX_code, B5),alternative = "two.sided")
eta_squared(aov(O ~ X_code, B5),alternative = "two.sided")

eta_squared(aov(E ~ XXXX_code, B5),alternative = "two.sided")
eta_squared(aov(E ~ XXX_code, B5),alternative = "two.sided")
eta_squared(aov(E ~ XX_code, B5),alternative = "two.sided")
eta_squared(aov(E ~ X_code, B5),alternative = "two.sided")

eta_squared(aov(A ~ XXXX_code, B5),alternative = "two.sided")
eta_squared(aov(N ~ XXXX_code, B5),alternative = "two.sided")
eta_squared(aov(C ~ XXXX_code, B5),alternative = "two.sided")

XXXX_means = B5 %>% group_by(XXXX_code) %>% summarise_at(pVars, c(mean, sd, length)) %>% mutate(XXX_code = strtrim(XXXX_code,3)) %>%
  left_join(B5 %>% group_by(XXX_code) %>% summarise_at(pVars, c(mean, sd, length)) %>% mutate(XX_code = strtrim(XXX_code,2)), by = "XXX_code") %>% 
  left_join(B5 %>% group_by(XX_code) %>% summarise_at(pVars, c(mean, sd, length))%>% mutate(X_code = strtrim(XX_code,1)), by = "XX_code") %>% 
  left_join(B5 %>% group_by(X_code) %>% summarise_at(pVars, c(mean, sd, length)), by = "X_code")

names(XXXX_means) = names(XXXX_means) %>% gsub("fn1","mean",., fixed = T) %>%  gsub("fn2","sd",., fixed = T) %>% gsub("fn3","N",., fixed = T)  %>% 
  gsub(".y.y","_X",., fixed = T) %>% gsub(".x.x","_XX",., fixed = T) %>% gsub(".y","_XXX",., fixed = T) %>% gsub(".x","_XXXX",., fixed = T) 

XXXX_means = ISCO %>% select(XXXX_code, XXXX_name) %>% right_join(XXXX_means, by="XXXX_code")

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

## To see how the weights of data and priors vary with sample size
w = vector(length = 150)
for(i in 1:length(w)) {
  dp = (i/k)^2 * k
  w[i] = dp / (k + dp)
}
plot(w, type="l", xlab="N", ylab="Proportion of data over prior", ylim=c(0,1))

### Bayesian averaging of the Big Five scores
# Those in 3-digit ISCO groups with at least 100 individuals averaged towards the 3-digit group
# Others averaged towards 2-digit groups, and if N < 100 in that group, towards 1-digit group

tmp1 = XXXX_means %>% filter(N_N_XXX >= 100) %>% select(contains("XXXX"), ends_with("XXX")) ## almost all observations
tmp2 = XXXX_means %>% filter(N_N_XXX < 100 & N_N_XX >= 100)  %>% select(contains("XXXX"), ends_with("_XX"))
tmp3 = XXXX_means %>% filter(N_N_XX < 100 & N_N_X >= 100)  %>% select(contains("XXXX"), ends_with("_X"))

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

names(smoothed1_means) = names(smoothed2_means) = names(smoothed3_means) = c(paste(pVars, "mean",sep ="_"), "XXXX_code","XXXX_name")
names(smoothed1_sds) = names(smoothed2_sds) = names(smoothed3_sds) = c(paste(pVars, "sd",sep ="_"), "XXXX_code","XXXX_name")

smoothed_means = rbind(smoothed1_means, smoothed2_means, smoothed3_means)
smoothed_sds = rbind(smoothed1_sds, smoothed2_sds, smoothed3_sds)

## Combine Bayesian-averaged group means with the raw means

res = XXXX_means %>% left_join(smoothed_means, by="XXXX_code") %>% left_join(smoothed_sds, by="XXXX_code")

### Sanity check

cor(select(res, N_mean_XXXX:C_mean_XXXX), select(res, N_mean:C_mean))
cor(select(res, N_sd_XXXX:C_sd_XXXX), select(res, N_sd:C_sd))

plot(res$N_mean, res$N_mean_XXXX)
lines(c(-10,10),c(-10,10))

plot(res$N_sd, res$N_sd_XXXX)
lines(c(0,2),c(0,2))

### Are means and variances correlated, suggesting scores with higher/lower Big Five trait levels are more selection?

cor(XXXX_means %>% select(N_mean_XXXX:C_mean_XXXX), XXXX_means %>% select(N_sd_XXXX:C_sd_XXXX), method="spearman")
cor(res %>% select(N_mean:C_mean), res %>% select(N_sd:C_sd), method="spearman")

### wolfram comparison

wolfram = readxl::read_xlsx("wolfram.xlsx") %>% mutate(XXXX_code = as.character(strtrim(Occupation,4)))

wolfram = res %>% select(XXXX_code, XXXX_name, N_mean:O_sd, N_N_XXXX, -ends_with(".y")) %>% left_join(wolfram, by="XXXX_code")

cor(select(wolfram, N_mean:C_mean), select(wolfram, contains("Big 5")), use="pairwise") %>% round(2)

### Just exploring ...

res %>% arrange(desc(O_mean)) %>% select(XXXX_name, N_mean:C_sd, N_N_XXXX) %>% slice(1:10)

res %>% filter(XXXX_name %in% "Psychologists") %>% select(XXXX_name, N_mean:C_sd, N_N_XXXX) 

#res %>% select(XXXX_name, N_mean:C_sd, N_N_XXXX, -ends_with(".y")) %>% mutate_at(2:11, ~round(.,2)) %>% datatable %>% saveWidget("Means.html")

### MDS

d = dist(res %>% select(N_mean:C_mean))
fit = cmdscale(d,eig=TRUE, k=2)
plot(fit$points, cex=0)
text(fit$points, res$XXXX_name, cex=.6)
cor(fit$points, select(res, N_mean:C_mean))
new = unclass(psych::target.rot(fit$points, select(res, E_mean,O_mean))$load)
cor(new,select(res, N_mean:C_mean))
svg("jobs.svg", width=25, height=25)

plot(new, cex=0, xlab="~ E+,C+,N-,A- ", ylab="~ O+,C-,A+")
text(new, strtrim(res$XXXX_name,50), cex=.5)
dev.off()

svg("jobs.svg", width=25, height=25)
plot(new, cex=0, xlab="~ E+,C+,N-,A- ", ylab="~ O+,C-,A+")
text(new, strtrim(res$XXXX_name,50), cex=.5,
     col=c("black","red")[as.factor(strtrim(res$XXXX_code,2)==83)])
dev.off()

colored_groups <- ifelse(strtrim(res$XXXX_code, 2) == "83", "red", 
                         ifelse(strtrim(res$XXXX_code, 2) == "23", "green", 
                                ifelse(strtrim(res$XXXX_code, 2) == "12", "orange",
                                ifelse(strtrim(res$XXXX_code, 2) == "25", "blue", "black"))))

svg("jobs.svg", width=25, height=25)
plot(new, cex=0, xlab="~ E+,C+,N-,A- ", ylab="~ O+,C-,A+")
text(new, strtrim(res$XXXX_name,50), cex=1, col=colored_groups)

legend("topright",                   
       legend = c("ISCO 83 (Drivers and Mobile Plant Operators)", "ISCO 23 (Teaching Professionals)", "ISCO 12 (Administrative and Commercial Managers)", "ISCO 25 (IT-professionals)", "Other"),
       fill = c("red", "green", "orange", "blue", "black"), 
       cex = 2,                    
       title = "Groups",             
       box.lwd = 1)
dev.off()

### K-Means Clustering?
means <- res[, c("N_mean", "E_mean", "O_mean", "A_mean", "C_mean")]
wss <- sapply(1:20, function(k) {
  kmeans(means, centers = k, nstart = 10)$tot.withinss
})

plot(1:20, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")

set.seed(123)
kmeans_result <- kmeans(means, centers = 5, nstart = 10)

clusters <- kmeans_result$cluster
results_df1 <- data.frame(res$XXXX_name, clusters)

cluster_centers <- kmeans_result$centers
print(cluster_centers)

# visualising
library(factoextra)
fviz_cluster(kmeans_result, data = means)

pca_result <- prcomp(means)
pca_scores <- pca_result$x[, 1:2]
pca_df <- data.frame(pca_scores, cluster = kmeans_result$cluster)

ggplot(pca_df, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "PCA Plot with K-means Clusters", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

centers_long <- kmeans_result$centers %>% 
  as.data.frame() %>%
  rownames_to_column("cluster") %>%
  gather(trait, mean_value, N_mean:C_mean)

ggplot(centers_long, aes(x = trait, y = mean_value, group = cluster, color = factor(cluster))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Cluster Centers across Personality Traits",
       x = "Personality Trait",
       y = "Mean Value",
       color = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cluster_assignment <- res %>%
  select(XXXX_name) %>%
  mutate(cluster = kmeans_result$cluster) %>%
  arrange(cluster, XXXX_name)
