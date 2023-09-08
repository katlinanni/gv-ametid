require(tidyverse)
require(splitstackshape)
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
  filter(!is.na(XXXX_code)) %>%
  rename(O = `O-`) %>%
  stratified("XXXX_code", 1000) %>%
  mutate_at(pVars, ~scale(residuals(lm(. ~ gender + age, ,)))) %>% #if residualising for age and sex
  #mutate_at(pVars, scale) %>% ## if no residualising for age and sex
  mutate(O = -1*O)

eta2s = data.frame(
  XXXX = pVars %>% as.list %>% map_dbl(~ eta2(aov(pull(B5,.x) ~ XXXX_code, B5))),
  XXX = pVars %>% as.list %>% map_dbl(~ eta2(aov(pull(B5,.x) ~ XXX_code, B5))),
  XX = pVars %>% as.list %>% map_dbl(~ eta2(aov(pull(B5,.x) ~ XX_code, B5))),
  X = pVars %>% as.list %>% map_dbl(~ eta2(aov(pull(B5,.x) ~ X_code, B5)))
) %>% mutate(trait = pVars)

raw = B5 %>% group_by(XXXX_code) %>% summarise_at(pVars, c(mean, sd, length)) %>% mutate(XXX_code = strtrim(XXXX_code,3)) %>%
  left_join(B5 %>% group_by(XXX_code) %>% summarise_at(pVars, c(mean, sd, length)) %>% mutate(XX_code = strtrim(XXX_code,2)), by = "XXX_code") %>% 
  left_join(B5 %>% group_by(XX_code) %>% summarise_at(pVars, c(mean, sd, length))%>% mutate(X_code = strtrim(XX_code,1)), by = "XX_code") %>% 
  left_join(B5 %>% group_by(X_code) %>% summarise_at(pVars, c(mean, sd, length)), by = "X_code") %>%
  left_join(select(ISCO, XXXX_code, XXXX_name, XXX_name, XX_name, X_name), raw, by="XXXX_code") %>%
  filter(!is.na(XXXX_code))
  
names(raw) = names(raw) %>% gsub("fn1","mean",., fixed = T) %>%  gsub("fn2","sd",., fixed = T) %>% gsub("fn3","N",., fixed = T)  %>% 
  gsub(".y.y","_X",., fixed = T) %>% gsub(".x.x","_XX",., fixed = T) %>% gsub(".y","_XXX",., fixed = T) %>% gsub(".x","_XXXX",., fixed = T) 



#### Bayesian averaging of the Big Five scores ####

# Means/variances of all 4-digit job groups will be smoothed towards the means/variances of broader groups, according to the weighting below
# Means/variances of 4-digit ISCO groups with at least 100 individuals in 3-digit group are averaged towards the 3-digit group means/variances
# Means/variances of other groups are averaged towards 2-digit group means/variances, and where N < 100 in 2-digit groups, towards the means/variances of 1-digit groups

## To see how the weights of data and priors vary with sample size
w = vector(length = 201)
for(i in 0:length(w)-1) {  
  dp = (i/k)^2 * k
  w[i+1] = dp / (k + dp)
}
plot(0:200, w, type="l", xlab="Number of people in the ISCO XXXX group", ylab="Proportion of ISCO XXXX mean/variance over XXX/XX/X mean/variance", ylim=c(0,1), xaxt="n", axes=F)
lines(0:25, w[1:26], col="white", lwd=2, lty=2)
axis(2, at=seq(0,1,0.1))
axis(1, at=seq(0, length(w)-1, by=25), labels=seq(0, length(w)-1, by=25))
map(as.list(c(25,50,75,100,125,150,175,200)), ~lines(c(.x,.x),c(-0.5,w[.x+1]), col="grey80"))
map(as.list(c(25,50,75,100,125,150, 175, 200)), ~lines(c(-1*.x,.x),c(w[.x+1],w[.x+1]), col="grey80"))

tmp1 = raw %>% filter(N_N_XXX >= 100) %>% select(contains("XXXX"), ends_with("XXX")) ## almost all observations; N > 100 for XXX code
tmp2 = raw %>% filter(N_N_XXX < 100 & N_N_XX >= 100)  %>% select(contains("XXXX"), ends_with("_XX")) ## include jobs where N < 100 for XXX codes but n > 100 for XX code
tmp3 = raw %>% filter(N_N_XX < 100 & N_N_X >= 100)  %>% select(contains("XXXX"), ends_with("_X")) ## include jobs where N < 100 for XX codes but n > 100 for X code

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

rm(tmp1, tmp2, tmp3, smoothed1, smoothed2, smoothed3, smoothed1_means, smoothed1_sds, smoothed2_means, smoothed2_sds, smoothed3_means, smoothed3_sds)

## Combine Bayesian-averaged (smoothed) group means with the raw means

res = raw %>% left_join(smoothed_means, by="XXXX_code") %>% left_join(smoothed_sds, by="XXXX_code") %>% select(-contains(".x"), -contains(".y"))

# res %>% select(XXXX_name, N_mean:C_sd, N_N_XXXX) %>% rename(N = N_N_XXXX) %>% mutate_at(2:11, ~round(.,2)) %>% datatable %>% saveWidget("Means.html")

### Sanity check

cor(select(res, N_mean_XXXX:C_mean_XXXX), select(res, N_mean:C_mean))
cor(select(res, N_sd_XXXX:C_sd_XXXX), select(res, N_sd:C_sd))

ggplot(res, aes(x = N_mean, y = N_mean_XXXX, size = N_N_XXXX )) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(res, aes(x = N_sd, y = N_sd_XXXX, size = N_N_XXXX )) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1)


### Are means and variances correlated, suggesting scores with higher/lower Big Five trait levels are more selection?

cor(raw %>% select(N_mean_XXXX:C_mean_XXXX), raw %>% select(N_sd_XXXX:C_sd_XXXX), method="spearman") ## non-smoothed, for check
cor(res %>% select(N_mean:C_mean), res %>% select(N_sd:C_sd), method="spearman") ## smoothed

map2(
  .x = as.list(c("N_mean","E_mean","O_mean","A_mean","C_mean")), 
  .y = as.list(c("N_sd","E_sd","O_sd","A_sd","C_sd")),
  ~summary(lm(rank(pull(res,.y)) ~ rank(pull(res, .x)) + res$N_N_XXXX))$coef) %>% 
  as.data.frame %>% select(-contains("value")) %>% round(3) %>%
  `rownames<-`(c("Intercept","Mean","N")) %>%
  `names<-`(paste(rep(pVars,each=3),rep(c("Est","SE","p"), times=3), sep="_"))

p = map2(
  .x = as.list(c("N_mean","E_mean","O_mean","A_mean","C_mean")), 
  .y = as.list(c("N_sd","E_sd","O_sd","A_sd","C_sd")),
  ~ggplot(tibble(Mean = pull(res,.x), SD = pull(res, .y), N =  pull(res,N_N_XXXX)), aes(x = Mean, y = SD, size = N)) + 
    geom_point() +
    geom_smooth() + 
    theme_minimal() +
    theme(legend.position = "none")
)

# cowplot them

### wolfram comparison

wolfram = readxl::read_xlsx("wolfram.xlsx") %>% mutate(XXXX_code = as.character(strtrim(Occupation,4)))

wolfram = res %>% select(XXXX_code, XXXX_name, N_mean:O_sd, N_N_XXXX, -ends_with(".y")) %>% left_join(wolfram, by="XXXX_code")

cor(select(wolfram, N_mean:C_mean), select(wolfram, contains("Big 5")), use="pairwise") %>% round(2)


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


# Interaktiivne MDS
require(ggiraph)
require(htmlwidgets)
library(RColorBrewer)

res <- res %>%
  mutate(across(N_mean:C_mean, ~ 50 + 10 * ((. - mean(.)) / sd(.)), .names = "T_{col}"))

d = dist(res %>% select(starts_with("T_")))
fit = cmdscale(d,eig=TRUE, k=2)

new = psych::target.rot(fit$points, select(res, T_E_mean,T_O_mean))$load %>% unclass
new = tibble(as.data.frame(new), select(res, starts_with("T_"), XXXX_name, XXXX_code)) %>% 
  left_join(select(ISCO, XXXX_code, X_name),by="XXXX_code") %>% 
  `names<-`(c("C","O","NT","ET","OT","AT","CT","Job","Code","Group")) %>%
  mutate(tooltip = paste(
    Job,
    "",
    paste("Neuroticism",round(NT,2), sep =" = "),
    paste("Extraversion",round(ET,2), sep =" = "),
    paste("Openness",round(OT,2), sep =" = "),
    paste("Agreeableness",round(AT,2), sep =" = "),
    paste("Conscientiousness",round(CT,2), sep =" = "), sep="\n"))

theme_set(theme_minimal())
sp = ggplot(new %>% filter(!is.na(Group))) +
  geom_point_interactive(aes(x=C,y=O, color=Group, tooltip = tooltip), size=3) +
  xlab("~ E+,N-,C+,A-,O+") +
  ylab("~ O+,C-,A+,E+") +
  theme(
    legend.text = element_text(size=4), 
    legend.title = element_text(size=5),
    axis.title = element_text(size=8)) +
  scale_color_brewer_interactive(palette="Paired")

girafe(ggobj = sp) %>% saveWidget("JobMap.html", selfcontained = F, libdir = "JobMap", title = "Personality trait map of jobs")




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

