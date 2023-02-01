
```{r}
library(biotools)
library(vegan)
library(ks)
library(car)
library(klaR)
```

Read in the data.
```{r}
prim <- read.csv("/Users/briannaschuh/Documents/Yale-Spring-2022/S&DS363/premierfifa20.csv", na.strings = " ")
```

Take the first position in the player_positions column
```{r}
prim$player_positions <- as.character(prim$player_positions)
for (i in 1:length(prim$player_positions)){
  prim$player_positions[i] <- unlist(strsplit(prim$player_positions[i], split = ',', fixed = TRUE))[1]
}
```

Create a vector for each of the three positions
```{r}
forward <- c("ST", "LS", "RS", "CF", "LW", "RW")
midfielder <- c("CAM", "CDM", "CM", "LCM", "LDM", "LM", "RCM", "RDM", "RM")
defense <- c("CB", "GK", "LB", "LCB", "LWB", "RB", "RCB", "RWB")
pos <- c("forward", "midfielder", "defense")
```

Classify each position into one of the three positions: forward, midfielder, and defense
```{r}
prim$player_positions[prim$player_positions %in% forward] <- "forward"
prim$player_positions[prim$player_positions %in% midfielder] <- "midfielder"
prim$player_positions[prim$player_positions %in% defense] <- "defense"
prim$player_positions <- as.factor(prim$player_positions)
```

Summary of players in the Premier League
```{r}
tablepos <- table(prim$player_positions)
barplot(tablepos, main = "Barplot of Player Positions", col = "blue", ylim = c(0, 300), xlab = "Positions", ylab = "Number of players")
```

Create boxplots for skill_moves, international_reputation, and weak_foot by position
```{r}
responsevec <- c("skill_moves", "international_reputation", "weak_foot")
for (i in responsevec){
  boxplot(prim[, i] ~ player_positions, data = prim, col = 'pink', main = paste(i, "by position"), ylab = "")
  #calculate means using the tapply function - could also use the by function
  means <- tapply(prim[, i], prim$player_positions, mean)
  points(means, col = "red", pch = 19, cex = 1.2)
  text(x = c(1:4), y = means+.2, labels = round(means,2))
}
```

Determine if skill_moves, if interational_reputation, or if weak_foot vary by positions
```{r}
surveyManova <- lm(cbind(skill_moves, international_reputation, weak_foot) ~ player_positions, 
                   data = prim)
summary.aov(surveyManova)
anova(surveyManova)   #Default is Pillai's trace
anova(surveyManova, test = "Wilks")
anova(surveyManova, test = "Roy")
```

Plot the residuals to see if they are normally distributed.
```{r}
source("http://www.reuningscherer.net/multivariate/R/CSQPlot.r.txt")

CSQPlot(surveyManova$residuals, label = "Residuals from Env. Survey MANOVA")

```

Perform transformations on overall score, mentatility composure, agility, and wages so that they are normally distributed.
```{r}
prim$overall <- prim$overall^(2)
prim$mentality_composure <- prim$mentality_composure^(2)
prim$movement_agility <- prim$movement_agility^(2)
prim$wage_eur <- prim$wage_eur^(1/3)
```

Look at the chi-square quantiles by position
```{r}
source("http://www.reuningscherer.net/multivariate/R/CSQPlot.r.txt")

par(mfrow = c(1,2), pty = "s", cex = 0.8)
CSQPlot(prim[prim$player_positions == "forward", c("overall", "wage_eur",  "mentality_composure", "movement_agility")], label = "Forward")
par(mfrow = c(1, 1))

par(mfrow = c(1,2), pty = "s", cex = 0.8)
CSQPlot(prim[prim$player_positions == "midfielder", c("overall", "wage_eur", "mentality_composure", "movement_agility")], label = "Midfielder")
par(mfrow = c(1, 1))

par(mfrow = c(1,2), pty = "s", cex = 0.8)
CSQPlot(prim[prim$player_positions == "defense", c("overall", "wage_eur", "mentality_composure", "movement_agility")], label = "Defense")
par(mfrow = c(1, 1))
```
Make covarience matrices. 
Perform Box-M test. We probably failed the Box-M test because we have a lot of data. 
```{r}
dim(prim)

print("Covariance Matrix for Forward Players")
cov(prim[prim$player_positions=="forward", c("overall", "wage_eur", "mentality_composure", "movement_agility")])
print("Covariance Matrix for Midfielders")
cov(prim[prim$player_positions=="midfielder", c("overall", "wage_eur", "mentality_composure", "movement_agility")])
print("Covariance Matrix for Defenders")
cov(prim[prim$player_positions=="defense", c("overall","wage_eur", "mentality_composure", "movement_agility")])

#Look at ratios of largest to smallest elements of the covariance matrices - if all less than 4, we're probably OK assuming covariances matrices are similar enough.  

for_cov <- cov(prim[prim$player_positions=="forward", c("overall", "wage_eur", "mentality_composure", "movement_agility")])
mid_cov <- cov(prim[prim$player_positions=="midfielder", c("overall", "wage_eur", "mentality_composure", "movement_agility")])
def_cov <- cov(prim[prim$player_positions=="defense", c("overall", "wage_eur", "mentality_composure", "movement_agility")])

print("Ratio of Largest to Smallest Covariance Elements for Forwards and Midfielders")
cov_rat <- for_cov/mid_cov
cov_rat[abs(cov_rat) < 1] <- 1/(cov_rat[abs(cov_rat) < 1])
round(cov_rat, 1)

print("Ratio of Largest to Smallest Covariance Elements for Forwards and Defenders")
cov_rat <- for_cov/def_cov
cov_rat[abs(cov_rat) < 1] <- 1/(cov_rat[abs(cov_rat) < 1])
round(cov_rat, 1)

print("Ratio of Largest to Smallest Covariance Elements for Midfilders and Defenders")
cov_rat <- mid_cov/def_cov
cov_rat[abs(cov_rat) < 1] <- 1/(cov_rat[abs(cov_rat) < 1])
round(cov_rat, 1)

boxM(prim[,c("overall", "wage_eur", "mentality_composure", "movement_agility")], prim$player_positions)
```

Make box plots of overall, wage, mentality composure, and movement agility by position.
```{r}
#Make labels vector
responsevec <- c("overall", "wage_eur", "mentality_composure", "movement_agility")
for (i in responsevec){
  boxplot(prim[, i] ~ player_positions, data = prim, col = 'yellow', main = paste(i, "by position"), ylab = "")
  #calculate means using the tapply function - could also use the by function
  means <- tapply(prim[, i], prim$player_positions, mean)
  points(means, col = "red", pch = 19, cex = 1.2)
  text(x = c(1:4), y = means+.2, labels = round(means,2))
}
boxplot(prim$overall)
meano <- mean(prim$overall)
meano
```

Perform manova on overall score, wage, mentality compsure, and movement agility by player_positions
```{r}
surveyManova <- lm(cbind(overall, wage_eur, mentality_composure, movement_agility) ~ player_positions, 
                   data = prim)
summary.aov(surveyManova)
anova(surveyManova)   #Default is Pillai's trace
anova(surveyManova, test = "Wilks")
anova(surveyManova, test = "Roy")
```

Anova of previous Manova
```{r}
summary(Anova(surveyManova), univariate = T)
```

Residual plots from MANOVA
```{r}
CSQPlot(surveyManova$residuals, label = "Residuals from Player Positions MANOVA")
```

Look at cofficients of MANOVA
```{r}
options(contrasts = c("contr.treatment", "contr.poly")) 

contrasts(prim$player_positions)
surveyManova$contrasts
surveyManova
```

Linear discriminant analysis
```{r}
source("http://www.reuningscherer.net/multivariate/R/discrim.r.txt")
prim.disc <- lda(prim[, c("overall", "wage_eur", "mentality_composure", "movement_agility")], grouping = prim$player_positions, prior = c(1/3, 1/3, 1/3))
names(prim.disc)
prim.disc
```

Coefficients of LDA
```{r}

print("Raw (Unstandardized) Coefficients")
round(prim.disc$scaling,2)

print("Normalized Coefficients")
round(prim.disc$scaling/sqrt(sum(prim.disc$scaling^2)),2)

print("Standardized Coefficients")
round(lda(scale(prim[, c("overall", "wage_eur", "mentality_composure", "movement_agility")]), grouping = prim$player_positions, priors = c(1/3, 1.3, 1/3))$scaling, 2)
```

Results of LDA
```{r}
# raw results - use the 'predict' function
ctraw <- table(prim$player_positions, predict(prim.disc)$class)
ctraw

# total percent correct
round(sum(diag(prop.table(ctraw))), 2)


#cross-validated results
prim.discCV <- lda(prim[, c("overall", "wage_eur", "mentality_composure", "movement_agility")], grouping = prim$player_positions, prior = c(1/3, 1/3, 1/3), CV = TRUE)
ctCV <- table(prim$player_positions, prim.discCV$class)
ctCV
# total percent correct
round(sum(diag(prop.table(ctCV))), 2)
```

We will look at this with no priors.
```{r}
prim.disc <- lda(prim[, c("overall", "wage_eur", "mentality_composure", "movement_agility")], grouping = prim$player_positions)
names(prim.disc)
prim.disc

```

Results with no priors. We have a higher accurarcy
```{r}
# raw results - use the 'predict' function
ctraw <- table(prim$player_positions, predict(prim.disc)$class)
ctraw

# total percent correct
round(sum(diag(prop.table(ctraw))), 2)


#cross-validated results
prim.discCV <- lda(prim[, c("overall", "wage_eur", "mentality_composure", "movement_agility")], grouping = prim$player_positions, CV = TRUE)
ctCV <- table(prim$player_positions, prim.discCV$class)
ctCV
# total percent correct
round(sum(diag(prop.table(ctCV))), 2)
```

Coefficiets with no prior assumptions
```{r}
print("Raw (Unstandardized) Coefficients")
round(prim.disc$scaling,2)

print("Normalized Coefficients")
round(prim.disc$scaling/sqrt(sum(prim.disc$scaling^2)),2)

print("Standardized Coefficients")
round(lda(scale(prim[, c("overall", "wage_eur", "mentality_composure", "movement_agility")]), grouping = prim$player_positions)$scaling, 2)
```

Boxplot in direction of LDA fuction
```{r}
#make boxplot in direction of linear discriminant function
#da_data
#get the scores
scores <- as.matrix(scale(prim[, c("overall", "wage_eur", "mentality_composure", "movement_agility")]))%*%matrix(prim.disc$scaling, ncol = 2)
#NOTE - if use cross-validation option, scores are calculated automatically
plot(scores[,1], scores[,2], type = "n", main = "Linear DCA scores for position data",
     xlab = "DCA Axis 1", ylab = "DCA Axis 2")
#scores
positions <- unique(prim$player_positions)
#positions <- names(summary(prim[, ]))

for (i in 1:3){
  points(scores[prim$player_positions == positions[i], 1],
         scores[prim$player_positions == positions[i], 2], col = i+1, pch = 15+i, cex = 1.1)
}
legend("topright", legend = positions, col = c(2:4), pch = c(15, 16, 17))
```

LDA again but without wage
```{r}
prim.disc <- lda(prim[, c("wage_eur", "mentality_composure", "movement_agility")], grouping = prim$player_positions)
names(prim.disc)
prim.disc
```

Scores of LDA without overall scores
```{r}
# raw results - use the 'predict' function
ctraw <- table(prim$player_positions, predict(prim.disc)$class)
ctraw

# total percent correct
round(sum(diag(prop.table(ctraw))), 2)


#cross-validated results
prim.discCV <- lda(prim[, c("wage_eur", "mentality_composure", "movement_agility")], grouping = prim$player_positions, CV = TRUE)
ctCV <- table(prim$player_positions, prim.discCV$class)
ctCV
# total percent correct
round(sum(diag(prop.table(ctCV))), 2)
```

Cofficiets of LDA without the overall scores
```{r}
print("Raw (Unstandardized) Coefficients")
round(prim.disc$scaling,2)

print("Normalized Coefficients")
round(prim.disc$scaling/sqrt(sum(prim.disc$scaling^2)),2)

print("Standardized Coefficients")
round(lda(scale(prim[, c("wage_eur", "mentality_composure", "movement_agility")]), grouping = prim$player_positions)$scaling, 2)
```

Linear DCA scores without the overall score
```{r}
#make boxplot in direction of linear discriminant function
#da_data
#get the scores
scores <- as.matrix(scale(prim[, c("wage_eur", "mentality_composure", "movement_agility")]))%*%matrix(prim.disc$scaling, ncol = 2)
#NOTE - if use cross-validation option, scores are calculated automatically
plot(scores[,1], scores[,2], type = "n", main = "Linear DCA scores for position data",
     xlab = "DCA Axis 1", ylab = "DCA Axis 2")
#scores
positions <- unique(prim$player_positions)
#positions <- names(summary(prim[, ]))

for (i in 1:3){
  points(scores[prim$player_positions == positions[i], 1],
         scores[prim$player_positions == positions[i], 2], col = i+1, pch = 15+i, cex = 1.1)
}
legend("topright", legend = positions, col = c(2:4), pch = c(15, 16, 17))
```

Stepwise Classificatio
```{r}
(step1 <- stepclass(player_positions ~ overall + wage_eur + mentality_composure + movement_agility, data = prim, method = "lda", direction = "both", fold = nrow(prim)))
names(step1)
step1$result.pm

(step3 <- stepclass(player_positions ~ overall + wage_eur + mentality_composure + movement_agility, data = prim, method = "qda", direction = "both", fold = nrow(prim)))
names(step3)
step3$result.pm

```

Multiple Response Permutation Procedure
```{r}
(mrpp1 <- mrpp(prim[,c("attacking_short_passing", "weight_kg", "defending_sliding_tackle", "potential")], prim$player_positions))
mrpp1

```

Chi-Sqaure Quatiles for Liverpool
```{r}
source("http://www.reuningscherer.net/multivariate/R/CSQPlot.r.txt")

par(mfrow = c(1,2), pty = "s", cex = 0.8)
CSQPlot(prim[prim$club == "Liverpool", c("overall", "wage_eur",  "mentality_composure", "movement_agility")], label = "Liverpool")
par(mfrow = c(1, 1))

par(mfrow = c(1,2), pty = "s", cex = 0.8)
CSQPlot(prim[prim$club == "Liverpool", c("overall", "wage_eur",  "mentality_composure", "movement_agility")], label = "Forward")
par(mfrow = c(1, 1))
```

Re-read the data
```{r}
prim <- read.csv("/Users/briannaschuh/Documents/Yale-Spring-2022/S&DS363/premierfifa20.csv", na.strings = " ")
```

Clean the data for PCA
```{r}
pcaprim <-prim[ , c(15, 5, 7, 8, 11, 12, 13, 14, 17, 18, 19, 45, 46, 47, 48, 49, 71, 72, 73)]
```

Remove NA values
```{r}
pcaprim <- na.omit(pcaprim)

dim(pcaprim)
```

Convert to chars
```{r}
pcaprim$player_positions <- as.character(pcaprim$player_positions)
for (i in 1:length(pcaprim$player_positions)){
  pcaprim$player_positions[i] <- unlist(strsplit(pcaprim$player_positions[i], split = ',', fixed = TRUE))[1]
}
```

Create a vector of each positio
```{r}
forward <- c("ST", "LS", "RS", "CF", "LW", "RW")
midfielder <- c("CAM", "CDM", "CM", "LCM", "LDM", "LM", "RCM", "RDM", "RM")
defense <- c("CB", "GK", "LB", "LCB", "LWB", "RB", "RCB", "RWB")
pos <- c("forward", "midfielder", "defense")
```

Group each positio into either forward, midfielder, or defense
```{r}
pcaprim$player_positions[pcaprim$player_positions %in% forward] <- "forward"
pcaprim$player_positions[pcaprim$player_positions %in% midfielder] <- "midfielder"
pcaprim$player_positions[pcaprim$player_positions %in% defense] <- "defense"
pcaprim$player_positions <- as.factor(pcaprim$player_positions)
#prim <- prim[prim$player_positions %in% pos, ]
```

qqPlot of each column
```{r}
par(mfrow = c(2, 2))
x = numeric(18)
for(i in 2:19){
  var = colnames(pcaprim)[i]
  x[i] <- qqPlot(pcaprim[, i], ylab = var)
}

for(i in 2:19){
  prim[i]
}
```

Put data ito tpca
```{r}
tpca <- pcaprim
```

Transform the data
```{r}
tpca$overall <- tpca$overall^2
tpca$value_eur <- tpca$value_eur^(1/3)
tpca$wage_eur <- tpca$wage_eur^(1/3)

```

Principal Component Analysis
```{r}
pc1 <- princomp(tpca[, -c(1)], cor = TRUE)
names(pc1)
print(summary(pc1), digits = 2, loadings = pc1$loadings, cutoff = 0)
```

Standard deviatios
```{r}
round(pc1$sdev^2,2)
```

Scree plot
```{r}
screeplot(pc1, type = "lines", col = "red", lwd = 2, pch = 19, cex = 1.2, 
          main = "Scree Plot of Transformed Player Data")
```

Score plots
```{r}
source("http://reuningscherer.net/multivariate/r/ciscoreplot.R.txt")
ciscoreplot(pc1, c(1, 2), c(1:pc1$n.obs))
text(pc1$scores[, 1], pc1$scores[, 2], labels = tpca[, 1], cex = 0.6, col = as.numeric(tpca[, 1]))

ciscoreplot(pc1, c(1, 3), c(1:pc1$n.obs))
text(pc1$scores[, 1], pc1$scores[, 2], labels = tpca[, 1], cex = 0.6, col = as.numeric(tpca[, 1]))

ciscoreplot(pc1, c(1, 4), c(1:pc1$n.obs))
text(pc1$scores[, 1], pc1$scores[, 2], labels = tpca[, 1], cex = 0.6, col = as.numeric(tpca[, 1]))

ciscoreplot(pc1, c(2, 3), c(1:pc1$n.obs))
text(pc1$scores[, 1], pc1$scores[, 2], labels = tpca[, 1], cex = 0.6, col = as.numeric(tpca[, 1]))

ciscoreplot(pc1, c(2, 4), c(1:pc1$n.obs))
text(pc1$scores[, 1], pc1$scores[, 2], labels = tpca[, 1], cex = 0.6, col = as.numeric(tpca[, 1]))

ciscoreplot(pc1, c(3, 4), c(1:pc1$n.obs))
text(pc1$scores[, 1], pc1$scores[, 2], labels = tpca[, 1], cex = 0.6, col = as.numeric(tpca[, 1]))

biplot(pc1, choices = c(1, 2), pc.biplot = T)

biplot(pc1, choices = c(1, 3), pc.biplot = T)
```