library(knitr)
library(psych)

#######################################
####### DATASET #1: SMOKE ZIPS ########
#######################################

zips <- read.table('enriched_zips.csv', sep = ',', header = T)
head(zips)

dems <- zips[-1]

head(dems)

cor(dems)

pairs(dems)

dem.fa <- fa(dems, nfactors = 3, n.obs = 461, covar = FALSE, fm ='ml', rotate = "varimax", scores = "regression")

dem.fa$PVAL

sort(dem.fa$loadings[,3])

plot(dem.fa$loadings)

plot(dem.fa$scores)
 


#### PRINCIPAL COMPONENT ANALYSIS #####


dem.pca <- princomp(~., data = dems, cor = TRUE)

# There are three components that describe the variance in the data.

plot(dem.pca, type = 'l', pch = 19)

library(devtools)

dem.pca$loadings


#######################################
####### DATASET #2: SPEND ZIPS ########
#######################################


zip2 <- read.table('spend_zips.csv', sep = ',', header = T)
head(zip2)

zipfigs <- zip2[-1] # numerical 

cor(zipfigs)

corrplot(cor(zipfigs), method="number",shade.col=NA, tl.col="black", tl.srt=45)

# highly correlated variables

