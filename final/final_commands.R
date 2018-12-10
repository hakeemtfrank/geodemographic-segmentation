# Program Commands #
# Final Project: STA5650 #

# import libraries#

library(MASS) # LDA
library(psych) # FA
library(knitr)# create tables
library(corrplot)
library(kableExtra)

# set working directory #
# setwd('C:/Users/hake9512/Documents/ArcGIS/Projects/VoteClassifier/R-Scripts/')
setwd('~/psychographic-segmentation/vote-classifier/')
vote_df <- read.csv('percapita_clean.csv', header = T, sep = ',')


```{r, echo = FALSE, results = 'hide'}
vote_data <- vote_df[,-c(1:11, 26, 27, 28, 30:35)] # remove unnecessary columns for PCA and EFA

names(vote_data) <- c('med_inc', 'bought_car', 'child_spent', 'poli_dem', 'poli_rep', 'disp_inc', 'bachdeg_cy',
                      'rel_club','bar_spend', 'males_cy', 'females_cy', 'married_cy', 'food_percapita',
                      'health_percapita','population')

```

# Introduction 

## Abstract:

|    Understanding the patterns and behaviors of consumers has grown significantly in importance. Companies and political groups are fighting for the attention of the general popuation to buy a product, vote for, or believe in an idea that favors the influencer. Consequently, data-driven practices in fields such as marketing, psychology, and political science have emerged to meet the demands of this trend. Two of these concepts are *Psychographic Segmentation* and *Political Forecasting*. In this report, these concepts are introduced, and multivariate data analysis is used to explore how an analysis of this sort works in practice. Specifically, this study seeks to investigate how the demographics of different postal codes in Louisiana can be used to predict the dominant political affiliaton of the area, and how they may be tied to underlying factors that describe multiple demographics simulatenously.
|    While an analysis of this type would usually be applied at the national level, a smaller, state-level study is pursued in order to focus on the statistical techniques and outcomes rather than on the idiosyncrasies of population demographics and large data processing.
|    The multivariate data analysis techniques used in this paper are Principal Components Analysis (PCA), Factor Analysis (EFA), and Linear Discriminant Analysis (LDA). PCA and EFA were used to explore the potential for latent variables in the data, while LDA was used to classify postal codes as either predominantly Liberal or Conservative based on their underlying demographics. The results show that there are two distinct factors that can be used to describe the characteristics of each postal code; these two factors are described as social livelihood and per-capita spending. Urbanized areas score higher on both factors, while rural areas score low in both factors. Results from the political forecast indicate that the given dataset can be used to predict the political affilation of each postal code using LDA with a 22.5% actual error rate. 

## Research Questions

|    Political analysts and participants in prediction markets have been interested in the influencing factors of election results since the 16th century, with its origins in betting odds on papal succession. This practice continues and is growing in complexity, as the access to measurements of human behavior (such as consumer spending) and social demographics increases. While traditional methods for predicting a geography's political affilation have existed before using advanced statistical methods, statistical methods can be used to increase the accuracy of political predictions.
|    This report is focused on analyzing a set of demographics for different geographies, and investigating how the political affilation of an area can be predicted using multivariate data analysis. This is done by answering the following questions:

- To what extent can bipartisan political affilation (Democrat or Republican) of Louisiana postal codes be predicted using a set demographics?
- Are these demographics influenced by latent factors?

## Data

|    The data on which the analysis was performed is demographic data for 461 different zip codes in the state of Louisiana. The geographic boundaries were sourced from the ArcGIS Living Atlas of the World from ESRI (Environmental Systems Research Institute). These zip code boundaries were last updated in 2016. The demographic data comes from ESRI's demographic dataset, which compiles data from different data providers, such as the American Community Survey, the U.S. Census Bureau, and the Bureau of Labor Statistics. These figures are compiled as of July 2018, and are aggregated by the input postal code boundary. The postal code demographics can be grouped into three categories: spatial, social, and financial.
|    Social attributes include number of men and women in a zip code, the number of people that have visited a bar in the past twelve months, number of people that bought toys for their child, number of people in a religious club, number of people that have bought a car, total population, the amount of the population with a bachelor's degree, number of married households, and the number of people that affiliate as democrat or republican.
|    Financial attributes include median household income, average disposable income, total food spend, and total health insurance spend. Total health spend and total food spend were normalized by population for this analysis. 
|    Spatial attributes include the name of the zip code, the zip code's area, its length, square mileage, associated city, and metadata that was included as output when enriching each postal code with data. These spatial attributes are considered labels, and were excluded from the analysis.
|    The dimension of the raw dataset contains 461 observations with 15 attributes, where each observation is a postal code, and each attribute contains demographic information from its respective postal code. 45 postal codes were omitted from the analysis, since they included null entries in multiple attributes. Unnecessary spatial attributes such as polygon area, shape, width, and length were omitted as well. The spatial dataset is found in the appendix of this report. 

# Methodology and Results

## Preliminary Analysis

|    The motivation in this analysis was to use LDA to predict political affilation in each postal code, and use PCA and EFA to investigate latent factors in the dataset. The correlation between each variable was checked to verify that the dataset had potential clusters of correlated variables before moving forward with PCA and EFA. If the variables were mostly independent, then there would be little reason to perform dimensionality reduction or EFA. 

```{r, fig.height = 3.5, fig.width = 3.5, fig.align='center' ,echo = FALSE}
# PRELIMINARY ANALYSIS #


corrplot(cor(vote_data), type = 'upper', number.cex = 1, tl.cex = .65)
```

```{r, fig.height = 3.5, fig.width = 4.5, fig.align='center' ,echo = FALSE}
target <- vote_df$Republican
vote_temp <- vote_data
vote_temp$target <- as.factor(target)
## Pairs Plot ##
cols <- character(nrow(vote_temp))
cols[] <- "blue"
cols[vote_temp$target == '1'] <- "red"
cols[vote_temp$target == "0"] <- "blue"

pairs(vote_temp[,c(1,3,6,7)], pch = 19, cex = 0.3, lower.panel = NULL, col=cols)
####
```

|    There exists high correlation among the data's variables. Upon inspecting the correlation plot, it appeared that there were roughly two clusters of correlated variables in the data that suggested two potential underlying factors. The variance of each attribute was investigated in order to justify using the correlation matrix in PCA and EFA.There were large differences between the variance of each variable, which warrants the use of the correlation matrix for eigen decomposition. The scatter plot matrix of the data brings attention to the class separation that appears in the plot of bachelor's degree and median household income.

## Main Analysis: Psychographic Segementation

|    The preliminary analysis results were positive and prompted further investigation into the data using multivariate statistical techniques. PCA and EFA was conducted on the data using the correlation matrix, due to the large differences in variance between variables. 


```{r, fig.height = 3.5, fig.width = 5, fig.align = 'center', echo = FALSE}
vote.pca <- princomp(~., data = vote_data, cor = TRUE) # PCA WITH CORRELATION MATRIX

plot(vote.pca, type ='l', pch = 19, main = 'Proportion of Variance Explained', xlim = c(0.5,10), ylim = c(0,13), cex.main = 0.9,
     cex.axis = 0.5)
text(vote.pca$sdev^2, c('71.34%', '14.47%', '10.70%', '1.45%','1.07%', '0.35%', '0.19%', '0.13%','0.08%',
                        '0.07%'), pos =3, cex = 0.58)


vote.fa.rotate <- fa(vote_data, nfactors = 2, n.obs = 416, covar = FALSE, fm = 'ml', rotate = 'varimax', 
                     scores = 'regression')


par(mfrow = c(1,2))

# PCA Loadings #
plot(vote.pca$loadings[,1:2], type = 'n', xlim = c(-.6, 0.6), ylim  = c(-0.6, 0.6), main = 'PC Loadings', 
     cex.main = 0.9)
arrows(0, 0, vote.pca$loadings[,1], vote.pca$loadings[,2], length = 0.05)
text(vote.pca$loadings[c(1,6,13,14),1:2], names(vote.pca$loadings[c(1,6,13,14),1]), pos = 4, cex = 0.55)
text(vote.pca$loadings[-c(1,6,13,14),1:2], names(vote.pca$loadings[,1])[-c(1,6,13,14)], pos = 2, cex = 0.55)
# 

# Factor Loadings #
plot(vote.fa.rotate$loadings[,1:2], type = 'n', xlim = c(-1.2,1.2), ylim = c(-1.01,1.01), main = 'Factor Loadings',
     cex.main = 0.9)
arrows(0, 0, vote.fa.rotate$loadings[,1], vote.fa.rotate$loadings[,2], length = 0.05)
text(vote.fa.rotate$loadings[-c(13,14),1:2], names(vote.fa.rotate$loadings[,1])[-c(13,14)], pos = 4, cex = 0.55)
text(vote.fa.rotate$loadings[c(13,14),1:2], names(vote.fa.rotate$loadings[,1])[c(13,14)], pos = 2, cex = 0.55)

par(mfrow = c(1,2))

plot(vote.pca$scores[-c(27, 68, 106, 325, 185, 269),1:2], pch = 16, cex = 0.5, xlim = c(-13,4), ylim = c(-15,11),
     main = 'PC Scores', cex.main = 0.9)
text(vote.pca$scores[c(27, 68, 106, 325, 185, 269), 1:2], rownames(vote_df[c(27, 68, 106, 325, 185, 269),]), cex = 0.55)

# Factor Scores
plot(vote.fa.rotate$scores[-c(27, 68, 106, 325, 269),1:2], pch = 16, cex = 0.5, xlim = c(-1.5,4), ylim = c(-15,11),
     main = 'Factor Scores', cex.main = 0.9)
text(vote.fa.rotate$scores[c(27, 68, 106, 325, 269), 1:2], rownames(vote_df[c(27, 68, 106, 325, 269),]), cex = 0.55)

```

|    The first factor was loaded with variables that describe individuals within the postal code, such as political affilation, gender, number of people that bought cars, and number of people that spent on their children. The second factor is loaded with variables that measure per-capita spending in the postal code, such as health insurance spend per capita and food spend per capita. Median household income and average disposable income were highly unique at 0.65 and 0.68, respectively. Thus, they were not mapped to either factor. This uniqueness was also the case when considering a third factor in the EFA.
|    Factor scores for each postal code were plotted on a geographic map for visual interpretation, although it is omitted from this report. The results from the factor analysis and PCA suggest that the first latent factor can be described as social livelihood, while the second latent factor can be described as per-capita spend. With this interpretation, postal codes with higher scores in both factors have high spending and high human activity, while postal codes that score low in both factors are likely rural areas. Interesting cases are postal codes that are high in the social livelihood factor but low in per-capita expense, which describes postal codes that may be financially disadvantaged. On the other hand, postal codes with a high per-capita spending score and low social livelihood score are likely wealthier zip codes.
|    Observations 68 and 269 are outliers in both analyses. Observation 68 is zip code 70803 in Baton Rouge, and is likely an outlier due to inaccurate data. For example, the data indicates that the population of this zip code is 19, but the number of males is 680, and the number of females is 726. The second outlier discussed is observation 269, which is zip code 70534 in Estherwood, Louisiana. The data for this zip code is also inaccurate; the population figure stands at -99, but with a count of males and females at 499 and 476, respectively. However, outliers along the first factor (observations 27, 106, 185, and 325) are merely zip codes in more urbanized areas.


## Main Analysis: Political Forecasting

|    Linear Discriminant Analysis was used to classify zip codes as either Democrat or Republican. Each postal code was labeled with a '1' for Republican, and a '0' for Democrat, depending on the predominant number of people that affiliated with Republican or Democrat in the postal code. 
|    An initial look at the grouped scatterplots in the preliminary analysis hinted at a dimension in the data that could be used to maximize variance between classes and minimize the variance within each class. LDA was conducted on a random sample of 376 zip codes from the original dataset (amounting to 90% of the original observations). In addition, the number of people that affiliate with Republican and Democrat were removed as predictor variables in the training and testing dataset. 
|    As a result, the data was reduced to a single dimension LD1, where most observations labeled as "0" (Democrat) assumed lower values, while observations labeled with a "1" (Republican) assumed higher values. While there is some overlap in classes using LD1, there is still notable separation between classes in this dimension.


```{r, echo = FALSE, fig.height = 4.5, fig.width =5, fig.align= 'center'}
# Create dataframe for LDA  #
lda_data <- vote_df[,-c(1:11, 15, 16, 26:34)]
names(lda_data) <- c('med_inc', 'bought_car', 'child_spent', 'disp_inc', 'bachdeg_cy',
                     'rel_club','bar_spend', 'males_cy', 'females_cy', 'married_cy', 'food_percapita',
                     'health_percapita','Republican')

# Split training and testing data #
#indx <- sample(nrow(lda_data), 40) # COMMENT OUT: TAKES RANDOM SAMPLE
imut_index <- c(73, 180, 184, 95, 70, 79, 193, 314, 317, 190, 251, 29, 345, 118, 16, 63, 400, 86, 117, 414, 102, 122, 415,
                164, 80, 103, 11, 401, 379, 407, 174, 116, 32, 295, 159, 42, 158, 312, 169, 297) # values from index
test_data <- lda_data[imut_index,] 
train_data <- lda_data[-imut_index,]

# Run LDA using training data #
vote.lda <- lda(Republican~., data = train_data)
plot(vote.lda, col = 'brown')
```

                                                                                                          
###################

---
  title: "Factor Analysis, PCA, and Linear Discriminant Analysis"
output:
  pdf_document: default
html_notebook: default
---
  
  
  ```{r}
# import libraries#

library(MASS) # LDA
library(psych) # FA
library(knitr)# create tables
library(corrplot)
library(kableExtra)

# set working directory #
# setwd('C:/Users/hake9512/Documents/ArcGIS/Projects/VoteClassifier/R-Scripts/')
setwd('~/psychographic-segmentation/vote-classifier/')
vote_df <- read.csv('percapita_clean.csv', header = T, sep = ',')
# enriched_df <- read.csv('enriched_zips.csv', header = T, sep =',')
```

```{r}
# IMPORT DATA #

# Per capita clean dataset #


vote_df2 <- enriched_df[,-1] # dataframe with new variables
#corrplot.mixed(cor(vote_df2))


#names(enriched_df)
#names(vote_df2)
#head(vote_df, 10)
```

In this notebook we investigate the relationship between different demographics, and create a model that predicts the political affiliation of zip codes in Louisiana (Republican vs. Democrat). 

The statistical methods that we will be using are:
  
  - Principal Components Analysis (PCA)
- Factor Analysis (FA)
- Linear Discriminant Analysis (LDA)


The data that we are using is as follows:
  
  
  
  ```{r}
names(vote_data)
pairs(vote_data)
vote_temp <- vote_data
vote_temp$target <- target
vote_temp$target <- as.factor(target)
#####
cols <- character(nrow(vote_temp))
cols[] <- "black"

cols[vote_temp$target == '1'] <- "red"
cols[vote_temp$target == "0"] <- "blue"
pairs(vote_temp[,1:7], pch = 19, cex = 0.5, lower.panel = NULL, col=cols)
pairs(vote_temp[,8:15], pch = 19, cex = 0.5, lower.panel = NULL, col=cols)
pairs(vote_temp[,1:15], pch = 19, cex = 0.5, lower.panel = NULL, col=cols)
#####
dim(vote_temp)

#library(GGally)
#ggpairs(vote_temp, aes(colour = target, alpha = 0.4))
```

Demographic Variables:
  Number of people that bought cars in the past twelve months
Number of people that identify as democrat
Number of people that identify as Republican
Median Disposable Income
Median Household Income
Number of people in a religious club
Number of Males
Number of Females
Number of people that spent on children's toys
Number of people with a Bachelor's degree
Number of People that spend money at bars
Number of married people
Annual Food Spend Per Capita
Annual Healthcare Spend Per Capita

Target Variable:
  Political Affiliation

Spatial Variables:
  Apportionment Confidence Score (reliability of the data enrichment tool)
Shape area
Shape length


We begin the analysis by investigating the dataset at a high level, and then move to the main analysis. 



The data loaded successfully, although we can now isolate our relevant variables from the rest of the dataset. We do not need attributes such as the zip code, postal code name, etc.


```{r}
names(vote_df)

vote_data <- vote_df[,-c(1:11, 26, 27, 28, 30:35)] # remove unnecessary columns for PCA and EFA

names(vote_data)

target <- vote_df$Republican

# Create TRAIN and TEST data for LDA #

indx <- sample(nrow(vote_df), 40) # Uses 1/10th of data for K-Fold Cross Validation

test_data <- vote_df[indx,]
train_data <- vote_df[-indx,]

# # # # # # # #
```

We now have a dataset that we can use to check for correlations. In Principal Components Analysis, we seek correlated variables in our dataset.



```{r}
# Extract variance #
kable(sort(diag(cov(vote_data)), decreasing = TRUE))

cov(vote_data)
```

The variance for each variable differ by a large magnitude, suggesting that an unstandardized PCA will lead to skewed results. Next, we check the correlation between variables to justify dimensionality reduction.

```{r}
# PRELIMINARY ANALYSIS #
corrplot(cor(vote_data), type = 'upper', number.cex = 1, tl.cex = .65)
```

Our variables are highly correlated. We conduct PCA as a preliminary analysis for Factor Analysis. After using eigen decomposition to create our factor scores and factor loadings, we check the skree plot to visualize the proportion of variance explained by each component.

## Principal Components

```{r}
vote.pca <- princomp(~., data = vote_data, cor = TRUE) # PCA WITH CORRELATION MATRIX
vote.pca.cov <- princomp(~., data = vote_data, cor = FALSE) # PCA WITH COVARIANCE MATRIX

plot(vote.pca, type ='l', pch = 19, main = 'PVE, Louisiana Demographics (R Matrix)')
text(speed.fa.rot$scores, rownames(X2))

plot(vote.pca.cov, type ='l', pch = 19, main = 'PVE, Louisiana Demographics (S Matrix)', addlabels = TRUE)
vote.pca
```

```{r}
names(summary(vote.pca))
summary(vote.pca)
summary(vote.pca.cov)
```

It appears that the first two components in our PCA using the correlation matrix explain 85.80% of variance in the dataset, while the first three components explain 96.51% of the variance. We will visualize our component scores in a table.

Next, we wish to visualize our factor loadings and factor scores to 

```{r}
plot(x = vote.pca$scores[,1], y = vote.pca$scores[,2], main = 'Prinicipal Component Scores', xlab = 'Z1', ylab = 'Z2') # plot factor scores
plot(x = vote.pca$scores[,1], y = vote.pca$scores[,3], main = 'Priniciapl Component Scores')
plot(x = vote.pca$scores[,2], y = vote.pca$scores[,3], main = 'Priniciapl Component Scores')


plot(x = vote.pca.cov$scores[,1], y = vote.pca.cov$scores[,2], main = 'PC Scores, S-Matrix PCA')
plot(vote.pca.cov$loadings[,1:2], type = 'n', main = 'Loadings Plot for S-Matrix PCA')
text(vote.pca.cov$loadings[,1:2], names(vote_data)) 


### LOADINGS PLOT, R-MATRIX ####
plot(vote.pca$loadings[,1:2], type = 'n', main = 'Loadings Plot for R-Matrix PCA', xlab = 'PC1', ylab = 'PC2')
text(vote.pca$loadings[,1:2], names(vote_data)) 


## LOADINGS FOR UP TO 3 COMPONENTS
plot(vote.pca$loadings[,c(1,3)], type = 'n', main = 'Loadings Plot for R-Matrix PCA')
arrows(0,0, vote.pca$loadings[,1], vote.pca$loadings[,3])
text(vote.pca$loadings[,c(1,3)], names(vote_data)) 



plot(vote.pca$loadings[,c(2,3)], type = 'n', main = 'Loadings Plot for R-Matrix PCA')
arrows(0,0, vote.pca$loadings[,2], vote.pca$loadings[,3])
text(vote.pca$loadings[,c(2,3)], names(vote_data)) 

plot(vote.fa)
plot(vote.pca$loadings)
```

```{r}
par(mfrow = c(1,2))
biplot(vote.pca, scale = 0, xlabs = rep('.', nrow(vote_data)))
biplot(vote.fa.rotate, main ="", xlabs = rep('.', nrow(vote_data)))
#plot(vote.pca$scores[,1:2])
```


Here, we see that R-Matrix PCA separates the demographics in these zip codes into three clusters. The first one includes the health insurance spend per capita and food spend per capita. The second cluster includes the disposable income and median household income of the zip code, while the third cluster is number of people with bachelors degrees, republican / democratic affiliation, married households, and other social demographics. We can take the scores of each zip code and map them to visualize how each zip code fares.

We note that observation 269 is an outlier; this observation corresponds with zipcode 70534 (Estherwood, Louisiana, near Lafayette). This may be because it has a negative value for health insurance and food spend.

Finally, we evaluate the 


```{r}
qqnorm(vote.pca$scores[,1], pch = 19, main = 'Louisiana PCA Q-Q Plot')
```

```{r}
vote.pca$scores
```

It appears that our data is not quite normally distributed.


## Factor Analysis of Louisiana Postal Code Demographics

While Principal Components Analysis is useful in taking an initial look at our data, we continue our analysis using Factor Analysis to increase the level of granularity.

Factor analysis describes the covariance relationships among many variables in terms of a few unobservable random quantities (latent variables).

Here, we approximate the covariance matrix using something beyond an eigen decomposition. 

We seek to answer the question: are the data consistent with a prescribed, underlying structure?

We have a model for our original data matrix $X$ based on the assumption that the matrix is linearly dependent on a few unobservable random variables (factors). So we seek to find our $L$ matrix (factor loadings). 

We assume:
  Observations are independent, that the residual terms and F are independent, and that we have constant variance.

The purpose of this factor analysis is to see if there is an underlying prescribed structure.\

We will conduct a factor analysis using the correlation matrix, calculated using the maximum likelihood estimation. We conduct an analysis without rotation, and one with rotation to interpret the results.

```{r}
vote.fa <- fa(vote_data, nfactors = 3, n.obs = 416, covar = FALSE, fm ='ml', rotate = "none", 
              scores = "regression")
vote.fa.rotate <- fa(vote_data, nfactors = 2, n.obs = 416, covar = FALSE, fm = 'ml', rotate = 'varimax', 
                     scores = 'regression')

vote.fa.rotate
# Check Factors#

vote.fa$loadings
vote.fa.rotate$loadings
vote.fa.rotate$rotation


print(vote.fa.rotate, digits = 2, cutoff = 0.3, sort = TRUE)
vote.fa.rotate

ev <- eigen(cor(vote_data)) # Extract eigenvalues to determine the number of factors in our model

ev$values # check eigenvalues
plot(ev$values, type = 'b', pch = 19, ylab = 'Eigenvalues', xlab = 'Lambda', main = 'Eigenvalues of Correlation Matrix') # plot eigenvalues

sort(vote.fa.rotate$uniquenesses, decreasing = TRUE)
```

** Why 2 factors? **
  
  Three factors were used in an initial analysis. However, they explained 89% of the variance, while two explain 84% of the variance. In addition, median household income and median disposable income have high uniqueness in both our 2 and 3-factor analyses. Every other variable is loaded on two factors, and we obtain a simple structure.


Two factors explain 84% of the variance of our data. Our first factor explains 65%, then 13%, and then 11%. The rate of change in variance explained between factors is decreasing, so we can include two or three factors. We decide on three factors by checking the eigenvalues of our data's correlation matrix. Here, our eigenvalue is greater than 1 up to the third eigenvalue.

Sum of square loadings for each factor are all still greater than 1, which also suggests that three factors are sufficient.


** Adequacy of our model **

```{r}
# Create Residual Matrix

vote.load <- vote.fa.rotate$loadings[,1:2] # save loading matrix for speed FA
vote.psi <- vote.fa.rotate$uniq # save uniqueness vector for speed FA
vote.sigma <- vote.load %*% t(vote.load) + diag(vote.psi) # save estimated covariance matrix
R <- cor(vote_data) # save covariance matrix to create residual matrix

k. <- kable(round(R - vote.sigma, 3)) %>%
kable_styling(full_width = F)
add_header_above(k., c("Record Times Covariance Residual Matrix" = 16), bold = TRUE)

round(tr(R - vote.sigma),2) # check the trace; should be close to zero
```

The diagonals are zero, and the rest of the entries in the residual matrix are close to zero. 

** Uniqueness and Communality **

We notice that the uniqueness of our variables are quite low, except for disposable income and median household income. Since our values of uniqueness for these two variables are near 1, it means that they aren't explained well by any of the latent factors constructed in this analysis.

** Loading Vectors (L) **
  
  ```{r}
k. <- kable(vote.fa.rotate$loadings[,1:2]) %>%
  kable_styling(full_width = F)
add_header_above(k., c("Factor Loadings Using R Matrix" = 3), bold = TRUE)
```

Our loading vectors show the correlation of variables with each underlying factor. Our first factor is loaded by variables such as social demographics, while the second factor is loaded by food and healthcare expenditures per capita, and the third factor is loaded by a mix of both, includig median household income and disposable income. This may be a general lifestyle factor that describes the prosperity of a zip code. What is peculiar, is that republican affiliation is loaded on this factor, but democrat is not.


** Factor Scores: Diagnostic and Outliers **
  
  ```{r}
plot(vote.fa.rotate$scores, main = 'Factor Plot for R')
plot(vote.fa.rotate$loadings[,1:2], main = 'Factor Loadings')
text(vote.fa.rotate$loadings[,1:2], names(vote_data))
biplot(vote.fa.rotate)

ggbiplot(vote.pca, choices = 1:2, scale = 1, xlim = c(-10,10))

```

Visualizing our results, we see that most zip codes fall near the origin, meaning that they score moderately. The factor loadings separate the variables distinctly, and our two ambiguous variables map in between both. An interesting observation we make by looking at the biplot is that most of our zip codes score low on both factors.

Finally, we map our results to visualize them spatially.

We do this by:
  
  1. Create dataframe with the original features and their factor scores.
2. Export to CSV
3. Upload to ArcGIS and visualize.

```{r}
vote.fa.rotate$scores # factor scores for rotated factor analysis

vote_df$fac1 <- vote.fa.rotate$scores[,1]
vote_df$fac2 <- vote.fa.rotate$scores[,2]

```


```{r}
write.csv(vote_df, 'vote_fa.csv')
```

## Linear Discriminant Analysis

We explored our dataset in the previous analysis and found that they can be described by two latent variables. The first factor describes the qualitative variables, such as marriage status, number of people with a bachelors degree, number of males and females, etc. The other factor describes quantitative variables, such as median household income and disposable income. We explore how these variables influence the political affiliation of each zip code. 


We want to visualize our results (how the classifier classified each zip code).
```{r}
## OMIT COLUMNS ##

#names(train_data)
train_data <- train_data[,-c(1:11, 15, 16, 26:34)]
test_data <- test_data[, -c(1:11, 15, 16, 26:34)]
```


```{r}
names(train_data)
```

```{r}
vote.lda <- lda(Republican~., data = train_data)

vote.lda
```

We were able to reduce our data to a single linear discriminant. Let's plot it to see how the data is distributed on it.


```{r}
plot(vote.lda)

plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  # second
# hist(thing) LD1 group 0 
# LD1 for group 1
vote.lda
```
It appears that there is some overlap in our democrat and republican groups. Let's predict the target variable, Republican, using LDA.

We see in the plot that when $LD_1 > 0$, the probability that a zip code is predominantly republican increases,, and when $LD_1 < 0$, the probability that a zip code is republican decreases.

```{r}
train.pred <- predict(vote.lda, data = train_data)


republican.train <-  data.frame(train_data, pred = train.pred$class)
republican.train
```

```{r}
res_tab = table(republican.train$Republican, republican.train$pred)
res_tab

# Count the number of misclassifications #
misclass = sum(res_tab[2], res_tab[3])

# Calculate the AER #
aer = misclass / nrow(train_data)
aer
```

There is a 23.4% misclassification rate (AER) for our linear classifier. Next, we test the classifier on data that it hasn't seen before.

```{r}
test.pred <- predict(vote.lda, newdata = test_data)

rep.test <- data.frame(test_data, test.pred$class)

restab.test <- table(rep.test$Republican, rep.test$test.pred.class)

restab.test

misclass2 <- sum(restab.test[2], restab.test[3])
  
aer <- misclass2 / nrow(test_data)
aer

accur <- sum(restab.test[1], restab.test[4])

ac_perf <- accur / nrow(test_data)
ac_perf

# Null accuracy

1 - mean(test_data$Republican) # our classifier's null error rate
```

The LDA classifier has an accuracy rate of 72.5%. 

5 zip codes were misclassified as democrat, while 6 were misclassified as republican.
```{r}
rep.test
write.csv(rep.test, 'class_results.csv')
write.csv(test_data, 'test_data.csv')
```
