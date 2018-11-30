setwd('C:/Users/hake9512/Documents/ArcGIS/Projects/VoteClassifier/R-Scripts')


vote <- read.csv('ldadata2.csv', header = T, sep = ',')
head(vote, 10)
dim(vote)

# Assign republican if > 0.51 #
partycode <- ifelse(party == "Democrat", 1, 0)


