#!/usr/bin/env Rscript

library(DBI)
library(RMySQL)
library(rpart)
library(ggplot2)
con <- dbConnect(MySQL(),
	   user = "root", password = "root",
       dbname = "baseballStats", host = "localhost")

# rs = dbSendQuery(con, "SELECT W, COUNT(W) FROM Pitching WHERE yearID = \"2011\" GROUP BY W LIMIT 0, 20")

rs = dbSendQuery(con, "SELECT W, L, WHIP, G, GS, CG, SHO, ER, HR, BAOpp, WP, HBP, BK, BFP, GF, R, SH, SF, GIDP, ERA, SO, SV, salary, eventType FROM TrainingSet_2005 WHERE eventType = \"WBC 2006\"")
trainingSet_R = fetch(rs, n=-1)
rs = dbSendQuery(con, "SELECT W, L, WHIP, G, GS, CG, SHO, ER, HR, BAOpp, WP, HBP, BK, BFP, GF, R, SH, SF, GIDP, ERA, SO, SV, salary, eventType FROM TrainingSet_2005 WHERE eventType = \"None\" ORDER BY RAND() LIMIT 0, 200")
trainingSet_nR = fetch(rs, n=-1)
trainingSet <- rbind(trainingSet_R, trainingSet_nR)

formula <- eventType ~ W+L+WHIP+G+GS+CG+SHO+ER+HR+BAOpp+WP+HBP+BK+BFP+GF+R+SH+SF+GIDP+ERA+SO+SV+salary
tree <- rpart(formula, data = trainingSet, control = rpart.control(minsplit = 2))
plot(tree, uniform=TRUE, main="Decision Tree - WBC 2006")
text(tree, use.n=TRUE, cex=1)
print(tree)




# W <- as.vector(data$W)
# ERA <- as.vector(data$ERA)
# WHIP <- as.vector(data$WHIP)
# SO <- as.vector(data$SO)

# "Normalization"
# W_z <- scale(W)
# ERA_z <- scale(ERA)
# WHIP_z <- scale(WHIP)
# scale <- scake(SO)