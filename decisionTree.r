#!/usr/bin/env Rscript

library(DBI)
library(RMySQL)
library(rpart)

con <- dbConnect(MySQL(),
	   user = "root", password = "root",
       dbname = "baseballStats", host = "localhost")

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