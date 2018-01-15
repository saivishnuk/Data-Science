library(readxl)
library(arules)
library(arulesViz)

#step 1
assocs_data = read.transactions("transactions.csv", format = "single", sep = ",", cols = c("Transaction", "Product"), rm.duplicates = FALSE)

itemFrequencyPlot(assocs_data,topN=20,type="absolute")

#step 2
rules <- apriori(assocs_data, parameter = list(supp = 0.03, conf = 0.20, minlen = 1)) 
rules <- sort(rules, by="lift", decreasing=TRUE)
options(digits=10)
summary(rules)
inspect(rules[1:10])

#step 3
redundant_index <- is.redundant(rules)
pruned_rules <- rules[!redundant_index]
summary(pruned_rules)
inspect(pruned_rules[1:8])

#Step 4
trancsv<-read.csv("transactions.csv")
#number of transactions with Candy Bar
length(unique(trancsv$Transaction[which(trancsv$Product == "Candy Bar")]))
#number of transactions with Greeting Cards
length(unique(trancsv$Transaction[which(trancsv$Product == "Greeting Cards")]))
#total number of unique transaction
length(unique(trancsv$Transaction))

