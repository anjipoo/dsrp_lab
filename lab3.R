#assoc rule mining

#data exploration
library(arules)
data("Groceries")
data<-Groceries
print(data)
summary(data)  #no of transactions, unique items, most freq purchased items
inspect(data[1:5])  #first 5 transactions

#item freq dist
library(arulesViz)
itemFrequencyPlot(data, topN=10, type="absolute", main="top 10 items", col="orange")

#apply apriori
rules<-apriori(data, parameter = list(supp=0.01, conf=0.5))
summary(rules) #summary of rules
inspect(head(rules,10))

#sorted by confidence
sorted_rules<-sort(rules, by="confidence", decreasing = TRUE)
inspect(head(sorted_rules,10))

#sorted by lift
rules_lft<-sort(rules, by="lift", decreasing = TRUE)
inspect(head(rules_lft, 10))

plot(rules, method="scatterplot")
plot(rules, method="grouped")
plot(rules,method="graph") #nodes=items, edges=rules, red=stronger

#top 20 items
itemFrequencyPlot(data, topN=20, type="relative")

#rules where rhs is milk
mil_rules<-apriori(data, parameter = list(supp=0.01, conf=0.5), appearance = list(rhs="whole milk"))
inspect(mil_rules)

#remove redundant
rules_clean<-rules[!is.redundant(rules)]
inspect(head(rules_clean))

