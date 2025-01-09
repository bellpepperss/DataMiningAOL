data <- read.csv("Assignment-1_Data.csv", sep=";")

# installing packages
install.packages('Matrix')
library(arules)

# using datasets from both regions
data.UK <- data[data$Country=="United Kingdom",]
data.fr <- data[data$Country=="France",]
data.UK <- na.omit(data.UK)
data.fr <- na.omit(data.fr)

# make transaction list
transactions.fr <- split(data.fr$Itemname,data.fr$BillNo)
transactions.UK <- split(data.UK$Itemname,data.UK$BillNo)

# make transaction list from both regions
list_transactions.fr <- as(transactions.fr, 'transactions')
list_transactions.UK <- as(transactions.UK, 'transactions')
inspect(list_transactions.UK)
inspect(list_transactions.fr)

# to determine support, we will set initial for
# a product that is bought 5 times within at least 7 day period
# 5*7/392 = 0.0892 for france
# 5*7/16649 = 0.0021 for UK
# 0.8 confidence to make sure frequent repeated occurence in the dataset
# we look for rules with more than 1 lift

# FP-GROWTH ALGORITHM

# applying FP-Growth in the fim4r function on the France data set
rules_fpgrowth.fr <- fim4r(list_transactions.fr, method = "fpgrowth", support = 0.0892, confidence = 0.8, target = "rules")
rules_fpgrowth.fr <- sort(rules_fpgrowth.fr, by = "lift")
inspect(rules_fpgrowth.fr)

# applying FP-Growth in the fim4r function on the UK data set
# note: support is increased since the initial had too many rules
rules_fpgrowth.UK <- fim4r(list_transactions.UK, method = "fpgrowth", support = 0.0100, confidence = 0.8, target = "rules")
rules_fpgrowth.UK <- sort(rules_fpgrowth.UK, by = "lift")
inspect(rules_fpgrowth.UK)

# ECLAT ALGORITHM

# using eclat on the France data set
frequent_eclat.fr <- eclat(transactions.fr, parameter = list(supp=0.0892, minlen = 3))
frequent_eclat.fr <- sort(frequent_eclat.fr, by = 'support')
inspect(frequent_eclat.fr)

# using eclat on the UK data set
# note: support is increased since the initial had too many rules
frequent_eclat.UK <- eclat(transactions.UK, parameter = list(supp=0.0100, minlen = 3))
frequent_eclat.UK <- sort(frequent_eclat.UK, by = 'support')
inspect(frequent_eclat.UK)

# APRIORI ALGORITHM

# frequent itemset using the apriori algorithm in arules on the france dataset
frequent_apriori.fr <- apriori(list_transactions.fr, parameter = list(target = "frequent itemsets"), support = 0.0892)
inspect(frequent_apriori.fr)
rules_apriori.fr <- ruleInduction(frequent_apriori.fr, confidence = 0.8)
rules_apriori.fr <- sort(rules_apriori.fr, by='lift')
inspect(rules_apriori.fr)

# frequent itemset using the apriori algorithm in arules on the UK dataset
# note: support is increased since the initial had too many rules
frequent_apriori.UK <- apriori(list_transactions.UK,parameter = list(target = "frequent itemsets"),support = 0.0100)
inspect(frequent_apriori.UK)
rules_apriori.UK <- ruleInduction(frequent_apriori.UK, confidence = 0.8)
rules_apriori.UK <- sort(rules_apriori.UK, by='lift')
inspect(rules_apriori.UK)
