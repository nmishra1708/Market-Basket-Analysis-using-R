# Project : Market Basket Analysis in "R" programming

#1. Task to be performed;
  # 1. Find the total no of transactions
  # 2. Find the total no of items in inventory 
  # 3. Find the total no of item purchased
  # 4. Find out the 10 first frequently bought item and make a plot


#2. Building First set of association rule
  # 1.Build apriori algo with support value->0.005 and confidence value->0.8
  # 2.Sort the rule w.r.t. confidence and inspect the top 5 rules and the bottom rules
  # 3.Sort the rules w.r.t.lift and inspect the TOP 5 rules
  # 4.Plot the rules used different method  

#3. Building Second set of association Rule
  # 1.Build apriori algo with support value->0.009 and confidence value->0.3
  # 2.Sort the rules w.r.t.confidence and inspect the top 5 rules and bottom 5 rules.
  # 3.plot the rules using different methods.

#4. Building Third set of association Rule
  #1.Build apriori algo with support value->0.02 and confidence value->0.5
  #2.Sort the rules w.r.t.support and inspect the top 5 rules and bottom 5 rules
  #3.Plot the rules using different methods

#-------------- #1. Task to be performed;
# 1. Find the total no of transactions
# 2. Find the total no of items in inventory 
# 3. Find the total no of item purchased
# 4. Find out the 10 first frequently bought item and make a plot

# STEP(1)- Install Required library 
 install.packages("arules")
 install.packages("arulesViz")
 
# STEP(2)- Import required library
 library(arules)
 library(arulesViz)
 
# STEP(3)- read the transactions
  market_basket <- read.transactions(
   +     file = "D:/Intellipaath/R programming/project/PROJECT-1-8211-MARKET-BASKET-ANALYSIS-28MAR2020140642/market_basket.csv",
   +     sep = ',',
   +     quote = "",
   +     format = 'basket',
   +     rm.duplicates = TRUE,
   +     skip = 1
    )

# STEP(4) - glance of transactions summary
  summary(market_basket)
  
# STEP(5) - Compute the number of itmes that were purchased.
   18440*22346*0.0009915565 
   408581
   
# STEP(6) - Glance of First five transactions
   library(dplyr)
   market_basket %>% head(n=5) %>% inspect
   
# STEP(7) - plot the graph
   library(RColorBrewer)
   itemFrequencyPlot(x=market_basket,topN=10, type = 'absolute', horiz = TRUE, col=brewer.pal(10,'Spectral'))
   
   
#-------------------#2. Building First set of association rule
   # 1.Build apriori algo with support value->0.005 and confidence value->0.8
   # 2.Sort the rule w.r.t. confidence and inspect the top 5 rules and the bottom rules
   # 3.Sort the rules w.r.t.lift and inspect the TOP 5 rules
   # 4.Plot the rules used different method  
   
# STEP(1)- Set the rule 
   rule1 <- market_basket %>% apriori(parameter = list(supp=0.005, conf=0.8)) %>% sort(by='confidence')
   
# STEP(2)- Glance of rule1
   summary(rule1)

# STEP(3)- Inspect the first five rules
   rule1 %>% head(n=5) %>% inspect

# STEP(4)- Inspect the last five rules
   rule1 %>% tail(n=5) %>% inspect

# STEP(5)- Sort the rule by lift
   rule1 %>% sort(by="lift")->rule1
   
# STEP(6)-Inspect the first five sorted rule
   rule1 %>% head(n=5) %>% inspect
   
# STEP(7)-Plotting
   plot(rule1, engine = "htmlwidget")
   plot(rule1, method = "two-key" , engine = "htmlwidget")
   plot(rule1, method = "graph" , engine = "htmlwidget")
   
#-------------------#3. Building Second set of association Rule
   # 1.Build apriori algo with support value->0.009 and confidence value->0.3
   # 2.Sort the rules w.r.t.confidence and inspect the top 5 rules and bottom 5 rules.
   # 3.plot the rules using different methods.
   
# STEP(1)-Build apriori algo with support value->0.009 and confidence value->0.3
   rule2 <- market_basket %>% apriori(parameter = list(supp=0.009, conf=0.3)) %>% sort(by="confidence")
   
# STEP(2)-Summary of RULE-2
   summary(rule2)
   
# STEP(3)-Glance of First 5 and bottom 5 Rules
   rule2 %>% head(n=5) %>% inspect
   rule2 %>% tail(n=5) %>% inspect
   
# STEP(4)-Plot the Rules using diferent method
   plot(rule2, engine = "htmlwidget")
   plot(rule2, method = "two-key" , engine = "htmlwidget")
   plot(rule2, method = "graph" , engine = "htmlwidget")
   
#----------------------#4. Building Third set of association Rule
   #1.Build apriori algo with support value->0.02 and confidence value->0.5
   #2.Sort the rules w.r.t.support and inspect the top 5 rules and bottom 5 rules
   #3.Plot the rules using different methods
   
# STEP(1)-Build apriori algo with support value->0.02 and confidence value->0.5
   rule3 <- market_basket %>% apriori(parameter = list(supp=0.02, conf=0.5)) %>% sort(by="support")

# STEP(2)-Glance the summary of rule2
   summary(rule3)
   
# STEP(3)-Inspect the first 5 and last 5 Rules present in set of rules-3
   rule3 %>% head(n=5) %>% inspect
   rule3 %>% tail(n=5) %>% inspect
   
# STEP(4)-Plot the rules using different methods
   plot(rule3,engine = "htmlwidget")
   plot(rule3, method = "two-key", engine = "htmlwidget")
   plot(rule3, method = "graph", engine = "htmlwidget")