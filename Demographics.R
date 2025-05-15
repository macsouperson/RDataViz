load("PubPol4557Session15Lab.Rdata")

library(ggplot2)
library(dplyr)
library(haven)


#Part 2
#Question 1
# How are the ages of respondents distributed in our sample? 

agegraph <- ggplot(data=ANES,na.rm=T) +
  geom_histogram(aes(x=dem_age_r_x),
                 breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,
                                             65,70,75,80,85,90,100),
                 fill="red",color="black") +
  labs(x="Age", y="Number of People") +
  ggtitle("Age Distribution") +
  theme(plot.title = element_text(hjust=0.5))
agegraph

#Question 2
# What is the distribution of incomes of our survey respondents?

incomegraph <- ggplot(data=ANES) +
  geom_histogram(aes(x=incgroup_prepost_x,
                     fill=factor(incgroup_prepost_x)),bins=28,color="white") +
  labs(x="Income Category", caption="ANES Data") +
  ggtitle("Income Distribution") +
  theme(plot.title = element_text(hjust=0.5), plot.caption = element_text(hjust=0.5))
incomegraph$labels$fill <- "Income Level"
incomegraph

#Question 3
# Using the 7-point self-identified Party ID scale (pid_x), what is the distribution of party self-identification of our respondents?

partygraph <- ggplot(data=ANES) +
  geom_histogram(aes(x=pid_x,fill=factor(pid_x)),bins=7)+
  labs(x="Party ID") + scale_fill_discrete(labels=c('Strong Democrat','Not very strong Democract',
                                           'Independent-Democrat', 'Independent','Independent-Republican',
                                           'Not very strong Republican','Strong Republican'))
partygraph$labels$fill <- "Party Affiliation"
partygraph

#Question 4
# How does the distribution of feelings towards unions (ftgr_unions) compare across respondents identifying with different parties? 
party_labels <- c("Strong Democrat","Not Strong Democrat",
                  "Independent Democrat","Independent", "Independent Republican",
                  "Not Strong Republican", "Strong Republican")
party_labeller <-function(variable, value){
  return(party_labels[value])
}

uniongraph <- ggplot(data=ANES) +
  geom_histogram(aes(x=ftgr_unions),breaks=c(0,10,20,30,40,50,
                                              60,70,80,90,100),color="white",fill="blue")+
  facet_wrap(~pid_x, labeller = party_labeller)
uniongraph

#Question 5
# Pick one of the feeling thermometers we looked at previously (dpc, rpc, dem, rep) and compare the distribution of responses across Party identification. Do feelings towards a party or candidate vary across self-identified party affiliation?

idgraph <- ggplot(data=ANES) +
  geom_histogram(aes(x=ft_dpc),breaks=c(0,10,20,30,40,50,
                                        60,70,80,90,100),color="white",fill="green") +
  facet_wrap(~pid_x, labeller = party_labeller)
idgraph
