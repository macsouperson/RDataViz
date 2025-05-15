#install.packages("coefplot")

library(dplyr)
library(ggplot2)
library(haven)
library(stats)
library(coefplot)
library(estimatr)

load("PubPol4557Session22Lab.Rdata")

#Question 1A
sameparty <- ggplot(data=ANES,aes(y=ft_dem,x=ft_dpc)) +
  geom_point(color="blue",alpha=0.5) +
  geom_smooth(method = lm,se=FALSE) + 
  theme_minimal() + 
  labs(y="Feelings Towards Party (Dem.)", 
       x="Feelings Towards Candidate (Dem.)",
       title="Feelings Towards Democratic Candidate and Party")
sameparty

#Question 1B
diffparty <- ggplot(data=ANES,aes(y=ft_dem,x=ft_rpc)) +
  geom_point(color="red",alpha=0.5) +
  geom_smooth(method = lm,se=FALSE) + 
  theme_minimal() + 
  labs(y="Feelings Towards Party (Dem.)", 
       x="Feelings Towards Candidate (Rep.)",
       title="Feelings Towards Rep. Candidate and Dem. Party")
diffparty

#Question 1C
ageparty <- ggplot(data=ANES,aes(y=ft_dem,x=dem_age_r_x)) +
  geom_point(color="green",alpha=0.5) +
  geom_smooth(method = "loess",se=FALSE,span=0.2) + 
  theme_minimal() + 
  labs(y="Feelings Towards Party (Dem.)", 
       x="Respondent Age",
       title="Feelings Dem. Party Given Age")
ageparty

#Question 1D
unionparty <- ggplot(data=ANES,aes(y=ft_dem,x=ftgr_unions)) +
  geom_point(color="purple",alpha=0.5) +
  geom_smooth(method = "loess",se=FALSE,color="orange") +
  geom_smooth(method = lm,se=FALSE,color="red") +
  theme_minimal() + 
  labs(y="Feelings Towards Party (Dem.)", 
       x="Feelings Towards Union",
       title="Feelings Dem. Party and Unions")
unionparty

#Question 1E
partyparty <- ggplot(data=ANES,aes(y=ft_dem,x=pid_x)) +
  geom_point(color="orange",alpha=0.5) +
  geom_smooth(method = lm,se=FALSE) + 
  theme_minimal() + 
  labs(y="Feelings Towards Party (Dem.)", 
       x="Party ID",
       title="Feelings Dem. Party Given Party ID")
partyparty

#Question 2
thermmodel <- lm_robust(ft_dem ~ ft_dpc + ft_rep + dem_age_r_x + ftgr_unions + pid_x,
                 data = ANES)
thermmodel

coefplot(thermmodel,title="Therm. Graph")

#Question 3
mymodel <- lm_robust(ct ~ highT + lowT + precip + clouds + wday + month, 
              data = RIDERS)
mymodel

coefplot(mymodel, title="Count of Riders","color.Q")

#Question 4
clsscmodel <- lm_robust(ct ~ highT + lowT + precip + clouds + wday, 
            data = RIDERS, se_type="classical")
robmodel <- lm_robust(ct ~ highT + lowT + precip + clouds + wday, 
                    data = RIDERS, se_type="stata")
hc2model <- lm_robust(ct ~ highT + lowT + precip + clouds + wday, 
                    data = RIDERS, se_type="HC2")
hc3model <- lm_robust(ct ~ highT + lowT + precip + clouds + wday, 
                     data = RIDERS, se_type="HC3")

supergraph <- multiplot(clsscmodel,robmodel,hc2model,hc3model)
supergraph
