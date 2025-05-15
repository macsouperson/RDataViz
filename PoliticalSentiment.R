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