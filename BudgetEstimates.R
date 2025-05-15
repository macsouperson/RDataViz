library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

load("PubPol4557Session19Lab.Rdata")

#Question 1: Show observed deficits and the CBO's projection of future deficits.
##including the "2/3 of possible outcomes" range.
DEFICITS$FiscalYear <- lubridate::ymd(DEFICITS$FiscalYear, truncated=2L)

dfctgraph <- ggplot(data=DEFICITS) +
  geom_line(aes(x=FiscalYear, y=Observed),color="green", size = 1) +
  geom_line(aes(x=FiscalYear, y=CBOMayEstAdj),color="green", size = 1) +
  geom_ribbon(aes(x=FiscalYear,
                  ymin=CBOMayEstAdj - CBOError,
                  ymax=CBOMayEstAdj + CBOError), 
              fill = "gray",alpha=0.5) +
  labs(x="Fiscal Year", y="Observed/Estimated budget surpluses/deficits",
       title="Observed & Estimated budget deficits") +
  scale_x_date(date_breaks = "1 year", date_labels = '%Y')
 dfctgraph
 
 #Question 2: Show observed deficits and the OMB's projection of future deficits. 
 ##including the 95% confidence interval.
defmean = mean(DEFICITS$CBOMayEstAdj, na.rm=T)

confgraph <- ggplot(data=DEFICITS) +
  geom_col(aes(x=FiscalYear,Observed),fill="blue") +
  geom_col(aes(x=FiscalYear,CBOMayEstAdj),fill="red") +
  geom_errorbar(aes(x=FiscalYear,
                    ymin = defmean - 1.96 * AdminRMSE,
                    ymax = defmean + 1.96 * AdminRMSE)) +
  labs(x="Fiscal Year", y="Observed/Estimated deficits",
       title="Observed/Estimated Budget Deficits 2016-27")
confgraph

#Question 3: Show both the CBO and OMB projection of future deficits
##but with similar uncertainty ranges around each projection.

bothgraph <- ggplot(data=DEFICITS) +
  geom_line(aes(x=FiscalYear, y=AdminMarEst),color="blue",linewidth=2) +
  geom_line(aes(x=FiscalYear, y=CBOMayEstAdj),color="red",linewidth=2) +
  geom_ribbon(aes(x=FiscalYear,
                  ymin=AdminMarEst - 1*AdminRMSE,
                  ymax=AdminMarEst + 1*AdminRMSE), 
              fill = "blue",alpha=0.6) +
  geom_ribbon(aes(x=FiscalYear,
                  ymin=CBOMayEstAdj - 1*AdminRMSE,
                  ymax=CBOMayEstAdj + 1*AdminRMSE),
              fill = "red",alpha=0.6) +
  labs(x="Fiscal Year", y="Observed/Estimated budget surpluses/deficits",
       title="OMB & CBO Observed/Estimated budget surpluses/deficits 2016-27")
bothgraph


