

library(ggplot2)
library(plotly)
library(dplyr)
library(plyr)
library(moments)
library(ddply)



movies <- read.csv("movies.csv")
head(movies)
movies <- as.data.frame(movies)
movies$year

movies

budget <- data.frame(movies$year, movies$budget)
budget
ggplot(budget, aes(x=movies.year, y=movies.budget)) + geom_bar(stat = "identity") + coord_flip()

year <- movies$year
binary <-movies$binary
clean_test <- movies$clean_test
binary

tab <- count(binary)
tab.df <- data.frame(tab)
tab.df
values
tab
ggplot(tab.df)
tab.df
sum(tab.df$freq)
tab.df <- tab.df %>% 
  arrange(desc(x)) %>%
  mutate(prop = round(freq / sum(tab.df$freq) *100,2)) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

df = data.frame(year, binary,clean_test)

df
sum(tab.df$freq)
dfSummary <- ddply(df, c("year"), summarise, Pass=sum(binary=="PASS"), Fail=sum(binary=="FAIL"), ok=sum(clean_test=="ok"),NoTalk=sum(clean_test=="notalk"), 
                   men=sum(clean_test=="men"),nowomen=sum(clean_test=="nowomen" ), dubious=sum(clean_test=="dubious"))

dfSummary

ggplot(dfSummary, aes(x=year, y=Pass)) + geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=1)

movies.budget <- data.frame(year,budget)

dfSummary <- ddply(df, c("year"), summarise, Pass=sum(binary=="PASS"), Fail=sum(binary=="FAIL"), ok=sum(clean_test=="ok"),NoTalk=sum(clean_test=="notalk"), 
                   men=sum(clean_test=="men"),nowomen=sum(clean_test=="nowomen" ), dubious=sum(clean_test=="dubious"))
dfSummary


movies.budget
budgetSummary <- ddply(movies.budget, c("year"), summarise, max=max(budget), avg = mean(budget), min=min(budget), SD=sd(budget), skew=skewness(budget), kurt=kurtosis(budget)
                       ,below_thousand=sum(budget<1000000),below_twothousand=sum(budget<5000000),oneMillion=sum(budget>10000000&budget<100000000),upper_hundredMillion=sum(budget>100000000))
budgetSummary

perc.rank(budget)

dfSummary

total <- dfSummary$Pass + dfSummary$Fail
perc <- (dfSummary$Pass/total)*100
perc
plot(dfSummary$year, perc, type="l", fill="blue" )



density.year <- density(dfSummary$year)
density.perc <- density(perc)

fig <-plot_ly(x= dfSummary$year, y=perc,type="scatter", mode="lines",fill="tozeroy")

fig <- fig %>% layout(xaxis = list(title="Year"),yaxis=list(title="Percent"))
fig


write.csv(dfSummary,"C:/Users/celeb/Desktop/FALL 2020/CTIS 365/Lab Project/bechdel.csv")
write.csv(budgetSummary,"C:/Users/celeb/Desktop/FALL 2020/CTIS 365/Lab Project/budget.csv")







