library(tidyverse)
gapminder<-read_csv("data/gapminder_data.csv")

gapminder_1977<-gapminder%>%
  filter(year==1977)
