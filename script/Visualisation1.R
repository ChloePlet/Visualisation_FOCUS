library(tidyverse)
gapminder<-read_csv("data/gapminder_data.csv")

gapminder_1977<-gapminder%>%
  filter(year==1977)

ggplot(gapminder_1977)

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap,y = lifeExp, colour = continent, size = pop)) + geom_point()

ggplot(data = gapminder_1977,
       mapping = aes(x=gdpPercap, y = lifeExp, colour = continent, size = pop)) + geom_point() + scale_x_log10()
       
       