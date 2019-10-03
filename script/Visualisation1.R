library(tidyverse)
gapminder<-read_csv("data/gapminder_data.csv")

gapminder_1977<-gapminder%>%
  filter(year==1977)

ggplot(gapminder_1977)

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap,y = lifeExp, colour = continent, size = pop)) + geom_point()

ggplot(data = gapminder_1977,
       mapping = aes(x=gdpPercap,
                     y = lifeExp,
                     colour = continent,
                     size = pop)) + 
  geom_point() + 
  scale_x_log10()
       

#challenge 4
#there are six variable in gapminder_1977, but "year" not informative
#use default template

ggplot(gapminder_1977, 
       aes(x = country,
           y= gdpPercap, 
           colour= continent, 
           size = lifeExp))+ geom_point()

ggplot(data = gapminder_1977) + 
  geom_point(mapping = aes(x=gdpPercap,
                           y = lifeExp,
                           colour = continent,
                           size = pop)) + 
  scale_x_log10()

#challenge 5
#geom_point: aesthetics: x, y, alpha, colour, fill, group, shape, size, stroke








