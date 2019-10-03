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


#challenge 6: try setting soe aesthetics
ggplot(data = gapminder_1977, 
  mapping = aes(x = gdpPercap, 
                y = lifeExp, 
                colour = continent, 
                size = pop)) +
  geom_point(aes(alpha= pop), shape="star", colour= "pink", size=2) +
  scale_x_log10()


#challenge 7:  shows how life expectancy has changed over time with gapminder data
gapminder_grouped<-gapminder%>%
  group_by(year)%>%
  select(year, continent, lifeExp, gdpPercap)%>%
  summarise(mean_gdpPercap=mean (gdpPercap), mean_lifeExp=mean(lifeExp))
  
ggplot(gapminder,
       aes(x=year,
           y=lifeExp, 
           colour= continent,
           size=pop))+geom_point(shape= "triangle")

#this one works
ggplot(data= gapminder_grouped, 
       mapping= aes(x=year,
                    y=mean_lifeExp))+
  geom_point(shape = "triangle")+
  scale_x_continuous()

#try to get mean_lifeExp per continent per year plotted
gapminder_byContinent<-gapminder%>%
  group_by(continent,year)%>%
  summarise(mean_lifeExp=mean(lifeExp))


ggplot(gapminder_byContinent,
       aes(x=year,
           y=mean_lifeExp, 
           colour=continent))+
  geom_point(shape="triangle",size=3)







