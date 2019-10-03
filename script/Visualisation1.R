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


best_plot<-ggplot(gapminder_byContinent,
       aes(x=year,
           y=mean_lifeExp, 
           colour=continent))+
  geom_point(shape="triangle",size=3)

#change to line
ggplot(gapminder,
       aes(x=year,
           y=lifeExp, 
           group=country,
           colour= continent,
           size=pop))+
   geom_point(colour="black")+
   geom_line()  

ggsave("figure/my_best_plot.png", plot=best_plot, width = 12, height = 10, units= "cm", dpi = 300)


#transformation and statistics

ggplot(gapminder,
       mapping= aes(x= gdpPercap, 
                    y= lifeExp))+
  geom_point(alpha=0.3)+
  scale_x_log10() +
  geom_smooth(method="lm",size=2)

ggplot(gapminder,
       mapping= aes(x= gdpPercap, 
                    y= lifeExp,
                    colour=continent))+
  geom_point(alpha=0.3)+
  scale_x_log10() 

#equivalent to

ggplot(gapminder,
       mapping = aes(x=gdpPercap,
                     y=lifeExp))+
  geom_point(aes(colour=continent), alpha=0.8)+
  scale_x_log10()


#choose colour manually
ggplot(gapminder,
       mapping = aes(x=gdpPercap,
                     y=lifeExp))+
  geom_point(aes(colour=continent), alpha=0.8)+
  scale_x_log10()+
  scale_colour_manual(values = c("red", "green","pink", "blue", "yellow"))

#challenge 9

ggplot(gapminder,
       mapping = aes(x=gdpPercap,
                     y=lifeExp))+
  geom_point(colour= "orange", size= 6, alpha=0.8)+
  scale_x_log10()


#challenge 10

ggplot(gapminder,
       mapping = aes(x=gdpPercap,
                     y=lifeExp,
                     colour=continent))+
  geom_point(shape= "square", size= 2, alpha=0.8)+
  scale_x_log10()+
  geom_smooth(method='lm', size=2, se= FALSE) 

#challenge 11

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  scale_colour_manual(values = c("red", "green", "blue", "purple", "black"))

colours()

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  scale_colour_manual(values = c("salmon4", "turquoise3", "slategray", "navy", "green"))

ggplot(gapminder, aes(x=gdpPercap, y=lifeExp,
                      colour=continent,
                      shape=continent))+
         geom_point()+
         scale_x_log10()+
         geom_smooth(method='lm', size=1.5)

ggplot(gapminder, aes(x=gdpPercap, y=lifeExp,
                    colour=continent,
                    shape=continent))+ geom_point(size=4)+
  scale_x_log10()+
  scale_shape_manual(values=c(7,8,3,10,18))

ggplot(gapminder, aes(x=gdpPercap, y=lifeExp,
                      colour=continent,
                      shape=continent))+ geom_point(size=4)+
  scale_x_log10()+
  scale_colour_brewer(palette= "YlGnBu")

#split the the dataset into multiple plots with the same aesthetics

a_countries<-gapminder %>% 
  filter(str_starts(country, "A"))

ggplot(a_countries,
       mapping= aes(x=year,
                    y= lifeExp,
                    colour=continent,
                    group=country))+
  geom_line()+
  facet_wrap(~country)

ggplot(data = gapminder_1977,
        aes(x=gdpPercap,
                     y = lifeExp,
                     colour = continent,
                     size = pop,
                     label = country)) + 
  geom_point() + 
  scale_x_log10()+
  facet_wrap(~year)+
  geom_text(data=gapminder_rich)



gapminder_rich<-filter(gapminder_1977, gdpPercap>30000) 
                       
rough_plot<-ggplot(
  data=a_countries,
  mapping= aes(x=year,y=lifeExp,
               colour= continent, group=country))+
  geom_line()+
  facet_wrap(~country)

rough_plot + scale_color_brewer(palette = "Dark2")
                       
rough_plot + labs(title= "Life expectancy over time for 'A' countries",
                  caption= "Data from gapminder",
                  x= "Year",
                  y= "Life expectancy", 
                  colour= "Continent")+
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size=4),
    axis.line = element_line(colour="blue", size = 4))

ggsave("figure/my_first_plot.png", plot= rough_plot)                       
                       
                       
                       
                       
                       
                       
            