library(WDI)
library(countrycode)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(viridis)
library(waffle)
library(emojifont)
library(showtext)
library(fontawesome)
library(hrbrthemes)
library(waffle)
library(stringr)
library(grid)
library(gridExtra)
library(cowplot)

#-----------------------------------------------------------------
new_wdi_cache <- WDIcache()
maternalmortality <- WDI(country = 'all', indicator = c('SH.STA.MMRT'),
                         start = 2010, end = 2022, 
                         cache = new_wdi_cache)
contraused <- WDI(country = 'all', indicator = c('SP.DYN.CONU.ZS'),
                        start = 2010, end = 2022, 
                        cache = new_wdi_cache)

staffattended <- WDI(country = 'all', indicator = c('SH.STA.BRTC.ZS'),
                     start = 2000, end = 2017, 
                     cache = new_wdi_cache)


secondaryEducation <- WDI(country = 'all', indicator = c('SE.SEC.CUAT.LO.FE.ZS'),
                          start = 2005, end = 2022, 
                          cache = new_wdi_cache)
#---------------------------------------------------------------------

rm.groups <- unique(maternalmortality$country[is.na(countrycode(maternalmortality$iso2c, "iso2c", "iso2c"))])
rm.groups <- rm.groups[-which(rm.groups %in% c("Kosovo", "Channel Islands"))]

mm.nogroups <- maternalmortality[!maternalmortality$country %in% rm.groups, ]
mm.nogroups <- mm.nogroups %>% rename(modelled_maternalmortality = SH.STA.MMRT)
mm.nogroups_latest <- mm.nogroups[!is.na(mm.nogroups$modelled_maternalmortality),]

View(mm.nogroups_latest)
countryPlot <- read.csv('MMfullcountries.csv')

contraused <- contraused[!contraused$country %in% rm.groups, ]
contraused_latest <- contraused %>% rename(contraprevalance = SP.DYN.CONU.ZS)
contraused_latest <- contraused_latest[!is.na(contraused_latest$contraprevalance),]

staffattendedlatest <- staffattended[!staffattended$country %in% rm.groups,]
staffattendedlatest <- staffattendedlatest %>% rename(staffAttended = SH.STA.BRTC.ZS)
staffattendedlatest <- staffattendedlatest[!is.na(staffattendedlatest$staffAttended),]

View(secondaryEdulatest)
secondaryEdulatest <- secondaryEducation[!secondaryEducation$country %in% rm.groups,]
secondaryEdulatest <- secondaryEdulatest %>% rename(secondaryEdustat = SE.SEC.CUAT.LO.FE.ZS)
secondaryEdulatest <- secondaryEdulatest[!is.na(secondaryEdulatest$secondaryEdustat),]


View(mm.nogroups_latest)

mm.nogroups_latest <- mm.nogroups_latest %>% 
  group_by(country)%>%
  arrange(year)%>%filter(row_number()==n())

contraused_latest <- contraused_latest %>% 
  group_by(country)%>%
  arrange(year)%>%filter(row_number()==n())

staffattendedlatest <- staffattendedlatest %>% 
  group_by(country)%>%
  arrange(year)%>%filter(row_number()==n())

secondaryEdulatest <- secondaryEdulatest %>% 
  group_by(country)%>%
  arrange(year)%>%filter(row_number()==n())


contraused_latest <- arrange(contraused_latest,country)
contraused_latest <- contraused_latest%>% select(country, contraprevalance)

secondaryEdulatest <- arrange(secondaryEdulatest,country)
secondaryEdulatest <- secondaryEdulatest%>% select(country, secondaryEdustat)

View(mm.nogroups_latest)
mm.nogroups_latest <- mm.nogroups_latest %>% 
  arrange(desc(modelled_maternalmortality))

mm.nogroups_latest <- mm.nogroups_latest %>% left_join(contraused_latest,
                                                       by = 'country' )
mm.nogroups_latest <- mm.nogroups_latest[!is.na(mm.nogroups_latest$contraprevalance),]

View(mm.nogroups_latest)

mm.nogroups_latest <- mm.nogroups_latest %>% mutate(contra_rate = contraprevalance*10)

#------------------------Visualisations--------------------------------

#------------Visualisation 1: Circular bar----------------------------
mm.topandbottom <- rbind(head(mm.nogroups_latest, 5), tail(mm.nogroups_latest, 5))
mm.topandbottom <- mm.topandbottom %>% mutate(mortality_group = if_else(modelled_maternalmortality > 500,'high','low'))

plt <- ggplot(mm.topandbottom)+
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 25),
    color = "lightgrey"
  ) +
  geom_col(
    aes(x = reorder(str_wrap(country, 5), contraprevalance), y = contraprevalance,
        fill = modelled_maternalmortality),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9)+
  geom_segment(
    aes(
      x = reorder(str_wrap(country, 5), contraprevalance),
      y = 0,
      xend = reorder(str_wrap(country, 5), contraprevalance),
      yend = 80
    ),
    linetype = "dashed",
    color = "black"
  )+ coord_polar()+
  annotate(
    x = 11.7, 
    y = 33, 
    label = "25%", 
    geom = "text", 
    color = "black",
    size = 8
  )

plt <- plt+annotate(
  x = 11.7, 
  y = 58, 
  label = "50%", 
  geom = "text", 
  color = "black",
  size = 8
)+
  annotate(
    x = 11.7, 
    y = 83, 
    label = "75%", 
    geom = "text", 
    color = "black",
    size = 8
  )+
  scale_y_continuous(
    limits = c(-25, 90),
    expand = c(0, 0),
    breaks = c(0, 25, 50, 75,100)
  ) +
  # New fill and legend title for number of tracks per region
  scale_fill_gradientn(
    "Maternal Mortality per 100,000",
    colours = c( "#3C32E0","#8334DD","#C935CE","#D63B66","#E5643F")
  )+theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "black", size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(size = 9),
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          title = element_text(size = 17, face = 'bold')
  )+
  guides(
    fill = guide_colorsteps(barwidth = 15, barheight = .5, title.position = "top", title.hjust = 0.5))

plt <- plt +labs(
  title = "\nPercentage of population practising any kind of contraception")

plot(plt)

#--------------------------------Visualisation2-----------------

mm.nogroups_latest <- mm.nogroups_latest%>% arrange(desc(modelled_maternalmortality))

staffattendedlatest <- staffattendedlatest %>% select(country,staffAttended)

View(staffattendedlatest)
mm.nogroups_latest <- mm.nogroups_latest %>% 
  left_join(staffattendedlatest, by = 'country')

mortality <- rbind(head(mm.nogroups_latest, 5), tail(mm.nogroups_latest, 5))
mortality <- mortality %>% mutate(mortality_group = if_else(modelled_maternalmortality > 500,'high','low'))
mortality <- mortality %>% mutate(staffRounded = ceiling(staffAttended))
View(mortality)
mortality$index <- 1:nrow(mortality)
index <- 1:nrow(mortality)

mortality$index2 <- str_c(mortality$index,'.',mortality$country)


mortality <- mortality %>% arrange(desc(modelled_maternalmortality))
top_mortality <- head(mortality,5)
top_mortality$index <- 1:nrow(top_mortality)
top_mortality$index2 <- str_c(top_mortality$index,'.',top_mortality$country, ' (',top_mortality$staffAttended,'%)')
low_mortality <- tail(mortality,5)
low_mortality$index <- 1:nrow(low_mortality)
low_mortality$index2 <- str_c(low_mortality$index,'.',low_mortality$country, ' (',low_mortality$staffAttended,'%)')

plot1<- ggplot(top_mortality, aes(fill=mortality_group, values=c(staffRounded))) +
  geom_waffle(color = "white", size=1, n_rows = 10, flip = TRUE) +
  facet_wrap(~index2, ncol=5)+
  ggthemes::scale_fill_tableau(name=NULL) + theme(panel.grid = element_blank(),
                                                   axis.ticks = element_blank(),
                                                   axis.text = element_blank(),
                                                  legend.position="none", 
                                                  plot.background = element_rect(fill="#FAF8E4", color = NA))+
  coord_equal()+
    scale_fill_manual(name = NULL,values = c("#973C6F"))+
  labs(
    title = "Top 5 countries with the highest maternal mortality"
  )
plot2 <- ggplot(low_mortality, aes(fill=mortality_group, values=c(staffRounded))) +
  geom_waffle(color = "white", size=1, n_rows = 10, flip = TRUE) +
  facet_wrap(~index2, ncol=5)+
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal()+theme(panel.grid = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_blank(),
                      legend.position="none",
                      plot.background = element_rect(fill="#FAF8E4", color = NA))+
  scale_fill_manual(name = NULL,values = c("#637F17"))+
  labs(
    title = "Top 5 countries with the least maternal mortality"
  )

g <- grid.arrange(plot1,plot2, top = textGrob('Percentage of Live Births Attended by medical Professionals',gp=gpar(fontsize=20,font=2)))

g2 <- ggdraw(g) + theme(plot.background = element_rect(fill="#FAF8E4", color = NA))
plot(g2)


#----------------------Scatter plot-----------------------------
mm.nogroups_latest <- mm.nogroups_latest %>% 
  left_join(secondaryEdulatest, by = 'country')

mm.nogroups_latest <- mm.nogroups_latest %>% mutate(uneducatedstat = 100 - secondaryEdustat)
mm.nogroups_latest <- mm.nogroups_latest %>% mutate(nocontracept = 100 - contraprevalance)
mm.nogroups_latest <- mm.nogroups_latest %>% mutate(unattendedStaff = 100 - staffAttended)

mm.nogroups_latest <- mm.nogroups_latest %>% mutate(contracateg = ifelse(contraprevalance <40, 'Low',
                                                                         ifelse(contraprevalance >= 40 & contraprevalance <60, 'Moderate','High')))


View(mm.nogroups_latest)



ggplot(data = mm.nogroups_latest, aes(x = uneducatedstat,y = modelled_maternalmortality,size = unattendedStaff,colour = contracateg))+
  geom_point(alpha = 0.7)+
  scale_size(range = c(2, 12))+
  labs(x = 'Percentage of uneducated women', y = 'Modelled maternal mortality', title = 'Maternal mortality and the factors associated with it.')+
  scale_color_manual(values=c("#1E88E5", "#D81B60", "#FFC107"))+
  guides(color = guide_legend(title = "Percentage of people \nusing contraceptives"),
         size= guide_legend(title = 'Percentage of births \nnot attended by \nmedical staff'))+
  theme(plot.title=element_text(hjust = 0, vjust = -2.5, margin=margin(t=40,b=-30), face = 'bold', size= 18),
        axis.title = element_text(size = 17))
  

#---------------------------------------------------------------------

world_coordinates <- map_data('world')

world_map <- world_coordinates %>% left_join(
  countryPlot, by = c('region'='country')
)

View(countryPlot)

#country_coords <- read.csv('world_country_longitudes.csv')

#country_coords <- country_coords %>% select(latitude,longitude,country)
#mm.nogroups_latest <- mm.nogroups_latest %>% left_join(country_coords, by = 'country')

cb2 <- c('#24674E','#D5B5C5','#B786A0','#8D5E77')

ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long,lat, map_id = region, fill = modelled_maternalmortality), 
           color = "black", size = 0.07)+
  scale_y_continuous(limits=c(-50,100))+
  scale_fill_gradientn(
             "Mortality \nper 100,000",
             colours = cb2,
             na.value = '#333333'
           )+ 
  labs(title = 'Maternal mortality across the world')+
  theme_void()+
  theme(panel.background = element_rect(fill = '#F3F1E3'),
        legend.key.height= unit(4.2, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.title = element_text(size = 9),
        plot.title=element_text(hjust = 0.5,vjust = 1, margin=margin(t=40,b=-30), face = 'bold', size= 20),
        aspect.ratio = 9/16)

#------------------------------------------------------------------



