library(gtrendsR)
library(usmap)
library(ggplot2)
library(gganimate)
library(tidyverse)
library(gifski)

###All codes below can be wrapped into a single function, here displayed step by step

##Define key word and date range
##date range unit is month, e.g. start_dt<-'2020-01-01' to end_dt<-'2021-06-01' 
##means from Jan 2020 to Jun 2021 (returns full Jun even though entered 06-01)
keywd<-'Myeloma'
start_dt<-'2019-01-01'
end_dt<-'2021-06-01'

##Create date ranges
dt_1<-seq(as.Date(start_dt),as.Date(end_dt),by = 'months')
dt_2<-seq(seq(as.Date(start_dt),by='1 months',length=2)[2],
          seq(as.Date(end_dt),by='1 months',length=2)[2],
          by='months')
dt_ranges<-paste(dt_1,dt_2)

#within lapply create function to call int_by_states
out=lapply(dt_ranges,function(x){
  a=gtrends(keyword = keywd,geo = c("US"),time = x,
            onlyInterest = FALSE)
  a$interest_by_region
})

google_trend_states<-as.data.frame(do.call(rbind,out))
google_trend_states$date=sort(rep(dt_1,51))

#https://www.seobility.net/en/wiki/Google_Trends
## Under Interest by region section -
### "For the interest by region view, the number of search queries for a term 
### within a specific region is determined relative to the total volume of all 
### search queries within the region (over a certain time period).
### The region with the highest relative volume then receives a score of 100 and all 
### other countries are scaled down accordingly."
## Therefore no need for population adjustment

#Not used
#state_names<-as.data.frame(rbind(cbind(state.abb,state.name),c('DC','District of Columbia')))

google_trend_states%>%
  #filter(date=='2019-01-01' | date=='2019-02-01' | date=='2019-03-01')%>%
  mutate(hits2=as.numeric(ifelse(!hits %in% c(1:100),'0',hits)),
         fips=fips(location),
         date_format = format(date, '%b %Y'),
         date_format=factor(date_format, unique(date_format)))->google_trend_states_2

##Make sub plot of DC for zoom effect
dc_map<-plot_usmap(data = google_trend_states_2,
                   values = 'hits2',labels = T,include = 'DC') 
dc_map[[1]]

#Main plot
plot_usmap(data = google_trend_states_2,
           values = 'hits2',labels = F) +
  labs(title = paste0("Google Search Trends - ",keywd),
       subtitle = "Date: {closest_state}")+ 
  #manipulate x & y to enlarge DC area
  geom_polygon(aes(x=((20*(x-min(x))+1.2*min(x))),y=(20*(y-max(y))+max(y)),fill=hits2,group=fips),
               data=dc_map[[1]],color='black')+
  #set color limit to match DC records
  scale_fill_continuous(low = "green", high = "blue", name = "Trend Index",limits = c(0,100))+
  geom_text(aes(x=1.3*min(x),y=1.4*max(y)),data=dc_map[[1]],label='DC',size=3)+
  guides(fill = guide_colourbar(barwidth = 20, barheight = 1))+
  theme(panel.background=element_blank(),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        legend.position = 'bottom',
        legend.title = element_text(size=16),
        legend.text  = element_text(size=12))#+
  transition_states(date_format,transition_length = 10,state_length = 1,wrap = FALSE)+
  ease_aes('cubic-in-out')->gtrend_anim


animate(gtrend_anim,duration = 30, fps = 10, end_pause = 40,
                  height=500,width=500, res=80,renderer = gifski_renderer())
