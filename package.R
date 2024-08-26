library(tidyverse)
library(ggplot2)
library(scales)
library(tidyr)
library(plotly)

Olympic_Games_data <- read.csv("Olympic_Games_Medal_Tally.csv")
Olympic_data <- read.csv("Olympic_Athlete_Event_Results.csv")
Olympic_Bio_data <- read.csv("Olympic_Athlete_Bio.csv")

inter=left_join(Olympic_data,Olympic_Bio_data,by = "athlete_id")
inter[,"season"] = unlist(lapply(str_split(inter$edition,pattern = " "), function(e) e[2]))
inter[,"edition"] = as.integer(unlist(lapply(str_split(inter$edition,pattern = " "), function(e) e[1])))
inter["born"]  = unlist(lapply(str_split(inter$born," ") , function(e) e[3]))
inter["age"] = as.integer(inter$edition) - as.integer(inter$born)


inter = inter %>% select(edition,sport,age,athlete,medal,sex,height,weight,country,season,edition_id,born,country_noc.y)


Olympics_Cou <- read_csv("Olympics_Country.csv")
inter= inter %>% left_join(Olympics_Cou,by = join_by(country_noc.y == noc))
inter = inter %>% rename(country=country.x)


inert = na.omit(inter %>% select(height,sex,weight,edition,medal) %>% filter(height!="",weight!=""))
inert$weight = as.integer(inert$weight)
inert=na.omit(inert)
inert = inert %>% mutate(BMI=(weight/(height)**2)*10000)

inert$stat_BMI =  ifelse(inert$BMI<18.5,"insuffisance ponderal(BMI<18.5)",ifelse(inert$BMI<24.9 | inert$BMI>18.5,"Normal(BMI<24.9)",ifelse(inert$BMI<29.9 | inert$BMI>25,"surpoids","obésite")))


medal_year<-function(data=Olympic_Games_data,year=1896,height=450){
  a=year
s=data %>% filter(year==a) %>% select(country,gold,silver,bronze) %>% pivot_longer(cols = c("gold","silver","bronze"),names_to = "medail",values_to = "Eff") 
s=s %>% filter(country %in% (s %>% group_by(country) %>% summarise(Eff=sum(Eff)) %>% top_n(12))$country)

return(ggplotly(ggplot(s,aes(x=reorder(str_wrap(country,12),Eff),y=Eff,fill=medail)) +
           geom_bar(stat="identity") +
           geom_text(aes(label=Eff),
           color="white",position = position_stack(vjust = 0.5) ) +
           coord_flip() + 
           theme(legend.position = c(0.92, 0.72),
                 axis.ticks.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(size = 8),
                 plot.title = element_text(hjust = 0.5)
                 ) + 
           labs(x="",y="",title=paste("Top 12 des country par  medal in",a)),height = height)            %>% layout(legend = list(orientation = "h",x = 0.76,xanchor = "center",y = 0.15,title="")))
}


medal_year_country<-function(data=Olympic_Games_data,countries=c("United States","France"),years=seq(1980,2020),height=700){
 s=data %>% filter(country %in% countries) %>% select(year,total,country) %>% group_by(year,country) %>% summarise(total=sum(total)) %>% filter(year %in% years) %>% arrange(country)

  return(ggplotly(ggplot(s,aes(x=year)) + 
  geom_line(aes(y=total,color=country)) +
    labs(x="",y="",legend="",
         title="Total medal per year and per country") + 
    theme(legend.title =element_text("Country"),
          axis.text.x = element_text(size = 8,angle=45),
          plot.title = element_text(hjust = 0.5)) + 
    scale_x_continuous(breaks = data$year),height=height
  ))
}




top_n_coutry<-function(data=Olympic_Games_data,height = 400,varr,tit){
  s = data %>% group_by(country) %>% 
    summarise(total=sum({{varr}})) %>% 
    arrange(-total) %>%  top_n(n = 5)
 return(s %>% plot_ly(labels=~country,type="pie",values=~total,hole=0.5,height =height,title=paste("<b>",s[1,1]$country ,s[1,2]$total,tit,"</b>",sep = " \n")) %>% layout(showlegend = TRUE,legend = list(orientation = "h",x = 0.5,xanchor = "center",y = -0.2),title=paste("Top 5 of best ",tit ," country")))
}


total_paet_sex<-function(data=inter,slice=seq(2000,2020),height=400,titre="Total participant per year and per sex"){
  s= na.omit(data %>%  filter(edition %in% slice) %>% select(edition,sex) %>% group_by(edition,sex) %>% summarise(total=n()) %>% arrange(sex))


return(ggplotly(ggplot(s,aes(x=
edition)) + 
  geom_line(aes(y=total,color=sex)) +
    labs(x="",y="",legend="",
         title=titre) + 
  scale_x_continuous(breaks = seq(min(s$edition),max(s$edition),3)) + 
    theme(legend.title =element_text("Country"),
          axis.text.x = element_text(size = 8,angle=45),
          plot.title = element_text(hjust = 0.5)),heigth=heigth) )
}


participant_season<-function(data=inter,height=400,cols=seq(1920,2020)){
  da= data %>% select(season,edition) %>% group_by(edition,season) %>% summarise(eff=n()) %>% filter(season!="",edition %in% cols)

  return(ggplotly(ggplot(da,aes(x=edition,y=eff,col=season)) + geom_line()+ 
  scale_x_continuous(breaks = seq(min(da$edition),max(da$edition),6))  + labs(y='',x="",title = "Participant repartition per season and per year") + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size = 8,angle=45)),height = height))
  
}

participant_sice_begin<-function(data=inter,height=350,cols=seq(1920,2000)){
  s=na.omit(data %>% select(medal,sex,edition) %>% filter(edition %in% cols) %>% group_by(sex) %>% summarise(eff=n())) 
  return(s %>% plot_ly(labels=~sex,type="pie",values=~eff,hole=0.5,title=paste("<b>",(s %>% arrange(eff))[2,1]$sex ,(s %>% arrange(eff))[2,2]$eff,"</b>",sep = " \n"),height=height) %>% layout(showlegend = TRUE,legend = list(orientation = "h",x = 0.5,xanchor = "center",y = -0.1),title="Number of man and woman participant"))
}


winner_sice_begin<-function(data=inter,height=350,cols=seq(1920,2000)){
  p = data %>% select(medal,sex,edition) %>% filter(medal!="") %>% filter(edition %in% cols)

  s=na.omit(p %>% select(medal,sex,edition) %>% group_by(sex) %>% summarise(eff=n())) 
  return(s %>% plot_ly(labels=~sex,type="pie",values=~eff,hole=0.5,title=paste("<b>",(s %>% arrange(eff))[2,1]$sex ,(s %>% arrange(eff))[2,2]$eff,"</b>",sep = " \n"),height=height) %>% layout(showlegend = TRUE,legend = list(orientation = "h",x = 0.5,xanchor = "center",y = -0.1),title="Number of man and woman participant winner"))
}

icone_partitcipant<-function(data=inter,interval=seq(1920,2020)){
  
  data=data %>% filter(edition %in% interval,season!="")
  
  total_Equestrian= format(as.vector(table(data$season)[1]),big.mark = " ")

  total_Summer= format(as.vector(table(data$season)[2]),big.mark = " ")

  total_Winter= format(as.vector(table(data$season)[3]),big.mark = " ")

  total_participiant = format(dim(data)[1],big.mark = " ")
  
  
  
  return(c(ifelse(total_Equestrian>0,total_Equestrian,0),ifelse(total_Summer>0,total_Summer,0),ifelse(total_participiant>0,total_participiant,0),ifelse(total_Winter>0,total_Winter,0)))
}

bmi_stat<-function(datas=inert,height = 350){
  d= datas %>% group_by(edition) %>% summarize(mean_=mean(BMI))

  s=ggplot(data = d,aes(x=edition)) + geom_line(aes(y=mean_,color="all participante")) + labs(y="",x=""     ,title="Participant  BMI per status of win")

  d = datas %>% filter(medal!="")  %>%  group_by(edition) %>% summarize(mean_=mean(BMI))

  s= s + geom_line(data = d,aes(y=mean_,color="winner"))


  d = datas %>% filter(medal=="")  %>%  group_by(edition) %>% summarize(mean_=mean(BMI))

  s= s + geom_line(data = d,aes(y=mean_,color="Non winner")) + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

  return(ggplotly(s+ 
  scale_x_continuous(breaks = seq(min(d$edition),max(d$edition),6))+ 
    theme(axis.text.x = element_text(size = 8,angle=45)) ,height = height)  %>% layout(showlegend = TRUE,legend = list(orientation = "h",x = 0.5,xanchor = "center",y    = 1,title="")))

}

participant_age_season<-function(data=inter,height=400){
  return(
    ggplotly(ggplot(na.omit(data %>% select(age,sex,season) %>% filter(season!="Equestrian"))) + geom_histogram(aes(x=age),fill="green")  + facet_grid(
rows = vars(season),
cols = vars(sex))+labs(x="",y="",title="participante age repartition by sex and by season")+theme(plot.title = element_text(hjust = 0.5)),height=height)
  )
}

winner_age_season<-function(data=inter,height=400){
  ggplotly(ggplot(na.omit(data %>% select(age,sex,season,medal) %>% filter(season!="Equestrian",medal!=""))) + geom_histogram(aes(x=age),fill="green") + facet_grid(
rows = vars(season),
cols = vars(sex))+labs(x="",y="",title="participante winner age repartition by sex and by season")+theme(plot.title = element_text(hjust = 0.5)),height=height)
}


total_spec_sex<-function(data=dat,slice=seq(2000,2020),height=400,titre="Total participant per year and per sex",couty){
  s= na.omit(data %>%  filter(edition %in% slice,country.y==couty) %>% select(edition,sex) %>% group_by(edition,sex) %>% summarise(total=n()) %>% arrange(sex)) 


return(ggplotly(ggplot(s,aes(x=
edition)) + 
  geom_line(aes(y=total,color=sex)) +
    labs(x="",y="",legend="",
         title=paste(titre,couty)) + 
  scale_x_continuous(breaks = seq(min(s$edition),max(s$edition),3)) + 
    theme(legend.title =element_text("Country"),
          axis.text.x = element_text(size = 8,angle=45),
          plot.title = element_text(hjust = 0.5)),heigth=heigth) )
}





