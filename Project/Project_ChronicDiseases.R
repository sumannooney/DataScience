rm(list = ls())
library(dplyr)
library(ggplot2)

#data < - read.csv(file='C:\\Users\\p398510\\Downloads\\DataScience350-master (3)\\Project\\U.S._Chronic_Disease_Indicators__CDI_.csv', header = TRUE, stringsAsFactors = FALSE)
file = '/Users/sumannooney/Documents/Data Science/DataAnalysis/Assignment/Project/U.S._Chronic_Disease_Indicators__CDI_.csv' 
#"U.S._Chronic_Disease_Indicators__CDI_.csv"
data.read <- read.csv(file = file, header = TRUE, stringsAsFactors = FALSE) 

# mortality rate for each topic across US
# % of people affected for each topic across US (different cases)




chronic.mortality.data<- data.read  %>% select(LocationDesc,Topic,YearStart, DataValueUnit,DataValueType,DataValueAlt,StratificationCategory1) %>% 
  filter(DataValueUnit %in% c("cases per 100,000"), StratificationCategory1 == "Overall", DataValueType =="Crude Rate", !Topic %in% c("Asthma","Diabetes","Older Adults","Overarching Conditions"))
#%>%
#  group_by(Topic,LocationDesc) %>%
#  summarise(max.year = max(DataValueAlt)) %>%
#  arrange(LocationDesc)


#chronic.mortality.data<- data.read  %>% select(LocationDesc,Topic,YearStart, DataValueUnit,DataValueType,DataValueAlt,StratificationCategory1) %>% 
#  filter(DataValueUnit %in% c("cases per 100,000"), StratificationCategory1 == "Overall", DataValueType =="Crude Rate", !Topic %in% c("Older Adults","Asthma"))

na.rows<-apply(chronic.mortality.data,1,function(x){any(is.na(x))})

chronic.mortality.data <-chronic.mortality.data[!na.rows,]


vec.Topics<-unique(chronic.mortality.data$Topic)
library(dplyr)
result.mean <- chronic.mortality.data %>% group_by(Topic,YearStart) %>%
                summarise(mean.value = mean(DataValueAlt))

rm(result)
vec.years = unique(chronic.mortality.data$YearStart)
result=NULL
for (i in 1:length(vec.Topics)){
  print(vec.Topics[i])
  for(j in 1:length(vec.years)){
    tempdata <- chronic.mortality.data %>% filter(Topic==vec.Topics[i], YearStart==vec.years[j])
    result <- rbind(result, c(vec.Topics[i],vec.years[j],
                              tempdata[which.max(tempdata$DataValueAlt),"LocationDesc"],
                              tempdata[which.max(tempdata$DataValueAlt),"DataValueAlt"],
                              tempdata[which.min(tempdata$DataValueAlt),"LocationDesc"],
                              tempdata[which.min(tempdata$DataValueAlt),"DataValueAlt"]))
  }
  }
result <- as.data.frame(result)
names(result)=c("Topic","Year","Max_State","Max_Value","Min_State","Min_Value")

ggplot(data = result,aes(x=factor(Topic, levels = Topic[order(Max_Value)]), y=as.numeric(as.character(Max_Value)), fill=Topic)) +
    geom_bar(position = "dodge", stat =  "identity") +
    facet_grid(.~Year, scales = "free_y")+
    theme(axis.text.x=element_blank()) +
    #coord_flip() 
    geom_text(aes(label=Max_State),
              size=3, 
              angle=90, 
              hjust=0.1,
              vjust=0.5, 
              colour="black")
+
  scale_x_discrete(limits=Topic)


ggplot(data = result.mean,aes(x=YearStart, y=mean.value, fill=YearStart)) +
  geom_bar(position = "dodge", stat =  "identity") +
  facet_grid(.~Topic)+
  theme(axis.text.x=element_blank()) +
  coord_flip() 
  geom_text(aes(label=mean.value),
            size=3, 
            angle=90, 
            hjust=0.1,
            vjust=0.5, 
            colour="black") 

  #### Hypothesis
  #NULL Hypothesis: Consistent across US over a period of time or less than the earlier years
  #Alternate Hypothesis : Mortality is not consistent and is increasing year over year

  for (j in 1:length(vec.Topics)) {
  for (i in 1:(length(vec.years)-1)) {
    pop_A = chronic.mortality.data %>% select(DataValueAlt, Topic, YearStart) %>% filter(Topic == vec.Topics[j], YearStart==vec.years[i]) 
    pop_B = chronic.mortality.data %>% select(DataValueAlt, Topic, YearStart) %>% filter(Topic == vec.Topics[j], YearStart==vec.years[i+1]) 
    
    pop_B$DataValueAlt=log(pop_B$DataValueAlt)
    pop_A$DataValueAlt=log(pop_A$DataValueAlt)
    
    tresult <- t.test(pop_A$DataValueAlt,pop_B$DataValueAlt,alternative = "two.sided")
    if (tresult$p.value<0.05) {
      cat("\n",vec.Topics[j], ":", "Accept Null hypo", " ", vec.years[i], " ", vec.years[i+1])
    }else{
      cat("\n",vec.Topics[j], ":", "Reject Null hypo", " ", vec.years[i], " ", vec.years[i+1])
    }
    
  }
  }
  
  aov.df=chronic.mortality.data
  aov.df$DataValueAlt=log(aov.df$DataValueAlt)
  
  df_aov = aov(DataValueAlt ~ Topic + YearStart ,data = aov.df)
  summary(df_aov)  
  print(df_aov)
  
  
names(chronic.mortality.ByRace)

states=map_data("state")
str(states)
chronic.mortality.ByRace$region=tolower(chronic.mortality.ByRace$LocationDesc)
states=merge(states,chronic.mortality.ByRace,by="region",all.x=T)
str(states)
# Life expectancy in African American
ggplot(states, aes(x = long, y = lat, group = group, fill = DataValueAlt)) + 
  geom_polygon(color = "white") +
  scale_fill_gradient(name = "Years", low = "#ffe8ee", high = "#c81f49", guide = "colorbar", na.value="#eeeeee", breaks = pretty_breaks(n = 5)) +
  #labs(title=Topic) +
  coord_map() +
  facet_grid(YearStart~Topic)+
  geom_text(aes(label=region),
            size=3, 
            angle=90, 
            hjust=0.1,
            vjust=0.5, 
            colour="black")

  
  