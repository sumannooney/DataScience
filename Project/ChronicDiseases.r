rm(list = ls())
library(dplyr)
require(ggplot2)

#data < - read.csv(file='C:\\Users\\p398510\\Downloads\\DataScience350-master (3)\\Project\\U.S._Chronic_Disease_Indicators__CDI_.csv', header = TRUE, stringsAsFactors = FALSE)
file = 'C:\\Tejo\\Datascience\\350\\Classwork\\DSClasswork\\Lecture10\\U.S._Chronic_Disease_Indicators__CDI_.csv' 
#"U.S._Chronic_Disease_Indicators__CDI_.csv"
data.read <- read.csv(file = file, header = TRUE, stringsAsFactors = FALSE) 

# mortality rate for each topic across US
# % of people affected for each topic across US (different cases)


chronic.mortality.data<- data.read  %>%
              select(
                      LocationDesc,
                      Topic,
                      YearStart, 
                      DataValueUnit,
                      DataValueType,
                      DataValueAlt,
                      StratificationCategory1) %>% 
              filter(
                      DataValueUnit %in% c("cases per 100,000"), 
                      StratificationCategory1 == "Overall", 
                      DataValueType == "Crude Rate",
                      !Topic %in% c(
                                      "Asthma",
                                      "Diabetes",
                                      "Older Adults",
                                      "Overarching Conditions"
                                    )
                    )


#chronic.mortality.data<- data.read  %>% select(LocationDesc,Topic,YearStart, DataValueUnit,DataValueType,DataValueAlt,StratificationCategory1) %>% 
#  filter(DataValueUnit %in% c("cases per 100,000"), StratificationCategory1 == "Overall", DataValueType =="Crude Rate", !Topic %in% c("Older Adults","Asthma"))

na.rows <- apply(chronic.mortality.data,1,function(x){any(is.na(x))})

chronic.mortality.data <- chronic.mortality.data[!na.rows,]

vec.Topics<-unique(chronic.mortality.data$Topic)


result.mean <- chronic.mortality.data %>% group_by(Topic,YearStart) %>%
                summarise(mean.value = mean(DataValueAlt))


vec.years = unique(chronic.mortality.data$YearStart)
result = NULL

for (i in 1:length(vec.Topics)){
  print(vec.Topics[i])
    for(j in 1:length(vec.years)){
      tempdata <- chronic.mortality.data %>% 
                      filter(
                              Topic     == vec.Topics[i],
                              YearStart == vec.years[j]
                            )
      
      result <- rbind(result, 
                        c(
                          vec.Topics[i],vec.years[j],
                          tempdata[which.max(tempdata$DataValueAlt),"LocationDesc"],
                          tempdata[which.max(tempdata$DataValueAlt),"DataValueAlt"],
                          tempdata[which.min(tempdata$DataValueAlt),"LocationDesc"],
                          tempdata[which.min(tempdata$DataValueAlt),"DataValueAlt"]
                          )
                      )
    }
  }
result <- as.data.frame(result)
names(result) <- c(
                  "Topic",
                  "Year",
                  "Max_State",
                  "Max_Value",
                  "Min_State",
                  "Min_Value"
                )


chronic.mortality.ByRace <- data.read  %>% 
                    select(
                          LocationDesc,
                          Topic,
                          YearStart,
                          DataValueType,
                          DataValueUnit,
                          DataValueAlt,
                          StratificationCategory1,
                          Stratification1,
                          GeoLocation) %>% 
                    filter(
                            DataValueUnit %in% c("cases per 100,000"),
                            StratificationCategory1 == "Race/Ethnicity",
                            DataValueType =="Crude Rate",
                            !Topic %in% c(
                                            "Asthma",
                                            "Diabetes",
                                            "Older Adults",
                                            "Overarching Conditions"
                                          )
                            )

na.rows.race <- apply(chronic.mortality.ByRace,1,function(x){any(is.na(x))})

chronic.mortality.ByRace <- chronic.mortality.ByRace[!na.rows.race,]



ggplot(data = result,aes(x=Topic, y=Max_Value, fill=Topic)) +
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


ggplot(data = result.mean,aes(x=YearStart, y=mean.value, fill=YearStart)) +
  geom_bar(position = "dodge", stat =  "identity") +
  facet_grid(.~Topic)+
  theme(axis.text.x = element_blank()) +
  coord_flip() 
  geom_text(aes(
                label = mean.value),
                size  = 3, 
                angle = 90, 
                hjust = 0.1,
                vjust = 0.5, 
                colour= "black"
                ) 

#  ggplot(data= result.mean, aes(Topic, mean.value))+
#    geom_boxplot()
  
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


chiTest <- with(chronic.mortality.data, chisq.test(DataValueAlt,Topic))

summary(chiTest)
print(chiTest)
chiTest$observed


#ggplot(chronic.mortality.data, aes(x = DataValueAlt, y = DataValueAlt))+ geom_point(aes(colour= Topic))+
#  facet_wrap(LocationDesc~.)


#p = ggplot(chronic.mortality.data, aes(x=DataValueAlt, fill=Topic)) + geom_density(alpha=.3)
#p + facet_grid(.~ DataValueAlt)

  ggplot(data = chronic.mortality.ByRace, aes(YearStart,DataValueAlt, fill=Topic))+
  geom_bar(stat="identity")+
  facet_wrap(LocationDesc ~ Stratification1, nrow=10 )
