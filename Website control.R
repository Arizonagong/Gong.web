rm(list=ls())
blogdown::serve_site()
setwd("/Users/bgong/Documents/Career management/Job/CV development/R portfolio/Gong.web")

# Post_Nested Data
rm(list=ls())
library(tidyr)
BigM_blog <- transform(BigM, id=match(Identifier, unique(Identifier)))
BigM_blog <- BigM_blog %>% select(id, Pre_Post, Questions, Rating, Year, Position, Section, GroupMean)
BigM_blog <- BigM_blog %>% select(id, Pre_Post, Questions, Rating, Year, Position, Section, GroupMean)
rm(list=setdiff(ls(), "BigM_blog"))
BigM_blog$Section<-gsub(" ", "",BigM_blog$Section)
BigM_blog1<-BigM_blog %>% filter(Section =="S1"|Section== "S3C1"|Section== "S2C1")
BigM_blog2<-BigM_blog1 %>% filter(Year == "18-19" & Section != "S2C1"|
                                 Year == "19-20" & Section != "S3C1")
BigM_blog3 <- BigM_blog2 %>% select(id, Pre_Post, Year, Position, Section, GroupMean) %>%
  unique()
BigM_blog4 <- BigM_blog3 %>% mutate(Section=recode(Section, S1="Self_Efficacy", 
                                                   S2C1="Intercultural_Understanding", 
                                                   S3C1="Intercultural_Understanding"))
BigM_Short<-spread(BigM_blog4, key = Section, value = GroupMean)
Studyabroad<-BigM_Short %>% filter(Pre_Post==1) %>% 
  select(!Pre_Post) %>% mutate(Major=Position) %>% 
  select(Intercultural_Understanding, Self_Efficacy,Major,Year)
Studyabroad$Major<-as.factor(Studyabroad$Major)
Studyabroad$Year<-as.factor(Studyabroad$Year)
ggplot(BigM_Short, aes(x=Self_Efficacy, y=Intercultural_Understanding, col=Position, group=Position)) + 
  geom_point(position = "jitter") + geom_smooth(method = lm, se= FALSE,size= .5, alpha  = .8)+
  theme_minimal()
Studyabroad<-Studyabroad%>%drop_na(Intercultural_Understanding, Self_Efficacy)
Studyabroad<-Studyabroad%>%unite(Group, Major, Year, sep="_")
# Extract out the coefficents 
modelOutPlot <- tidy(lmerModel, conf.int = TRUE)

# Grab the coefficents of interest
modelOutPlot <- modelOutPlot[ modelOutPlot$effect =="fixed" &
                                modelOutPlot$term != "(Intercept)", ]

# plot the coefficients of interest
ggplot(modelOutPlot, aes(x = term, y = estimate,
                         ymin = conf.low,
                         ymax = conf.high)) +
  theme_minimal() +
  geom_hline(yintercept = 0.0, color = 'red', size = 2.0) +
  geom_point() +
  geom_linerange() + coord_flip()    