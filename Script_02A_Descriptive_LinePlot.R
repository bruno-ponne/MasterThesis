#--------------------------------------------------#
# Script 02A - Descriptive Statistics - Line Plots #
#--------------------------------------------------#

library(tidyverse)


load("data/DATA_COMPLETE.RData")

# Time Series Plots (Figures 3 and 4)

DATA_COMPLETE$group <- -1
DATA_COMPLETE$group[DATA_COMPLETE$abbr_state == "CE"] <- "CE"
DATA_COMPLETE$group[DATA_COMPLETE$abbr_state != "CE"] <- "Other States"

DATA_GRAPH <- DATA_COMPLETE %>% 
  group_by(year, group, subject, grade) %>% 
  summarise(score = mean(score))

DATA_GRAPH$grade[DATA_GRAPH$grade=="P"] <- "Primary School"
DATA_GRAPH$grade[DATA_GRAPH$grade=="LS"] <- "Lower Secondary School"
DATA_GRAPH$grade[DATA_GRAPH$grade=="US"] <- "Upper Secondary School"
DATA_GRAPH$grade <- factor(DATA_GRAPH$grade, levels = c("Primary School", "Lower Secondary School", "Upper Secondary School"))

DATA_GRAPH$subject[DATA_GRAPH$subject=="math"] <- "Math"
DATA_GRAPH$subject[DATA_GRAPH$subject=="port"] <- "Portuguese"


ggplot(data = filter(DATA_GRAPH, grade != "Upper Secondary School" ), aes(x=year, y= score, color = group))+
  geom_line(size=0.8)+
  scale_color_manual(values= c("dodgerblue4","brown2"), 
                     labels= c( "Ceará", "Average of other States"), 
                     name = "")+
  geom_vline(xintercept = 2008, linetype = "dashed", color = "darkgrey")+
  ylab("Score")+
  xlab("Year")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  facet_grid(vars(grade), vars(subject))

ggsave(filename = "Figure3.png", path = "plots")

ggplot(data = filter(DATA_GRAPH, grade == "Upper Secondary School" ), aes(x=year, y= score, color = group))+
  geom_line(size=0.8)+
  scale_color_manual(values= c("dodgerblue4","brown2"), 
                     labels= c( "Ceará", "Average of other States"), 
                     name = "")+
  geom_vline(xintercept = 2011, linetype = "dashed", color = "darkgrey")+
  ylab("Score")+
  xlab("Year")+
  ylim(200, 300)+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  facet_grid(vars(grade), vars(subject))

ggsave(filename = "Figure4.png", path = "plots")

