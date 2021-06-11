#-----------------------------------------------------#
# Script 02A - Descriptive Statistics - Density Plots #
#-----------------------------------------------------#

library(tidyverse)

# Loading data:

load("data/DATA_COMPLETE.RData")

# Figure 5

DATA_DIST <- DATA_COMPLETE %>% 
  select(abbr_state, grade, year, score)

DATA_DIST$LEGEND <- -1
DATA_DIST$LEGEND[DATA_DIST$abbr_state == "CE"] <- "Ceará"
DATA_DIST$LEGEND[DATA_DIST$abbr_state != "CE"] <- "Other States"

PRIMARY <- DATA_DIST

PRIMARY$PERIOD <- -1
PRIMARY$PERIOD[PRIMARY$year <= 2007] <- "pre-reform"
PRIMARY$PERIOD[PRIMARY$year >= 2011] <- "post-reform"
PRIMARY$PERIOD <- factor(PRIMARY$PERIOD, levels = c("pre-reform", "post-reform"))

PRIMARY$grade[PRIMARY$grade == "P"] <- "Primary School"
PRIMARY$grade[PRIMARY$grade == "LS"] <- "Lower Secondary School"
PRIMARY$grade[PRIMARY$grade == "US"] <- "Upper Secondary School"
PRIMARY$grade <- factor(PRIMARY$grade, levels = c("Primary School", "Lower Secondary School", "Upper Secondary School"))
PRIMARY <- na.omit(PRIMARY)

ggplot(data = PRIMARY, aes(x = score, color = LEGEND))+
  geom_density(size = 0.8)+
  scale_color_manual(values= c("dodgerblue4","brown2"), 
                     labels= c( "Ceará", "Other States"), 
                     name = "Unit:")+
  scale_linetype_manual(values= c("solid","dashed"), 
                        labels= c("Post", "Pre"), 
                        name = "Time:")+
  ylab("Density")+
  xlab("Scores in Mathematics and Portuguese")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  xlim(130,290)+
  facet_grid(vars(PERIOD),vars(grade))

ggsave(filename = "Figure5.png", path = "plots")

