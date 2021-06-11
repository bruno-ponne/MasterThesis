#--------------------------------------------------#
# Script 02A - Descriptive Statistics - Dot Plots  #
#--------------------------------------------------#

library(tidyverse)

load("data/DATA_COMPLETE.RData")

# Figure 7

EFFICIENCY <- DATA_COMPLETE %>% 
  filter(year>=2007) 

AVERAGE_INVEST <- EFFICIENCY %>% 
  group_by(abbr_state) %>% 
  summarise(avg_inv = mean(edu_invest_pc))

SCORE_2007 <- EFFICIENCY %>% 
  filter(year==2007) %>% 
  mutate(score_2007 = score)

SCORE_2019 <- EFFICIENCY %>% 
  filter(year==2019) %>% 
  mutate(score_2019 = score)

SCORE_CHANGE <- left_join(SCORE_2007, 
                          SCORE_2019, 
                          by = c("abbr_state", "grade", "subject")) %>%
  mutate(score_change = score_2019 - score_2007)

SCORE_CHANGE <- select(SCORE_CHANGE, abbr_state, grade, subject, score_change)


PLOT_DATA <- left_join(SCORE_CHANGE, AVERAGE_INVEST, by = "abbr_state")
PLOT_DATA$grade[PLOT_DATA$grade=="P"] <- "Primary School"
PLOT_DATA$grade[PLOT_DATA$grade=="LS"] <- "Lower Secondary School"
PLOT_DATA$grade[PLOT_DATA$grade=="US"] <- "Upper Secondary School"

PLOT_DATA$subject[PLOT_DATA$subject=="math"] <- "Math"
PLOT_DATA$subject[PLOT_DATA$subject=="port"] <- "Portuguese"

PLOT_DATA$FACET <- -1
PLOT_DATA$FACET <- paste(PLOT_DATA$grade, "-", PLOT_DATA$subject)

PLOT_DATA$grade <- factor(PLOT_DATA$grade, 
                          levels = c("Primary School", 
                                     "Lower Secondary School",
                                     "Upper Secondary School"))

ggplot(data = PLOT_DATA, aes(x = avg_inv, 
                             y = score_change, 
                             color = abbr_state == "CE",
                             shape = abbr_state == "CE"))+
  geom_point(size=2.5, alpha = 0.8)+
  scale_shape_manual(values=c(16,17), 
                     labels=c("Other States", "Ceará"), 
                     name = "Legend")+
  scale_color_manual(values= c("brown2","deepskyblue4"), 
                     labels= c("Other States", "Ceará"), 
                     name = "Legend")+
  ylab("Score Change between 2007 and 2019")+
  xlab("Average Spending in education and culture per capita")+
  ggtitle("Score change vs. Spending")+
  theme(legend.position="bottom")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  facet_grid(vars(grade), vars(subject))

ggsave(filename = "Figure7.png", path = "plots")


