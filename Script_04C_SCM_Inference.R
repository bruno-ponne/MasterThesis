#-------------------------------------------------------------------#
# Script 04C - Synthetic Control Inference (Permutation Test)       #
#-------------------------------------------------------------------#

library(Synth)
library(tidyverse)

source("functions/permutate_p_ls.R")
source("functions/permutate_us.R")


load("data/DATA_COMPLETE.RData")

PRIMARY_M <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "math"))
PRIMARY_P <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "port"))
LOWERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "math"))
LOWERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "port"))
UPPERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "math"))
UPPERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "port"))

############################################################
## Permutation test for Primary and Lower Secondary School #
############################################################

# Executing the synthetic control for each state in each subgroup:
GAP_PRIMARY_M <- permutate_p_ls(PRIMARY_M)
GAP_PRIMARY_M$subject <- "Math"
GAP_PRIMARY_M$grade <- "Primary School"

GAP_PRIMARY_P <- permutate_p_ls(PRIMARY_P)
GAP_PRIMARY_P$subject <- "Portuguese"
GAP_PRIMARY_P$grade <- "Primary School"

GAP_LOWERS_M <- permutate_p_ls(LOWERS_M)
GAP_LOWERS_M$subject <- "Math"
GAP_LOWERS_M$grade <- "Lower Secondary School"

GAP_LOWERS_P <- permutate_p_ls(LOWERS_P)
GAP_LOWERS_P$subject <- "Portuguese"
GAP_LOWERS_P$grade <- "Lower Secondary School"

# Joining all the data for the plots
GAP_P_LS <- rbind(GAP_PRIMARY_M, GAP_PRIMARY_P, GAP_LOWERS_M, GAP_LOWERS_P)

GAP_P_LS$grade <- factor(GAP_P_LS$grade, levels = c("Primary School", "Lower Secondary School"))

GAP_P_LS$isCE[GAP_P_LS$code_state == "X23"] <- "Ceará"
GAP_P_LS$isCE[GAP_P_LS$code_state != "X23"] <- "Control States"

GAP_P_LS$isCE <- factor(GAP_P_LS$isCE, levels = c("Control States", "Ceará"))

# Figure 14:

# Permutation Graphs with all 27 states except one (the one treated with placebo)
ggplot()+
  geom_line(data = filter(GAP_P_LS, code_state != "X23"), aes(x=year, y= gap, group = code_state),size=0.5, color = "gray")+
  geom_line(data = filter(GAP_P_LS, code_state == "X23"), aes(x=year, y= gap),size=1.0, color = "brown2")+
  geom_vline(xintercept = 2008, linetype = "dashed", color = "grey")+
  geom_hline(yintercept = 0, size = 0.7, color = "darkgrey")+
  ylab("Score Gap")+
  xlab("Year")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))
ggsave(filename = "Figure14.png", path = "plots")






# Preintervention mean squared prediction error (MSPE)

MSPE <- GAP_P_LS %>%
  filter(year < 2008) %>% 
  group_by(code_state, grade, subject) %>% 
  summarize(MSPE = mean(gap*gap))

PLOT_MSPE <- left_join(GAP_P_LS, MSPE, by = c("code_state", "grade", "subject"))

# MSPE for Ceará (Primary - Math): 19.75
# 2x19.75 = 39.5
MSPE_PRIMARY_M <- PLOT_MSPE %>% 
  filter(grade == "Primary School", subject == "Math", MSPE < 39.5)

# MSPE for Ceará (Primary - Portuguese): 8.8
# 2x8.8 = 17.6
MSPE_PRIMARY_P <- PLOT_MSPE %>% 
  filter(grade == "Primary School", subject == "Portuguese", MSPE < 17.6)

# MSPE for Ceará (Lower Secondary - Math): 12.15
# 2x12.15 = 24.3
MSPE_LS_M <- PLOT_MSPE %>% 
  filter(grade=="Lower Secondary School", subject == "Math", MSPE<24.3)

# MSPE for Ceará (Lower Secondary - Math): 6.44
# 2x6.44 = 12.88
MSPE_LS_P <- PLOT_MSPE %>% 
  filter(grade=="Lower Secondary School", subject == "Portuguese", MSPE<12.88)

GAP_P_LS_2x <- rbind(MSPE_PRIMARY_M, MSPE_PRIMARY_P, MSPE_LS_M, MSPE_LS_P)

# Figure 15:

# Permutation Graphs ONLY with states whose pre-intervention MSPE was lower than 2x pre-intervention MSPE Ceará
ggplot()+
  geom_line(data = filter(GAP_P_LS_2x, code_state != "X23"), aes(x=year, y= gap, group = code_state),size=0.5, color = "gray")+
  geom_line(data = filter(GAP_P_LS_2x, code_state == "X23"), aes(x=year, y= gap),size=1.0, color = "brown2")+
  geom_vline(xintercept = 2008, linetype = "dashed", color = "grey")+
  geom_hline(yintercept = 0, size = 0.7, color = "darkgrey")+
  ylab("Score Gap")+
  xlab("Year")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))
ggsave(filename = "Figure15.png", path = "plots")


############################################################
## Permutation test for Upper Secondary School             #
############################################################

# Executing the synthetic control for each state in each subgroup:

GAP_UPPERS_M <- permutate_us(UPPERS_M)
GAP_UPPERS_M$subject <- "Math"
GAP_UPPERS_M$grade   <- "Upper Secondary"

GAP_UPPERS_P <- permutate_us(UPPERS_P)
GAP_UPPERS_P$subject <- "Portuguese"
GAP_UPPERS_P$grade   <- "Upper Secondary"

# Joining all the data for the plots
GAP_US <- rbind(GAP_UPPERS_M, GAP_UPPERS_P)

# Graph Complete Permutation Upper Secondary School (not necessary, since the effects are not substantial or even negative)

ggplot()+
  geom_line(data = filter(GAP_US, code_state != "X23"), aes(x=year, y= gap, group = code_state),size=0.5, color = "gray")+
  geom_line(data = filter(GAP_US, code_state == "X23"), aes(x=year, y= gap),size=1.0, color = "brown2")+
  geom_vline(xintercept = 2011, linetype = "dashed", color = "grey")+
  geom_hline(yintercept = 0, size = 0.7, color = "darkgrey")+
  ylab("Score")+
  xlab("Year")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))

############################################################
## Post/Pre Reform MSPE                                    #
############################################################

# Preintervention mean squared prediction error (MSPE)

MSPE_POST <- GAP_P_LS %>%
  filter(year > 2008) %>% 
  group_by(code_state, grade, subject) %>% 
  summarize(MSPE_POST = mean(gap*gap))

POST_PRE <- left_join(MSPE, MSPE_POST, by = c("code_state", "grade","subject"))

POST_PRE$RATIO <- POST_PRE$MSPE_POST/POST_PRE$MSPE

POST_PRE$isCE[POST_PRE$code_state == "X23"] <- "Ceará"
POST_PRE$isCE[POST_PRE$code_state != "X23"] <- "Control States"

# Figure 16:

ggplot(data = POST_PRE, aes(x= RATIO, color = isCE, fill = isCE))+
  geom_histogram(alpha = 0.5)+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  facet_grid(vars(grade), vars(subject))
ggsave(filename = "Figure16.png", path = "plots")







