#-----------------------------------------------------#
# Script 02C - Descriptive Statistics - Spatial Plots #
#-----------------------------------------------------#

library(sf)
library(tidyverse)
library(readxl)

# Loading map
states_map <- read_sf("map/UFEBRASIL.shp")
states_map <- rename(states_map, code_state = CD_GEOCODU)

# Loading HDI data
IDHM <- read_excel("data/IDHM.xlsx")

# Loading abbreviations of the states
load("data/abbr_code.RData")

# Figure 1
IDHM <- IDHM[2:28,]
IDHM <- rename(IDHM, abbreviation = Territorialidades, IDH = 'IDHM 2017')
IDHM <- left_join(IDHM, abbr_state, by = "abbreviation")
IDHM <- rename(IDHM, code_state = code)
IDHM$code_state <- as.character(IDHM$code_state)
IDH_MAP <- left_join(states_map, IDHM, by = "code_state")

ggplot()+ 
  geom_sf(data = IDH_MAP, aes(fill = IDH), size = 0, color = "lightgray")+
  scale_fill_gradient(limits = c(0.68, 0.85), name = "HDIM", low = '#d0d1e6', high = '#08519c')+
  theme_minimal()

# Loading SAEB data:

load("data/DATA_COMPLETE.RData")

# Primary education (Figure 6)
DATA_2007_19 <- DATA_COMPLETE %>% 
  filter(year == 2007 | year == 2019, grade == "P") %>% 
  mutate(code_state = as.character(code_state))

IDEB_MAP_M_P <- left_join(states_map, DATA_2007_19, by = "code_state")
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2007] <- "pre-reform"
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2019] <- "post-reform"
IDEB_MAP_M_P$year <- factor(IDEB_MAP_M_P$year, levels = c("pre-reform", "post-reform"))

IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="math"] <- "Math"
IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="port"] <- "Portuguese"


ggplot()+ 
  geom_sf(data = IDEB_MAP_M_P, aes(fill = score ), size = 0)+
  scale_fill_gradient(name = "Score: ", low = '#d0d1e6', high = '#084594')+
  theme_minimal()+
  facet_grid(vars(year), vars(subject))+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(axis.text = element_blank(),
      axis.ticks = element_blank())

ggsave(filename = "Figure6.png", path = "plots")

# Lower and upper secondary education (Appendix)

DATA_2007_19 <- DATA_COMPLETE %>% 
  filter(year == 2007 | year == 2019, grade == "LS") %>% 
  mutate(code_state = as.character(code_state))

IDEB_MAP_M_P <- left_join(states_map, DATA_2007_19, by = "code_state")
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2007] <- "pre-reform"
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2019] <- "post-reform"
IDEB_MAP_M_P$year <- factor(IDEB_MAP_M_P$year, levels = c("pre-reform", "post-reform"))

IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="math"] <- "Math"
IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="port"] <- "Portuguese"


ggplot()+ 
  geom_sf(data = IDEB_MAP_M_P, aes(fill = score ), size = 0)+
  scale_fill_gradient(name = "Score: ", low = '#d0d1e6', high = '#084594')+
  theme_minimal()+
  facet_grid(vars(year), vars(subject))+
  ggtitle(label ="Spatial plots: Scores in Lower Secondary School")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = "FigureA02_01.png", path = "plots")

DATA_2007_19 <- DATA_COMPLETE %>% 
  filter(year == 2007 | year == 2019, grade == "US") %>% 
  mutate(code_state = as.character(code_state))

IDEB_MAP_M_P <- left_join(states_map, DATA_2007_19, by = "code_state")
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2007] <- "pre-reform"
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2019] <- "post-reform"
IDEB_MAP_M_P$year <- factor(IDEB_MAP_M_P$year, levels = c("pre-reform", "post-reform"))

IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="math"] <- "Math"
IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="port"] <- "Portuguese"


ggplot()+ 
  geom_sf(data = IDEB_MAP_M_P, aes(fill = score ), size = 0)+
  scale_fill_gradient(name = "Score: ", low = '#d0d1e6', high = '#084594')+
  theme_minimal()+
  facet_grid(vars(year), vars(subject))+
  ggtitle(label ="Spatial plots: Scores in Upper Secondary School")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = "FigureA02_02.png", path = "plots")