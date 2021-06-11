#-------------------------------------------------------------------#
# Script 04A - Synthetic Control Estimates and in-time placebo test #
#-------------------------------------------------------------------#


library(tidyverse)
library(Synth)
library(kableExtra)
library(xtable)


source("functions/plot_scm.R")
source("functions/prepare_p_ls.R")
source("functions/prepare_us.R")
source("functions/prepare_time_placebo.R")


load("data/DATA_COMPLETE.RData")
load("data/abbr_code.RData")

# Subsetting data:

PRIMARY_M <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "math"))
PRIMARY_P <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "port"))
LOWERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "math"))
LOWERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "port"))
UPPERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "math"))
UPPERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "port"))


# Preparing data for Synth:
DATA_PM <- prepare_p_ls(PRIMARY_M)
DATA_PP <- prepare_p_ls(PRIMARY_P)

DATA_LSM <- prepare_p_ls(LOWERS_M)
DATA_LSP <- prepare_p_ls(LOWERS_P)

DATA_USM <- prepare_us(UPPERS_M)
DATA_USP <- prepare_us(UPPERS_P)


##############################################
# SCM for primary and lower secondary school #
##############################################

# Run synth
SCM_PM    <- synth(DATA_PM)
TABLES_PM <- synth.tab(dataprep.res = DATA_PM, synth.res = SCM_PM)

SCM_PP    <- synth(DATA_PP)
TABLES_PP <- synth.tab(dataprep.res = DATA_PP, synth.res = SCM_PP)

SCM_LSM    <- synth(DATA_LSM)
TABLES_LSM <- synth.tab(dataprep.res = DATA_LSM, synth.res = SCM_LSM)

SCM_LSP    <- synth(DATA_LSP)
TABLES_LSP <- synth.tab(dataprep.res = DATA_LSP, synth.res = SCM_LSP)


# Graphs in ggplot
PM <- plot_scm(PRIMARY_M, TABLES_PM)
PM_SC <- PM[[1]]
PM_GAP <- PM[[2]]

PP <- plot_scm(PRIMARY_P, TABLES_PP)
PP_SC <- PP[[1]]
PP_GAP <- PP[[2]]

LSM <- plot_scm(LOWERS_M, TABLES_LSM)
LSM_SC <- LSM[[1]]
LSM_GAP <- LSM[[2]]

LSP <- plot_scm(LOWERS_P, TABLES_LSP)
LSP_SC <- LSP[[1]]
LSP_GAP <- LSP[[2]]

DATA_GRAPH <- rbind(PM_SC, PP_SC, LSM_SC, LSP_SC)

DATA_GRAPH$grade[DATA_GRAPH$grade=="P"] <- "Primary School"
DATA_GRAPH$grade[DATA_GRAPH$grade=="LS"] <- "Lower Secondary School"
DATA_GRAPH$grade <- factor(DATA_GRAPH$grade, levels = c("Primary School", "Lower Secondary School"))

DATA_GRAPH$subject[DATA_GRAPH$subject=="math"] <- "Math"
DATA_GRAPH$subject[DATA_GRAPH$subject=="port"] <- "Portuguese"

# Figure 8:

ggplot(data = DATA_GRAPH, aes(x=year, y= score, color = unit))+
  geom_line(size=0.8)+
  scale_color_manual(values= c("dodgerblue4","brown2"), 
                     labels= c( "Ceará", "Synthetic Ceará"), 
                     name = "")+
  ylab("Score")+
  xlab("Year")+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  facet_grid(vars(grade), vars(subject))

ggsave(filename = "Figure8.png", path = "plots",   width = 15, height = 11, units = "cm")

DATA_GAP <- rbind(PM_GAP, PP_GAP, LSM_GAP, LSP_GAP)

DATA_GAP$grade[DATA_GAP$grade=="P"] <- "Primary School"
DATA_GAP$grade[DATA_GAP$grade=="LS"] <- "Lower Secondary School"
DATA_GAP$grade <- factor(DATA_GAP$grade, levels = c("Primary School", "Lower Secondary School"))

DATA_GAP$subject[DATA_GAP$subject=="math"] <- "Math"
DATA_GAP$subject[DATA_GAP$subject=="port"] <- "Portuguese"

# Figure 9:

ggplot(data = DATA_GAP, aes(x=year, y=gap))+
  geom_line(size=0.8, color = "dodgerblue4")+
  ylab("Effect")+
  xlab("Year")+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))

ggsave(filename = "Figure9.png", path = "plots")

##############################################
# SCM for upper secondary school             #
##############################################

# Run synth
SCM_USM    <- synth(DATA_USM)
TABLES_USM <- synth.tab(dataprep.res = DATA_USM, synth.res = SCM_USM)

SCM_USP    <- synth(DATA_USP)
TABLES_USP <- synth.tab(dataprep.res = DATA_USP, synth.res = SCM_USP)


# Graphs
USM <- plot_scm(UPPERS_M, TABLES_USM)
USM_SC <- USM[[1]]
USM_GAP <- USM[[2]]

USP <- plot_scm(UPPERS_P, TABLES_USP)
USP_SC <- USP[[1]]
USP_GAP <- USP[[2]]

GRAPH_US <- rbind(USM_SC, USP_SC)
GRAPH_US$subject[GRAPH_US$subject=="math"] <- "Math"
GRAPH_US$subject[GRAPH_US$subject=="port"] <- "Portuguese"
GRAPH_US$grade <- "Upper Secondary School"

# Figure 10

ggplot(data = GRAPH_US, aes(x=year, y= score, color = unit))+
  geom_line(size=0.8)+
  scale_color_manual(values= c("dodgerblue4","brown2"), 
                     labels= c( "Ceará", "Synthetic Ceará"), 
                     name = "")+
  ylab("Score")+
  xlab("Year")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  ylim(210,280)+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  facet_grid(vars(grade), vars(subject))

ggsave(filename = "Figure10.png", path = "plots")

DATA_GAP_US <- rbind(USM_GAP,USP_GAP)

DATA_GAP_US$subject[DATA_GAP_US$subject=="math"] <- "Math"
DATA_GAP_US$subject[DATA_GAP_US$subject=="port"] <- "Portuguese"
DATA_GAP_US$grade <- "Upper Secondary School"

# Figure 11

ggplot(data = DATA_GAP_US, aes(x=year, y=gap))+
  geom_line(size=0.8, color = "dodgerblue4")+
  ylab("Effect")+
  xlab("Year")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))
ggsave(filename = "Figure11.png", path = "plots")

# Table with W vectors (Table 4)

W_PM <- as.data.frame(TABLES_PM$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(PM = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_PP <- as.data.frame(TABLES_PP$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(PP = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_LSM <- as.data.frame(TABLES_LSM$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(LSM = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_LSP <- as.data.frame(TABLES_LSP$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(LSP = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_USM <- as.data.frame(TABLES_USM$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(USM = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_USP <- as.data.frame(TABLES_USP$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(USP = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

TABLE_W <- left_join(abbr_state, W_PM, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_PP, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_LSM, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_LSP, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_USM, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_USP, by="abbreviation")

TABLE_W[is.na(TABLE_W)] <- 0.00
write.csv(TABLE_W, file="TABLE_W.csv")

# Table with V vectors (Table 5)

V_PM <- as.data.frame(TABLES_PM$tab.v) %>% 
  rename(PM = v.weights)
V_PP <- as.data.frame(TABLES_PP$tab.v) %>% 
  rename(PP = v.weights)
V_LSM <- as.data.frame(TABLES_LSM$tab.v) %>% 
  rename(LSM = v.weights)
V_LSP <- as.data.frame(TABLES_LSP$tab.v) %>% 
  rename(LSP = v.weights)
V_USM <- as.data.frame(TABLES_USM$tab.v) %>% 
  rename(USM = v.weights)
V_USP <- as.data.frame(TABLES_USP$tab.v) %>% 
  rename(USP = v.weights)
TABLE_V <- cbind(V_PM, V_PP, V_LSM, V_LSP, V_USM, V_USP)


print(xtable(TABLE_V, include.rownames=TRUE), type="html", file="Table_V.html")

# Table of predictions and comparison (Table 6)

PRED_PM <- as.data.frame(TABLES_PM$tab.pred) %>% 
  rename(Ceará_M = Treated, SyntheticPM = Synthetic, Mean_M = 'Sample Mean')

PRED_PP <- as.data.frame(TABLES_PP$tab.pred) %>% 
  rename(SyntheticPP = Synthetic) %>% 
  select(SyntheticPP)

PRED_LSM <- as.data.frame(TABLES_LSM$tab.pred) %>% 
  rename(SyntheticLSM = Synthetic) %>% 
  select(SyntheticLSM)

PRED_LSP <- as.data.frame(TABLES_LSP$tab.pred) %>% 
  rename(SyntheticLSP = Synthetic) %>% 
  select(SyntheticLSP)

PRED_USM <- as.data.frame(TABLES_USM$tab.pred) %>% 
  rename(SyntheticUSM = Synthetic) %>% 
  select(SyntheticUSM)

PRED_USP <- as.data.frame(TABLES_USP$tab.pred) %>% 
  rename(SyntheticUSP = Synthetic) %>% 
  select(SyntheticUSP)

PRED_PRI <- cbind(PRED_PM, PRED_PP, PRED_LSM, PRED_LSP, PRED_USM, PRED_USP)

print(xtable(PRED_PRI, include.rownames=TRUE), type="html", file="PRED_PRI.html")

##########################
## in-time placebo test ##
##########################

# Figure 12 (atificial intervention in 2003)

# Preparing data with Synth:
DATA_PM_P <- prepare_time_placebo(PRIMARY_M)
DATA_PP_P <- prepare_time_placebo(PRIMARY_P)

DATA_LSM_P <- prepare_time_placebo(LOWERS_M)
DATA_LSP_P <- prepare_time_placebo(LOWERS_P)


# Run synth
SCM_PM_P    <- synth(DATA_PM_P)
TABLES_PM_P <- synth.tab(dataprep.res = DATA_PM_P, synth.res = SCM_PM_P)

SCM_PP_P   <- synth(DATA_PP_P)
TABLES_PP_P <- synth.tab(dataprep.res = DATA_PP_P, synth.res = SCM_PP_P)

SCM_LSM_P    <- synth(DATA_LSM_P)
TABLES_LSM_P <- synth.tab(dataprep.res = DATA_LSM_P, synth.res = SCM_LSM_P)

SCM_LSP_P    <- synth(DATA_LSP_P)
TABLES_LSP_P <- synth.tab(dataprep.res = DATA_LSP_P, synth.res = SCM_LSP_P)

# Graphs in ggplot
PM <- plot_scm(PRIMARY_M, TABLES_PM_P)
PM_SC <- PM[[1]]
PM_GAP <- PM[[2]]

PP <- plot_scm(PRIMARY_P, TABLES_PP_P)
PP_SC <- PP[[1]]
PP_GAP <- PP[[2]]

LSM <- plot_scm(LOWERS_M, TABLES_LSM_P)
LSM_SC <- LSM[[1]]
LSM_GAP <- LSM[[2]]

LSP <- plot_scm(LOWERS_P, TABLES_LSP_P)
LSP_SC <- LSP[[1]]
LSP_GAP <- LSP[[2]]

DATA_GRAPH <- rbind(PM_SC, PP_SC, LSM_SC, LSP_SC)

DATA_GRAPH$grade[DATA_GRAPH$grade=="P"] <- "Primary School"
DATA_GRAPH$grade[DATA_GRAPH$grade=="LS"] <- "Lower Secondary School"
DATA_GRAPH$grade <- factor(DATA_GRAPH$grade, levels = c("Primary School", "Lower Secondary School"))

DATA_GRAPH$subject[DATA_GRAPH$subject=="math"] <- "Math"
DATA_GRAPH$subject[DATA_GRAPH$subject=="port"] <- "Portuguese"

ggplot(data = DATA_GRAPH, aes(x=year, y= score, color = unit))+
  geom_line(size=0.8)+
  scale_color_manual(values= c("dodgerblue4","brown2"), 
                     labels= c( "Ceará", "Synthetic Ceará"), 
                     name = "")+
  ylab("Score")+
  xlab("Year")+
  geom_vline(xintercept = 2003, linetype = "dashed", color = "brown2")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(0.8, "lines"))+
  facet_grid(vars(grade), vars(subject))
ggsave(filename = "Figure12.png", path = "plots")

############################################################
# Average yearly effects for SCM reported in Figure 17     #
############################################################

# Average yearly effect for Mathematics (primary education):

PM_AVG <- PM_GAP %>% 
  filter(year>2007) %>% 
  summarise(mean(gap))

# Average yearly effect for Mathematics (lower secondary education):

LSM_AVG <- LSM_GAP %>% 
  filter(year>2007) %>% 
  summarise(mean(gap))

# Average yearly effect for Portuguese (primary education):

PP_AVG <- PP_GAP %>% 
  filter(year>2007) %>% 
  summarise(mean(gap))

# Average yearly effect for Portuguese (lower secondary education):

LSP_AVG <- LSP_GAP %>% 
  filter(year>2007) %>% 
  summarise(mean(gap))

# Average yearly effect for Mathematics (upper secondary education):

USM_AVG <- USM_GAP %>% 
  filter(year>2010) %>% 
  summarise(mean(gap))

# Average yearly effect for Portuguese (upper secondary education):

USP_AVG <- USP_GAP %>% 
  filter(year>2010) %>% 
  summarise(mean(gap))
