#-------------------------------------------------------------------#
# Script 04B - Synthetic Control - Leave-one-out test               #
#-------------------------------------------------------------------#

library(tidyverse)
library(Synth)

load("data/DATA_COMPLETE.RData")

# Subsetting data:

PRIMARY_M <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "math"))
PRIMARY_P <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "port"))
LOWERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "math"))
LOWERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "port"))


# Primary Education - Mathematics
  
states <- c(11:17, 21:22, 24:29, 31:33, 35, 41:43, 50:53)
leave_out <- c(22, 23, 26, 29)
predictors <- c("homicides", "TWh", "unemployment", "ln_pop", "edu_invest_pc")
  
results <- list()
results_synth <- list()
one_out <- list()
  
for (i in leave_out) {
  if (i != 23) {
    states_one_out <- states[!states %in% i]
  } else {
    states_one_out <- states
  }
  
  dataprep.out <- dataprep(foo = PRIMARY_M,
                             predictors = predictors,
                             dependent     = "score",
                             unit.variable = "code_state",
                             time.variable = "year",
                             unit.names.variable = "abbr_state",
                             treatment.identifier  = 23,
                             controls.identifier   = states_one_out,
                             time.predictors.prior = seq(1995, 2007, 2),
                             time.optimize.ssr     = seq(1995, 2007, 2),
                             time.plot             = seq(1995, 2019, 2))
    
  results[[as.character(i)]] <- dataprep.out
  results_synth[[as.character(i)]] <- synth(results[[as.character(i)]])
  if (i==22) {
    one_out[["Y1"]] <- results[[as.character(i)]]$Y1plot}
  one_out[[paste(as.character(i), "Y0")]] <- results[[as.character(i)]]$Y0plot %*% results_synth[[as.character(i)]]$solution.w}
        
one_out_plot <- as.data.frame(one_out)
one_out_plot$year <- as.numeric(rownames(one_out_plot))
one_out_plot_PM <- rename(one_out_plot, Ceara = X23, Y022 = w.weight, SyntheticCeara = w.weight.1, Y026 = w.weight.2, Y029 = w.weight.3)
one_out_plot_PM <- pivot_longer(one_out_plot_PM, !year,  names_to = "unit", values_to = "score")
one_out_plot_PM$color <- "LOU"
one_out_plot_PM$color[one_out_plot_PM$unit == "Ceara"] <- "Ceara"
one_out_plot_PM$color[one_out_plot_PM$unit == "SyntheticCeara"] <- "SyntheticCeara"
one_out_plot_PM$subject <- "Math"
one_out_plot_PM$grade<- "Primary School"

# Primary Education - Portuguese

states <- c(11:17, 21:22, 24:29, 31:33, 35, 41:43, 50:53)
leave_out <- c(22, 23, 26, 29)
predictors <- c("homicides", "TWh", "unemployment", "ln_pop", "edu_invest_pc")

results <- list()
results_synth <- list()
one_out <- list()

for (i in leave_out) {
  if (i != 23) {
    states_one_out <- states[!states %in% i]
  } else {
    states_one_out <- states
  }
  
  dataprep.out <- dataprep(foo = PRIMARY_P,
                           predictors = predictors,
                           dependent     = "score",
                           unit.variable = "code_state",
                           time.variable = "year",
                           unit.names.variable = "abbr_state",
                           treatment.identifier  = 23,
                           controls.identifier   = states_one_out,
                           time.predictors.prior = seq(1995, 2007, 2),
                           time.optimize.ssr     = seq(1995, 2007, 2),
                           time.plot             = seq(1995, 2019, 2))
  
  results[[as.character(i)]] <- dataprep.out
  results_synth[[as.character(i)]] <- synth(results[[as.character(i)]])
  if (i==22) {
    one_out[["Y1"]] <- results[[as.character(i)]]$Y1plot}
  one_out[[paste(as.character(i), "Y0")]] <- results[[as.character(i)]]$Y0plot %*% results_synth[[as.character(i)]]$solution.w}

one_out_plot <- as.data.frame(one_out)
one_out_plot$year <- as.numeric(rownames(one_out_plot))
one_out_plot_PP <- rename(one_out_plot, Ceara = X23, Y022 = w.weight, SyntheticCeara = w.weight.1, Y026 = w.weight.2, Y029 = w.weight.3)
one_out_plot_PP <- pivot_longer(one_out_plot_PP, !year,  names_to = "unit", values_to = "score")
one_out_plot_PP$color <- "LOU"
one_out_plot_PP$color[one_out_plot_PP$unit == "Ceara"] <- "Ceara"
one_out_plot_PP$color[one_out_plot_PP$unit == "SyntheticCeara"] <- "SyntheticCeara"
one_out_plot_PP$subject <- "Portuguese"
one_out_plot_PP$grade<- "Primary School"

# Lower Secondary Education - Math

states <- c(11:17, 21:22, 24:29, 31:33, 35, 41:43, 50:53)
leave_out <- c(22, 23, 26, 29)
predictors <- c("homicides", "TWh", "unemployment", "ln_pop", "edu_invest_pc")

results <- list()
results_synth <- list()
one_out <- list()

for (i in leave_out) {
  if (i != 23) {
    states_one_out <- states[!states %in% i]
  } else {
    states_one_out <- states
  }
  
  dataprep.out <- dataprep(foo = LOWERS_M,
                           predictors = predictors,
                           dependent     = "score",
                           unit.variable = "code_state",
                           time.variable = "year",
                           unit.names.variable = "abbr_state",
                           treatment.identifier  = 23,
                           controls.identifier   = states_one_out,
                           time.predictors.prior = seq(1995, 2007, 2),
                           time.optimize.ssr     = seq(1995, 2007, 2),
                           time.plot             = seq(1995, 2019, 2))
  
  results[[as.character(i)]] <- dataprep.out
  results_synth[[as.character(i)]] <- synth(results[[as.character(i)]])
  if (i==22) {
    one_out[["Y1"]] <- results[[as.character(i)]]$Y1plot}
  one_out[[paste(as.character(i), "Y0")]] <- results[[as.character(i)]]$Y0plot %*% results_synth[[as.character(i)]]$solution.w}

one_out_plot <- as.data.frame(one_out)
one_out_plot$year <- as.numeric(rownames(one_out_plot))
one_out_plot_LSM <- rename(one_out_plot, Ceara = X23, Y022 = w.weight, SyntheticCeara = w.weight.1, Y026 = w.weight.2, Y029 = w.weight.3)
one_out_plot_LSM <- pivot_longer(one_out_plot_LSM, !year,  names_to = "unit", values_to = "score")
one_out_plot_LSM$color <- "LOU"
one_out_plot_LSM$color[one_out_plot_LSM$unit == "Ceara"] <- "Ceara"
one_out_plot_LSM$color[one_out_plot_LSM$unit == "SyntheticCeara"] <- "SyntheticCeara"
one_out_plot_LSM$subject <- "Math"
one_out_plot_LSM$grade<- "Lower Secondary School"

# Lower Secondary Education - Portuguese (includes Rio Grande do Sul)

states <- c(11:17, 21:22, 24:29, 31:33, 35, 41:43, 50:53)
leave_out <- c(22, 23, 26, 29, 43)
predictors <- c("homicides", "TWh", "unemployment", "ln_pop", "edu_invest_pc")

results <- list()
results_synth <- list()
one_out <- list()

for (i in leave_out) {
  if (i != 23) {
    states_one_out <- states[!states %in% i]
  } else {
    states_one_out <- states
  }
  
  dataprep.out <- dataprep(foo = LOWERS_P,
                           predictors = predictors,
                           dependent     = "score",
                           unit.variable = "code_state",
                           time.variable = "year",
                           unit.names.variable = "abbr_state",
                           treatment.identifier  = 23,
                           controls.identifier   = states_one_out,
                           time.predictors.prior = seq(1995, 2007, 2),
                           time.optimize.ssr     = seq(1995, 2007, 2),
                           time.plot             = seq(1995, 2019, 2))
  
  results[[as.character(i)]] <- dataprep.out
  results_synth[[as.character(i)]] <- synth(results[[as.character(i)]])
  if (i==22) {
    one_out[["Y1"]] <- results[[as.character(i)]]$Y1plot}
  one_out[[paste(as.character(i), "Y0")]] <- results[[as.character(i)]]$Y0plot %*% results_synth[[as.character(i)]]$solution.w}

one_out_plot <- as.data.frame(one_out)
one_out_plot$year <- as.numeric(rownames(one_out_plot))
one_out_plot_LSP <- rename(one_out_plot, 
                           Ceara = X23, 
                           Y022 = w.weight, 
                           SyntheticCeara = w.weight.1, 
                           Y026 = w.weight.2, 
                           Y029 = w.weight.3,
                           Y043 = w.weight.4)
one_out_plot_LSP <- pivot_longer(one_out_plot_LSP, !year,  names_to = "unit", values_to = "score")
one_out_plot_LSP$color <- "LOU"
one_out_plot_LSP$color[one_out_plot_LSP$unit == "Ceara"] <- "Ceara"
one_out_plot_LSP$color[one_out_plot_LSP$unit == "SyntheticCeara"] <- "SyntheticCeara"
one_out_plot_LSP$subject <- "Portuguese"
one_out_plot_LSP$grade <- "Lower Secondary School"

PLOT_DATA <- rbind(one_out_plot_PM, one_out_plot_PP, one_out_plot_LSM, one_out_plot_LSP)

PLOT_DATA$color <- factor(PLOT_DATA$color, levels = c("LOU", "SyntheticCeara", "Ceara"))
PLOT_DATA$unit <- factor(PLOT_DATA$unit, levels = c("Y022","Y026","Y029","Y043", "SyntheticCeara", "Ceara"))

# Figure 13 - Leave-one-out Test:

ggplot(data = PLOT_DATA, aes(x = year, y = score, color = color, group = unit))+
  geom_line(size=0.6)+
  scale_color_manual(values= c("darkgrey","brown2","dodgerblue4"), 
                     labels= c( "Synthetic Ceará (leave-one-out)", "Synthetic Ceará", "Ceará"), 
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
ggsave(filename = "Figure13.png", path = "plots")


