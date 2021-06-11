#-------------------------------------------------------------------#
# Script 05 - Results (Coefficient Plots)                           #
#-------------------------------------------------------------------#

library(ggplot2)


Variable <- c("Primary - Math",
              "Primary - Portuguese",
              "Lower Secondary - Math",
              "Lower Secondary - Portuguese",
              "Upper Secondary - Math",
              "Upper Secondary - Portuguese",
              "Primary - Math",
              "Primary - Portuguese",
              "Lower Secondary - Math",
              "Lower Secondary - Portuguese",
              "Upper Secondary - Math",
              "Upper Secondary - Portuguese")

Method <- c("Fixed-effects",
            "Fixed-effects",
            "Fixed-effects",
            "Fixed-effects",
            "Fixed-effects",
            "Fixed-effects",
            "Synthetic Control",
            "Synthetic Control",
            "Synthetic Control",
            "Synthetic Control",
            "Synthetic Control",
            "Synthetic Control")
Coefficient <- c(18.235, 17.129, 13.331, 14.429, 3.138, 3.049, 14.39, 16.02, 8.28, 10.51, 2.92, -0.55)
SE <- c(3.194, 2.983, 2.543, 2.789, 2.034, 2.579, NA, NA, NA, NA, NA, NA)

DATA <- data.frame(Variable, Method, Coefficient, SE)

DATA <- mutate(DATA, Conf_Low = Coefficient -1.96*SE, Conf_High = Coefficient +1.96*SE )

DATA$Variable <- factor(DATA$Variable, levels = c("Upper Secondary - Math",
                                                  "Upper Secondary - Portuguese",
                                                  "Lower Secondary - Math",
                                                  "Lower Secondary - Portuguese",
                                                  "Primary - Math",
                                                  "Primary - Portuguese"))

# Figure 17:

ggplot(DATA, aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 1) +
  geom_linerange(aes(x = Variable, 
                     ymin = Conf_Low,
                     ymax = Conf_High),
                 color = "brown2",
                 lwd = 1) +
  geom_point(aes(x = Variable, 
                 y = Coefficient, shape = Method), size = 2.5, color = "dodgerblue4")+
  ylab("Effect")+
  xlab("Outcome of interest")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  theme(legend.position="bottom")+
  coord_flip()

