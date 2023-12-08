
# ggplots 

# Permanent Interest response to inflation shock. 

GgIrf1 <- as.data.frame(PermIrf1$irf$Inflation)
GgIrf1$Time <- 1:nrow(GgIrf1)
#GgIrf1$Upper <- PermIrf1$Upper$Inflation[,1]
#GgIrf1$Lower <- PermIrf1$Lower$Inflation[,1]

plot1 <- ggplot(data = GgIrf1, 
                aes(x = Time)) +
  geom_hline(yintercept = 0, color = "blue") +
  #geom_line(aes(y = Upper), 
  # color = "grey") +
  #geom_line(aes(y = Lower), 
  #color = "grey") +
  #geom_ribbon(aes(ymin = Lower, 
  #ymax = Upper), 
  #fill = "grey", 
  # alpha = 0.5) +
  geom_line(aes(y = Interest), color = "black") +
  labs(title = "Permanent Inflation Shock",
       subtitle = "Effect on Nominal Interest") +
  scale_y_continuous(limits = c(-0.5, 2)) +
  scale_x_discrete(limits = c(0, 40)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Permanent Inflation Response to Inflation Shock 

GgIrf2 <- as.data.frame(PermIrf2$irf$Inflation)
GgIrf2$Time <- 1:nrow(GgIrf2)
#GgIrf2$Upper <- PermIrf2$Upper$Inflation[,1]
#GgIrf2$Lower <- PermIrf2$Lower$Inflation[,1]

plot2 <- ggplot(data = GgIrf2, 
                aes(x = Time)) +
  geom_line(aes(y = Inflation), color = "black") +
  #geom_line(aes(y = Upper), 
  #color = "grey") +
  #geom_line(aes(y = Lower), 
  #color = "grey") +
  #geom_ribbon(aes(ymin = Lower, 
  #ymax = Upper), 
  # fill = "grey", 
  # alpha = 0.5) +
  labs(title = "Permanent Inflation Shock",
       subtitle = "Effect on Inflation") +
  scale_y_continuous(limits = c(0.5, 1.5)) +
  scale_x_discrete(limits = c(0, 40)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

## Temporary Shocks 
# Interest shock, response inflation

GgIrf3 <- as.data.frame(TempIRF1$irf$Interest)
GgIrf3$Time <- 1:nrow(GgIrf3)
#GgIrf3$Upper <- TempIRF1$Upper$Interest[,1]
#GgIrf3$Lower <- TempIRF1$Lower$Interest[,1]

plot3 <- ggplot(data = GgIrf3, 
                aes(x = Time)) +
  geom_hline(yintercept = 0, color = "blue") +
  #geom_line(aes(y = Upper), 
  #color = "grey") +
  #geom_line(aes(y = Lower), 
  #color = "grey") +
  #geom_ribbon(aes(ymin = Lower, 
  #ymax = Upper), 
  # fill = "grey", 
  # alpha = 0.5) +
  geom_line(aes(y = Inflation), color = "black") +
  labs(title = "Temporary Nominal Interest Shock",
       subtitle = "Effect on Inflation") +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_discrete(limits = c(0, 40)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

### Temporary Interest Shock to Interest

GgIrf4 <- as.data.frame(TempIRF2$irf$Interest)
GgIrf4$Time <- 1:nrow(GgIrf4)
#GgIrf4$Upper <- TempIRF2$Upper$Interest[,1]
#GgIrf4$Lower <- TempIRF2$Lower$Interest[,1]

plot4 <- ggplot(data = GgIrf4, 
                aes(x = Time)) +
  geom_hline(yintercept = 0, color = "blue") +
  #geom_line(aes(y = Upper), 
  #color = "grey") +
  #geom_line(aes(y = Lower), 
  #color = "grey") +
  #geom_ribbon(aes(ymin = Lower, 
  # ymax = Upper), 
  # fill = "grey", 
  #alpha = 0.5) +
  geom_line(aes(y = Interest), color = "black") +
  labs(title = "Temporary Nominal Interest Shock",
       subtitle = "Effect on Nominal Interest") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(limits = c(0, 40)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

###

