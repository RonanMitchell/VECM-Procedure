Decomp <- vars::fevd(IrfVec, n.ahead = 60)

X1 <- Decomp$Interest[,1]
X2 <- Decomp$Interest[,2]
X3 <- Decomp$Inflation[,1]
X4 <- Decomp$Inflation[,2]

VarDecomp1 <- as.data.frame(X1) # Interest
VarDecomp1$X2 <- X2
VarDecomp1$Time <- 1:60

VarDecomp2 <- as.data.frame(X3) # Inflation
VarDecomp2$X4 <- X4
VarDecomp2$Time <- 1:60

###

# ggplots 

plot5 <- ggplot(data = VarDecomp1, 
                aes(x = Time)) +
  geom_line(aes(y = X1), 
            color = "red3") +
  geom_line(aes(y = X2), 
            color = "blue3") +
  ylab("Proportion") +
  labs(title = "Nominal Interest Variance Decomposition",
       subtitle = "Sixty Period Forecast") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(limits = c(0, 60)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


plot6 <- ggplot(data = VarDecomp2, 
                aes(x = Time)) +
  geom_line(aes(y = X3), 
            color = "red3") +
  geom_line(aes(y = X4), 
            color = "blue3") +
  ylab("Proportion") +
  labs(title = "Inflation Variance Decomposition",
       subtitle = "Sixty Period Forecast") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(limits = c(0, 60)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))