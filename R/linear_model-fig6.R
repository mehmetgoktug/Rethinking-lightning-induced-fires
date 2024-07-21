################################################################################
# title: Linear Model - Figure 6
# author: Çağatay Tavşanoğlu
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# read and wrangle data --------------------------------------------------------
# read data
data <- read.csv("./data/output/data_for_model/liw_lightning.csv", stringsAsFactors = TRUE)

# create new variables for lightning and lightning fire count per km2
data$LperA <- data$lightning_count / data$area_km2
data$FperA <- data$lightning_fire_count / data$area_km2

# convert lightning fire data to 1-0
data$Fire <- ifelse(data$lightning_fire_count > 0, 1, 0)

# eliminate 0's for lightning fires
data_FireNZ <- data[data$FperA > 0, ]

# model ------------------------------------------------------------------------
# TURKEY -----------------------------------------------------------------------
# linear model - data=data
# lightning-induced fire number per km2 vs. lightning number per km2
modFN <- lm(FperA ~ LperA, data = data)
range(data$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modFN, list(LperA = xweight), type = "response")
plot(data$LperA, data$FperA, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire number per km2")
lines(xweight, yweight)
summary(modFN)
anova(modFN)

# binomial GLM (1-0 data) - data=data
# lightning-induced fire occurence vs. lightning number per km2
modPA <- glm(Fire ~ LperA, data = data, family = binomial)
range(data$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modPA, list(LperA = xweight), type = "response")
plot(data$LperA, data$Fire, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire occurence")
lines(xweight, yweight)
summary(modPA)
# Explained deviance (%) = (1977.7-1894.6) / 1977.7 = 4.2%

# linear model (only-presence data) - data=data_FireNZ
# lightning-induced fire number per km2 vs. lightning number per km2
modNZ <- lm(FperA ~ LperA, data = data_FireNZ)
range(data_FireNZ$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modNZ, list(LperA = xweight), type = "response")
plot(data_FireNZ$LperA, data_FireNZ$FperA, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire number per km2")
lines(xweight, yweight)
summary(modNZ)
anova(modNZ)

# MUGLA ------------------------------------------------------------------------
data_mugla <- data[data$obm == "mugla", ]
data_FireNZ_mugla <- data_FireNZ[data_FireNZ$obm == "mugla", ]

# binomial GLM (1-0 data) - data=data_mugla
# lightning-induced fire occurence vs. lightning number per km2
modPA <- glm(Fire ~ LperA, data = data_mugla, family = binomial)
range(data_mugla$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modPA, list(LperA = xweight), type = "response")
plot(data_mugla$LperA, data_mugla$Fire, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire occurence")
lines(xweight, yweight)
summary(modPA)

# linear model (only-presence data) - data=data_FireNZ_mugla
# lightning-induced fire number per km2 vs. lightning number per km2
modNZ <- lm(FperA ~ LperA, data = data_FireNZ_mugla)
range(data_FireNZ_mugla$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modNZ, list(LperA = xweight), type = "response")
plot(data_FireNZ_mugla$LperA, data_FireNZ_mugla$FperA, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire number per km2")
lines(xweight, yweight)
text(2.5,0.35, "Muğla")
summary(modNZ)
anova(modNZ)

# IZMIR ------------------------------------------------------------------------
data_izmir <- data[data$obm == "izmir", ]
data_FireNZ_izmir <- data_FireNZ[data_FireNZ$obm == "izmir", ]

# binomial GLM (1-0 data) - data=data_izmir
# lightning-induced fire occurence vs. lightning number per km2
modPA <- glm(Fire ~ LperA, data = data_izmir, family = binomial)
range(data_izmir$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modPA, list(LperA = xweight), type = "response")
plot(data_izmir$LperA, data_izmir$Fire, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire occurence")
lines(xweight, yweight)
summary(modPA)

# linear model (only-presence data) - data=data_FireNZ_izmir
# lightning-induced fire number per km2 vs. lightning number per km2
modNZ <- lm(FperA ~ LperA, data = data_FireNZ_izmir)
range(data_FireNZ_izmir$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modNZ, list(LperA = xweight), type = "response")
plot(data_FireNZ_izmir$LperA, data_FireNZ_izmir$FperA, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire number per km2")
lines(xweight, yweight)
text(6.9,0.03, "İzmir")
summary(modNZ)
anova(modNZ)

# BOLU -------------------------------------------------------------------------
data_bolu <- data[data$obm == "bolu", ]
data_FireNZ_bolu <- data_FireNZ[data_FireNZ$obm == "bolu", ]

# binomial GLM (1-0 data) - data=data_bolu
# lightning-induced fire occurence vs. lightning number per km2
modPA <- glm(Fire ~ LperA, data = data_bolu, family = binomial)
range(data_bolu$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modPA, list(LperA = xweight), type = "response")
plot(data_bolu$LperA, data_bolu$Fire, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire occurence")
lines(xweight, yweight)
text(1.5,0.8, "Bolu")
summary(modPA)
# Explained deviance (%) (97.738-84.061)/97.738 = 0.1399

# linear model (only-presence data) - data=data_FireNZ_bolu
# lightning-induced fire number per km2 vs. lightning number per km2
modNZ <- lm(FperA ~ LperA, data = data_FireNZ_bolu)
range(data_FireNZ_bolu$LperA)
xweight <- seq(0, 10, 0.01)
yweight <- predict(modNZ, list(LperA = xweight), type = "response")
plot(data_FireNZ_bolu$LperA, data_FireNZ_bolu$FperA, pch = 16, xlab = "Lightning number per km2", ylab = "Lightning-induced fire number per km2")
lines(xweight, yweight)
text(6.9,0.03, "Bolu")
summary(modNZ)
anova(modNZ)
