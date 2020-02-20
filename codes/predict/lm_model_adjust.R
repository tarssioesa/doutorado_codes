# Library

require(pracma)
require(tidyverse)
library(modelr)
library(rmatio)
library(MASS)
library(pracma)
library(quantmod)
library(zoo)
library(lemon)


# trecos

opt <- theme_bw()+
  theme(axis.title = element_text(face = "bold", color = "black", size = 10),
        axis.text.x = element_text(face = "plain", color = "black", 
                                   size = 10, angle = 90),
        axis.text.y = element_text(face = "plain", color = "black", size = 10),
        legend.text=element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"))

# Load Data

t <- 12
t1 <- 12
t2 <- 12
t3 <- 12

eto <- read.delim("data/sf/Eto.txt", sep = ";") %>% 
  mutate(rain = movavg(Eto, t)) %>% 
  slice(-c(1:t)) %>% 
  slice(1:300)

sf <- read.delim2("data/sf/teste2") %>% 
  mutate(rain = movavg(rain, t1)) %>% 
  mutate(flow = movavg(flow, t2)) %>% 
  slice(-c(1:228)) %>% 
  slice(-c(407:358)) %>% 
  slice(-c(1:t)) %>% 
  slice(1:nrow(eto))

u2 <- read.delim("data/sf/u2.txt", sep = ";") %>% 
  mutate(rain = movavg(u2, t)) %>% 
  slice(-c(1:t)) %>% 
  slice(1:nrow(eto))


temp_max <- read.delim("data/sf/temp_max.txt", sep = ";")

temp_min <- read.delim("data/sf/temp_min.txt", sep = ";")

temp_media <- movavg((temp_max$temp_max + temp_min$temp_max)/2, t3) %>% 
  as.tibble() %>% 
  slice(-(1:t3)) %>% 
  slice(1:nrow(eto))


# Ajustando X

sf_ <- cbind(sf, eto$Eto, u2$u2) %>% 
  dplyr::select(-flow)

colnames(sf_) <- c("date",  "rain","Eto", "u2")

sf_ <- sf_ %>% 
  slice(-nrow(sf_)) %>% 
  dplyr::select(-date)

# Ajustando Y

flow <- cbind(sf$flow, temp_media$value) %>% 
  as.tibble() %>% 
  slice(-1)

sf_ <- cbind(sf_, flow)

colnames(sf_) <- c("rain","Eto", "u2", "flow", "temp")


mean(sf_$flow)


# Regression: 

library(caret)
library(pls)

set.seed(123)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 180,
                              horizon = 60,
                              fixedWindow = FALSE,
                              allowParallel = TRUE)

train.data <- log10(sf_)

model3 <- train(
  flow ~ (rain * temp * Eto * u2), 
  data = train.data, 
  method = "lm",
  trControl = myTimeControl,
  tuneLength = 10)


x <- model3$finalModel$coefficients

coef(model3$finalModel, model3$bestTune$lambda)

save(model3, file = "sf_model_t2.RData")

a <- sf_$flow

b <- 10^predict(model3, log10(sf_))

# predict

mean(a)
mean(b)

Metrics::rmse(a,b)

# erro1 <- Metrics::rmse(a,b)
cor(a,b)

# plot : 

sf_$pred <- b

# Ajustar Date

sf_$date <- seq(as.Date("1981-01-01"), as.Date("2013-11-01"), by = "1 month")

sf_ %>% 
  dplyr::select(date, flow, pred) %>% 
  reshape2::melt("date") %>% 
  ggplot(aes(x = date, y = value, col = variable, group = 1)) +
  geom_pointline() +
  ylab("flow") +  
  opt


sf_estrela <- sf_[c("flow", "rain", "Eto", "u2", "temp")]

cor(sf_estrela)

varImp(model, scale = FALSE)
