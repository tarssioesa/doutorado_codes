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

# load historico

year <- sort(rep(seq(1980,2013), 12))

flow <- read.delim("data/sf/teste2", sep = "", dec = ",") %>% 
  slice(-c(1:228)) %>% 
  slice(-c(407:358)) %>% 
  mutate(year = year) %>% 
  group_by(year) %>% 
  summarise(rain = sum(rain)) %>% 
  filter(year < 2011)

Eto <- read.delim("data/sf/Eto.txt", sep = ";") %>% 
  mutate(year = year) %>% 
  group_by(year) %>% 
  summarise(Eto = sum(Eto)) %>% 
  filter(year < 2011)

temp_max <- read.delim("data/sf/temp_max.txt", sep = ";")

temp_min <- read.delim("data/sf/temp_min.txt", sep = ";")

temp_media <- data.frame(temp = (temp_max$temp_max + temp_min$temp_max)/2) %>% 
  mutate(year = year) %>% 
  group_by(year) %>% 
  summarise(temp = mean(temp)) %>% 
  filter(year < 2011)

mean(temp_media$temp)

u2 <- read.delim("data/sf/u2.txt", sep = ";") %>% 
  mutate(year = year) %>% 
  group_by(year) %>% 
  summarise(u2 = mean(u2)) %>% 
  filter(year < 2011)

mean(u2$u2)

# load models: 

load("data/sf/proj/sf_model_t1.RData")

load("data/sf/proj/sf_model_t2.RData")


# carregando miroc: 


miroc <- (read.delim(file = "data/sf/proj/proj_miroc.txt", sep = "")) %>% 
  dplyr::select(-Year) %>% 
  mutate(rain = rain/12) %>% 
  mutate(Eto = 12*Eto) %>% 
  mutate(u2 = u2*((2/10)^0.2))

miroc_flow <- predict(model, miroc) %>% 
  as.data.frame() %>% 
  mutate(year = seq(2006,2099))

miroc_flow %>% 
  filter(year > 2070) %>% 
  summarise(mean(.))

miroc_flow2 <- 10^predict(model2, log10(miroc)) %>% 
  as.data.frame() %>% 
  mutate(year = seq(2006, 2099))

miroc_flow2 %>% 
  filter(year > 2070) %>% 
  summarise(mean(.))

# carregando hadgem

hadgem <- (read.delim(file = "data/sf/proj/proj_hadgem.txt", sep = "")) %>% 
  dplyr::select(-Year) %>% 
  mutate(rain = rain/12) %>% 
  mutate(Eto = 12*Eto) %>% 
  mutate(u2 = u2*((2/10)^0.2))

hadgem_flow <- predict(model, hadgem) %>% 
  as.data.frame() %>% 
  mutate(year = seq(2006,2099))

hadgem_flow %>% 
  filter(year > 2070) %>% 
  summarise(mean(.))

hadgem_flow2 <- 10^(predict(model2, log10(hadgem))) %>% 
  as.data.frame() %>% 
  mutate(year = seq(2006, 2099))


hadgem_flow2 %>% 
  filter(year > 2070) %>% 
  summarise(mean(.))


# CANEMS: 

canems <- (read.delim(file = "data/sf/proj/proj_canems.txt", sep = "")) %>% 
  dplyr::select(-Year) %>% 
  mutate(rain = rain/12) %>% 
  mutate(Eto = 12*Eto) %>% 
  mutate(u2 = u2*((2/10)^0.2))

canems_flow <- predict(model, canems) %>% 
  as.data.frame() %>% 
  mutate(year = seq(2006,2099))

canems_flow %>% 
  filter(year > 2020 & year < 2051) %>%
  summarise(mean(.))

canems_flow2 <- 10^predict(model2, log(canems,10)) %>% 
  as.data.frame() %>% 
  mutate(year = seq(2006, 2099))

canems_flow2 %>% 
  filter(year > 2020 & year < 2051) %>%
  summarise(mean(.))


#### Médias

canems %>% 
  mutate(year = seq(2005, 2099)) %>% 
  na.omit() %>% 
  filter(year > 2020 & year < 2051) %>%
  summarise_all(funs(mean))

hadgem %>% 
  mutate(year = seq(2005, 2099)) %>% 
  na.omit() %>% 
  filter(year > 2070) %>%
  summarise_all(funs(mean))

miroc %>% 
  mutate(year = seq(2005, 2099)) %>% 
  na.omit() %>% 
  filter(year > 2020 & year < 2051) %>%
  summarise_all(funs(mean))

