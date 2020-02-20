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

#Miroc
{ 
# Miroc 

miroc_hist <- (read.delim(file = "data/sf/historico/proj_miroc.txt", sep = "")) %>% 
  mutate(rain = rain/12) %>% 
  mutate(Eto = 12*Eto) %>% 
  mutate(u2 = u2*((2/10)^0.2)) %>% 
  mutate(id = "1")



miroc_proj <- (read.delim(file = "data/sf/proj/proj_miroc.txt", sep = "")) %>% 
  mutate(rain = rain/12) %>% 
  mutate(Eto = 12*Eto) %>% 
  mutate(u2 = u2*((2/10)^0.2)) %>% 
  mutate(id = "2")


miroc_total <- rbind(miroc_hist, miroc_proj) %>% 
  na.omit()


### Gráfico :


miroc_total %>% 
  reshape2::melt(c("Year", "id")) %>% 
  ggplot(aes(x = Year, y = value, col = id, group = variable)) +
  geom_pointline() + 
  geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal()

ggsave("miroc.jpeg")

}

# Canems
{ 

canems_hist <- (read.delim(file = "data/sf/historico/proj_canems.txt", 
                           sep = "")) %>% 
    mutate(rain = rain/12) %>% 
    mutate(Eto = 12*Eto) %>% 
    mutate(u2 = u2*((2/10)^0.2)) %>% 
    mutate(id = "1")
  
  
  
canems_proj <- (read.delim(file = "data/sf/proj/proj_canems.txt", sep = "")) %>% 
    mutate(rain = rain/12) %>% 
    mutate(Eto = 12*Eto) %>% 
    mutate(u2 = u2*((2/10)^0.2)) %>% 
    mutate(id = "2")
  
  
canems_total <- rbind(canems_hist, canems_proj) %>% 
    na.omit()
  
  
### Gráfico :
  
  
canems_total %>% 
    reshape2::melt(c("Year", "id")) %>% 
    ggplot(aes(x = Year, y = value, col = id, group = variable)) +
    geom_pointline() + 
    geom_smooth(method = "lm") +
    facet_wrap(~variable, scales = "free_y") +
    theme_minimal()
  
  ggsave("canems.jpeg")
  
}

# Hadgem
{ 
  
  hadgem_hist <- (read.delim(file = "data/sf/historico/proj_hadgem.txt", 
                             sep = "")) %>% 
    mutate(rain = rain/12) %>% 
    mutate(Eto = 12*Eto) %>% 
    mutate(u2 = u2*((2/10)^0.2)) %>% 
    mutate(id = "1")
  
  
  
  hadgem_proj <- (read.delim(file = "data/sf/proj/proj_hadgem.txt", sep = "")) %>% 
    mutate(rain = rain/12) %>% 
    mutate(Eto = 12*Eto) %>% 
    mutate(u2 = u2*((2/10)^0.2)) %>% 
    mutate(id = "2")
  
  
  hadgem_total <- rbind(hadgem_hist, hadgem_proj) %>% 
    na.omit()
  
  
  ### Gráfico :
  
  
  hadgem_total %>% 
    reshape2::melt(c("Year", "id")) %>% 
    ggplot(aes(x = Year, y = value, col = id, group = variable)) +
    geom_pointline() + 
    geom_smooth(method = "lm") +
    facet_wrap(~variable, scales = "free_y") +
    theme_minimal()
  
  ggsave("hadgem.jpeg")
  
}


