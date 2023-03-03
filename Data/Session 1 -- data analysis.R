library(tidyverse) 

df <- read_csv("Data/Session_1_BDM_in_action_data.csv")

df %>% 
  filter(period == 1) %>% 
  group_by(control, product) %>% 
  summarise(mean_bid = mean(bid))

df %>% 
  filter(period == 1) %>% 
  filter(product == "wine") %>% 
  group_modify(~t.test(.$bid~.$control)) %>% 
  broom::tidy()


df %>% 
  filter(period <= 4) %>% 
  group_by(product, control, period) %>% 
  summarise(m_bid = mean(bid), sd_bid = sd(bid)) %>% 
  pivot_wider(names_from = period, values_from = c("m_bid", "sd_bid"))


df %>% 
  filter(period == 4 | period == 5) %>% 
  select(subject, product, control, shock, bid) %>% 
  pivot_wider(names_from = shock, values_from = bid) %>% 
  mutate(impact = `TRUE` - `FALSE`) %>% 
  group_by(product, control) %>% 
  summarise(mean_impact = mean(impact))


df %>% 
  filter(period == 4 | period == 5) %>% 
  select(subject, product, control, shock, bid) %>% 
  pivot_wider(names_from = shock, values_from = bid) %>% 
  mutate(impact = `TRUE` - `FALSE`) %>% 
  filter(product == "chocolate") %>% 
  group_modify(~broom::tidy(t.test(.$impact~.$control)))


df %>% 
  filter(period > 4) %>% 
  group_by(product, control, period) %>% 
  summarise(m_bid = mean(bid), sd_bid = sd(bid)) %>% 
  pivot_wider(names_from = period, values_from = c("m_bid", "sd_bid"))
