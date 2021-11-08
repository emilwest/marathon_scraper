install.packages("ggthemes") # Install 
library(ggthemes) # Load
library(tidyverse)
df_weekly

ll <- df_daily %>% 
  mutate(`Summa tid` = seconds_to_period(dminutes(tot_duration_min))) %>% 
  filter(comment != "Vila") %>% 
  group_by(Week) %>% 
  mutate(pass = row_number()) %>% 
  ungroup()


df_daily


ggplot() +
  geom_bar(data = df_weekly, aes(y = `Summa tid`, x = Week), stat = "identity") +
  geom_bar(data = ll, aes(y = `Summa tid`, x = Week, fill=factor(pass)), stat = "identity") +
  scale_y_time() +
  theme_wsj()
