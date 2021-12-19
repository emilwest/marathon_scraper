load("Data/df_weekly_list.RData")
load("Data/list_cleaned2.RData")

  
# Weekly
df <- df_weekly_list %>% 
  map_df(
    ~ .x %>% 
      mutate(`Summa tid` = as.duration(`Summa tid`)) %>% 
      summarise(`Genomsnitt tid per vecka` = seconds_to_period(round(mean(`Summa tid`),0)),
                `Genomsnitt km per vecka`  = mean(`Summa km`),
                `Veckan med högst volym` = str_glue("{max(`Summa km`, na.rm=T)} km (v {.x[which(.x$`Summa km` == max(`Summa km`,na.rm = T)),1]})"),
                `Störst % ökning i volym mot föregående vecka` = str_glue("{max(`% Diff`,na.rm = T)} (v {.x[which(.x$`% Diff` == max(`% Diff`,na.rm = T)),1]})"),
                `Störst % minskning i volym mot föregående vecka` = str_glue("{min(`% Diff`,na.rm = T)} (v {.x[which(.x$`% Diff` == min(`% Diff`,na.rm = T)),1]})")
              
                ),
    .id = "Program"
  )

# Make program a little nicer. 300 -> 3:00, etc.
df <- df %>% 
  mutate(Program = str_glue("{str_sub(Program,1,1)}:{str_sub(Program,2,3)}"))

# veckan med högst volym
# veckan med lägst volym

# veckan med högst % diff

df_weekly_list$`600` %>% view
df_weekly_list$`330` %>% view
df_weekly_list$`345` %>% view

df %>% 
  ggplot(aes(x = Program, y = `Genomsnitt km per vecka`)) + 
  geom_bar(stat = "identity")

df %>% 
  separate(`Veckan med högst volym`, sep = " km ", into = c("value","v")) %>% 
  mutate(value = as.double(value),
         v = as.double(str_extract(v, "[0-9]+"))) %>% 
  ggplot(aes(x = Program, y = value, color = factor(v))) + 
  geom_point(aes(size = value))
  
# 
# df %>% 
#   ggplot(aes(x = Program, y = `Genomsnitt km per vecka`)) + 
#   geom_point(aes(size = period_to_seconds(`Genomsnitt tid per vecka`)))
# 
# df %>% 
#   ggplot(aes(x = Program, y = as.duration(`Genomsnitt tid per vecka`))) + 
#   geom_point(aes(size = `Genomsnitt km per vecka`)) 

