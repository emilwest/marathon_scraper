library(tidyverse)

load("Data/list_cleaned.RData")
list_cleaned

# Remove km from dist and min from tot_duration_min : 
list_cleaned2 <- list_cleaned %>% 
  map( ~ .x %>% mutate(dist = str_remove(dist, " km"),
                       dist = str_replace(dist, ",", "."),
                       dist = as.double(dist),
                       tot_duration_min = as.double(str_remove(tot_duration_min, " min")),
                       datum = parse_date_time(datum, "%d-%m-%Y") %>% ymd()
  ))


list_cleaned2

# Adds a short daily summary per week like "dec 6 - dec 12"  "dec 13 - dec 19"
# It's the same for all programs so we can do it once:
dayshort_summary <- list_cleaned2$`300` %>%
  group_by(Week) %>% 
  mutate(id = 1:n()) %>% 
  filter(id ==1 | id == 7) %>% 
  mutate(daypart = str_glue("{month} {day}")) %>% 
  mutate(dayshort = str_c(daypart, collapse = " - ")) %>% 
  pull(dayshort) %>% 
  unique()


# Calculate Weekly stats:
weekly_tot_list <- list_cleaned2 %>% 
  map(
    ~ .x %>%
      group_by(Week) %>% 
      summarise(`Summa km` = sum(dist, na.rm=T),
                `Summa tid` = sum(tot_duration_min, na.rm=T)
                ) %>% 
      mutate(`Summa tid` = seconds_to_period(dminutes(`Summa tid`)),
             `% Diff` = round(((`Summa km`/lag(`Summa km`))-1)*100,2),
             `Period` = dayshort_summary)
  )

# Wide format, one week per row and workouts on the columns:
df_weekly_list <- list_cleaned2 %>% 
  map(
    ~ .x %>%
      filter(comment != "Vila") %>% 
      group_by(Week) %>% 
      mutate(pass = row_number()) %>% 
      select(-comment, -datum,-month, -day, -wday, -tot_duration_min) %>% 
      pivot_wider(names_from = "pass", values_from = dist, names_prefix = "Pass ") %>% 
      ungroup()
  )

# Join with weekly stats:
df_weekly_list <- map2(df_weekly_list, weekly_tot_list, left_join, by = "Week")

# Put comments in same dimension as df_weekly (26 x number_of_workouts+1)
# This is used when we export to excel later.
df_list_of_weekly_comments <- list_cleaned2 %>% 
  map(
    ~ .x %>% 
      filter(comment != "Vila") %>% 
      group_by(Week) %>% 
      mutate(pass = row_number()) %>% 
      select(-datum,-month, -day, -wday, -tot_duration_min,-dist) %>% 
      pivot_wider(names_from = "pass", values_from = comment) %>% 
      ungroup()
  )


save(df_list_of_weekly_comments, file = "Data/df_list_of_weekly_comments.RData") # Comments to be added over wide format on excel
save(df_weekly_list, file = "Data/df_weekly_list.RData") # Weekly observations, wide format
save(list_cleaned2, file = "Data/list_cleaned2.RData") # Daily observations, long format
