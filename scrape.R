library(rvest)
library(tidyverse)

week_nr <- 1:26
urls <- str_glue("https://trana.marathon.se/asics-stockholm-marathon-2022/415-programmet/{week_nr}")
urls

resps <- map(urls, read_html)


get_weeks <- function(r) {
  r %>% 
    html_nodes(".view-display-id-training_week") %>% 
    html_children()
}

weeks <- map(resps, get_weeks)

get_weeks_as_list <- function(w) {
  ids <- w %>% 
    map(html_attr, "id")
  w %>% 
    html_text2() %>% 
    map(str_split,"\\n") %>% 
    flatten() %>% 
    set_names(ids)
}

comments <- weeks %>% 
  map(html_nodes, ".exercise-content") %>% 
  map(html_text2)

week_list <- map(weeks, get_weeks_as_list)


# https://stackoverflow.com/questions/15201305/how-to-convert-a-list-consisting-of-vector-of-different-lengths-to-a-usable-data/49301947
df <- map_df(week_list, ~ plyr::ldply(.x, rbind) )
df <- df %>% as_tibble()

df_simple <- df %>% 
  select(1:6)

comments_vec <- comments %>% flatten() %>% plyr::ldply() %>% pull
# alternativt:
#as.data.frame(do.call(rbind, comments%>% flatten() ))   

df_simple <- df_simple %>% 
  mutate(comment = comments_vec)
colnames(df_simple) <- c("datum", "month", "day", "wday", "dist", "tot_duration", "comment")
df_simple

df_simple$week <- rep(1:26, each = 7)
df_simple <- df_simple %>% select(week, everything())


df_simplest <- df_simple %>% 
  filter(comment != "Vila") %>% 
  group_by(week) %>% 
  mutate(pass = row_number()) %>% 
  select(-comment, -datum,-month, -day, -wday, -tot_duration) %>% 
  pivot_wider(names_from = "pass", values_from = dist, names_prefix = "Pass ") %>% 
  ungroup()

df_simplest

df_simple %>% 
  filter(comment != "Vila") %>% 
  group_by(week) %>% 
  tally

# put comments in same dimension as df_simplest (26 x 4)
df_simplest_comments <- df_simple %>% 
  filter(comment != "Vila") %>% 
  group_by(week) %>% 
  mutate(pass = row_number()) %>% 
  select(-datum,-month, -day, -wday, -tot_duration,-dist) %>% 
  pivot_wider(names_from = "pass", values_from = comment)
  

c1 <- map_if(df_simplest_comments$`1`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
c2 <- map_if(df_simplest_comments$`2`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
c3 <- map_if(df_simplest_comments$`3`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
c4 <- map_if(df_simplest_comments$`4`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)

c1[[1]]
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "4_15")
wb
writeData(wb, sheet = "4_15", df_simplest)
map(df_simplest$week, function(x) writeComment(wb, sheet = "4_15", col = 2, row = x+1, comment = c1[[x]]))
map(df_simplest$week, function(x) writeComment(wb, sheet = "4_15", col = 3, row = x+1, comment = c2[[x]]))
map(df_simplest$week, function(x) writeComment(wb, sheet = "4_15", col = 4, row = x+1, comment = c3[[x]]))
map(df_simplest$week, function(x) writeComment(wb, sheet = "4_15", col = 5, row = x+1, comment = c4[[x]]))

wb
getwd()
saveWorkbook(wb, file = "test1.xlsx",overwrite = T)
