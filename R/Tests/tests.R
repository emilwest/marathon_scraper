library(rvest)
library(tidyverse)

urls <- str_glue("https://trana.marathon.se/asics-stockholm-marathon-2022/415-programmet/{1:26}")
urls

urls[1]
resp <- rvest::read_html(urls[1])
resp <- rvest::read_html(urls[1])

resps <- map(urls, read_html)

resp %>% 
  html_elements("div") 

week <- resp %>% 
  html_nodes(".view-display-id-training_week") %>% 
  html_children()


id <- week %>% html_attr("id")
# 
# contents <- week %>%
#   html_nodes(".exercise-content")
# 
# 
# ex_moments <- contents %>%
#   map(html_nodes, ".field-exercise-moment")
# 
# ex_moments %>% map(html_text2)





week_list <- week %>% 
  html_text2() %>% 
  map(str_split,"\\n") %>% 
  flatten() %>% 
  set_names(id) 

comments <- week %>%
  html_nodes(".exercise-content") %>%
  html_text2() 

df <- tibble(datum = id,
             month = map_chr(week_list, 1),
             day = map_chr(week_list, 2),
             wday = map_chr(week_list, 3),
             dist_tot = map_chr(week_list, 4),
             tot_dur = map_chr(week_list, 5),
             comment = comments
)

df
