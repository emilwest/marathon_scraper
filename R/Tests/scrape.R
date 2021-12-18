library(rvest)
library(tidyverse)
library(openxlsx)
library(lubridate)

program <- c("300", "315", "330", "345", "400", "415", "430", "500", "600")
program <- c("400")
week_nr <- 1:26


urls <- str_glue("https://trana.marathon.se/asics-stockholm-marathon-2022/{program}-programmet/{week_nr}")
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

df_daily <- df %>% 
  select(1:6)


7*26

comments_vec <- comments %>% flatten() %>% plyr::ldply() %>% pull
# alternativt:
#as.data.frame(do.call(rbind, comments%>% flatten() ))   

df_daily <- df_daily %>% 
  mutate(comment = comments_vec)
colnames(df_daily) <- c("datum", "month", "day", "wday", "dist", "tot_duration_min", "comment")

df_daily$Week <- rep(1:26, each = 7)
df_daily <- df_daily %>% select(Week, everything())

# remove km from dist and min from tot_duration_min :
df_daily <- df_daily %>% 
  mutate(dist = str_remove(dist, " km"),
         dist = str_replace(dist, ",", "."),
         dist = as.double(dist),
         tot_duration_min = as.double(str_remove(tot_duration_min, " min")),
         datum = parse_date_time(datum, "%d-%m-%Y") %>% ymd()
         )

df_daily %>% 
  mutate(tot_duration_min2 = seconds_to_period(dminutes(tot_duration_min)))

# Calculate Weekly stats:
weekly_tot <-  df_daily %>% 
  group_by(Week) %>% 
  summarise(`Summa km` = sum(dist, na.rm=T),
            `Summa tid` = sum(tot_duration_min, na.rm=T)) %>% 
  mutate(`Summa tid` = seconds_to_period(dminutes(`Summa tid`)),
         `% Diff` = round(((`Summa km`/lag(`Summa km`))-1)*100,2))

# df one week per row:
df_weekly <- df_daily %>% 
  filter(comment != "Vila") %>% 
  group_by(Week) %>% 
  mutate(pass = row_number()) %>% 
  select(-comment, -datum,-month, -day, -wday, -tot_duration_min) %>% 
  pivot_wider(names_from = "pass", values_from = dist, names_prefix = "Pass ") %>% 
  ungroup()

df_weekly <- df_weekly %>% 
  left_join(weekly_tot)

# put comments in same dimension as df_weekly (26 x 4)
df_weekly_comments <- df_daily %>% 
  filter(comment != "Vila") %>% 
  group_by(Week) %>% 
  mutate(pass = row_number()) %>% 
  select(-datum,-month, -day, -wday, -tot_duration_min,-dist) %>% 
  pivot_wider(names_from = "pass", values_from = comment)
  


df_daily %>% 
  group_by(month) %>% 
  summarise(tot = sum(dist))

df_weekly %>% 
  ggplot(aes(x=Week, y = `Summa km`)) +
  geom_bar(stat="identity") 



# ---------------------
# GENERATE EXCEL SHEET
wb <- createWorkbook()
addWorksheet(wb, "4_15")
modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Arial Narrow")

# header style
hs1 <- createStyle(
  fontColour = "#ffffff", fgFill = "#4F80BD",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight", fontSize = 14
)

# Write data
writeData(wb, sheet = "4_15", df_weekly, withFilter = T, headerStyle = hs1)
freezePane(wb, sheet = "4_15", firstRow = T)
setColWidths(WB, "4_15", cols = 1:ncol(df_weekly), widths = "auto")

# Add bgcolor based on cell values. Based on min/max when rule=NULL
conditionalFormatting(wb, "4_15",
                      cols = 6, rows = 2:(nrow(df_weekly)+1),
                      style = c("lightblue", "darkred"),
                      rule = NULL,
                      type = "colourScale"
)

# Generating comments
c1 <- map_if(df_weekly_comments$`1`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
c2 <- map_if(df_weekly_comments$`2`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
c3 <- map_if(df_weekly_comments$`3`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
c4 <- map_if(df_weekly_comments$`4`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)

# write comments:
map(df_weekly$Week, ~ writeComment(wb, sheet = "4_15", col = 2, row = .x+1, comment = c1[[.x]]))
map(df_weekly$Week, ~ writeComment(wb, sheet = "4_15", col = 3, row = .x+1, comment = c2[[.x]]))
map(df_weekly$Week, ~ writeComment(wb, sheet = "4_15", col = 4, row = .x+1, comment = c3[[.x]]))
# if its not NA (logical), write comment
map_if(df_weekly$Week, .p = ~class(c4[[.x]])!="logical", .f = ~ writeComment(wb, sheet = "4_15", col = 5, row = .x+1, comment = c4[[.x]]))
# class(c4[[1]])=="logical"

saveWorkbook(wb, file = "test1.xlsx", overwrite = T)
shell.exec("test1.xlsx")
