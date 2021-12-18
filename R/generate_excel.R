library(tidyverse)
library(openxlsx)

load("Data/df_list_of_weekly_comments.RData")
load("Data/df_weekly_list.RData")
load("Data/list_cleaned2.RData")
load("Data/info_list.RData")

df_list_of_weekly_comments
df_weekly_list
list_cleaned2


# ---------------------
# GENERATE EXCEL SHEET

# header style
hs1 <- createStyle(
  fontColour = "#ffffff", fgFill = "#4F80BD",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight", fontSize = 14
)

map2(.x = df_weekly_list, .y = names(df_weekly_list),
     .f = function(data, name) {
       
       curr_sheetname <- str_glue("{name}_weekly")
       wb <- createWorkbook()
       addWorksheet(wb, curr_sheetname)
       addWorksheet(wb, "Info")
       modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Arial Narrow")
       
       # Write data
       writeData(wb, sheet = curr_sheetname, data, withFilter = T, headerStyle = hs1)
       freezePane(wb, sheet = curr_sheetname, firstRow = T)
       setColWidths(wb, 1, cols = 1:ncol(data), widths = "auto")
       
       writeData(wb, sheet = "Info", 
                 info_list[[name]] %>% 
                   str_split("\n"), 
                 startCol = 1, startRow = 1,
                 borderStyle = "none", headerStyle = hs1)
       
       
       # Add bgcolor based on cell values. Based on min/max when rule=NULL
       conditionalFormatting(wb, 
                             curr_sheetname,
                             cols = ncol(data)-3, 
                             rows = 2:(nrow(data)+1),
                             style = c("lightblue", "darkred"),
                             rule = NULL,
                             type = "colourScale"
       )
       
       
       #curr_comments <- df_list_of_weekly_comments[[name]]
       
       
       saveWorkbook(wb, file = str_glue("Output/{name}_programmet.xlsx"), overwrite = T)
       
       
       
     })



generate_comment <- function(comment_vec) {
  map_if(comment_vec, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
}

curr_comments_tmp <- df_list_of_weekly_comments[["300"]]
curr_comments_tmp <- curr_comments_tmp %>% 
  select(-Week)

generated_comments_list <- curr_comments_tmp %>% 
  map(generate_comment)

generated_comments_list[[5]][[1]]

generated_comments_list %>% unlist() %>% tibble()


1:26 %>% 
  map_if(.p = ~class(c4[[.x]])!="logical",
         .f = ~ writeComment(wb, sheet = curr_sheetname, col = i+1, row = .x+1, comment = generated_comments_list[[5]] ))

i<-1
for (i in seq_along(curr_comments_tmp)) {
  # Generating comments
  c_i <- map_if(curr_comments_tmp[[i]], .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
  
  # row id is used to calculate which rows to addthe comments to
  map(data[[row_id]], 
      .p = ~class(c4[[.x]])!="logical",
      .f = ~ writeComment(wb, sheet = curr_sheetname, col = i+1, row = .x+1, comment = c_i[[.x]]))
  
}
curr_comments_tmp


df_weekly_list$`300`$Week
c1 <- map_if(curr_comments_tmp$`1`, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
map(df_weekly_list$`300`$Week, ~ writeComment(wb, sheet = "4_15", col = 2, row = .x+1, comment = c1[[.x]]))

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