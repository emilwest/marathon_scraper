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

generate_comment <- function(comment_vec) {
  map_if(comment_vec, .p = ~ !is.na(.), .f = createComment, visible = F, .else = ~ NA)
}

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
       
       
       curr_comments <- df_list_of_weekly_comments[[name]]
       curr_comments <- curr_comments %>% 
         select(-Week)
       
       generated_comments_list <- curr_comments %>% 
         map(generate_comment)
       
       for (i in seq_along(generated_comments_list)) {
         comments_i <- generated_comments_list[[i]]
         length(comments_i)
         
         # if its not NA (logical), write comment
         map_if(1:length(comments_i), 
                .p = ~class(comments_i[[.x]])!="logical",
                .f = ~ writeComment(wb, sheet = curr_sheetname, col = i+1, row = .x+1, comment = comments_i[[.x]]))
         
       }
       
       saveWorkbook(wb, file = str_glue("Output/{name}_programmet.xlsx"), overwrite = T)

     })


