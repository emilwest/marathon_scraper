library(rvest)     # for webscraping, read_html()
library(tidyverse) # for data management, map(), etc.

# The following code is only run once. It is already saved to Data/info_list.RData
# This script extracts additional info for the marathon programs on what to expect,
# the base speed, if the program is for you or not.

main_url <- str_glue("https://trana.marathon.se/adidas-stockholm-marathon-2022")

info <- rvest::read_html(main_url) %>% 
  html_nodes(".BuyProgram-value") %>% 
  html_elements("a")

info

program_values <- str_extract(info[seq(1,to=18, by=2)], "[0-9]{3}-programmet") %>% 
  str_remove("-programmet")
program_links <- info[seq(2,to=18, by=2)] %>% 
  html_attr("href")

info_urls <- str_glue("https://trana.marathon.se{program_links}")

read_info <- function(u) {
  read_html(u) %>% 
    html_elements(".Lightbox-inner") %>% 
    html_text2()
}

info_list <- map(info_urls, read_info)
info_list <- info_list %>% 
  set_names(program_values)

# save(info_list, file = "Data/info_list.RData")
