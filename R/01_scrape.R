library(rvest)     # for webscraping, read_html()
library(tidyverse) # for data management, map(), etc.

# The following code is only run once. It is already saved to Data/out.RData 
# This script extracts all data for the marathon training programs and saves it
program <- c("300", "315", "330", "345", "400", "415", "430", "500", "600")
week_nr <- 1:26
# If you want to retrieve just a single program, uncomment this:
# urls <- str_glue("https://trana.marathon.se/adidas-stockholm-marathon-2022/415-programmet/{week_nr}")
# resps <- map(urls, read_html)

# Generate urls for all programs:
urls <- expand.grid(program = program, v = week_nr) %>% 
  arrange(program) %>% 
  mutate(url = str_glue("https://trana.marathon.se/adidas-stockholm-marathon-2022/{program}-programmet/{v}"))

urls

out <- vector("list")
for (i in program) {
  print(str_glue("Retrieving data for the {i}-program..."))
  url_i <- urls %>% 
    filter(program == i)
  out[[i]] <- map(url_i$url, read_html)
  print(str_glue("{i} completed."))
}
# Uncomment to save:
#save(out, file = "Data/out.RData")
