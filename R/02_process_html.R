library(rvest)     # for webscraping, html_nodes() etc
library(tidyverse) # for data management, map(), etc.

# This scripts processes the html data extracted for each marathon training program.
# It converts it to data frames where each row is a weekday and cleans the data

load("Data/out.RData")

# Function that gets the part of the weeks containing the training instructions.
# Can be identified by the class 'view-display-id-training_week'.
# Returns a list of weeks, each week is a xml_nodeset (7), 
# where each day is a div of class 'exercise'
get_weeks <- function(r) {
  r %>% 
    html_nodes(".view-display-id-training_week") %>% 
    html_children()
}

get_weeks_as_list <- function(w) {
  ids <- w %>% 
    map(html_attr, "id") # the id is the date, for ex "06-12-2021"
  w %>% 
    html_text2() %>% # extracts text values from html
    map(str_split,"\\n") %>% # each field is separated by \n 
    flatten() %>% 
    set_names(ids) # each list element is gets named by id
}

# To get a single program, set p to a custom value, 
# otherwise loop through each program here
# p <- 1
list_cleaned <- vector("list")
for (p in seq_along(out)) {
  curr_program <- names(out[p])
  print(curr_program)

  weeks_p <- map(out[[p]], get_weeks)
  week_list_p <- map(weeks_p, get_weeks_as_list)

  # Get the comments for each day
  comments_p <- weeks_p %>%
    map(html_nodes, ".exercise-content") %>%
    map(html_text2)

  # Check that the number of comments are correct, we expect one comment/day.
  # For the 400-program, some comments are missing (week 26)
  # or duplicated comments in the source data (week 21, 22)
  # w <- 21
  for (w in seq_along(comments_p)) {
    l <- length(comments_p[[w]])
    if (l != 7) {
      print(str_glue("Not 7 comments for week {w}, it's {length(comments_p[[w]])}"))
      if (l == 6) {
        print("Adding Vila")
        comments_p[[w]] <- c(comments_p[[w]], "Vila")
      }
      if (l > 7) {
        print("Removing duplicate")
        comments_p[[w]] <- comments_p[[w]] %>%
          enframe(name = NULL) %>%
          mutate(dup = ifelse(value != "Vila", duplicated(value), F)) %>%
          filter(dup != TRUE) %>%
          pull(value)
      }
    }
  }

  # The first five elements are always the following:
  # 1: month, 2: day number, 3: weekday string, 4: distance (km), 5: minutes
  # we extract only these and treat the rest elements as comments.
  tbl_colnames <- c("datum", "month", "day", "wday", "dist", "tot_duration_min")
  # creates empty tibble:
  df_daily <- tbl_colnames %>% purrr::map_dfc(setNames, object = list(character()))

  for (w in seq_along(week_list_p)) {
    for (day in seq_along(week_list_p[[w]])) {
      # print(str_glue("{w} {day}"))

      week_list_p[[w]][[day]] <- week_list_p[[w]][[day]][1:5]
      tmp <- week_list_p[[w]][[day]]

      df_daily <- df_daily %>%
        add_row(
          datum = names(week_list_p[[w]][day]),
          month = tmp[1],
          day = tmp[2],
          wday = tmp[3],
          dist = tmp[4],
          tot_duration_min = tmp[5]
        )

      # Special case scenario since it's missing in the source data
      if (curr_program == "400" & w == 26 & day == 6) {
        df_daily <- df_daily %>%
          add_row(
            datum = "05-6-2022",
            month = "jun",
            day = "5",
            wday = "söndagsön",
            dist = "0 km",
            tot_duration_min = "0 min"
          )
      }
    }
  }


  df_daily$comment <- flatten(comments_p) %>% unlist() # Add comments
  df_daily$Week <- rep(1:26, each = 7) # Add week number

  list_cleaned[[curr_program]] <- df_daily
}

# Correcting mistakes in source data: 

# Add missing 'Vila' for the following day
list_cleaned$`400` <- list_cleaned$`400` %>% 
  mutate(comment = ifelse(Week == 4 & day == 27, "Vila", comment))

# 330-programmet is missing the marathon day, so we add it:
list_cleaned$`330` <- list_cleaned$`330` %>% 
  mutate(comment = ifelse(Week == 26 & day == 4, 
                       "Uppvärmning\nDistans:1 kmTid:5-6 min\nDynamisk rörlighet\nTid:5 min\nTävling\nDistans:42,2 kmTid:3.30", 
                       comment),
         dist = ifelse(Week == 26 & day == 4, "43 km", dist),
         tot_duration_min  = ifelse(Week == 26 & day == 4, "221 min", tot_duration_min) # 180+30+11
         )
  
# 345 v. 21 is 0 km when it should be 25km so we correct it:
list_cleaned$`345` <- list_cleaned$`345` %>% 
  mutate(dist = ifelse(Week == 21 & day == 1, "25 km", dist))


# list_cleaned$`400` %>% view
# save(list_cleaned, file = "Data/list_cleaned.RData")
