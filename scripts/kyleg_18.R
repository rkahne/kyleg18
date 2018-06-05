library(tabulizer)
library(tidyverse)
library(jsonlite)
library(magrittr)

# # Pull data about bills from legiscan
# # Api key redacted
# master_list <- fromJSON(read_csv('legiscan_url.csv')$url[1])
# 
# # Pull relevant information from master JSON
# # Also, pull specific bill information from API
# kyleg_18 <- tibble(
#   bill_id = master_list[["masterlist"]] %>% map(extract, 'bill_id') %>% unlist() %>% unname(),
#   number = master_list[["masterlist"]] %>% map(extract, 'number') %>% unlist() %>% unname(),
#   status_date = master_list[["masterlist"]] %>% map(extract, 'status_date') %>% unlist() %>% unname(),
#   status = master_list[["masterlist"]] %>% map(extract, 'status') %>% unlist() %>% unname(),
#   last_action_date = master_list[["masterlist"]] %>% map(extract, 'last_action_date') %>% unlist() %>% unname(),
#   last_action = master_list[["masterlist"]] %>% map(extract, 'last_action') %>% unlist() %>% unname(),
#   title = master_list[["masterlist"]] %>% map(extract, 'title') %>% unlist() %>% unname(),
#   description = master_list[["masterlist"]] %>% map(extract, 'description') %>% unlist() %>% unname()
# ) %>% 
#   mutate(bill_data = map(bill_id, function(id){
#     fromJSON(paste0(read_csv('legiscan_url.csv')$url[2],id))
#   }))

# dir.create('raw_text')

kyleg_18 <- read_rds('legiscan_18.rds')


# Functions to pull out votes from LRC website.  PDF based, so Tabulizer dependent

get_senate_vote <- function(txt){
  redownload <- T
  txt <- txt
  fail <- F
  
  # HCR is stored as HC in the url of the LRC website.
  if(str_detect(txt, 'HCR')) txt <- str_replace_all(txt, 'R', '')
  
  # Don't need to continually download all these files, unless we want to update.
  # if(redownload == T | !(paste0(txt, '.pdf') %in% list.files('raw_text'))){
  #   tryCatch({
  #     download.file(paste0('http://www.lrc.ky.gov/record/18RS/',txt,'/vote_history.pdf'), paste0('./raw_text/',txt,'.pdf'), mode = 'wb')
  #     close(gzfile(paste0('./raw_text/',txt,'.pdf')))
  #   },warning = function(w){
  #     fail <<- T
  #   }, error = function(e){
  #     fail <<- T
  #   })
  # }
  
  tryCatch({
    n_pages <- get_n_pages(paste0('./raw_text/',txt,'.pdf'))
    close(gzfile(paste0('./raw_text/',txt,'.pdf')))
  }, error = function(e){
    fail <<- T
  })
  
  if(fail == T) return("No Vote")
  
  # Create a tibble of text by page, then search for specific text and keep it.
  # Text searched for : "COMMONWEALTH OF KENTUCKY SENATE" & "PASS <bill number>"
  bill_txt <- tibble(p_num = 1:n_pages) %>% 
    mutate(raw = map(p_num, ~extract_text(paste0('./raw_text/',txt,'.pdf'), pages = .))) %>% 
    mutate(bill_txt = map(raw, function(t){
      str_detect(t, 'COMMONWEALTH OF KENTUCKY SENATE') & 
        str_detect(str_replace_all(t, '\\s', ''), paste0('PASS',txt))
    })) %>% 
    filter(bill_txt == T) %>% 
    tail(1) %>% 
    select(raw) %>% 
    unlist()
  
  # If none of the pages fit that qualification, return.
  if(is.null(bill_txt)) return('No Vote')
  
  # Format text.  Have to split out based on names, using captial letters, so some names need to be changed.
  # Also a problem: spaces within names.
  # Luckily, these names are totally predictable.
  # Many thanks to the Louisville R Meetup for all of this.
  bill <- str_sub(bill_txt, str_locate(bill_txt, 'YEAS :')[2], str_length(bill_txt)) %>% 
    str_replace_all("Carroll D", "Carroll_d") %>%
    str_replace_all("Carroll J", "Carroll_j") %>%
    str_replace_all('Raque Adams', 'Raque_adams') %>% 
    str_replace_all('McGarvey', 'Mcgarvey') %>% 
    str_replace_all('Harper Angel', 'Harper_angel') %>% 
    str_replace_all('McDaniel', 'Mcdaniel') %>% 
    str_replace_all(": ([0-9]|[0-9][0-9])\r\n", "") %>% 
    str_replace_all("\r\n", " ") %>% 
    str_replace_all("NAYS", "\r\nNay") %>% 
    str_replace_all("PASSES", "\r\nPass") %>% 
    str_replace_all("NOT VOTING", "\r\nNv") %>% 
    str_replace_all('YEA', 'Yea') %>% 
    str_replace_all('([A-Z])', '|\\1')
  
  # Split all this stuff out by line and look for specific words and then shove all the legislators together in a list.
  bill_list <- bill %>% tokenize(tokenizer = tokenizer_line()) %>% 
    str_replace_all("\\s+", " ") %>% 
    str_trim(side = "both") %>% 
    str_split("\\|") %>% 
    map(function(l) subset(l, !str_trim(l) %in% c('Yea', 'Nay', 'Pass', 'Nv', '')))
  
  # Name the list.
  # This only works because the PDFs follow a predictbale format
  names(bill_list) <- c('YEA', 'NAY', 'PASS', 'NV')
  close(gzfile(paste0('./raw_text/',txt,'.pdf')))
  
  # Split the list into a data frame, and return.
  tibble(legislator = str_trim(unlist(bill_list)),
         vote = str_replace(names(unlist(bill_list)), '\\d+', ''),
         bill = txt)
}


# House is the exact same form as Senate, 
# but the names are all different, so two functions were necessary.
get_house_vote <- function(txt){
  redownload <- T
  txt <- txt
  fail <- F
  if(str_detect(txt, 'HCR')) txt <- str_replace_all(txt, 'HCR', 'HC')
  
  # if(redownload == T | !(paste0(txt, '.pdf') %in% list.files('raw_text'))){
  #   tryCatch({
  #     download.file(paste0('http://www.lrc.ky.gov/record/18RS/',txt,'/vote_history.pdf'), paste0('./raw_text/',txt,'.pdf'), mode = 'wb')
  #     close(gzfile(paste0('./raw_text/',txt,'.pdf')))
  #   },warning = function(w){
  #     fail <<- T
  #     return('No Vote')
  #   }, error = function(e){
  #     fail <<- T
  #     return('No Vote')
  #   })
  # }
  
  tryCatch({
    n_pages <- get_n_pages(paste0('./raw_text/',txt,'.pdf'))
  }, error = function(e){
    fail <<- T
  })
  
  if(fail == T) return("No Vote")
  
  bill_txt <- tibble(p_num = 1:n_pages) %>% 
    mutate(raw = map(p_num, ~extract_text(paste0('./raw_text/',txt,'.pdf'), pages = .))) %>% 
    mutate(bill_txt = map(raw, function(t){
      str_detect(t, 'Commonwealth of Kentucky\r\nHouse of Representatives') &
        !str_detect(t, 'Suspend the Rules') &
        !str_detect(t, 'Lay on the Clerks Desk') &
        !str_detect(t, 'Consider Motion')
    })) %>%
    filter(bill_txt == T) %>% 
    tail(1) %>% 
    select(raw) %>% 
    unlist()
  
  if(is.null(bill_txt)) return('No Vote')
  
  bill <- str_sub(bill_txt, str_locate(bill_txt, 'YEAS :')[2], str_length(bill_txt)) %>% 
    str_replace_all("Brown G", "Brown_g") %>% 
    str_replace_all("Brown L", "Brown_l") %>%
    str_replace_all("DeCesare", "Decesare") %>%
    str_replace_all("DuPlessis", "Duplessis") %>%
    str_replace_all("Johnson DJ", "Johnson_dj") %>%
    str_replace_all("McCoy", "Mccoy") %>%
    str_replace_all("Miller J", "Miller_j") %>%
    str_replace_all("St. Onge", "St_onge") %>%
    str_replace_all("Miller C", "Miller_c") %>%
    str_replace_all(": ([0-9]|[0-9][0-9])\r\n", "") %>% 
    str_replace_all("\r\n", " ") %>% 
    str_replace_all("NAYS", "\r\nNay") %>% 
    str_replace_all("ABSTAINED", "\r\nPass") %>% 
    str_replace_all("NOT VOTING", "\r\nNv") %>% 
    str_replace_all('YEA', 'Yea') %>% 
    str_replace_all('([A-Z])', '|\\1')
  
  bill_list <- bill %>% tokenize(tokenizer = tokenizer_line()) %>% 
    str_replace_all("\\s+", " ") %>% 
    str_trim(side = "both") %>% 
    str_split("\\|") %>% 
    map(function(l) subset(l, !str_trim(l) %in% c('Yea', 'Nay', 'Pass', 'Nv', '')))
  
  names(bill_list) <- c('YEA', 'NAY', 'PASS', 'NV')
  close(gzfile(paste0('./raw_text/',txt,'.pdf')))
  tibble(legislator = str_trim(unlist(bill_list)),
         vote = str_replace(names(unlist(bill_list)), '\\d+', ''),
         bill = txt)
}

kyleg_18_votes <- kyleg_18 %>% 
  mutate(senate_vote = map(number, get_senate_vote),
         house_vote = map(number, get_house_vote))

write_rds(kyleg_18_votes, paste0('kyleg_votes',format(Sys.time(), '%Y%m%d %H%M'),'.rds'))

# # Use the functions above to append how each chamber voted on each bill we found that has passed a chamber.
# passed <- kyleg_18 %>% 
#   mutate(passed = map_lgl(bill_data, function(d){
#     T %in% str_detect(d$bill$history$action, 'passed')
#   })) %>% 
#   filter(passed == T) %>% 
#   mutate(senate_vote = map(number, get_senate_vote),
#          house_vote = map(number, get_house_vote))
# 


  
