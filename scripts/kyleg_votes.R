library(tabulizer)
library(tidyverse)

txt <- 'SB70'

download.file(paste0('http://www.lrc.ky.gov/record/18RS/',txt,'/vote_history.pdf'), paste0('./raw_text/',txt,'.pdf'), mode = 'wb')

bill_txt <- extract_text(paste0('./raw_text/',txt,'.pdf'))

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

bill_list <- bill %>% tokenize(tokenizer = tokenizer_line()) %>% 
  str_replace_all("\\s+", " ") %>% 
  str_trim(side = "both") %>% 
  str_split("\\|") %>% 
  map(function(l) subset(l, !str_trim(l) %in% c('Yea', 'Nay', 'Pass', 'Nv', '')))

names(bill_list) <- c('YEA', 'NAY', 'PASS', 'NV')

tibble(senator = str_trim(unlist(bill_list)),
                 vote = str_replace(names(unlist(bill_list)), '\\d+', '')) %>% 
  View()


txt <- 'HB22'

download.file(paste0('http://www.lrc.ky.gov/record/18RS/',txt,'/vote_history.pdf'), paste0('./raw_text/',txt,'.pdf'), mode = 'wb')

bill_txt <- extract_text(paste0('./raw_text/',txt,'.pdf'))

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

tibble(senator = str_trim(unlist(bill_list)),
       vote = str_replace(names(unlist(bill_list)), '\\d+', ''),
       bill = txt)
