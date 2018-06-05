library(tidyverse)
library(jsonlite)

sessions <- fromJSON('https://api.legiscan.com/?key=6ddeedfc381c2f82e6f65a0ef37b4fb6&op=getSessionList&state=KY')

sessions <- sessions[['sessions']]

master_list_17 <- fromJSON('https://api.legiscan.com/?key=6ddeedfc381c2f82e6f65a0ef37b4fb6&op=getMasterList&state=KY&id=1257')

kyleg_17 <- tibble(
  bill_id = master_list_17[["masterlist"]] %>% map(extract, 'bill_id') %>% unlist() %>% unname(),
  number = master_list_17[["masterlist"]] %>% map(extract, 'number') %>% unlist() %>% unname(),
  status_date = master_list_17[["masterlist"]] %>% map(extract, 'status_date') %>% unlist() %>% unname(),
  status = master_list_17[["masterlist"]] %>% map(extract, 'status') %>% unlist() %>% unname(),
  last_action_date = master_list_17[["masterlist"]] %>% map(extract, 'last_action_date') %>% unlist() %>% unname(),
  last_action = master_list_17[["masterlist"]] %>% map(extract, 'last_action') %>% unlist() %>% unname(),
  title = master_list_17[["masterlist"]] %>% map(extract, 'title') %>% unlist() %>% unname(),
  description = master_list_17[["masterlist"]] %>% map(extract, 'description') %>% unlist() %>% unname()
) %>% 
  mutate(bill_data = map(bill_id, function(id){
    fromJSON(paste0('https://api.legiscan.com/?key=6ddeedfc381c2f82e6f65a0ef37b4fb6&op=getBill&id=',id))
  }))
