library(rbokeh)
library(tidyverse)

# kyleg_18_votes <-readRDS('kyleg_votes_01262018.rds')
kyleg_18_votes <- readRDS('kyleg_votes20180210 1240.rds')
leg_data <- read_csv('https://query.data.world/s/r2uPfq9E2KTqhDuV1HuREp_GKZM5xi')

house_tbl <- kyleg_18_votes %>% 
  filter(house_vote != 'No Vote') %>% 
  select(house_vote) %>% 
  map_df(bind_rows)

legislator_names <- unique(house_tbl$legislator)

no_nays <- house_tbl %>% 
  group_by(vote, bill) %>% 
  summarize(n=n()) %>% 
  filter(vote == 'NAY') %>%
  ungroup() %>% 
  select(bill, n) %>% 
  right_join(house_tbl %>% select(bill) %>% distinct()) %>% 
  filter(is.na(n)) %>% 
  select(bill) %>% 
  as_vector()

get_match_score <- function(legislatorA, legislatorB){
  if(legislatorA == legislatorB) return(-1)
  temp <- house_tbl %>% 
    spread(bill, vote) %>% 
    filter(legislator %in% c(legislatorA, legislatorB)) %>% 
    gather(bill, vote, -legislator) %>% 
    spread(legislator, vote) %>% 
    filter(!bill %in% no_nays)
  
  names(temp) <- c('bill', 'a', 'b')
  
  temp %>% 
    filter(a != 'NV' & b != 'NV') %>% 
    mutate(match = a == b) %>% 
    summarize(mean(match)) %>% 
    as_vector() %>% 
    unname()
}

leg_data %<>%
  mutate(chamber = str_sub(District, 1, 1)) %>%
  filter(chamber == 'H') %>% 
  mutate(LastUnique = LastUnique %>% 
           str_replace_all("Brown G", "Brown_g") %>% 
           str_replace_all("Brown L", "Brown_l") %>%
           str_replace_all("DeCesare", "Decesare") %>%
           str_replace_all("DuPlessis", "Duplessis") %>%
           str_replace_all("Johnson DJ", "Johnson_dj") %>%
           str_replace_all("McCoy", "Mccoy") %>%
           str_replace_all("Miller J", "Miller_j") %>%
           str_replace_all("St. Onge", "St_onge") %>%
           str_replace_all("Miller C", "Miller_c") %>% 
           str_replace_all('Harris C', 'Harris') %>% 
           str_replace_all('Meredith M', 'Meredith') %>% 
           str_replace_all('Turner T', 'Turner') %>% 
           str_replace_all('Thomas W', 'Thomas') %>% 
           str_replace_all('Bunch', 'Huff'))

hoover_score <- tibble(legislator = legislator_names) %>% 
  mutate(hoover_score = map2_dbl(legislator, 'Hoover', get_match_score)) %>% 
  left_join(house_tbl %>% filter(bill == 'HB77') %>% select(-bill)) %>% 
  left_join(leg_data %>% select(LastUnique, Party), by = c('legislator' = 'LastUnique'))


hoover_score %>% 
  filter(vote != 'NV',
         legislator != 'Hoover') %>% 
  group_by(Party, vote) %>% 
  summarize(avg_hoover = mean(hoover_score),
            median_hoover = median(hoover_score))

hoover_score %>% 
  filter(Party == 'Republican',
         legislator != 'Hoover',
         vote != 'NV') %>% 
  ggplot(aes(x = vote, y = hoover_score)) +
  geom_jitter(alpha = 0.6, width = 0.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = 'HB77 Vote Analysis',
       x = 'HB77 Vote',
       y = '% of Votes with Hoover')
  
hb77 <- hoover_score %>% 
  filter(Party == 'Republican',
         legislator != 'Hoover',
         vote != 'NV') %>% 
  mutate(pct_format = paste0(round(hoover_score,3)*100, '%')) 

hb77 %>% 
  figure() %>% 
  ly_points(x = catjitter(vote), y = hoover_score * 100,
            hover = 'Rep. @legislator votes with Rep. Hoover @pct_format') %>% 
  ly_abline(h = hb77 %>% filter(vote == 'YEA') %>% summarize(mean(hoover_score)) %>% as_vector()) %>%
  x_axis(label = 'Vote on HB 77') %>% 
  y_axis(label = '% of Votes with Jeff Hoover', 
         number_formatter = 'printf',
         format = '%0.1f%%')


# saveRDS(hb77, file ='hb77_bokeh.rds')
