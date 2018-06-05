# source('kyleg_18.R')

leg_data <- read_csv('https://query.data.world/s/r2uPfq9E2KTqhDuV1HuREp_GKZM5xi')

leg_data_h <- leg_data %>%
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

#### Votes by Chamber
house_tbl <- kyleg_18_votes %>% 
  filter(house_vote != 'No Vote') %>% 
  select(house_vote) %>% 
  map_df(bind_rows) 

# ERROR ON 2/10
house_tbl$legislator[which(house_tbl$legislator == 'ott')] <- 'Scott'

house_tbl <- house_tbl %>% 
  left_join(leg_data_h, by = c('legislator' = 'LastUnique'))

senate_tbl <- kyleg_18_votes %>% 
  filter(senate_vote != 'No Vote') %>% 
  select(senate_vote) %>% 
  map_df(bind_rows)


house_tbl %>%
  select(-bill) %>% 
  group_by(Full.Name, vote, Party) %>% 
  summarize(n=n())  %>% 
  ungroup() %>% 
  spread(vote, n) %>% 
  replace_na(list(NAY = 0, NV = 0, YEA = 0)) %>% 
  mutate(Full.Name = fct_reorder(Full.Name, NAY)) %>% 
  gather(vote, n, -Full.Name, -Party) %>% 
  filter(Party == 'Democratic') %>% 
  ggplot(aes(Full.Name, n, fill = vote)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = glue::glue('House Votes as of {Sys.Date()}'), x = '', y = '') +
  theme(axis.text.x = element_text(angle = 90))


senate_tbl %>%
  select(-bill) %>% 
  group_by(legislator, vote) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  spread(vote, n) %>% 
  replace_na(list(NAY = 0, NV = 0, YEA = 0)) %>% 
  mutate(legislator = fct_reorder(legislator, NAY)) %>% 
  gather(vote, n, -legislator) %>% 
  ggplot(aes(legislator, n, fill = vote)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = glue::glue('Senate Votes as of {Sys.Date()}'), x = '', y = '') +
  theme(axis.text.x = element_text(angle = 90))

sponsors <- map2(kyleg_18$number, kyleg_18$bill_data, function(n, d) d[['bill']]$sponsors %>% mutate(bill_number = n)) %>% 
  map_df(bind_rows)

sponsors %>% 
  group_by(name) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  View()

sponsors %>% 
  filter(name == 'John Sims') %>% 
  select(bill_number)

kyleg_18 %>% 
  group_by(last_action) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% View()

#### TIMELINE OF ACTIVITY
library(lubridate)
history <- kyleg_18 %>% 
  mutate(data = map(bill_data, ~.$bill$history)) %>% 
  select(number, data) %>% 
  unnest()

history %>% 
  mutate(date = ymd(date)) %>% 
  group_by(chamber, date) %>% 
  summarize(n = n()) %>% 
  mutate(w_day = wday(date)) %>% 
  filter(date > '2018-01-01',
         w_day < 6) %>% 
  mutate(date = as.character(date),
         cumsum_n = cumsum(n)) %>% 
  ggplot(aes(x = date, y = cumsum_n, color = chamber, label = n)) +
  geom_label(show.legend = F) +
  theme_minimal() +
  labs(x = 'Date', y = 'Number of Actions', title = 'Total Actions by Chamber') +
  theme(axis.text.x = element_text(angle = 90))

# House Pre Filing Deadline Actions
history %>% 
  filter(date <= '2018-01-30', date > '2018-01-02', chamber == 'H') %>% 
  group_by(date) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  summarize(avg_actions = mean(n))

# House Post Filing Deadline Actions
history %>% 
  filter(date > '2018-01-30', date > '2018-01-02', chamber == 'H') %>% 
  group_by(date) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  summarize(avg_actions = mean(n))

# Senate Pre Filing Deadline Actions
history %>% 
  filter(date <= '2018-01-30', date > '2018-01-02', chamber == 'S') %>% 
  group_by(date) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  summarize(avg_actions = mean(n))

# Senate Post Filing Deadline Actions
history %>% 
  filter(date > '2018-01-30', date > '2018-01-02', chamber == 'S') %>% 
  group_by(date) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  summarize(avg_actions = mean(n))

#### Similarities
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
    # filter(a != 'NV' & b != 'NV') %>% 
    mutate(match = a == b) %>% 
    summarize(mean(match, na.rm = T)) %>% 
    as_vector() %>% 
    unname()
}

comparison <- matrix(nrow = 98, ncol = 98, 
                     dimnames = list(unique(house_tbl$legislator), unique(house_tbl$legislator))) %>% 
  as_tibble()

col <- unique(house_tbl$legislator)

for(i in 1:ncol(comparison)){
  for(j in 1:ncol(comparison)){
    comparison[[i]][j] <- get_match_score(names(comparison)[i], col[j])
  }
}

comparison <- bind_cols(comparison, tibble(col = col))
# install.packages('heatmaply')

# heatmaply::heatmaply(comparison)

legislator_names <- unique(house_tbl$legislator)

comp2 <- tibble(legislator = legislator_names) %>% 
  mutate(comparison = map(legislator, function(l){
    tibble(other_legislators = subset(legislator_names, legislator_names != l),
           leg = l) %>% 
      mutate(score = map2_dbl(leg, other_legislators, get_match_score))
  }))

comp2 %>% 
  mutate(best_friend = map_chr(comparison, function(d) d %>% 
                                 arrange(desc(score)) %>% 
                                 head(1) %>% 
                                 select(other_legislators) %>% 
                                 as_vector() %>% 
                                 unname())) %>% 
  select(-comparison) %>% 
  View('bff')



hoover_score <- tibble(legislator = legislator_names) %>% 
  mutate(hoover_score = map2_dbl(legislator, 'Hoover', get_match_score)) %>% 
  left_join(house_tbl %>% filter(bill == 'HB77') %>% select(-bill))

scott_score <- tibble(legislator = legislator_names) %>% 
  mutate(scott_score = map2_dbl(legislator, 'Hoover', get_match_score)) %>% 
  left_join(house_tbl %>% filter(bill == 'HB77') %>% select(-bill))

get_vote <- function(df, the_vote){
  if(df == 'No Vote') NA
  else{
    df %>% filter(vote == the_vote) %>% nrow()
  }
}

votes <- kyleg_18_votes %>% 
  mutate(house_yea = map2_dbl(house_vote, 'YEA', get_vote),
  house_nay = map2_dbl(house_vote, 'NAY', get_vote),
  house_nv = map2_dbl(house_vote, 'NV', get_vote),
  senate_yea = map2_dbl(senate_vote, 'YEA', get_vote),
  senate_nay = map2_dbl(senate_vote, 'NAY', get_vote),
  senate_nv = map2_dbl(senate_vote, 'NV', get_vote)) %>% 
  select(number, description, house_vote, senate_vote, house_yea, house_nay, house_nv, senate_yea, senate_nay, senate_nv) %>% 
  filter(house_vote != 'No Vote' | senate_vote != 'No Vote')

controversial <- votes %>% 
  filter(house_nay > 20 | senate_nay > 7)