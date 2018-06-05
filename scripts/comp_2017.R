library(tidyverse)

house_2017 <- read_csv('https://query.data.world/s/wnocy6ecjapo65ma744jdwuxryvscv')
leg_data_2017 <- read_csv('https://query.data.world/s/k35f6xnhpe3s6yfco6x7bms6c5r2s6')
votes_2017 <- read_csv('https://query.data.world/s/8ta9wcevg9crkp319k8knal2c')
legislators_2017 <- read_csv('https://query.data.world/s/6i8gpey8jqq8epjbrg7xkyycn') %>% 
  mutate(chamber = str_sub(District, 1, 1),
         party = ifelse(Party == 'Democratic', 'D', ifelse(Party == 'Republican', 'R', NA)))

house_2017 <- filter(legislators_2017, chamber == 'H')

house_votes_2017 <- votes_2017 %>% 
  filter(legislator_id %in% house_2017$legislator_id) %>% 
  select(legis.names = MemberName, BillName, Vote) %>% 
  mutate(Vote = case_when(
    Vote == 'YEA' ~ 1,
    Vote == 'NAY' ~ 6,
    Vote == 'NV' ~ 9,
    Vote == 'ABS' ~ 9,
    is.na(Vote) ~ 9
  )) %>% 
  spread(key = BillName, value = Vote)
house_votes_2017[is.na(house_votes_2017)] <- 9

house_votes_m_2017 <- as.matrix(select(house_votes_2017, -legis.names))
dimnames(house_votes_m_2017) <- list(house_votes_2017$legis.names, dimnames(house_votes_m_2017)[[2]])

house_rc_2017 <- rollcall(
  data = house_votes_m_2017,
  yea = 1, nay = 6, missing = 9, notInLegis = 0,
  vote.names = subset(names(house_votes_2017), names(house_votes_2017) != 'legis.names'),
  legis.data = legislators_2017 %>% filter(chamber == 'H') %>%  select(LastUnique, party, Race, Gender, First.Elected),
  desc = 'Kentucky General Assembly, 2017',
  source = 'Derby Pie Politics API'
)


senate <- filter(legislators, chamber == 'S')

senate_votes <- votes %>% 
  filter(legislator_id %in% senate$legislator_id) %>% 
  select(legis.names = MemberName, BillName, Vote) %>% 
  mutate(Vote = case_when(
    Vote == 'YEA' ~ 1,
    Vote == 'NAY' ~ 6,
    Vote == 'NV' ~ 9,
    Vote == 'ABS' ~ 9,
    is.na(Vote) ~ 9
  )) %>% 
  spread(key = BillName, value = Vote)
senate_votes[is.na(senate_votes)] <- 9

senate_votes_m <- as.matrix(select(senate_votes, -legis.names))
dimnames(senate_votes_m) <- list(senate_votes$legis.names, dimnames(senate_votes_m)[[2]])

senate_rc <- rollcall(
  data = senate_votes_m,
  yea = 1, nay = 6, missing = 9, notInLegis = 0,
  vote.names = subset(names(senate_votes), names(senate_votes) != 'legis.names'),
  legis.data = legislators %>% filter(chamber == 'S') %>%  select(LastUnique, party, Race, Gender, First.Elected),
  desc = 'Kentucky General Assembly, 2017',
  source = 'Derby Pie Politics API'
)

house_wn <- wnominate(house_rc, polarity = c(35, 45)) # 35 = Hoover, 45 = Lee
senate_wn <- wnominate(senate_rc, polarity = c(30, 28)) # 30 = Stivers, 28 = Seum 

house_2017 %>% 
  select(legislator_id, MemberName, BillName, Vote) %>%
  nest(-BillName) %>% 
  mutate(data = map(data, function(x){
    x %>% filter(Vote == 'NAY')
  }),
  n_nays = map_dbl(data, nrow)
  ) %>% 
  filter(n_nays < 5, n_nays > 0) %>% 
  unnest() %>% 
  count(legislator_id, MemberName) %>% 
  left_join(leg_data_2017 %>% select(legislator_id, Party)) %>%
  distinct() %>% 
  ggplot(aes(x = fct_reorder(MemberName, n), y = n, label = n, fill = Party)) +
  geom_bar(stat = 'identity') +
  geom_label(show.legend = F) +
  scale_fill_manual(values = c('Democratic' = 'blue', 'Republican' = 'red')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Count of NAY votes where opposition was < 5 members (2017)',
       x = '', y = '')
