library(tidyverse)

votes_18 <- read_rds('kyleg_votes20180419 2207.rds')
leg_data <- read_csv('leg_data.csv')

votes_18$bill_data[[1]]$bill -> b

sponsors <- votes_18 %>% 
  select(number, bill_data) %>% 
  mutate(sponsors = map(bill_data, ~.$bill$sponsors)) %>% 
  select(number, sponsors) %>% 
  unnest()

sponsors %>% 
  count(district) %>% 
  arrange(desc(n)) %>% 
  mutate(chamber = str_sub(district, 1, 1)) %>% 
  ggplot(aes(x = fct_reorder(district,n), y = n)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~chamber, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 90))

### History
history <- votes_18 %>% 
  mutate(data = map(bill_data, ~.$bill$history)) %>% 
  select(number, data) %>% 
  unnest() %>% 
  mutate(date = ymd(date)) %>% 
  mutate(consolidated_action = action %>% 
           str_replace_all('passed (?!over).*', 'passed') %>% 
           str_replace_all('(defeated).*$', 'defeated') %>%
           str_replace_all('(adopted).*$', 'adopted') %>%  
           str_replace_all('(posted for passage in the Consent Orders of the Day).*$', 'posted for passage in the Consent Orders of the Day') %>% 
           str_replace_all('(posted for passage in the Regular Orders of the Day).*$', 'posted for passage in the Regular Orders of the Day') %>% 
           str_replace_all('(floor amendment(s)?).*(filed).*(to Bill)', 'floor amendments filed to Bill') %>%
           str_replace_all('(floor amendment(s)?).*(filed).*(to Committee Substitute)', 'floor amendments filed to committee substitute') %>%
           str_replace_all('(floor amendment(s)?).*(filed)', 'floor amendments filed') %>%  
           str_replace_all('(floor amendment(s)?).*(withdrawn)', 'floor amendments withdrawn'))

# Passed
passed <- history %>% filter(str_detect(action, 'signed by Governor') |
                               str_detect(action, 'delivered to Secretary of State') |
                               str_detect(action, "filed without Governor's signature with the Secretary of State") | 
                               str_detect(action, 'became law'))


sponsors %>% 
  filter(last_name == 'Jenkins') %>% 
  select(number) %>% 
  inner_join(passed) %>% View()


sponsors %>% 
  filter(party == 'D') %>% 
  filter(number %in% passed$number) %>% 
  count(name) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = fct_reorder(name, n), y = n, label = n)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
  