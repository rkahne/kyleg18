library(tidyverse)
library(lubridate)
library(gridExtra)
library(magrittr)

kyleg_18_votes <- read_rds('kyleg_votes20180419 2207.rds')
leg_data <- read_csv('https://query.data.world/s/r2uPfq9E2KTqhDuV1HuREp_GKZM5xi')
pal <- RColorBrewer::brewer.pal(6, 'YlGnBu')[2:6]

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

leg_data_s <- leg_data  %>%
  mutate(chamber = str_sub(District, 1, 1)) %>%
  filter(chamber == 'S') %>% 
  mutate(LastUnique = LastUnique %>%
           str_replace_all("Carroll D", "Carroll_d") %>%
           str_replace_all("Carroll J", "Carroll_j") %>%
           str_replace_all('Adams', 'Raque_adams') %>% 
           str_replace_all('McGarvey', 'Mcgarvey') %>% 
           str_replace_all('Harper Angel', 'Harper_angel') %>% 
           str_replace_all('McDaniel', 'Mcdaniel') %>% 
           str_replace_all('Meredith S', 'Meredith') %>% 
           str_replace_all('Stivers II', 'Stivers') %>% 
           str_replace_all('Harris E', 'Harris') %>% 
           str_replace_all('Thomas R', 'Thomas') %>% 
           str_replace_all('Jones II', 'Jones') %>% 
           str_replace_all('Embry Jr.', 'Embry') %>% 
           str_replace_all('Turner J', 'Turner'))

#### Votes by Chamber
house_votes <- kyleg_18_votes %>% 
  filter(house_vote != 'No Vote') %>% 
  select(house_vote) %>% 
  map_df(bind_rows) %>% 
  left_join(leg_data_h, by = c('legislator' = 'LastUnique'))

senate_votes <- kyleg_18_votes %>% 
  filter(senate_vote != 'No Vote') %>% 
  select(senate_vote) %>% 
  map_df(bind_rows) %>% 
  left_join(leg_data_s, by = c('legislator' = 'LastUnique'))

#### Bills with votes without any NAYs
no_nays_h <- house_votes %>% 
  group_by(vote, bill) %>% 
  summarize(n=n()) %>% 
  filter(vote == 'NAY') %>%
  ungroup() %>% 
  select(bill, n) %>% 
  right_join(house_votes %>% select(bill) %>% distinct()) %>% 
  filter(is.na(n)) %>% 
  select(bill) %>% 
  as_vector()
# 2/10 update: 51 / 76 had no nays

no_nays_s <- senate_votes %>% 
  group_by(vote, bill) %>% 
  summarize(n=n()) %>% 
  filter(vote == 'NAY') %>%
  ungroup() %>% 
  select(bill, n) %>% 
  right_join(senate_votes %>% select(bill) %>% distinct()) %>% 
  filter(is.na(n)) %>% 
  select(bill) %>% 
  as_vector()
# 2/10 update: 26 / 38 had no nays.


## Votes on Contested Bills
senate_votes %>%
  filter(!bill %in% no_nays_s) %>% 
  select(-bill) %>% 
  group_by(Full.Name, vote, Party) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  spread(vote, n) %>% 
  replace_na(list(NAY = 0, NV = 0, YEA = 0)) %>% 
  mutate(Full.Name = fct_reorder(Full.Name, NAY)) %>% 
  gather(vote, n, -Full.Name, -Party) %>% 
  ggplot(aes(Full.Name, n, fill = vote)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = '',
                    breaks = c('NAY', 'NV', 'YEA'),
                    labels = c('Nay', 'No Vote', 'Yea'),
                    values = pal
                    ) +
  theme_minimal() +
  labs(title = glue::glue('Senate Votes on Contested Bills as of {Sys.Date()}'), x = '', y = '') +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Party, scales = 'free_x')

grid.arrange(
  house_votes %>%
    filter(!bill %in% no_nays_h) %>%
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
    scale_fill_manual(name = '',
                      breaks = c('NAY', 'NV', 'YEA'),
                      labels = c('Nay', 'No Vote', 'Yea'),
                      values = pal
    ) +
    theme_minimal() +
    labs(title = glue::glue('Democrats'), x = '', y = '') +
    theme(axis.text.x = element_text(angle = 90)),
  house_votes %>%
    filter(!bill %in% no_nays_h) %>%
    select(-bill) %>% 
    group_by(Full.Name, vote, Party) %>% 
    summarize(n=n())  %>% 
    ungroup() %>% 
    spread(vote, n) %>% 
    replace_na(list(NAY = 0, NV = 0, YEA = 0)) %>% 
    mutate(Full.Name = fct_reorder(Full.Name, NAY)) %>% 
    gather(vote, n, -Full.Name, -Party) %>% 
    filter(Party == 'Republican') %>% 
    ggplot(aes(Full.Name, n, fill = vote)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(name = '',
                      breaks = c('NAY', 'NV', 'YEA'),
                      labels = c('Nay', 'No Vote', 'Yea'),
                      values = pal
    ) +
    theme_minimal() +
    labs(title = glue::glue('Republicans'), x = '', y = '') +
    theme(axis.text.x = element_text(angle = 90)),
  top = glue::glue('House Votes on Contested Bills as of {Sys.Date()}')
)



### Retiring Dems
house_votes %>% 
  filter(District %in% (c(6, 56, 87, 43, 20, 31, 65, 3, 35, 72, 100) %>% paste0('H',.)),
         vote == 'NV') %>% 
  group_by(Full.Name) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(fct_reorder(Full.Name, n), n, label = scales::percent_format()(round(n / length(unique(house_votes$bill)), 4)))) +
  geom_bar(stat = 'identity') +
  geom_label() +
  theme_minimal() +
  labs(title = 'Missed Votes by Retiring Democrats', x = '', y = '') +
  theme(axis.text.x = element_text(angle = 90))

### History
history <- kyleg_18_votes %>% 
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

history_nest <- nest(history, -number) %>% 
  mutate(house_committee = map_chr(data, function(d){
    if(T %in% str_detect(d$action, 'WITHDRAWN')){
      'WITHDRAWN'
    }else if(T %in% str_detect(d$action, 'reassigned.*\\(H\\)')){
      d %>% 
        filter(str_detect(action, 'reassigned.*\\(H\\)')) %>% 
        arrange(date) %>% 
        tail(1) %>% 
        mutate(committee = str_replace_all(consolidated_action, 'reassigned to','') %>% 
                 str_replace_all('\\([HS]\\)','') %>% 
                 str_trim()) %>% 
        select(committee) %>% 
        as_vector()
    }else if(T %in% str_detect(d$action, '^to.*\\(H\\)')){
      d %>% 
        filter(str_detect(action, '^to.*\\(H\\)')) %>% 
        arrange(date) %>% 
        head(1) %>% 
        mutate(committee = str_replace_all(consolidated_action, 'to','') %>% 
                 str_replace_all('\\([HS]\\)','') %>% 
                 str_trim()) %>% 
        select(committee) %>% 
        as_vector()
    }else{
      'Not Yet Assigned'
    }
  }),
  house_rpt_favorable = map_lgl(data, function(d){
    d %<>% 
      filter(chamber == 'H')
    T %in% str_detect(d$action, 'reported favorably')
  }),
  house_passed = map_lgl(data, function(d){
    d %<>% 
      filter(chamber == 'H')
    T %in% str_detect(d$action, 'passed (?!over).*')
  }),
  senate_committee = map_chr(data, function(d){
    if(T %in% str_detect(d$action, 'WITHDRAWN')){
      'WITHDRAWN'
    }else if(T %in% str_detect(d$action, 'reassigned.*\\(S\\)')){
      d %>% 
        filter(str_detect(action, 'reassigned.*\\(S\\)')) %>% 
        arrange(date) %>% 
        tail(1) %>% 
        mutate(committee = str_replace_all(consolidated_action, 'reassigned to','') %>% 
                 str_replace_all('\\([HS]\\)','') %>% 
                 str_trim()) %>% 
        select(committee) %>% 
        as_vector()
    }else if(T %in% str_detect(d$action, '^to.*\\(S\\)')){
      d %>% 
        filter(str_detect(action, '^to.*\\(S\\)')) %>% 
        arrange(date) %>% 
        head(1) %>% 
        mutate(committee = str_replace_all(consolidated_action, 'to','') %>% 
                 str_replace_all('\\([HS]\\)','') %>% 
                 str_trim()) %>% 
        select(committee) %>% 
        as_vector()
    }else{
      'Not Yet Assigned'
    }
  }),
  senate_rpt_favorable = map_lgl(data, function(d){
    d %<>% 
      filter(chamber == 'S')
    T %in% str_detect(d$action, 'reported favorably')
  }),
  senate_passed = map_lgl(data, function(d){
    d %<>% 
      filter(chamber == 'S')
    T %in% str_detect(d$action, 'passed (?!over).*')
  }),
  senate_defeated = map_lgl(data, function(d){
    d %<>% 
      filter(chamber == 'S',
             str_detect(action, 'defeated'), 
             str_detect(action, '^((?!amendment).)*$')) 
    nrow(d) > 0
  }),
  house_defeated = map_lgl(data, function(d){
    d %<>% 
      filter(chamber == 'H',
             str_detect(action, 'defeated'), 
             str_detect(action, '^((?!amendment).)*$')) 
    nrow(d) > 0
  }),
  delivered = map_lgl(data, function(d) T %in% str_detect(d$action, 'delivered')))

history %>% 
  mutate(date = ymd(date)) %>% 
  group_by(chamber, date) %>% 
  summarize(n = n()) %>% 
  mutate(w_day = wday(date)) %>% 
  filter(date > '2018-01-01',
         w_day <= 6) %>% 
  mutate(date = 1:length(date),
         cumsum_n = cumsum(n)) %>% 
  ggplot(aes(x = date, y = cumsum_n, color = chamber, label = n)) +
  geom_vline(xintercept = 16) +
  geom_smooth(se = F) +
  geom_label(show.legend = F) +
  geom_text(aes(x=16, y=0, label='Filing Deadline'), size=4, angle=90, vjust=-0.4, hjust=0, color = 'black', show.legend = F) +
  scale_color_manual(values = pal[3:4]) +
  theme_minimal() +
  labs(x = 'Legislative Day', y = 'Cumulative Number of Actions', title = 'Total Actions by Chamber') +
  theme(axis.text.x = element_text(angle = 90))


#### Committee Info
short_committee <- function(committee){
  case_when(committee == "Appropriations & Revenue" ~ 'A&R',
            committee == "Economic Development & Workforce Investment" ~ 'Econ Dev',
            committee == "Elections, Const. Amendments & Intergovernmental Affairs" ~ 'Elections',
            committee == "Licensing, Occupations, & Admin Regs"  ~ 'Licensing & Occupations',
            committee == "Small Business & Information Technology" ~ 'Small Business & IT',
            committee == "Veterans, Military Affairs, and Public Protection" ~ 'Vets & Mil Affairs',
            committee == "Licensing, Occupations, & Administrative Regulations"  ~ 'Licensing & Occupations',
            committee == "Veterans, Military Affairs, & Public Protection" ~ 'Vets & Mil Affairs',
            committee == 'Economic Development, Tourism, and Labor' ~ 'Econ Dev./Tourism/Labor',
            committee == 'Tourism & Outdoor Recreation' ~ 'Tourism & Rec.',
            T ~ committee
  )
}

## House Committee Work
history_nest %>% 
  filter(!house_committee %in% c('WITHDRAWN', 'Not Yet Assigned')) %>%
  group_by(house_committee) %>% 
  summarize(n=n()) %>% 
  left_join(history_nest %>% 
              filter(house_rpt_favorable == T) %>% 
              group_by(house_committee) %>% 
              summarize(fav_report=n()),
            by = 'house_committee') %>%
  left_join(history_nest %>% 
              filter(house_passed == T) %>% 
              group_by(house_committee) %>% 
              summarize(passed_house = n()),
            by = 'house_committee') %>% 
  left_join(history_nest %>% 
              filter(delivered == T) %>% 
              group_by(house_committee) %>% 
              summarize(delivered = n()),
            by = 'house_committee') %>% 
  left_join(history_nest %>% 
              filter(house_defeated == T) %>% 
              group_by(house_committee) %>% 
              summarize(defeated = n()),
            by = 'house_committee') %>% 
  mutate(house_committee = short_committee(house_committee)) %>% 
  replace_na(list(fav_report = 0, passed_house = 0, delivered = 0, defeated = 0)) %>% 
  mutate(house_committee = fct_reorder(house_committee, n),
         passed_house = passed_house - delivered,
         fav_report = fav_report - passed_house - delivered - defeated,
         other_status = n - fav_report - passed_house - delivered - defeated) %>% 
  mutate(passed_house = ifelse(passed_house == 0, NA, passed_house),
         fav_report = ifelse(fav_report == 0, NA, fav_report),
         delivered = ifelse(delivered == 0, NA, delivered),
         defeated = ifelse(defeated == 0, NA, defeated)) %>% 
  select(-n) %>% 
  gather(var, val, -house_committee) %>% 
  mutate(var = factor(var, levels = c('delivered','passed_house','fav_report','other_status', 'defeated'))) %>% 
  ungroup() %>% 
  ggplot(aes(x = house_committee, y = val, label = val, fill = var)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = 'Status',
                    breaks = c('delivered','passed_house','fav_report','other_status', 'defeated'),
                    labels = c('Delivered', 'Passed House', 'Favorable Report', 'Other Status', 'Defeated'),
                    values = pal) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Assignments to Committees', x= '' ,y = '')

## Senate Committee Work
history_nest %>% 
  filter(!senate_committee %in% c('WITHDRAWN', 'Not Yet Assigned', 'Rules')) %>%
  group_by(senate_committee) %>% 
  summarize(n=n()) %>% 
  left_join(history_nest %>% 
              filter(senate_rpt_favorable == T) %>% 
              group_by(senate_committee) %>% 
              summarize(fav_report=n()),
            by = 'senate_committee') %>%
  left_join(history_nest %>% 
              filter(senate_passed == T) %>% 
              group_by(senate_committee) %>% 
              summarize(passed_senate = n()),
            by = 'senate_committee') %>% 
  left_join(history_nest %>% 
              filter(delivered == T) %>% 
              group_by(senate_committee) %>% 
              summarize(delivered = n()),
            by = 'senate_committee') %>% 
  left_join(history_nest %>% 
              filter(senate_defeated == T) %>% 
              group_by(senate_committee) %>% 
              summarize(defeated = n()),
            by = 'senate_committee') %>% 
  mutate(senate_committee = short_committee(senate_committee)) %>% 
  replace_na(list(fav_report = 0, passed_senate = 0, delivered = 0, defeated = 0)) %>% 
  mutate(senate_committee = fct_reorder(senate_committee, n),
         passed_senate = passed_senate - delivered,
         fav_report = fav_report - passed_senate - delivered - defeated,
         other_status = n - fav_report - passed_senate - delivered - defeated) %>% 
  mutate(passed_senate = ifelse(passed_senate == 0, NA, passed_senate),
         fav_report = ifelse(fav_report == 0, NA, fav_report),
         delivered = ifelse(delivered == 0, NA, delivered),
         defeated = ifelse(defeated == 0, NA, defeated)) %>% 
  select(-n) %>% 
  gather(var, val, -senate_committee) %>% 
  mutate(var = factor(var, levels = c('delivered','passed_senate','fav_report','other_status', 'defeated'))) %>% 
  ungroup() %>% 
  ggplot(aes(x = senate_committee, y = val, label = val, fill = var)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(breaks = c('delivered','passed_senate','fav_report','other_status', 'defeated'),
                    labels = c('Delivered', 'Passed Senate', 'Favorable Report', 'Other Status', 'Defeated'),
                    values = pal,
                    name = 'Status') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Assignments to Senate Committees', x= '' ,y = '')



       