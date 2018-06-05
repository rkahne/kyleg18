library(pscl)
library(wnominate)
library(tidyverse)
library(dummies)

kyleg_18_votes <- readRDS('kyleg_votes20180528 1613.rds')
leg_data <- read_csv('leg_data.csv')
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

# senate_votes %>% select(Initial.Name, bill, vote, District, Party) %>% write_csv('senate_votes.csv')
# senate_votes %>% select(Initial.Name, bill, vote, District, Party) %>% write_csv('senate_votes_wide.csv')
# house_votes %>% select(Initial.Name, bill, vote, District, Party) %>% spread(bill, vote) %>% write_csv('house_votes_wide.csv')
# house_votes %>% select(Initial.Name, bill, vote, District, Party) %>% write_csv('house_votes.csv')
#################### W NOMINATE ########################

house_matrix <- house_votes %>% 
  select(legislator, bill, vote) %>%   
  mutate(vote = case_when(
    vote == 'YEA' ~ 1,
    vote == 'NAY' ~ 6,
    vote == 'NV' ~ 9,
    vote == 'PASS' ~ 9,
    is.na(vote) ~ 9
  )) %>% 
  spread(bill, vote) %>% 
  mutate_at(vars(-legislator), ~if_else(is.na(.), 8, .))

house_votes_m <- as.matrix(select(house_matrix, -legislator))
dimnames(house_votes_m) <- list(house_matrix$legislator, dimnames(house_votes_m)[[2]])

legis.dat <- filter(leg_data_h, LastUnique %in% dimnames(house_votes_m)[[1]]) %>%  
  select(LastUnique, Party, Race, Gender, First.Elected)

legis.dat <- tibble(LastUnique = dimnames(house_votes_m)[[1]]) %>% 
  left_join(leg_data_h, by = 'LastUnique') %>% 
  select(LastUnique, party = Party, Race, Gender, First.Elected)

house_rc <- rollcall(
  data = house_votes_m,
  yea = 1, nay = 6, missing = 9, notInLegis = 8,
  vote.names = subset(names(house_matrix), names(house_matrix) != 'legislator'),
  legis.data = legis.dat,
  desc = 'Kentucky General Assembly, 2018',
  source = 'Robert Kahne'
)

house_wn <- wnominate(house_rc, polarity = c(5, 100)) # 5 = Benvenuti, 100 = York


house_wn_df <- house_wn$legislators
house_gg <- ggplot(house_wn_df, aes(x = coord1D, y = coord1D, color = party)) +
  geom_text(aes(label = LastUnique, angle = 90)) + 
  scale_color_manual(values = c('Democratic' = 'blue', 'Republican' = 'Red')) +
  theme_minimal() +
  scale_y_continuous('') +
  scale_x_continuous('Left-Right, using Benvenuti & York') +
  ggtitle('Ranking Ideology of KY House, 2018') +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())
house_gg

senate_matrix <- senate_votes %>% 
  select(legislator, bill, vote) %>%   
  mutate(vote = case_when(
    vote == 'YEA' ~ 1,
    vote == 'NAY' ~ 6,
    vote == 'NV' ~ 9,
    vote == 'PASS' ~ 9,
    is.na(vote) ~ 9
  )) %>% 
  spread(bill, vote) %>% 
  mutate_at(vars(-legislator), ~if_else(is.na(.), 8, .))

senate_votes_m <- as.matrix(select(senate_matrix, -legislator))
dimnames(senate_votes_m) <- list(senate_matrix$legislator, dimnames(senate_votes_m)[[2]])

legis.dat <- tibble(LastUnique = dimnames(senate_votes_m)[[1]]) %>% 
  left_join(leg_data_s, by = 'LastUnique') %>% 
  select(LastUnique, Party, Race, Gender, First.Elected)

senate_rc <- rollcall(
  data = senate_votes_m,
  yea = 1, nay = 6, missing = 9, notInLegis = 8,
  vote.names = subset(names(senate_matrix), names(senate_matrix) != 'legislator'),
  legis.data = legis.dat,
  desc = 'Kentucky General Assembly, 2018',
  source = 'Robert Kahne'
)

senate_wn <- wnominate(senate_rc, polarity = c(36, 28)) # 36 = Westerfield, 28 = Seum

senate_wn_df <- senate_wn$legislators
senate_gg <- ggplot(senate_wn_df, aes(x = coord1D, y = coord1D, color = Party)) +
  geom_text(aes(label = LastUnique, angle = 90)) + 
  scale_color_manual(values = c('Democratic' = 'blue', 'Republican' = 'Red')) +
  theme_minimal() +
  scale_y_continuous('') +
  scale_x_continuous('Left-Right, using Westerfield & Seum') +
  ggtitle('Ranking Ideology of KY House, 2018') +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())
senate_gg


house_wide <- house_votes %>% 
  select(Initial.Name, bill, vote) %>% 
  spread(bill, vote)


########### ABSENTEEISM #######################

absenteeism_h <- house_votes %>% 
  group_by(legislator, vote) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(legislator) %>% 
  mutate(n_pct = n / sum(n)) %>% 
  filter(vote == 'NV') 

absenteeism_h %>% 
  ggplot(aes(x = fct_reorder(legislator, n_pct), y = n_pct)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))

################## K MEANS CLUSTERING ########################

house_votes_nest <- nest(house_votes, -bill) %>% 
  mutate(vote = map(data, function(x){
    tibble(Initial.Name = house_wide$Initial.Name) %>% 
      left_join(x %>% select(Initial.Name, vote)) %>% 
      replace_na(list(vote = 'NIL'))
  })) %>% 
  mutate(dummies = map2(vote, bill, function(x, b){
    dumb <- x$vote %>% dummy() %>% as_data_frame()
    names(dumb) <- paste0(b, unlist(str_extract_all(names(dumb), '(NV$|NAY$|YEA$|NIL$|PASS$)')))
    dumb$Initial.Name <- x$Initial.Name
    dumb
  }))

cluster_data <- house_votes_nest$dummies %>%
  reduce(full_join) 

wss <- map_dbl(1:15, function(k){
  model <- kmeans(cluster_data %>% select(-Initial.Name), centers = k)
  model$tot.withinss
})

library(cluster)

tibble(k = 1:15, wss = wss) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

avg_sil <- map_dbl(2:15, function(k){
  model <- pam(cluster_data %>% select(-Initial.Name), k = k)
  model$silinfo$avg.width
})

tibble(k = 2:15, wss = avg_sil) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

pam_4 <- pam(cluster_data %>% select(-Initial.Name), k = 4)
kmeans_3 <- kmeans(cluster_data %>% select(-Initial.Name), centers = 3)

clustered_leg <- cluster_data %>% 
  select(Initial.Name) %>% 
  mutate(pam_4 = pam_4$cluster,
         k_3= kmeans_3$cluster)

cluster_data_absenteeism <- clustered_leg %>% 
  filter(k_3 == 3) %>% 
  select(Initial.Name) %>% 
  left_join(cluster_data)

wss <- map_dbl(1:15, function(k){
  model <- kmeans(cluster_data_absenteeism %>% select(-Initial.Name), centers = k)
  model$tot.withinss
})

tibble(k = 1:15, wss = wss) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

avg_sil <- map_dbl(2:15, function(k){
  model <- pam(cluster_data_absenteeism %>% select(-Initial.Name), k = k)
  model$silinfo$avg.width
})

tibble(k = 2:15, wss = avg_sil) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

clustered_leg_a <- cluster_data_absenteeism %>% 
  select(Initial.Name) %>% 
  mutate(cluster = kmeans(cluster_data_absenteeism %>% select(-Initial.Name), centers = 3)$cluster)

# Clustering the Democrats
house_dems <- leg_data %>% 
  filter(Party == 'Democratic',
         Initial.Name %in% cluster_3) %>% 
  select(Initial.Name) %>% 
  left_join(cluster_data)

wss <- map_dbl(1:15, function(k){
  model <- kmeans(house_dems %>% select(-Initial.Name), centers = k)
  model$tot.withinss
})
tibble(k = 1:15, wss = wss) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

avg_sil <- map_dbl(2:15, function(k){
  model <- pam(house_dems %>% select(-Initial.Name), k = k)
  model$silinfo$avg.width
})
tibble(k = 2:15, wss = avg_sil) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

clustered_dems <- house_dems %>% 
  select(Initial.Name) %>% 
  mutate(cluster = kmeans(house_dems %>% select(-Initial.Name), centers = 2)$cluster)


# Clustering the GOP
house_gop <- leg_data %>% 
  filter(Party == 'Republican',
         Initial.Name %in% cluster_3) %>% 
  select(Initial.Name) %>% 
  left_join(cluster_data)

wss <- map_dbl(1:15, function(k){
  model <- kmeans(house_gop %>% select(-Initial.Name), centers = k)
  model$tot.withinss
})
tibble(k = 1:15, wss = wss) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

avg_sil <- map_dbl(2:15, function(k){
  model <- pam(house_gop %>% select(-Initial.Name), k = k)
  model$silinfo$avg.width
})
tibble(k = 2:15, wss = avg_sil) %>% 
  ggplot(aes(k, wss)) +
  geom_line()

house_gop %>% 
  select(Initial.Name) %>% 
  mutate(cluster = kmeans(house_gop %>% select(-Initial.Name), centers = 2)$cluster) %>% View()

#################### BILLS WITH LOW OPPOSITION ##########################
house_votes %>% 
  select(legislator, bill, vote) %>% 
  nest(-bill) %>% 
  mutate(data = map(data, function(x){
      x %>% filter(vote == 'NAY')
    }),
    n_nays = map_dbl(data, nrow)
  ) %>% 
  filter(n_nays < 5, n_nays > 0) %>% 
  unnest() %>% 
  count(legislator) %>% 
  left_join(house_votes %>% select(legislator, Party)) %>% 
  distinct() %>% 
  ggplot(aes(x = fct_reorder(legislator, n), y = n, fill = Party, label = n)) +
  geom_bar(stat = 'identity') +
  geom_label(show.legend = F) +
  scale_fill_manual(values = c('Democratic' = 'blue', 'Republican' = 'red')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Count of NAY votes where opposition was < 5 members',
       x = '', y = '')

senate_votes %>% 
  select(legislator, bill, vote) %>% 
  nest(-bill) %>% 
  mutate(data = map(data, function(x){
    x %>% filter(vote == 'NAY')
  }),
  n_nays = map_dbl(data, nrow)
  ) %>% 
  filter(n_nays < 5, n_nays > 0) %>% 
  unnest() %>% 
  count(legislator) %>% 
  left_join(senate_votes %>% select(legislator, Party)) %>% 
  distinct() %>% 
  ggplot(aes(x = fct_reorder(legislator, n), y = n, fill = Party, label = n)) +
  geom_bar(stat = 'identity') +
  geom_label(show.legend = F) +
  scale_fill_manual(values = c('Democratic' = 'blue', 'Republican' = 'red')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Count of NAY votes where opposition was < 5 members',
       x = '', y = '')

######################### BUDDIES ##########################

nested_votes <- house_votes %>% 
  select(legislator, bill, vote) %>% 
  nest(-legislator)

nested_senate <- senate_votes %>% 
  select(legislator, bill, vote) %>% 
  nest(-legislator)

nested_votes %<>% 
  mutate(others = map(legislator, function(x){
    nested_votes %>% filter(legislator != x)
  })) %>% 
  mutate(buddies = map2(data, others, function(l, o){
    len_l <- l %>% unnest() %>% rename(leg_vote = vote)
    o %>% 
      unnest() %>% 
      left_join(len_l, by = 'bill') %>% 
      filter(vote == leg_vote) %>% 
      count(legislator) %>% 
      mutate(pct_n = n / nrow(len_l))
  }))

nested_senate %<>% 
  mutate(others = map(legislator, function(x){
    nested_senate %>% filter(legislator != x)
  })) %>% 
  mutate(buddies = map2(data, others, function(l, o){
    len_l <- l %>% unnest() %>% rename(leg_vote = vote)
    o %>% 
      unnest() %>% 
      left_join(len_l, by = 'bill') %>% 
      filter(vote == leg_vote) %>% 
      count(legislator) %>% 
      mutate(pct_n = n / nrow(len_l))
  }))

# Not distinct across similar people because not all legislators voted the same number of times.
buddies <- nested_votes %>% 
  select(legislator, buddies) %>% 
  unnest() %>% 
  mutate(legislators = map2_chr(legislator, legislator1, function(l, l1){
    paste(str_sort(c(l,l1)), collapse = ', ')
  }))

buddies_senate <- nested_senate %>% 
  select(legislator, buddies) %>% 
  unnest() %>% 
  mutate(legislators = map2_chr(legislator, legislator1, function(l, l1){
    paste(str_sort(c(l,l1)), collapse = ', ')
  }))

buddies %>% 
  select(-legislators) %>% 
  nest(-legislator) %>% 
  mutate(best_bud = map_chr(data, function(x){
    x %>% 
      filter(pct_n == max(pct_n)) %>% 
      mutate(chr = str_glue('{legislator1}, ({scales::percent_format()(pct_n)})')) %>% 
      select(chr) %>% 
      as_vector() %>% 
      paste(collapse = '; ')
  })) %>% 
  select(-data) %>% 
  View()

buddies_senate %>% 
  select(-legislators) %>% 
  nest(-legislator) %>% 
  mutate(best_bud = map_chr(data, function(x){
    x %>% 
      filter(pct_n == max(pct_n)) %>% 
      mutate(chr = str_glue('{legislator1}, ({scales::percent_format()(pct_n)})')) %>% 
      select(chr) %>% 
      as_vector() %>% 
      paste(collapse = '; ')
  })) %>% 
  select(-data) %>% 
  View()

best_buds <- buddies %>% 
  select(-legislators) %>% 
  nest(-legislator) %>% 
  mutate(best_bud = map(data, function(x){
      x %>% 
        filter(pct_n == max(pct_n)) %>% 
        select(legislator1) %>% 
        as_vector()
    }),
    best_bud_pct = map_dbl(data, ~max(.$pct_n))) %>% 
  select(-data)

best_buds_senate <- buddies_senate %>% 
  select(-legislators) %>% 
  nest(-legislator) %>% 
  mutate(best_bud = map(data, function(x){
    x %>% 
      filter(pct_n == max(pct_n)) %>% 
      select(legislator1) %>% 
      as_vector()
  }),
  best_bud_pct = map_dbl(data, ~max(.$pct_n))) %>% 
  select(-data)

best_buds %>% 
  mutate(is_bud = map_dbl(legislator, function(x){
    best_buds %>% 
      mutate(is_in = str_detect(best_bud, x)) %>% 
      filter(is_in == T) %>% 
      nrow()
  })) %>%
  mutate(best_bud = map_chr(best_bud, ~paste(., collapse = ', '))) %>% View()

best_buds_senate %>% 
  mutate(is_bud = map_dbl(legislator, function(x){
    best_buds %>% 
      mutate(is_in = str_detect(best_bud, x)) %>% 
      filter(is_in == T) %>% 
      nrow()
  })) %>%
  mutate(best_bud = map_chr(best_bud, ~paste(., collapse = ', '))) %>% View()

################### VOTES WITH LEADERSHIP #############################

# Bills where Leadership disagreed.
# Adkins was a NV on two votes, but Wilson Stone voted for both.
disagreement <- nested_votes %>% 
  filter(legislator == 'Adkins') %>% 
  mutate(Osborne = map2(data, others, function(l, o){
    osborne <- o %>% filter(legislator == 'Osborne') %>% unnest() %>% select(bill, o_vote = vote)
    l %>% 
      left_join(osborne, by = 'bill') %>% 
      filter(vote != o_vote)
  })) %>% 
  select(Osborne) %>% 
  unnest() %>% 
  filter(vote != 'NV') %>% 
  rename(dem_lead = vote, gop_lead = o_vote)

disagreement_senate <- nested_senate %>% 
  filter(legislator == 'Jones') %>% 
  mutate(Stivers = map2(data, others, function(l, o){
    stivers <- o %>% filter(legislator == 'Stivers') %>% unnest() %>% select(bill, s_vote = vote)
    l %>% 
      left_join(stivers, by = 'bill') %>% 
      filter(vote != s_vote)
  })) %>% 
  select(Stivers) %>% 
  unnest() %>% 
  filter(vote != 'NV') %>% 
  rename(dem_lead = vote, gop_lead = s_vote)

voted_leadership <- house_votes %>% 
  inner_join(disagreement) %>% 
  mutate(voted_leadership = pmap_lgl(list(Party, vote, dem_lead, gop_lead),
                                     function(p, v, d, r){
                                       if(v == 'NV') NA
                                       else if(p == 'Democratic'){
                                         if_else(v == d, T, F)
                                       }else if(p == 'Republican'){
                                         if_else(v == r, T, F)
                                       }else NA
                                     })) %>% 
  count(legislator, Party, voted_leadership) %>% 
  replace_na(list(voted_leadership = 'NV')) %>% 
  spread(voted_leadership, n) %>% 
  replace_na(list(`FALSE` = 0, NV = 0, `TRUE` = 0)) %>% 
  mutate(pct = pmap_dbl(list(`FALSE`, NV, `TRUE`), function(f, n, t) t / (t + n + f)),
         pct_tf = map2_dbl(`FALSE`, `TRUE`, function(f,t) t / (t + f)))

voted_leadership_senate <- senate_votes %>% 
  inner_join(disagreement_senate) %>% 
  mutate(voted_leadership = pmap_lgl(list(Party, vote, dem_lead, gop_lead),
                                     function(p, v, d, r){
                                       if(v == 'NV') NA
                                       else if(p == 'Democratic'){
                                         if_else(v == d, T, F)
                                       }else if(p == 'Republican'){
                                         if_else(v == r, T, F)
                                       }else NA
                                     })) %>% 
  count(legislator, Party, voted_leadership) %>% 
  replace_na(list(voted_leadership = 'NV')) %>% 
  spread(voted_leadership, n) %>% 
  replace_na(list(`FALSE` = 0, NV = 0, `TRUE` = 0)) %>% 
  mutate(pct = pmap_dbl(list(`FALSE`, NV, `TRUE`), function(f, n, t) t / (t + n + f)),
         pct_tf = map2_dbl(`FALSE`, `TRUE`, function(f,t) t / (t + f)))

ggplot(voted_leadership, aes(x = fct_reorder(legislator, pct_tf), y = pct_tf)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Party, scales = 'free', ncol = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Voted With Leadership', subtitle = 'ONLY bills where Dem/GOP leadership disagreed\nOnly considering bills where legislator voted',
       x = '', y = '')

ggplot(voted_leadership_senate, aes(x = fct_reorder(legislator, pct_tf), y = pct_tf)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Party, scales = 'free', ncol = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Voted With Leadership', subtitle = 'ONLY bills where Dem/GOP leadership disagreed\nOnly considering bills where legislator voted',
       x = '', y = '')

leadership_votes <- nested_votes %>% 
  filter(legislator == 'Adkins') %>% 
  mutate(Osborne = map2(data, others, function(l, o){
    osborne <- o %>% filter(legislator == 'Osborne') %>% unnest() %>% select(bill, o_vote = vote)
    l %>% 
      left_join(osborne, by = 'bill')
  })) %>% 
  select(Osborne) %>% 
  unnest() %>% 
  mutate(if_else(vote == 'NV', 'YEA', 'NAY')) %>% 
  rename(dem_lead = vote, gop_lead = o_vote)

leadership_senate <- nested_senate %>% 
  filter(legislator == 'Jones') %>% 
  mutate(Stivers = map2(data, others, function(l, s){
    stivers <- s %>% filter(legislator == 'Stivers') %>% unnest() %>% select(bill, s_vote = vote)
    l %>% 
      left_join(stivers, by = 'bill')
  })) %>% 
  select(Stivers) %>% 
  unnest() %>% 
  mutate(if_else(vote == 'NV', 'YEA', 'NAY')) %>% 
  rename(dem_lead = vote, gop_lead = s_vote)

voted_leadership_all <- house_votes %>% 
  inner_join(leadership_votes) %>% 
  mutate(voted_leadership = pmap_lgl(list(Party, vote, dem_lead, gop_lead),
                                     function(p, v, d, r){
                                       if(v == 'NV') NA
                                       else if(p == 'Democratic'){
                                         if_else(v == d, T, F)
                                       }else if(p == 'Republican'){
                                         if_else(v == r, T, F)
                                       }else NA
                                     })) %>% 
  count(legislator, Party, voted_leadership) %>% 
  replace_na(list(voted_leadership = 'NV')) %>% 
  spread(voted_leadership, n) %>% 
  replace_na(list(`FALSE` = 0, NV = 0, `TRUE` = 0)) %>% 
  mutate(pct = pmap_dbl(list(`FALSE`, NV, `TRUE`), function(f, n, t) t / (t + n + f)),
         pct_tf = map2_dbl(`FALSE`, `TRUE`, function(f,t) t / (t + f)))

voted_leadership_all_senate <- senate_votes %>% 
  inner_join(leadership_senate) %>% 
  mutate(voted_leadership = pmap_lgl(list(Party, vote, dem_lead, gop_lead),
                                     function(p, v, d, r){
                                       if(v == 'NV') NA
                                       else if(p == 'Democratic'){
                                         if_else(v == d, T, F)
                                       }else if(p == 'Republican'){
                                         if_else(v == r, T, F)
                                       }else NA
                                     })) %>% 
  count(legislator, Party, voted_leadership) %>% 
  replace_na(list(voted_leadership = 'NV')) %>% 
  spread(voted_leadership, n) %>% 
  replace_na(list(`FALSE` = 0, NV = 0, `TRUE` = 0)) %>% 
  mutate(pct = pmap_dbl(list(`FALSE`, NV, `TRUE`), function(f, n, t) t / (t + n + f)),
         pct_tf = map2_dbl(`FALSE`, `TRUE`, function(f,t) t / (t + f)))

ggplot(voted_leadership_all, aes(x = fct_reorder(legislator, pct_tf), y = pct_tf)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Party, scales = 'free', ncol = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Voted With Leadership', subtitle = 'ALL bills\nOnly considering bills where legislator voted',
       x = '', y = '')

ggplot(voted_leadership_all_senate, aes(x = fct_reorder(legislator, pct_tf), y = pct_tf)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Party, scales = 'free', ncol = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Voted With Leadership', subtitle = 'ALL bills\nOnly considering bills where legislator voted',
       x = '', y = '')
