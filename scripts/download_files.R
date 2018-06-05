library(tabulizer)
library(tidyverse)
library(jsonlite)
library(magrittr)

kyleg_18 <- read_rds('legiscan_18.rds')

files <- kyleg_18 %>% filter(str_detect(number, '^S')) %>% select(number)
# files <- kyleg_18 %>% filter(str_detect(number, '^H')) %>% select(number)
fail <- F

walk(files, function(txt){
  tryCatch({
    download.file(paste0('http://www.lrc.ky.gov/record/18RS/',txt,'/vote_history.pdf'), paste0('./raw_text/',txt,'.pdf'), mode = 'wb')
    close(gzfile(paste0('./raw_text/',txt,'.pdf')))
  },warning = function(w){
    fail <<- T
  }, error = function(e){
    fail <<- T
  })
})

download.file(paste0('http://www.lrc.ky.gov/record/18RS/','SB151','/vote_history.pdf'), paste0('./raw_text/','SB151','.pdf'), mode = 'wb')

for(i in 1:nrow(files)){
  tryCatch({
    download.file(paste0('http://www.lrc.ky.gov/record/18RS/',files$number[i],'/vote_history.pdf'), paste0('./raw_text/',files$number[i],'.pdf'), mode = 'wb')
    close(gzfile(paste0('./raw_text/',txt,'.pdf')))
  },warning = function(w){
    fail <<- T
  }, error = function(e){
    fail <<- T
  })
}
