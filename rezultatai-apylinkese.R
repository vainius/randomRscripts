library(httr)
library(jsonlite)
library(tidyverse)
library(glue)

urlToDataFrame <- function(url){
  response <- GET(url)
  content(response, "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    .[['data']] %>%
    as.data.frame()  
}

# rinkimu metaduomenys

meta_dt <- urlToDataFrame("https://www.vrk.lt/statiniai/puslapiai/rinkimai/rt.json")

# surinkti aktyvuma

rinkimu_dir <- '/1504/1/'
aktyvumo_hash <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}aktyvumas/hash.json"))

aktyvumo_failai <- aktyvumo_hash$resource %>% 
  .[grep("^aktyvumasRpg\\d+$", .)]

res <- list()
for (resource in aktyvumo_failai){
  res[[resource]] <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}aktyvumas/{resource}.json"))
}

aktyvumo_dt <- do.call(rbind, res)

# surinkti rezultatus  

some_id <- 2070
res_hash <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}{some_id}/rezultatai/hash.json"))
res_failai <- res_hash$resource %>% 
  .[grep("^rezultataiVienmRpl\\d+$", .)]

urlToDataFrame2 <- function(url){
  response <- GET(url)
  content(response, "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    .[['data']] %>%
    .[['balsai']] %>%
    as.data.frame()  
}

res_rpl <- list()
for (resource in res_failai){
   tmp_dt <- urlToDataFrame2(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}{some_id}/rezultatai/{resource}.json"))
   res_rpl[[resource]] <- mutate(tmp_dt, resource = resource)
}

res_dt <- do.call(rbind, res_rpl)

#

res_dt <- res_dt %>%
  mutate(resource = str_extract(resource, "\\d+"))

indicator_dt <- res_dt %>%
  group_by(resource) %>%
  summarize(sum_is_viso = sum(as.numeric(is_viso))) %>%
  mutate(atsiunte = sum_is_viso > 0)

indicator_join <- indicator_dt %>%
  inner_join(aktyvumo_dt, by = join_by(resource == rpl_id))

#

indicator_join %>%
  mutate(rinkeju_skaicius = as.numeric(rinkeju_skaicius)) %>%
  mutate(atsiunte = if_else(atsiunte, 'Suskaičiavusios', 'Nesuskaičiavusios')) %>%
  ggplot(aes(x = rinkeju_skaicius)) +
    geom_histogram() +
    facet_wrap(~atsiunte, ncol = 1, scales = 'fixed') +
    ggtitle('Suskaičiavusių ir nesuskaičiavusių balsus apylinkių dydžių palyginimas') +
    theme_bw() +
    xlab('Rinkėjų skaičius apylinkėje') +
    ylab('Apylinkių skaičius')

indicator_join %>%
  group_by(atsiunte) %>%
  summarize(mean(as.numeric(rinkeju_skaicius)),
            n())
