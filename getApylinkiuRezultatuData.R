library(httr)
library(jsonlite)
library(tidyverse)
library(glue)

urlToDataFrame <- function(url, subfield = NULL){
  response <- GET(url)
  data <- content(response, "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    .[['data']]
  
  if (!is.null(subfield)) {
    data <- data[[subfield]]
  }
  
  as.data.frame(data)
}

# rinkimu metaduomenys
meta_dt <- urlToDataFrame("https://www.vrk.lt/statiniai/puslapiai/rinkimai/rt.json")

# pasirinkti rinkimus
rinkimu_id <- 1546
rinkimu_dir <- meta_dt %>% 
  filter(id == rinkimu_id) %>% 
  .[['dir']]

# surinkti aktyvuma
aktyvumo_hash <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}aktyvumas/hash.json"))

aktyvumo_failai <- aktyvumo_hash$resource %>% 
  .[grep("^aktyvumasRpg\\d+$", .)]

res <- list()
for (resource in aktyvumo_failai){
  res[[resource]] <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}aktyvumas/{resource}.json"))
}
aktyvumo_dt <- do.call(rbind, res)

# apylinkiu metadata
rpg_dt <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}rpg.json")) %>%
  rename(rpg_pav = pav,
         rpg_id = id) %>%
  select(rpg_id, rpg_pav) %>%
  distinct()

gatves_dt <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}gatves.json")) %>%
  filter(!is.na(rpl_id)) %>%
  distinct(rpl_id, gatve) %>%
  group_by(rpl_id) %>%
  summarize(gatves = list(gatve))

vietoves_dt <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}vietoves.json")) %>%
  filter(!is.na(ter_rpl_id)) %>%
  group_by(ter_rpl_id) %>%
  summarize(ter_kodai = list(ter_kodas),
            ter_pavs = list(ter_pav))

rpl_dt <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}rpl.json")) %>%
  inner_join(rpg_dt, by = 'rpg_id') %>%
  left_join(gatves_dt, by = join_by(id == rpl_id)) %>%
  left_join(vietoves_dt, by = join_by(id == ter_rpl_id))

# surinkti rezultatus  
res_hash <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}{rinkimu_id}/rezultatai/hash.json"))

res_failai <- res_hash$resource %>% 
  .[grep("^rezultataiVienmRpl\\d+$", .)]

res_rpl <- list()
for (resource in res_failai){
  Sys.sleep(0.03)
  tmp_dt <- urlToDataFrame(glue("https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}{rinkimu_id}/rezultatai/{resource}.json"), subfield = "balsai")
  res_rpl[[resource]] <- mutate(tmp_dt, resource = resource)
}

res_dt <- do.call(rbind, res_rpl) %>%
  filter(!is.na(rknd_id))

# join

rpl_sub <- rpl_dt %>%
  select(id, pav, rpg_pav, adr, x, y, gatves, ter_kodai, ter_pavs)

joined_dt <- res_dt %>%
  mutate(rpl_id = str_extract(resource, "\\d+"),
         rinkimu_id = rinkimu_id) %>%
  select(rinkimu_id, rpl_id, viso_rinkeju, kandidatas, is_viso) %>%
  inner_join(rpl_sub, by = join_by(rpl_id == id)) %>%
  pivot_wider(names_from = kandidatas,
              values_from = is_viso)

if (nrow(joined_dt) == length(res_failai)) {
  message("all good")
} else {
  message("kažkas pasimetė")
}

saveRDS(joined_dt, glue('rezultatai_apylinkese_{rinkimu_id}.RDS'))
