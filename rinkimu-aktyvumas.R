library(httr)
library(jsonlite)
library(tidyverse)
library(glue)

# rinkimu metaduomenys

response <- GET("https://www.vrk.lt/statiniai/puslapiai/rinkimai/rt.json")
meta_dt <- content(response, "text", encoding = "UTF-8") %>%
  fromJSON() %>%
  .[['data']] %>%
  as.data.frame()

# gauti pasirinktus duomenis

rinkimu_dirs <- c("/904/1/", "/1504/1/")

res <- list()
for (rinkimu_dir in rinkimu_dirs){
  
  url <- "https://www.vrk.lt/statiniai/puslapiai/rinkimai{rinkimu_dir}aktyvumas/aktyvumasVrt.json"
  response <- GET(glue(url))
  
  aktyvumo_dt <- content(response, "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    .[['data']] %>%
    as.data.frame()
  
  res[[rinkimu_dir]] <- aktyvumo_dt %>%
    filter(is.na(rpg_id)) %>%
    select(isanksto:val20) %>%
    pivot_longer(-isanksto, names_to = 'valanda', values_to = 'aktyvumas') %>%
    mutate(dir = rinkimu_dir)
  
}

# paruosti ir nubrezti

meta_names <- meta_dt %>%
  select(vrt_pav, dir) %>%
  distinct() %>%
  mutate(vrt_pav = if_else(vrt_pav == '2019 m. gegužės 12 d. Respublikos Prezidento rinkimai (I turas)', 
                           paste(vrt_pav, 'ir referendumai*'),
                           vrt_pav))

prep_dt <- do.call(rbind, res) %>%
  inner_join(meta_names, by = 'dir') %>%
  mutate(valanda = as.numeric(sub("val", "", valanda))) %>%
  mutate(aktyvumas = as.numeric(if_else(aktyvumas == 0, NA, aktyvumas)) + as.numeric(isanksto))

prep_dt %>%
  ggplot(aes(x = valanda, 
             y = aktyvumas, 
             group = vrt_pav, 
             colour = vrt_pav)) +
    geom_path() + 
    geom_point() +
  scale_x_continuous(
    breaks = seq(8, 20, by = 2),
    minor_breaks = seq(8, 20, by = 1),
    labels = seq(8, 20, by = 2)
    ) +
  scale_y_continuous(
    breaks = seq(10, 60, by = 5),
    minor_breaks = seq(10, 60, by = 1),
    labels = seq(10, 60, by = 5)
  ) +
  expand_limits(y = 60) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.direction = "vertical") +
  ylab('balsavusių %')
