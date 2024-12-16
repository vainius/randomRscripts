library(tidyverse)

fraction_overlap <- function(list1, list2) {
  intersect_length <- length(intersect(list1, list2))
  union_length <- length(union(list1, list2))
  if (union_length == 0) {
    return(1)
  }
  return(intersect_length / union_length)
}

#

res_2070_dt <- readRDS("rezultatai_apylinkese_2070.RDS")
res_2150_dt <- readRDS("data/rezultatai_apylinkese_2150_2024-12-16 13:18:00.RDS") %>%
  rename(viso_rinkeju = rinkeju_skaicius)

# join; unikalus raktas: [pav ir adr.]

cols_to_drop <- c('rpl_id', 'adr', 'ter_pavs')

join_dt <- res_2150_dt %>%
  select(-one_of(cols_to_drop)) %>%
  inner_join(select(res_2070_dt, -one_of(cols_to_drop)), 
             by = c('pav', 'x', 'y')) %>%
  mutate_at(c('viso_rinkeju.x', 'viso_rinkeju.y'), as.numeric) %>%
  mutate(ters.x = map2(gatves.x, ter_kodai.x, c),
         ters.y = map2(gatves.y, ter_kodai.y, c)) %>%
  mutate(ter_overlap = map2_dbl(ters.x, ters.y, fraction_overlap))

join_dt %>%
  ggplot(aes(x = viso_rinkeju.x, y = viso_rinkeju.y)) +
    geom_point() +
    coord_fixed(ratio = 1)

join_dt %>%
  mutate(rinkeju_diff = (viso_rinkeju.y - viso_rinkeju.x)/viso_rinkeju.x) %>%
  ggplot(aes(x = rinkeju_diff)) +
  geom_histogram()

# analizei palikti tik teritoriskai daug nepasikeitusias apylinkes
  
regr_dt <- join_dt %>%
  filter(ter_overlap >= 0.5) %>%
  filter(viso_rinkeju.x > 300) %>%
  filter(viso_rinkeju.y > 300) %>%
  mutate(across(`is_viso_3. Lietuvos socialdemokratų partija`:`is_viso_10. Partija „Laisvė ir teisingumas“`, as.numeric)) %>%
  mutate(across(`Gitanas NAUSĖDA`:`Giedrimas JEGLINSKAS`, as.numeric)) %>%
  mutate(across(`is_viso_3. Lietuvos socialdemokratų partija`:`is_viso_10. Partija „Laisvė ir teisingumas“`, ~ . * (1 / viso_rinkeju.x))) %>%
  mutate(across(`Gitanas NAUSĖDA`:`Giedrimas JEGLINSKAS`, ~ . * (1 / viso_rinkeju.y)))

y_vars <- names(regr_dt)[which(names(regr_dt) == "is_viso_3. Lietuvos socialdemokratų partija"):which(names(regr_dt) == "is_viso_10. Partija „Laisvė ir teisingumas“")]
x_vars <- names(regr_dt)[which(names(regr_dt) == "Gitanas NAUSĖDA"):which(names(regr_dt) == "Giedrimas JEGLINSKAS")]

x_vars <- names(sort(colSums(regr_dt[, x_vars]), decreasing = TRUE))
y_vars <- names(sort(colSums(regr_dt[, y_vars]), decreasing = TRUE))

regr_dt <- regr_dt %>%
  select(all_of(x_vars), all_of(y_vars), everything())

# correlations

library(corrplot)

corr_m <- cor(regr_dt[x_vars], regr_dt[y_vars])

col <- colorRampPalette(c("blue", "white", "red"))(300)[50:250]

corrplot(corr_m, method = "circle", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8,
         col = col,
         is.corr = TRUE) 

# run regularized regression

library(glmnet)

run_glmnet <- function(df, x_vars, y_var, weights) {
  x_vars_escaped <- paste0("`", x_vars, "`")
  y_var_escaped <- paste0("`", y_var, "`")
  
  formula <- as.formula(paste(paste(y_var_escaped, paste(x_vars_escaped, collapse = " + "), sep = " ~ "), "+ factor(rpg_pav.y)"))
  model_matrix <- model.matrix(formula, data = df)[, -1]
  response_vector <- df[[y_var]]
  
  if (is.null(weights)) {
    model <- cv.glmnet(model_matrix, response_vector, alpha = 0.95)
  } else {
    model <- cv.glmnet(model_matrix, response_vector, alpha = 0.95, weights = weights)
  }

  return(model)
}

coefficients_list <- list()

for (y_var in y_vars) {
  model <- run_glmnet(regr_dt, x_vars, y_var, weights = regr_dt$viso_rinkeju.x)
  coefficients <- coef(model, s = model$lambda.1se)
  coefficients <- as.matrix(coefficients)
  
  colnames(coefficients) <- y_var
  coefficients_list[[y_var]] <- coefficients

}

coefficients_df <- do.call(cbind, coefficients_list)
coefficients_df <- as.data.frame(coefficients_df)

coefficients_df <- coefficients_df[!grepl("factor\\(rpg_pav.*\\)", rownames(coefficients_df)), ]
coefficients_df <- coefficients_df[!grepl("Intercept", rownames(coefficients_df)), ]
rownames(coefficients_df) <- gsub("`", "", rownames(coefficients_df))
colnames(coefficients_df) <- gsub("is_viso_\\d{1,2}\\. ", "", colnames(coefficients_df))

coefficients_df_sub <- coefficients_df[, c(1:10)]
colnames(coefficients_df_sub)[c(3, 7)] <- c('TS-LKD', 'LLRA-KŠS')

corrplot(as.matrix(coefficients_df_sub), method = "circle", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8,
         col = col,
         is.corr = FALSE,
         cl.pos = 'n') 

title(main = "Sąsaja tarp 2024 m. Prezidento ir Seimo rinkimų rezultatų", cex.main = 1, font.main = 2)
mtext("KOEFICIENTŲ INTEPRETACIJA:\nJei kandidatas apylinkėje surinko 1 p.p. daugiau, kiek p.p. daugiau gaus partija\n(lygiant su apygardos vidurkiu)", 
      side = 1, 
      line = 4, 
      cex = 0.8, 
      font = 3)
