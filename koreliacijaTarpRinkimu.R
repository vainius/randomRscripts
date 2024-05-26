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

res_1546_dt <- readRDS("rezultatai_apylinkese_1546.RDS")
res_2070_dt <- readRDS("rezultatai_apylinkese_2070.RDS")

# join; unikalus raktas: [pav ir adr.]

cols_to_drop <- c('rpl_id', 'adr', 'ter_pavs')

join_dt <- res_1546_dt %>%
  select(-one_of(cols_to_drop)) %>%
  inner_join(select(res_2070_dt, -one_of(cols_to_drop)), 
             by = c('rpg_pav', 'pav', 'x', 'y')) %>%
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
  mutate(across(`Ingrida ŠIMONYTĖ.x`:`Valentinas MAZURONIS`, as.numeric)) %>%
  mutate(across(`Gitanas NAUSĖDA.y`:`Giedrimas JEGLINSKAS`, as.numeric)) %>%
  mutate(across(`Ingrida ŠIMONYTĖ.x`:`Valentinas MAZURONIS`, ~ . * (1 / viso_rinkeju.x))) %>%
  mutate(across(`Gitanas NAUSĖDA.y`:`Giedrimas JEGLINSKAS`, ~ . * (1 / viso_rinkeju.y)))

x_vars <- names(regr_dt)[which(names(regr_dt) == "Ingrida ŠIMONYTĖ.x"):which(names(regr_dt) == "Valentinas MAZURONIS")]
y_vars <- names(regr_dt)[which(names(regr_dt) == "Gitanas NAUSĖDA.y"):which(names(regr_dt) == "Giedrimas JEGLINSKAS")]

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

run_glmnet <- function(df, x_vars, y_var, weights) {
  x_vars_escaped <- paste0("`", x_vars, "`")
  y_var_escaped <- paste0("`", y_var, "`")
  
  formula <- as.formula(paste(paste(y_var_escaped, paste(x_vars_escaped, collapse = " + "), sep = " ~ "), "+ factor(rpg_pav)"))
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

coefficients_df <- coefficients_df[!grepl("factor\\(rpg_pav\\)", rownames(coefficients_df)), ]
coefficients_df <- coefficients_df[!grepl("Intercept", rownames(coefficients_df)), ]
rownames(coefficients_df) <- gsub("`", "", rownames(coefficients_df))

corrplot(as.matrix(coefficients_df), method = "circle", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8,
         col = col,
         is.corr = FALSE,
         cl.pos = 'n') 

# run regression

run_regression <- function(df, x_vars, y_var) {
  x_vars_escaped <- paste0("`", x_vars, "`")
  y_var_escaped <- paste0("`", y_var, "`")
  formula <- as.formula(paste(paste(y_var_escaped, paste(x_vars_escaped, collapse = " + "), sep = " ~ "), "+ factor(rpg_pav) - 1"))
  lm(formula, data = df)
}

coefficients_list <- list()
for (y_var in y_vars) {
  model <- run_regression(regr_dt, x_vars, y_var)
  coefficients <- coef(model)
  coefficients_list[[y_var]] <- coefficients
}

coefficients_df <- do.call(rbind, coefficients_list)
coefficients_df <- as.data.frame(t(coefficients_df))

coefficients_df <- coefficients_df[!grepl("factor\\(rpg_pav\\)", rownames(coefficients_df)), ]

rownames(coefficients_df) <- gsub("`", "", rownames(coefficients_df))
colnames(coefficients_df) <- y_vars

corrplot(as.matrix(coefficients_df), method = "circle", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8,
         col = col,
         is.corr = FALSE) 

# debug V. Mazuronis

regr_dt %>%
  ggplot(aes(x = `Valentinas MAZURONIS`, y = `Gitanas NAUSĖDA.y`)) +
  geom_point() +
  geom_smooth()

regr_dt %>%
  filter(`Valentinas MAZURONIS` == 0) %>% View()

debug_form <- as.formula(paste(paste('`Gitanas NAUSĖDA.y`', 
                                     paste(paste0("`", x_vars, "`"), collapse = " + "), 
                                     sep = " ~ "), "+ factor(rpg_pav) - 1"))

regr_dt %>%
  lm(formula = debug_form) %>%
  summary()
