#!/usr/bin/env Rscript
# Scripts/run_analysis.R
# Standalone analytical pipeline: runs MCA, PCA, HCPC, MANOVA, and Multinomial Logit models
# Caches lightweight summaries and coordinates to cache/

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(haven)
  library(FactoMineR)
  library(factoextra)
  library(nnet)
  library(emmeans)
})

message("[1/5] Loading and recoding survey data...")
source(here("Scripts", "recode.dat.R"))
dat <- recode.dat()

# Define complete case analysis sample (N = 1,211)
photos <- c("landscape", "car_crash", "girl_with_cat", "pregnant_woman", "vase",
            "woman_breastfeeding", "metal_structure", "homeless_arguing", "fruits_vegetables",
            "sunset_over_sea", "weaver_at_loom", "folk_dance", "rope", "butcher",
            "bark_of_tree", "famous_monument", "scrapyard", "first_communion", "wounded_man", "snake")

pca_vars <- c("artpol_y", "artmoral_y", "artmoral_n", "artpol_n", 
              "cancel_y", "avoidpol_y", "distinction_y", "aesthetic_y", 
              "serious_y", "ironic_y")

covars_num <- c("age", "income", "educ", "peduc", "arts", "city", "relig")
covars_fac <- c("race.f", "sex.f", "gend.f", "educ.f", "arts.f", "inc.f", "age.f",
                 "spol.f", "epol.f", "peduc.f", "city.f", "relig.f")

dat_analysis <- dat |>
  dplyr::select(all_of(c(covars_num, covars_fac, photos, pca_vars))) |>
  # Strip haven_labelled attributes so numeric summaries work under current vctrs
  dplyr::mutate(dplyr::across(dplyr::all_of(pca_vars), ~ as.numeric(as.vector(.)))) |>
  na.omit()

message(sprintf("Complete analysis sample: N = %d respondents", nrow(dat_analysis)))

dir.create(here("cache"), showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. Stage 1: Multiple Correspondence Analysis (MCA) of Photographs
# ==============================================================================
message("[2/5] Running Multiple Correspondence Analysis (MCA)...")

# Prepare dataframe with active photo factors (1:20) and supplementary demographics (21:32)
dat_mca <- dat_analysis |>
  dplyr::select(all_of(photos), all_of(covars_fac))

mca_res <- MCA(dat_mca, quali.sup = 21:32, graph = FALSE)

# Extract individual MCA coordinates
mca_ind_coords <- as.data.frame(mca_res$ind$coord[, 1:2])
colnames(mca_ind_coords) <- c("mca_dim1", "mca_dim2")

# MCA MANOVA Model
mca_manova_df <- bind_cols(dat_analysis, mca_ind_coords)
fit_mca_manova <- manova(
  cbind(mca_dim1, mca_dim2) ~ educ.f + peduc.f + race.f + spol.f + epol.f + relig.f + age + income + sex.f + gend.f + city.f + arts.f,
  data = mca_manova_df
)

mca_manova_summary <- summary(fit_mca_manova, test = "Pillai")
saveRDS(mca_res, here("cache", "mca_res.rds"))
saveRDS(fit_mca_manova, here("cache", "fit_mca_manova.rds"))

# ==============================================================================
# 3. Stage 2: Principal Component Analysis (PCA) of Abstract Standards
# ==============================================================================
message("[3/5] Running Principal Component Analysis (PCA)...")

dat_pca_df <- dat_analysis |>
  dplyr::select(
    all_of(covars_num),
    all_of(covars_fac),
    all_of(pca_vars)
  )

# Active variables are 20:29; quali.sup: 8:19; quanti.sup: 1:7
pca_res <- PCA(
  dat_pca_df,
  graph = FALSE,
  scale.unit = TRUE,
  quali.sup = 8:19,
  quanti.sup = 1:7
)

# Extract individual PCA coordinates
pca_ind_coords <- as.data.frame(pca_res$ind$coord[, 1:3])
colnames(pca_ind_coords) <- c("pca_dim1", "pca_dim2", "pca_dim3")

# PCA MANOVA Model
pca_manova_df <- bind_cols(dat_analysis, pca_ind_coords)
fit_pca_manova <- manova(
  cbind(pca_dim1, pca_dim2, pca_dim3) ~ educ.f + peduc.f + race.f + spol.f + epol.f + relig.f + age + income + sex.f + gend.f + city.f + arts.f,
  data = pca_manova_df
)

saveRDS(pca_res, here("cache", "pca_res.rds"))
saveRDS(fit_pca_manova, here("cache", "fit_pca_manova.rds"))

# ==============================================================================
# 4. Stage 3: Cross-Model Correlations & HCPC Clustering
# ==============================================================================
message("[4/5] Computing Cross-Model Correlations and HCPC Typology...")

# Pearson correlation matrix between MCA (2 dims) and PCA (3 dims)
corr_matrix <- cor(mca_ind_coords, pca_ind_coords)
corr_pvals <- matrix(NA, nrow = 2, ncol = 3)
for (i in 1:2) {
  for (j in 1:3) {
    corr_pvals[i, j] <- cor.test(mca_ind_coords[, i], pca_ind_coords[, j])$p.value
  }
}
saveRDS(list(cor = corr_matrix, p = corr_pvals), here("cache", "corr_mca_pca.rds"))

# Hierarchical Clustering on Principal Components (HCPC)
hcpc_res <- HCPC(pca_res, nb.clust = 4, kk = Inf, graph = FALSE)

# Extract cluster data and bind PCA coordinates
clust_df <- hcpc_res$data.clust
clust_df$Dim.1 <- pca_res$ind$coord[, 1]
clust_df$Dim.2 <- pca_res$ind$coord[, 2]
clust_df$Dim.3 <- pca_res$ind$coord[, 3]

hcpc_dat <- clust_df |>
  mutate(
    Cluster_Group = factor(case_when(
      clust == "1" ~ "The Pure Aestheticists",
      clust == "2" ~ "The Selective Autonomists",
      clust == "3" ~ "The Consistent Anti-Autonomists",
      clust == "4" ~ "The Moralized Engaged Humanists",
      TRUE ~ paste("Cluster", clust)
    ), levels = c(
      "The Pure Aestheticists",
      "The Selective Autonomists",
      "The Consistent Anti-Autonomists",
      "The Moralized Engaged Humanists"
    ))
  )

saveRDS(hcpc_res, here("cache", "hcpc_res.rds"))
saveRDS(hcpc_dat, here("cache", "hcpc_dat.rds"))

# ==============================================================================
# 5. Multinomial Logistic Regression Modeling
# ==============================================================================
message("[5/5] Fitting Multinomial Logistic Regression Model...")

model_data <- bind_cols(
  dat_analysis,
  Cluster_Group = hcpc_dat$Cluster_Group,
  mca_ind_coords,
  pca_ind_coords
)

# Reference level: The Pure Aestheticists
model_data$Cluster_Group <- relevel(model_data$Cluster_Group, ref = "The Pure Aestheticists")

fit_multinom <- multinom(
  Cluster_Group ~ age + I(age^2) + income + I(income^2) + race.f + educ.f + peduc.f + city.f + arts.f + spol.f + epol.f + relig.f + gend.f + sex.f,
  data = model_data,
  trace = FALSE
)

saveRDS(fit_multinom, here("cache", "fit_multinom.rds"))
saveRDS(model_data, here("cache", "model_data.rds"))

message("Analytical pipeline complete! All model objects cached to cache/.")
