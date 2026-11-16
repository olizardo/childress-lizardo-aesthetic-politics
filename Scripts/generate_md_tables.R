#!/usr/bin/env Rscript
# Scripts/generate_md_tables.R
# Standalone table generation pipeline: produces standardized markdown tables in cache/
# Conforms strictly to APA 7th and Socius formatting standards

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(FactoMineR)
})

dir.create(here("cache"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Tabs"), showWarnings = FALSE, recursive = TRUE)

message("[1/7] Loading cached model objects and data...")
model_data <- readRDS(here("cache", "model_data.rds"))
fit_mca_manova <- readRDS(here("cache", "fit_mca_manova.rds"))
fit_pca_manova <- readRDS(here("cache", "fit_pca_manova.rds"))
corr_data <- readRDS(here("cache", "corr_mca_pca.rds"))
hcpc_dat <- readRDS(here("cache", "hcpc_dat.rds"))

format_pval <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

clean_labels_map <- c(
  "educ.f" = "Education (Highest Degree)",
  "peduc.f" = "Parent's Education",
  "race.f" = "Ethnoracial ID",
  "spol.f" = "Pol. Ideology (Social)",
  "epol.f" = "Pol. Ideology (Econ.)",
  "relig.f" = "Religiosity",
  "age" = "Age (Continuous)",
  "income" = "Household Income",
  "sex.f" = "Sexual ID",
  "gend.f" = "Gender ID",
  "city.f" = "City Size",
  "arts.f" = "Childhood Arts Exposure",
  "Residuals" = "Residuals"
)

# ==============================================================================
# Table 1: Descriptive Statistics (cache/table1_desc.md)
# ==============================================================================
message("[2/7] Generating Table 1: Sample Descriptive Statistics...")

desc_rows <- list()

# Numeric summary helper
add_num <- function(varname, label) {
  x <- model_data[[varname]]
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  data.frame(
    Variable = label,
    `Summary / Categories` = sprintf("%.2f (%.2f) [%.1f, %.1f]", m, s, mn, mx),
    check.names = FALSE
  )
}

# Factor summary helper
add_fac <- function(varname, label) {
  x <- model_data[[varname]]
  tbl <- table(x)
  pcts <- prop.table(tbl) * 100
  header_row <- data.frame(
    Variable = label,
    `Summary / Categories` = "",
    check.names = FALSE
  )
  sub_rows <- do.call(rbind, lapply(names(tbl), function(lvl) {
    data.frame(
      Variable = paste0("  ", lvl),
      `Summary / Categories` = sprintf("%d (%.1f%%)", tbl[lvl], pcts[lvl]),
      check.names = FALSE
    )
  }))
  rbind(header_row, sub_rows)
}

t1_list <- list(
  add_num("age", "Age (Years)"),
  add_num("income", "Family Income (1-12 Scale)"),
  add_fac("educ.f", "Educational Attainment (Factor)"),
  add_fac("peduc.f", "Parent's Education (Factor)"),
  add_fac("race.f", "Ethnoracial Identity"),
  add_fac("gend.f", "Gender Identity"),
  add_fac("sex.f", "Sexual Identity"),
  add_fac("spol.f", "Political Ideology (Social)"),
  add_fac("epol.f", "Political Ideology (Economic)"),
  add_fac("relig.f", "Religious Attendance"),
  add_fac("city.f", "City Size / Urbanicity"),
  add_fac("arts.f", "Childhood Arts Exposure"),
  add_num("artpol_y", "Art Clear Political POV (1-7)"),
  add_num("artmoral_y", "Art Clear Moral POV (1-7)"),
  add_num("artmoral_n", "Art Moral Reflexivity (1-7)"),
  add_num("artpol_n", "Art Political Reflexivity (1-7)"),
  add_num("cancel_y", "Wrong to Appreciate Transgressive Artists (1-7)"),
  add_num("avoidpol_y", "Avoid Opposing Political Views (1-7)"),
  add_num("distinction_y", "Prefer Unique/Distinct Taste (1-7)"),
  add_num("aesthetic_y", "Enjoy Difficult/Challenging Art (1-7)"),
  add_num("serious_y", "Best Comedy Says Something (1-7)"),
  add_num("ironic_y", "Ironic Cultural Consumption (1-5)")
)

tab1_df <- do.call(rbind, t1_list)
colnames(tab1_df) <- c("Variable", "N = 1,211")

write_md_table <- function(df, filepath) {
  lines <- c(
    paste0("| ", paste(colnames(df), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  )
  writeLines(lines, filepath)
}

write_md_table(tab1_df, here("cache", "table1_desc.md"))

# ==============================================================================
# Table 2: MANOVA MCA Space (cache/table2_manova_mca.md)
# ==============================================================================
message("[3/7] Generating Table 2: MANOVA in MCA Space...")

mca_stats <- as.data.frame(summary(fit_mca_manova, test = "Pillai")$stats)
mca_stats$Predictor <- clean_labels_map[rownames(mca_stats)]
mca_stats$Predictor[is.na(mca_stats$Predictor)] <- rownames(mca_stats)[is.na(mca_stats$Predictor)]
mca_stats$`Pr(>F)` <- sapply(mca_stats$`Pr(>F)`, format_pval)

pred_sorted_mca <- mca_stats |>
  dplyr::filter(rownames(mca_stats) != "Residuals") |>
  arrange(desc(`approx F`))

tab2_df <- pred_sorted_mca |>
  dplyr::select(
    `Predictor` = Predictor,
    `df` = Df,
    `Pillai's V` = Pillai,
    `Approx. F` = `approx F`,
    `Num df` = `num Df`,
    `Den df` = `den Df`,
    `p` = `Pr(>F)`
  ) |>
  mutate(
    `Pillai's V` = sprintf("%.4f", `Pillai's V`),
    `Approx. F` = sprintf("%.2f", `Approx. F`),
    `Num df` = as.character(`Num df`),
    `Den df` = as.character(`Den df`)
  )

write_md_table(tab2_df, here("cache", "table2_manova_mca.md"))

# ==============================================================================
# Table 3: Pearson Correlations MCA vs PCA (cache/table3_pca_mca_corr.md)
# ==============================================================================
message("[4/7] Generating Table 3: Cross-Model Correlations...")

tab3_df <- data.frame(
  `MCA Dimension (Practical Taste)` = c(
    "Dimension 1 (Aestheticized Engagement vs. Indifference)",
    "Dimension 2 (Highbrow Asceticism vs. Popular Conventionality)"
  ),
  `Dim 1: Politicized Moral` = sprintf("%.3f%s", corr_data$cor[, 1], ifelse(corr_data$p[, 1] < 0.001, "***", "")),
  `Dim 2: Autonomist Cancel` = sprintf("%.3f%s", corr_data$cor[, 2], ifelse(corr_data$p[, 2] < 0.001, "***", "")),
  `Dim 3: Formalist Pure` = sprintf("%.3f%s", corr_data$cor[, 3], ifelse(corr_data$p[, 3] < 0.001, "***", "")),
  check.names = FALSE
)

write_md_table(tab3_df, here("cache", "table3_pca_mca_corr.md"))

# ==============================================================================
# Table 4: MANOVA PCA Space (cache/table4_manova_pca.md)
# ==============================================================================
message("[5/7] Generating Table 4: MANOVA in PCA Space...")

pca_stats <- as.data.frame(summary(fit_pca_manova, test = "Pillai")$stats)
pca_stats$Predictor <- clean_labels_map[rownames(pca_stats)]
pca_stats$Predictor[is.na(pca_stats$Predictor)] <- rownames(pca_stats)[is.na(pca_stats$Predictor)]
pca_stats$`Pr(>F)` <- sapply(pca_stats$`Pr(>F)`, format_pval)

pred_sorted_pca <- pca_stats |>
  dplyr::filter(rownames(pca_stats) != "Residuals") |>
  arrange(desc(`approx F`))

tab4_df <- pred_sorted_pca |>
  dplyr::select(
    `Predictor` = Predictor,
    `df` = Df,
    `Pillai's V` = Pillai,
    `Approx. F` = `approx F`,
    `Num df` = `num Df`,
    `Den df` = `den Df`,
    `p` = `Pr(>F)`
  ) |>
  mutate(
    `Pillai's V` = sprintf("%.4f", `Pillai's V`),
    `Approx. F` = sprintf("%.2f", `Approx. F`),
    `Num df` = as.character(`Num df`),
    `Den df` = as.character(`Den df`)
  )

write_md_table(tab4_df, here("cache", "table4_manova_pca.md"))

# ==============================================================================
# Table 5: HCPC Cluster Profiles (cache/table5_hcpc_clusters.md)
# ==============================================================================
message("[6/7] Generating Table 5: Aesthetic Cluster Profiles...")

tab5_summary <- hcpc_dat |>
  group_by(Cluster_Group) |>
  summarize(
    N = n(),
    `Share (%)` = sprintf("%.1f%%", n() / nrow(hcpc_dat) * 100),
    `Dim 1 (Moral)` = sprintf("%.2f", mean(Dim.1)),
    `Dim 2 (Cancel)` = sprintf("%.2f", mean(Dim.2)),
    `Dim 3 (Pure)` = sprintf("%.2f", mean(Dim.3)),
    .groups = "drop"
  ) |>
  rename(`Aesthetic Cluster` = Cluster_Group)

write_md_table(tab5_summary, here("cache", "table5_hcpc_clusters.md"))

# ==============================================================================
# Appendices: Table A1 (Questions) & Table A2 (Photos)
# ==============================================================================
message("[7/7] Generating Table A1 (Question Wording) & Table A2 (Photos)...")

tab_a1 <- data.frame(
  `Item` = paste("Item", 1:10),
  `Survey Question Wording` = c(
    "The best art has a clear political point of view",
    "The best art has a clear moral point of view",
    "The best art makes us think about what is right and wrong in new ways",
    "The best art doesn't pick political sides and makes us think about politics in new ways",
    "It's wrong to appreciate music or art created by people who act poorly or hold objectionable beliefs",
    "I avoid engaging with art and music created by those who publicly endorse political candidates and policies that I do not support",
    "I like it when my tastes are different or unique from other people",
    "On a scale from 'entirely entertaining and enjoyable' (1) to 'entirely difficult and challenging' (7), what kind of art and media do you like?",
    "Thinking about comedy, on a scale from 'The only goal is to create laughter' (1) to 'The only goal is to say something' (7), what do you think the goal is for the best comedy?",
    "How frequently do you ironically consume music, TV shows, or movies? (i.e., consume something to laugh at it, or because it's 'so bad it's good')"
  ),
  `Scale Endpoints (1, 4, 7)` = c(
    "1: Strongly disagree, 4: Neither agree nor disagree, 7: Strongly agree",
    "1: Strongly disagree, 4: Neither agree nor disagree, 7: Strongly agree",
    "1: Strongly disagree, 4: Neither agree nor disagree, 7: Strongly agree",
    "1: Strongly disagree, 4: Neither agree nor disagree, 7: Strongly agree",
    "1: Strongly disagree, 4: Neither agree nor disagree, 7: Strongly agree",
    "1: Strongly disagree, 4: Neither agree nor disagree, 7: Strongly agree",
    "1: Strongly disagree, 4: Neither agree nor disagree, 7: Strongly agree",
    "1: Entirely entertaining/enjoyable, 4: Equally balanced, 7: Entirely difficult/challenging",
    "1: Only goal is laughter, 4: Equal goals, 7: Only goal is to say something important",
    "1: Never at all, 3: Sometimes, 5: Very frequently"
  ),
  `Theoretical Construct` = c(
    "Politicized Aesthetic Content",
    "Moralized Aesthetic Content",
    "Moral Reflexivity / Ethical Engagement",
    "Political Pluralism / Content Autonomy",
    "Creator Moral Censure / Cancel Culture",
    "Political Polarization Avoidance",
    "Aesthetic Distinction / Uniqueness",
    "Highbrow Intellectual Formalism",
    "Serious vs. Popular Comedy",
    "Ironic / Detached Consumption"
  ),
  check.names = FALSE
)

write_md_table(tab_a1, here("cache", "tableA1_questions.md"))

# Table A2: Photos
photos <- c("landscape", "car_crash", "girl_with_cat", "pregnant_woman", "vase",
            "woman_breastfeeding", "metal_structure", "homeless_arguing", "fruits_vegetables",
            "sunset_over_sea", "weaver_at_loom", "folk_dance", "rope", "butcher",
            "bark_of_tree", "famous_monument", "scrapyard", "first_communion", "wounded_man", "snake")

photo_display <- c("Landscape", "Car Crash", "Girl with a Cat", "Pregnant Woman", "Flower Vase",
                   "Woman Breastfeeding", "Metal Structure", "Homeless People Arguing", "Fruits and Vegetables",
                   "Sunset over the Sea", "Weaver at a Loom", "Folk Dance", "Rope", "Butcher",
                   "Tree Bark", "Historical Monument", "Scrapyard", "First Communion", "Wounded Man", "Snake")

tab_a2 <- do.call(rbind, lapply(seq_along(photos), function(i) {
  v <- photos[i]
  lbl <- photo_display[i]
  x <- model_data[[v]]
  t_c <- table(factor(x, levels = c("Be", "In", "Ug", "Me")))
  t_p <- prop.table(t_c) * 100
  data.frame(
    `Photograph Subject` = lbl,
    `Beautiful (Be)` = sprintf("%.1f%% (%d)", t_p["Be"], t_c["Be"]),
    `Interesting (In)` = sprintf("%.1f%% (%d)", t_p["In"], t_c["In"]),
    `Ugly (Ug)` = sprintf("%.1f%% (%d)", t_p["Ug"], t_c["Ug"]),
    `Meaningless (Me)` = sprintf("%.1f%% (%d)", t_p["Me"], t_c["Me"]),
    check.names = FALSE
  )
}))

write_md_table(tab_a2, here("cache", "tableA2_photos.md"))

# ==============================================================================
# Table A3: Full Multinomial Logistic Regression Estimates (cache/tableA3_multinom_reg.md)
# ==============================================================================
message("[8/8] Generating Table A3: Full Multinomial Logistic Regression Estimates...")
fit_multinom <- readRDS(here("cache", "fit_multinom.rds"))
# nnet >= 7.3-20 no longer registers summary.multinom as an S3 method
s_multi <- nnet:::summary.multinom(fit_multinom)
coefs <- s_multi$coefficients
ses <- s_multi$standard.errors
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))

pred_map <- c(
  "(Intercept)" = "Intercept",
  "age" = "Age (Years)",
  "I(age^2)" = "Age²",
  "income" = "Household Income",
  "I(income^2)" = "Household Income²",
  "race.fBlack" = "Ethnoracial ID: Black (Ref: White)",
  "race.fAsian" = "Ethnoracial ID: Asian",
  "race.fHispanic" = "Ethnoracial ID: Hispanic",
  "race.fMixed White" = "Ethnoracial ID: Mixed White",
  "race.fMixed Other" = "Ethnoracial ID: Mixed Other",
  "educ.fSome College" = "Education: Some College (Ref: HS or Less)",
  "educ.fCollege Degree" = "Education: College Degree",
  "educ.fProf./Graduate Degree" = "Education: Prof./Graduate Degree",
  "peduc.fHigh School" = "Parent's Educ: High School (Ref: < HS)",
  "peduc.fSome College" = "Parent's Educ: Some College",
  "peduc.fCollege Degree" = "Parent's Educ: College Degree",
  "peduc.fProf./Graduate Degree" = "Parent's Educ: Prof./Grad Degree",
  "city.fSmall Town" = "City Size: Small Town (Ref: Midsized)",
  "city.fBig City" = "City Size: Big City",
  "arts.fLow Exposure to Arts as Child" = "Childhood Arts: Low (Ref: Moderate)",
  "arts.fHigh Exposure to Arts as Child" = "Childhood Arts: High",
  "spol.fSoc. Lib." = "Social Ideology: Liberal (Ref: Moderate)",
  "spol.fSoc. Con." = "Social Ideology: Conservative",
  "epol.fEcon. Lib." = "Economic Ideology: Liberal (Ref: Moderate)",
  "epol.fEcon. Con." = "Economic Ideology: Conservative",
  "relig.fNot Religious" = "Religiosity: Not Religious (Ref: Somewhat)",
  "relig.fVery Religious" = "Religiosity: Very Religious",
  "gend.fWoman" = "Gender Identity: Woman (Ref: Man)",
  "gend.fNonbinary/Other" = "Gender Identity: Nonbinary/Other",
  "sex.fLGBTQ" = "Sexual Identity: LGBTQ (Ref: Straight)"
)

out_rows <- list()
for (v in colnames(coefs)) {
  v_label <- if (v %in% names(pred_map)) pred_map[v] else v
  
  c1 <- coefs["The Selective Autonomists", v]
  se1 <- ses["The Selective Autonomists", v]
  p1 <- p_vals["The Selective Autonomists", v]
  star1 <- ifelse(p1 < 0.001, "***", ifelse(p1 < 0.01, "**", ifelse(p1 < 0.05, "*", "")))
  rrr1 <- exp(c1)
  
  c2 <- coefs["The Consistent Anti-Autonomists", v]
  se2 <- ses["The Consistent Anti-Autonomists", v]
  p2 <- p_vals["The Consistent Anti-Autonomists", v]
  star2 <- ifelse(p2 < 0.001, "***", ifelse(p2 < 0.01, "**", ifelse(p2 < 0.05, "*", "")))
  rrr2 <- exp(c2)
  
  c3 <- coefs["The Moralized Engaged Humanists", v]
  se3 <- ses["The Moralized Engaged Humanists", v]
  p3 <- p_vals["The Moralized Engaged Humanists", v]
  star3 <- ifelse(p3 < 0.001, "***", ifelse(p3 < 0.01, "**", ifelse(p3 < 0.05, "*", "")))
  rrr3 <- exp(c3)
  
  out_rows[[length(out_rows) + 1]] <- data.frame(
    `Predictor` = v_label,
    `Selective Autonomists` = sprintf("%.2f%s (%.2f) [%.2f]", c1, star1, se1, rrr1),
    `Consistent Anti-Autonomists` = sprintf("%.2f%s (%.2f) [%.2f]", c2, star2, se2, rrr2),
    `Moralized Engaged Humanists` = sprintf("%.2f%s (%.2f) [%.2f]", c3, star3, se3, rrr3),
    check.names = FALSE
  )
}
tab_a3 <- do.call(rbind, out_rows)

write_md_table(tab_a3, here("cache", "tableA3_multinom_reg.md"))

message("All intermediate markdown tables (Tables 1-5, A1-A3) successfully written to cache/.")
