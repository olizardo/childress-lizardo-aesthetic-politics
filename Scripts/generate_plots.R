#!/usr/bin/env Rscript
# Scripts/generate_plots.R
# Standalone plotting pipeline: generates publication-grade PNGs to Plots/

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(corrplot)
  library(RColorBrewer)
  library(FactoMineR)
  library(factoextra)
  library(ggrepel)
  library(stringr)
  library(nnet)
  library(emmeans)
})

# Deterministic ggrepel label placement across runs
set.seed(2026)

dir.create(here("Plots"), showWarnings = FALSE, recursive = TRUE)

message("[1/10] Loading cached model objects...")
mca_res <- readRDS(here("cache", "mca_res.rds"))
pca_res <- readRDS(here("cache", "pca_res.rds"))
hcpc_res <- readRDS(here("cache", "hcpc_res.rds"))
hcpc_dat <- readRDS(here("cache", "hcpc_dat.rds"))
fit_multinom <- readRDS(here("cache", "fit_multinom.rds"))
model_data <- readRDS(here("cache", "model_data.rds"))

# ==============================================================================
# Global Palette Definitions (High-Contrast 4-Hue Scheme)
# Deep Blue (#0077BB), Teal (#009988), Warm Orange (#EE7733), Wine Purple (#882255)
# ==============================================================================
COLOR_BLUE   <- "#0077BB"  # Deep Blue (The Pure Aestheticists / Beautiful / Prof./Grad / Liberal / Not Religious)
COLOR_TEAL   <- "#009988"  # Teal / Pine (The Selective Autonomists / Interesting / College)
COLOR_ORANGE <- "#EE7733"  # Warm Orange (The Consistent Anti-Autonomists / Ugly / Some College / Moderate)
COLOR_PURPLE <- "#882255"  # Wine Purple (The Moralized Engaged Humanists / Meaningless / High School / Conservative / Religious)

# 4-Cluster Palette (HCPC Aesthetic Typology)
cluster_colors <- c(
  "The Pure Aestheticists"          = COLOR_BLUE,
  "The Selective Autonomists"       = COLOR_TEAL,
  "The Consistent Anti-Autonomists" = COLOR_ORANGE,
  "The Moralized Engaged Humanists" = COLOR_PURPLE
)

# 4-Category Photograph Response Palette (MCA Space)
response_colors <- c(
  "Be" = COLOR_BLUE,    # Beautiful
  "In" = COLOR_TEAL,    # Interesting
  "Ug" = COLOR_ORANGE,  # Ugly
  "Me" = COLOR_PURPLE   # Meaningless
)

response_labels <- c(
  "Be" = "Beautiful (Be)",
  "In" = "Interesting (In)",
  "Ug" = "Ugly (Ug)",
  "Me" = "Meaningless (Me)"
)

# Education Palette (4 levels)
educ_colors <- c(
  "Prof./Graduate Degree" = COLOR_BLUE,
  "College Degree"        = COLOR_TEAL,
  "Some College"          = COLOR_ORANGE,
  "High School or Less"   = COLOR_PURPLE
)

# Social Ideology Palette (3 levels)
spol_colors <- c(
  "Soc. Lib." = COLOR_BLUE,
  "Soc. Mod." = COLOR_ORANGE,
  "Soc. Con." = COLOR_PURPLE
)

# Religiosity Palette (3 levels)
relig_colors <- c(
  "Not Religious"      = COLOR_BLUE,
  "Somewhat Religious" = COLOR_ORANGE,
  "Very Religious"     = COLOR_PURPLE
)

# Helper for SE ellipses in MCA and PCA
get_se_ellipses <- function(df, dim1_name, dim2_name, group_var) {
  groups <- unique(df[[group_var]])
  ellipses_list <- list()
  for (g in groups) {
    sub_df <- df[df[[group_var]] == g, ]
    n <- nrow(sub_df)
    if (n > 2) {
      mean_x <- mean(sub_df[[dim1_name]], na.rm = TRUE)
      mean_y <- mean(sub_df[[dim2_name]], na.rm = TRUE)
      cov_mat <- cov(sub_df[, c(dim1_name, dim2_name)], use = "complete.obs") / n
      angles <- seq(0, 2 * pi, length.out = 100)
      circle <- cbind(cos(angles), sin(angles))
      chol_decomp <- chol(cov_mat)
      radius <- sqrt(qchisq(0.95, df = 2))
      ellipse_coords <- t(c(mean_x, mean_y) + radius * t(circle %*% chol_decomp))
      res_df <- data.frame(
        x = ellipse_coords[, 1],
        y = ellipse_coords[, 2],
        Group = as.character(g)
      )
      colnames(res_df)[1:2] <- c(dim1_name, dim2_name)
      ellipses_list[[as.character(g)]] <- res_df
    }
  }
  do.call(rbind, ellipses_list)
}

# ==============================================================================
# Figure 1: Active Modalities Cloud in MCA Space (cat-cloud.png)
# ==============================================================================
message("[2/10] Generating Figure 1: Active Modalities MCA Cloud...")

var_coords <- as.data.frame(mca_res$var$coord[, 1:2])
colnames(var_coords) <- c("Dim.1", "Dim.2")
var_coords$Category <- rownames(var_coords)
var_coords$Response <- sub(".*_", "", var_coords$Category)
var_coords$Object <- sub("_.*", "", var_coords$Category)

object_map <- c(
  "landscape" = "Landscape", "car.crash" = "Car Crash", "car_crash" = "Car Crash",
  "girl.with.cat" = "Girl with Cat", "girl_with_cat" = "Girl with Cat",
  "pregnant.woman" = "Pregnant Woman", "pregnant_woman" = "Pregnant Woman",
  "vase" = "Flower Vase", "woman.breastfeeding" = "Woman Breastfeeding",
  "woman_breastfeeding" = "Woman Breastfeeding", "metal.structure" = "Metal Structure",
  "metal_structure" = "Metal Structure", "homeless.arguing" = "Homeless Arguing",
  "homeless_arguing" = "Homeless Arguing", "fruits.vegetables" = "Fruits/Vegetables",
  "fruits_vegetables" = "Fruits/Vegetables", "sunset.over.sea" = "Sunset over Sea",
  "sunset_over_sea" = "Sunset over Sea", "weaver.at.loom" = "Weaver at Loom",
  "weaver_at_loom" = "Weaver at Loom", "folk.dance" = "Folk Dance",
  "folk_dance" = "Folk Dance", "rope" = "Rope", "butcher" = "Butcher",
  "bark.of.tree" = "Tree Bark", "bark_of_tree" = "Tree Bark",
  "famous.monument" = "Historical Monument", "famous_monument" = "Historical Monument",
  "scrapyard" = "Scrapyard", "first.communion" = "First Communion",
  "first_communion" = "First Communion", "wounded.man" = "Wounded Man",
  "wounded_man" = "Wounded Man", "snake" = "Snake"
)

var_coords$Object_Clean <- object_map[var_coords$Object]
var_coords$Object_Clean[is.na(var_coords$Object_Clean)] <- var_coords$Object[is.na(var_coords$Object_Clean)]
var_coords$Label <- paste0(var_coords$Object_Clean, " (", var_coords$Response, ")")

p_fig1 <- ggplot(var_coords, aes(x = Dim.1, y = Dim.2, color = Response, label = Label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray75", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray75", linewidth = 0.5) +
  geom_point(size = 3.2, alpha = 0.9) +
  geom_text_repel(
    size = 3.1,
    max.overlaps = 80,
    box.padding = 0.28,
    point.padding = 0.18,
    segment.color = "gray60",
    segment.size = 0.28,
    force = 3.5,
    force_pull = 0.4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = response_colors, labels = response_labels, name = "Evaluation Category") +
  scale_x_continuous(
    limits = c(-1.75, 4.40),
    breaks = seq(-1.5, 4.0, by = 1.0)
  ) +
  scale_y_continuous(
    limits = c(-1.45, 2.85),
    breaks = seq(-1.0, 2.5, by = 0.5)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Cloud of Active Photograph Evaluation Modalities (MCA Dims 1 & 2)",
    subtitle = "Multiple Correspondence Analysis of 20 photograph evaluations across 4 response categories (N = 1,211)",
    x = "Dimension 1: Aestheticized Engagement vs. Indifference (6.4% of Inertia)",
    y = "Dimension 2: Highbrow Asceticism vs. Popular Conventionality (5.0% of Inertia)"
  ) +
  annotate("text", x = -1.70, y = 2.75, label = "Ascetic Highbrow\n(Difficult/Unconventional Beautiful)", fontface = "italic", size = 3.2, color = "gray30", hjust = 0) +
  annotate("text", x = 4.30, y = 2.75, label = "Conventional Dislike\n(Unconventional Ugly)", fontface = "italic", size = 3.2, color = "gray30", hjust = 1) +
  annotate("text", x = -1.70, y = -1.35, label = "Popular Aesthetic\n(Conventional Pretty)", fontface = "italic", size = 3.2, color = "gray30", hjust = 0) +
  annotate("text", x = 4.30, y = -1.35, label = "Disengaged Indifference\n(Meaningless)", fontface = "italic", size = 3.2, color = "gray30", hjust = 1)

ggsave(here("Plots", "cat-cloud.png"), plot = p_fig1, width = 9.5, height = 7.5, units = "in", dpi = 300)

# ==============================================================================
# Figure 2: Supplementary Categories MCA Cloud (sup-cloud.png)
# ==============================================================================
message("[3/10] Generating Figure 2: Supplementary Categories MCA Cloud...")

sup_coords <- as.data.frame(mca_res$quali.sup$coord[, 1:2])
colnames(sup_coords) <- c("Dim.1", "Dim.2")

# Clean labels: strip technical prefixes like educ.f_ and peduc.f_
raw_sup_labels <- rownames(sup_coords)
clean_sup_labels <- raw_sup_labels
clean_sup_labels <- sub("^educ\\.f_", "", clean_sup_labels)
clean_sup_labels <- sub("^peduc\\.f_", "", clean_sup_labels)
clean_sup_labels <- sub("Moderate Exposure to Arts as Child", "Arts: Moderate", clean_sup_labels)
clean_sup_labels <- sub("Low Exposure to Arts as Child", "Arts: Low", clean_sup_labels)
clean_sup_labels <- sub("High Exposure to Arts as Child", "Arts: High", clean_sup_labels)

sup_coords$Label <- clean_sup_labels

p_fig2 <- ggplot(sup_coords, aes(x = Dim.1, y = Dim.2, label = Label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray75", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray75", linewidth = 0.5) +
  geom_point(color = COLOR_BLUE, size = 3.2) +
  geom_text_repel(
    size = 3.3,
    max.overlaps = 50,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.color = "gray60",
    segment.size = 0.3,
    force = 2
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Cloud of Supplementary Socio-Demographic Modalities in MCA Space",
    subtitle = "Centroid positions of social classes, demographics, and political orientations",
    x = "Dimension 1: Aestheticized Engagement vs. Indifference (6.4% of Inertia)",
    y = "Dimension 2: Highbrow Asceticism vs. Popular Conventionality (5.0% of Inertia)"
  )

ggsave(here("Plots", "sup-cloud.png"), plot = p_fig2, width = 9.5, height = 7.5, units = "in", dpi = 300)

# ==============================================================================
# Figure 3: Demographic Confidence Ellipses in MCA (mca-ellipses-cleavages.png)
# ==============================================================================
message("[4/10] Generating Figure 3: MCA Cleavages and Confidence Ellipses...")

mca_ind_df <- bind_cols(
  as.data.frame(mca_res$ind$coord[, 1:2]),
  model_data |> dplyr::select(educ.f, spol.f, relig.f, race.f)
)
colnames(mca_ind_df)[1:2] <- c("Dim.1", "Dim.2")

# Education
ell_educ <- get_se_ellipses(mca_ind_df, "Dim.1", "Dim.2", "educ.f")
cent_educ <- mca_ind_df |> group_by(educ.f) |> summarize(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2), .groups = "drop")
p_mca_educ <- ggplot(cent_educ, aes(x = Dim.1, y = Dim.2, color = educ.f, label = educ.f)) +
  geom_vline(xintercept = 0, color = "gray80") + geom_hline(yintercept = 0, color = "gray80") +
  geom_path(data = ell_educ, aes(x = Dim.1, y = Dim.2, group = Group, color = Group), inherit.aes = FALSE, alpha = 0.7, linewidth = 0.9) +
  geom_point(size = 3.5) + geom_text_repel(size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = educ_colors) +
  theme_minimal(base_size = 11) + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11)) +
  labs(title = "Panel A: Educational Attainment", x = "Dim 1 (Engagement)", y = "Dim 2 (Asceticism)")

# Social Ideology
ell_spol <- get_se_ellipses(mca_ind_df, "Dim.1", "Dim.2", "spol.f")
cent_spol <- mca_ind_df |> group_by(spol.f) |> summarize(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2), .groups = "drop")
p_mca_spol <- ggplot(cent_spol, aes(x = Dim.1, y = Dim.2, color = spol.f, label = spol.f)) +
  geom_vline(xintercept = 0, color = "gray80") + geom_hline(yintercept = 0, color = "gray80") +
  geom_path(data = ell_spol, aes(x = Dim.1, y = Dim.2, group = Group, color = Group), inherit.aes = FALSE, alpha = 0.7, linewidth = 0.9) +
  geom_point(size = 3.5) + geom_text_repel(size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = spol_colors) +
  theme_minimal(base_size = 11) + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11)) +
  labs(title = "Panel B: Political Ideology (Social)", x = "Dim 1 (Engagement)", y = "Dim 2 (Asceticism)")

# Religiosity
ell_relig <- get_se_ellipses(mca_ind_df, "Dim.1", "Dim.2", "relig.f")
cent_relig <- mca_ind_df |> group_by(relig.f) |> summarize(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2), .groups = "drop")
p_mca_relig <- ggplot(cent_relig, aes(x = Dim.1, y = Dim.2, color = relig.f, label = relig.f)) +
  geom_vline(xintercept = 0, color = "gray80") + geom_hline(yintercept = 0, color = "gray80") +
  geom_path(data = ell_relig, aes(x = Dim.1, y = Dim.2, group = Group, color = Group), inherit.aes = FALSE, alpha = 0.7, linewidth = 0.9) +
  geom_point(size = 3.5) + geom_text_repel(size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = relig_colors) +
  theme_minimal(base_size = 11) + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11)) +
  labs(title = "Panel C: Religious Attendance", x = "Dim 1 (Engagement)", y = "Dim 2 (Asceticism)")

p_fig3 <- (p_mca_educ | p_mca_spol | p_mca_relig) +
  plot_annotation(
    title = "Socio-Demographic Centroids and 95% Confidence Ellipses in MCA Space",
    subtitle = "Bivariate projections along Dimension 1 (Engagement) and Dimension 2 (Asceticism)",
    theme = theme(plot.title = element_text(size = 13, face = "bold"), plot.subtitle = element_text(size = 10.5))
  )

ggsave(here("Plots", "mca-ellipses-cleavages.png"), plot = p_fig3, width = 13, height = 4.8, units = "in", dpi = 300)

# ==============================================================================
# Figure 4: PCA Variable Correlation Tile Plot (corr.png)
# ==============================================================================
message("[5/10] Generating Figure 4: PCA Correlation Matrix...")

pca_var_cor <- as.data.frame(pca_res$var$coord[, 1:3])
colnames(pca_var_cor) <- c("Dim 1", "Dim 2", "Dim 3")
pca_var_cor$Item <- rownames(pca_var_cor)

item_labels <- c(
  "artpol_y" = "1. Art Clear Political POV",
  "artmoral_y" = "2. Art Clear Moral POV",
  "artmoral_n" = "3. Art Moral Reflexivity",
  "artpol_n" = "4. Art Political Reflexivity",
  "cancel_y" = "5. Wrong to Appreciate Transgressive Artists",
  "avoidpol_y" = "6. Avoid Opposing Political Views",
  "distinction_y" = "7. Prefer Unique/Distinct Taste",
  "aesthetic_y" = "8. Enjoy Difficult/Challenging Art",
  "serious_y" = "9. Best Comedy Says Something",
  "ironic_y" = "10. Ironic Cultural Consumption"
)

pca_var_cor$Item_Clean <- item_labels[pca_var_cor$Item]
pca_var_cor$Item_Clean[is.na(pca_var_cor$Item_Clean)] <- pca_var_cor$Item[is.na(pca_var_cor$Item_Clean)]

pca_cor_long <- pca_var_cor |>
  pivot_longer(cols = c("Dim 1", "Dim 2", "Dim 3"), names_to = "Dimension", values_to = "Correlation") |>
  mutate(
    Item_Clean = factor(Item_Clean, levels = rev(item_labels)),
    Dimension = factor(Dimension, levels = c("Dim 1", "Dim 2", "Dim 3"))
  )

p_fig4 <- ggplot(pca_cor_long, aes(x = Dimension, y = Item_Clean, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), fontface = "bold", size = 4.2) +
  scale_fill_gradient2(low = COLOR_PURPLE, mid = "white", high = COLOR_BLUE, midpoint = 0, limits = c(-1, 1), name = "Correlation (r)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 13, lineheight = 1.15),
    plot.subtitle = element_text(size = 10.5, lineheight = 1.15),
    axis.title = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(face = "bold", size = 11)
  ) +
  labs(
    title = "Correlations Between Aesthetic Opinion Items\nand First Three PCA Dimensions",
    subtitle = "Dim 1: Politicized Moral Aesthetics | Dim 2: Autonomist Cancel Culture\nDim 3: Formalist Pure Aesthetics"
  )

ggsave(here("Plots", "corr.png"), plot = p_fig4, width = 8.5, height = 6.2, units = "in", dpi = 300)

# ==============================================================================
# Figure 5: Pairwise Projections across PCA Dimensions (pca-pairwise-projections.png)
# ==============================================================================
message("[6/10] Generating Figure 5: Pairwise Projections in PCA Space...")

pca_ind_df <- bind_cols(
  as.data.frame(pca_res$ind$coord[, 1:3]),
  model_data |> dplyr::select(spol.f)
)
colnames(pca_ind_df)[1:3] <- c("Dim.1", "Dim.2", "Dim.3")

ell_spol_12 <- get_se_ellipses(pca_ind_df, "Dim.1", "Dim.2", "spol.f")
ell_spol_13 <- get_se_ellipses(pca_ind_df, "Dim.1", "Dim.3", "spol.f")
ell_spol_23 <- get_se_ellipses(pca_ind_df, "Dim.2", "Dim.3", "spol.f")

cent_pca_spol <- pca_ind_df |>
  group_by(spol.f) |>
  summarize(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2), Dim.3 = mean(Dim.3), .groups = "drop")

p12 <- ggplot(cent_pca_spol, aes(x = Dim.1, y = Dim.2, color = spol.f, label = spol.f)) +
  geom_vline(xintercept = 0, color = "lightgray") + geom_hline(yintercept = 0, color = "lightgray") +
  geom_path(data = ell_spol_12, aes(x = Dim.1, y = Dim.2, group = Group, color = Group), inherit.aes = FALSE, alpha = 0.7, linewidth = 0.9) +
  geom_point(size = 3.5) + geom_text_repel(size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = spol_colors) +
  theme_minimal(base_size = 11) + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11)) +
  labs(title = "Panel A: Dim 1 vs. Dim 2", x = "Dim 1 (Politicized/Moral)", y = "Dim 2 (Autonomy/Cancel)")

p13 <- ggplot(cent_pca_spol, aes(x = Dim.1, y = Dim.3, color = spol.f, label = spol.f)) +
  geom_vline(xintercept = 0, color = "lightgray") + geom_hline(yintercept = 0, color = "lightgray") +
  geom_path(data = ell_spol_13, aes(x = Dim.1, y = Dim.3, group = Group, color = Group), inherit.aes = FALSE, alpha = 0.7, linewidth = 0.9) +
  geom_point(size = 3.5) + geom_text_repel(size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = spol_colors) +
  theme_minimal(base_size = 11) + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11)) +
  labs(title = "Panel B: Dim 1 vs. Dim 3", x = "Dim 1 (Politicized/Moral)", y = "Dim 3 (Pure Aesthetic)")

p23 <- ggplot(cent_pca_spol, aes(x = Dim.2, y = Dim.3, color = spol.f, label = spol.f)) +
  geom_vline(xintercept = 0, color = "lightgray") + geom_hline(yintercept = 0, color = "lightgray") +
  geom_path(data = ell_spol_23, aes(x = Dim.2, y = Dim.3, group = Group, color = Group), inherit.aes = FALSE, alpha = 0.7, linewidth = 0.9) +
  geom_point(size = 3.5) + geom_text_repel(size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = spol_colors) +
  theme_minimal(base_size = 11) + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11)) +
  labs(title = "Panel C: Dim 2 vs. Dim 3", x = "Dim 2 (Autonomy/Cancel)", y = "Dim 3 (Pure Aesthetic)")

p_fig5 <- (p12 | p13 | p23) +
  plot_annotation(
    title = "Pairwise Projections of Pol. Ideology (Social) Centroids in 3D PCA Space",
    subtitle = "Centroids with 95% Standard Error Confidence Ellipses",
    theme = theme(plot.title = element_text(size = 13, face = "bold"), plot.subtitle = element_text(size = 10.5))
  )

ggsave(here("Plots", "pca-pairwise-projections.png"), plot = p_fig5, width = 13, height = 4.8, units = "in", dpi = 300)

# ==============================================================================
# Figure 6: Predicted Effects of Ideology on PCA Dimensions (polpred.png)
# ==============================================================================
message("[7/11] Generating Figure 6: Political Ideology Marginal Effects...")

pred_d1 <- lm(Dim.1 ~ spol.f + educ.f + age + income, data = pca_ind_df |> mutate(educ.f = model_data$educ.f, age = model_data$age, income = model_data$income))
pred_d2 <- lm(Dim.2 ~ spol.f + educ.f + age + income, data = pca_ind_df |> mutate(educ.f = model_data$educ.f, age = model_data$age, income = model_data$income))
pred_d3 <- lm(Dim.3 ~ spol.f + educ.f + age + income, data = pca_ind_df |> mutate(educ.f = model_data$educ.f, age = model_data$age, income = model_data$income))

em1 <- as.data.frame(emmeans(pred_d1, ~ spol.f)) |> mutate(Dimension = "Dim 1: Politicized Moral")
em2 <- as.data.frame(emmeans(pred_d2, ~ spol.f)) |> mutate(Dimension = "Dim 2: Autonomy/Cancel")
em3 <- as.data.frame(emmeans(pred_d3, ~ spol.f)) |> mutate(Dimension = "Dim 3: Pure Formalist")

pred_all <- bind_rows(em1, em2, em3) |>
  mutate(
    spol.f = factor(spol.f, levels = c("Soc. Lib.", "Soc. Mod.", "Soc. Con."))
  )

p_fig6 <- ggplot(pred_all, aes(x = spol.f, y = emmean, color = spol.f)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 0.9) +
  geom_point(size = 3.5) +
  facet_wrap(~ Dimension, ncol = 3) +
  scale_color_manual(values = spol_colors) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  labs(
    title = "Predicted Effects of Political Ideology (Social) on Abstract Aesthetic Dimensions",
    subtitle = "Marginal predicted means and 95% Confidence Intervals holding covariates constant",
    x = "Political Ideology (Social)",
    y = "Predicted Coordinate Score"
  )

ggsave(here("Plots", "polpred.png"), plot = p_fig6, width = 11, height = 4.8, units = "in", dpi = 300)

# ==============================================================================
# Figure 7: HCPC Clusters Cloud in PCA Space (pca-hcpc-clusters.png)
# ==============================================================================
message("[8/10] Generating Figure 7: HCPC Clusters PCA Cloud...")

pca_clust_df <- data.frame(
  Dim.1 = pca_res$ind$coord[, 1],
  Dim.2 = pca_res$ind$coord[, 2],
  Dim.3 = pca_res$ind$coord[, 3],
  Cluster_Group = hcpc_dat$Cluster_Group
)

p_fig7 <- ggplot(pca_clust_df, aes(x = Dim.1, y = Dim.2, color = Cluster_Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray75") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray75") +
  geom_point(alpha = 0.65, size = 2.4) +
  stat_ellipse(linewidth = 1.1, type = "t", level = 0.90) +
  scale_color_manual(values = cluster_colors, name = "Aesthetic Cluster") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Hierarchical Clustering on Principal Components (HCPC) in PCA Space",
    subtitle = "Cloud of respondents (N = 1,211) colored by the four aesthetic clusters with 90% confidence ellipses",
    x = "Dimension 1: Politicized Moral Aesthetics (23.8% of Inertia)",
    y = "Dimension 2: Autonomist Cancel Culture (19.4% of Inertia)"
  )

ggsave(here("Plots", "pca-hcpc-clusters.png"), plot = p_fig7, width = 9.5, height = 7.5, units = "in", dpi = 300)

# ==============================================================================
# Figure 8: Predicted Probabilities by Key Predictors (pca-multinom-key-probabilities.png)
# ==============================================================================
message("[9/10] Generating Figures 8, 9, 10: Multinomial Predictions...")

ref_list <- list(
  age = mean(model_data$age),
  income = mean(model_data$income),
  race.f = "White",
  educ.f = "High School or Less",
  peduc.f = "High School",
  city.f = "Midsized Town",
  arts.f = "Moderate Exposure to Arts as Child",
  spol.f = "Soc. Mod.",
  epol.f = "Econ. Mod.",
  relig.f = "Somewhat Religious",
  gend.f = "Man",
  sex.f = "Straight"
)

# ==============================================================================
# Figure 8: Predicted Probabilities Across Sociodemographic Predictors (pca-multinom-demographics-probabilities.png)
# ==============================================================================
message("[9/10] Generating Figures 8 & 9: Multinomial Predictions...")

ref_list <- list(
  age = mean(model_data$age),
  income = mean(model_data$income),
  race.f = "White",
  educ.f = "High School or Less",
  peduc.f = "High School",
  city.f = "Midsized Town",
  arts.f = "Moderate Exposure to Arts as Child",
  spol.f = "Soc. Mod.",
  epol.f = "Econ. Mod.",
  relig.f = "Somewhat Religious",
  gend.f = "Man",
  sex.f = "Straight"
)

# 1. Social Ideology
at_spol <- ref_list
at_spol$spol.f <- levels(model_data$spol.f)
em_spol <- as.data.frame(emmeans(fit_multinom, ~ Cluster_Group | spol.f, at = at_spol, mode = "prob", nesting = NULL)) |>
  rename(Value = spol.f) |>
  mutate(Predictor = "Pol. Ideology (Social)")

# 2. Religiosity
at_relig <- ref_list
at_relig$relig.f <- levels(model_data$relig.f)
em_relig <- as.data.frame(emmeans(fit_multinom, ~ Cluster_Group | relig.f, at = at_relig, mode = "prob", nesting = NULL)) |>
  rename(Value = relig.f) |>
  mutate(Predictor = "Religiosity")

# 3. Arts Exposure
at_arts <- ref_list
at_arts$arts.f <- levels(model_data$arts.f)
em_arts <- as.data.frame(emmeans(fit_multinom, ~ Cluster_Group | arts.f, at = at_arts, mode = "prob", nesting = NULL)) |>
  rename(Value = arts.f) |>
  mutate(Predictor = "Childhood Arts")

# 4. Education Level
at_educ <- ref_list
at_educ$educ.f <- levels(model_data$educ.f)
em_educ <- as.data.frame(emmeans(fit_multinom, ~ Cluster_Group | educ.f, at = at_educ, mode = "prob", nesting = NULL)) |>
  rename(Value = educ.f) |>
  mutate(Predictor = "Education Level")

# 5. Race
at_race <- ref_list
at_race$race.f <- c("White", "Black")
em_race <- as.data.frame(emmeans(fit_multinom, ~ Cluster_Group | race.f, at = at_race, mode = "prob", nesting = NULL)) |>
  rename(Value = race.f) |>
  mutate(Predictor = "Ethnoracial ID")

# 6. City Size
at_city <- ref_list
at_city$city.f <- c("Midsized Town", "Big City")
em_city <- as.data.frame(emmeans(fit_multinom, ~ Cluster_Group | city.f, at = at_city, mode = "prob", nesting = NULL)) |>
  rename(Value = city.f) |>
  mutate(Predictor = "City Size")

cat_levels <- c(
  "Soc. Lib.", "Soc. Mod.", "Soc. Con.",
  "Not Religious", "Somewhat Religious", "Very Religious",
  "Arts: High", "Arts: Moderate", "Arts: Low",
  "Educ: Prof./Grad Degree", "Educ: College Degree", "Educ: Some College", "Educ: High School or Less",
  "Race: Black", "Race: White",
  "City: Big City", "City: Midsized Town"
)

all_probs <- bind_rows(em_spol, em_relig, em_arts, em_educ, em_race, em_city) |>
  mutate(
    Value_Clean = case_when(
      Value == "Low Exposure to Arts as Child" ~ "Arts: Low",
      Value == "Moderate Exposure to Arts as Child" ~ "Arts: Moderate",
      Value == "High Exposure to Arts as Child" ~ "Arts: High",
      Value == "White" ~ "Race: White",
      Value == "Black" ~ "Race: Black",
      Value == "Midsized Town" ~ "City: Midsized Town",
      Value == "Big City" ~ "City: Big City",
      Value == "High School or Less" ~ "Educ: High School or Less",
      Value == "Some College" ~ "Educ: Some College",
      Value == "College Degree" ~ "Educ: College Degree",
      Value == "Prof./Graduate Degree" ~ "Educ: Prof./Grad Degree",
      Value == "Soc. Lib." ~ "Soc. Lib.",
      Value == "Soc. Mod." ~ "Soc. Mod.",
      Value == "Soc. Con." ~ "Soc. Con.",
      Value == "Not Religious" ~ "Not Religious",
      Value == "Somewhat Religious" ~ "Somewhat Religious",
      Value == "Very Religious" ~ "Very Religious",
      TRUE ~ as.character(Value)
    ),
    Value_Clean = factor(Value_Clean, levels = rev(cat_levels)),
    Predictor = factor(Predictor, levels = c(
      "Pol. Ideology (Social)", "Religiosity", "Childhood Arts", 
      "Education Level", "Ethnoracial ID", "City Size"
    )),
    Cluster_Label = factor(Cluster_Group, levels = c(
      "The Pure Aestheticists", "The Selective Autonomists", 
      "The Consistent Anti-Autonomists", "The Moralized Engaged Humanists"
    ))
  )

baseline_props <- data.frame(
  Cluster_Label = factor(c(
    "The Pure Aestheticists", "The Selective Autonomists", 
    "The Consistent Anti-Autonomists", "The Moralized Engaged Humanists"
  ), levels = c(
    "The Pure Aestheticists", "The Selective Autonomists", 
    "The Consistent Anti-Autonomists", "The Moralized Engaged Humanists"
  )),
  baseline = c(0.293, 0.258, 0.225, 0.224)
)

pred_colors <- c(
  "Pol. Ideology (Social)" = "#0072B2",
  "Religiosity"            = "#56B4E9",
  "Childhood Arts"         = "#009E73",
  "Education Level"        = "#E69F00",
  "Ethnoracial ID"         = "#D55E00",
  "City Size"              = "#CC79A7"
)

p_fig8 <- ggplot(all_probs, aes(x = prob, y = Value_Clean, color = Predictor)) +
  geom_vline(data = baseline_props, aes(xintercept = baseline), linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL), width = 0.25, linewidth = 0.75) +
  geom_point(size = 2.6) +
  facet_wrap(~ Cluster_Label, ncol = 2) +
  scale_color_manual(values = pred_colors, name = "Sociodemographic Predictor") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.7)) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 9)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Predicted Probability of Cluster Membership Across Sociodemographic Predictors",
    subtitle = "Adjusted predictions (95% Confidence Intervals) from Multinomial Logistic Regression (N = 1,211)",
    x = "Predicted Probability",
    y = ""
  )

ggsave(here("Plots", "pca-multinom-demographics-probabilities.png"), plot = p_fig8, width = 10.0, height = 9.2, units = "in", dpi = 300)

# ==============================================================================
# Figure 9: Predicted Probabilities Across Age (pca-multinom-age-curves.png)
# ==============================================================================
at_age <- ref_list
at_age$age <- seq(min(model_data$age), max(model_data$age), length.out = 30)

pred_age_em <- emmeans(
  fit_multinom, 
  ~ Cluster_Group | age, 
  at = at_age, 
  mode = "prob", 
  nesting = NULL
)

plot_age_df <- as.data.frame(pred_age_em) |>
  mutate(
    Cluster_Label = factor(Cluster_Group, levels = c(
      "The Pure Aestheticists", "The Selective Autonomists", "The Consistent Anti-Autonomists", "The Moralized Engaged Humanists"
    ))
  )

p_fig9 <- ggplot(plot_age_df, aes(x = age, y = prob, color = Cluster_Label, fill = Cluster_Label)) +
  geom_hline(data = baseline_props, aes(yintercept = baseline), linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.15, color = "transparent") +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ Cluster_Label, ncol = 2) +
  scale_color_manual(values = cluster_colors) +
  scale_fill_manual(values = cluster_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.7)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  labs(
    title = "Predicted Probability of Cluster Membership Across Continuous Age",
    subtitle = "Quadratic fits (Age + Age²) with 95% Confidence Intervals (Shaded Bands)",
    x = "Age (Years)",
    y = "Predicted Probability"
  )

ggsave(here("Plots", "pca-multinom-age-curves.png"), plot = p_fig9, width = 9.5, height = 6.8, units = "in", dpi = 300)

# ==============================================================================
# Appendices: Figure A1 (Correlation Matrix), Figure A2 (PCA Scree) & Figure A3 (MCA Scree)
# ==============================================================================
message("[10/11] Generating Figure A1: Numerical Correlation Matrix...")

num_vars <- c("age", "income", "educ", "peduc", "arts", "city", "relig", 
              "artpol_y", "artmoral_y", "artmoral_n", "artpol_n", "cancel_y", 
              "avoidpol_y", "distinction_y", "aesthetic_y", "serious_y", "ironic_y")

num_labels <- c(
  "age" = "Age",
  "income" = "Income",
  "educ" = "Education",
  "peduc" = "Parent's Educ",
  "arts" = "Child Arts",
  "city" = "City Size",
  "relig" = "Religiosity",
  "artpol_y" = "Art Political",
  "artmoral_y" = "Art Moral",
  "artmoral_n" = "Moral Reflexivity",
  "artpol_n" = "Pol. Reflexivity",
  "cancel_y" = "Cancel Artists",
  "avoidpol_y" = "Avoid Pol. Views",
  "distinction_y" = "Unique Tastes",
  "aesthetic_y" = "Difficult Art",
  "serious_y" = "Serious Comedy",
  "ironic_y" = "Ironic Consump."
)

cor_mat <- cor(model_data[num_vars], use = "pairwise.complete.obs")
colnames(cor_mat) <- num_labels[colnames(cor_mat)]
rownames(cor_mat) <- num_labels[rownames(cor_mat)]
diag(cor_mat) <- NA

cor_df <- as.data.frame(as.table(cor_mat))
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

cor_df$Var1 <- factor(cor_df$Var1, levels = rev(num_labels))
cor_df$Var2 <- factor(cor_df$Var2, levels = num_labels)
cor_df$Label <- ifelse(
  is.na(cor_df$Correlation), 
  "", 
  sub("^(-?)0\\.", "\\1.", sprintf("%.2f", cor_df$Correlation))
)

p_fig_a1 <- ggplot(cor_df, aes(x = Var2, y = Var1, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = Label, color = abs(Correlation) > 0.35 & !is.na(Correlation)), size = 2.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "gray15")) +
  scale_fill_gradient2(
    low = COLOR_PURPLE, mid = "white", high = COLOR_BLUE, midpoint = 0,
    limits = c(-0.20, 0.55), breaks = seq(-0.20, 0.50, by = 0.10),
    na.value = "gray96", name = "Pearson r"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 8.5),
    axis.text.y = element_text(face = "bold", size = 8.5),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12, lineheight = 1.15, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 9.5, lineheight = 1.15, margin = margin(b = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9.5),
    plot.margin = margin(t = 16, r = 10, b = 10, l = 10)
  ) +
  labs(
    title = "Correlation Matrix of Numerical Survey Indicators",
    subtitle = "Pearson correlation coefficients across sociodemographic and aesthetic indicators (N = 1,211; diagonal omitted)"
  )

ggsave(here("Plots", "varcortrue.png"), plot = p_fig_a1, width = 8.8, height = 7.0, units = "in", dpi = 300)

message("[11/11] Generating Combined Scree Plot (Figure A2)...")

# Figure A2: Combined MCA and PCA Scree Plots
mca_eig_df <- as.data.frame(mca_res$eig[1:min(5, nrow(mca_res$eig)), ])
mca_eig_df$Dimension <- factor(paste("Dim", 1:nrow(mca_eig_df)), levels = paste("Dim", 1:nrow(mca_eig_df)))
colnames(mca_eig_df)[2] <- "Variance_Pct"

p_mca_scree <- ggplot(mca_eig_df, aes(x = Dimension, y = Variance_Pct)) +
  geom_col(fill = COLOR_BLUE, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", Variance_Pct)), vjust = -0.4, fontface = "bold", size = 3.5) +
  scale_y_continuous(limits = c(0, 7.5), breaks = seq(0, 7.5, by = 2.5)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold", size = 10)
  ) +
  labs(
    title = "Panel A: MCA Photograph Judgments",
    x = "MCA Dimension",
    y = "Percentage of Inertia Explained (%)"
  )

pca_eig_df <- as.data.frame(pca_res$eig[1:min(5, nrow(pca_res$eig)), ])
pca_eig_df$Dimension <- factor(paste("Dim", 1:nrow(pca_eig_df)), levels = paste("Dim", 1:nrow(pca_eig_df)))
colnames(pca_eig_df)[2] <- "Variance_Pct"

p_pca_scree <- ggplot(pca_eig_df, aes(x = Dimension, y = Variance_Pct)) +
  geom_col(fill = COLOR_BLUE, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", Variance_Pct)), vjust = -0.4, fontface = "bold", size = 3.5) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold", size = 10)
  ) +
  labs(
    title = "Panel B: PCA Abstract Aesthetic Standards",
    x = "Principal Component",
    y = "Percentage of Variance Explained (%)"
  )

p_fig_a2 <- (p_mca_scree | p_pca_scree) +
  plot_annotation(
    title = "Eigenvalue Scree Plots of First Five Dimensions (MCA and PCA)",
    subtitle = "Percentage of inertia and variance accounted for across 20 photograph judgments and 10 abstract standards (N = 1,211)",
    theme = theme(plot.title = element_text(size = 12.5, face = "bold"), plot.subtitle = element_text(size = 10))
  )

ggsave(here("Plots", "eig.png"), plot = p_fig_a2, width = 9.5, height = 4.8, units = "in", dpi = 300)

# ==============================================================================
# Figure A3: Bivariate Association Matrix across 20 Photographs (Cramer's V)
# ==============================================================================
message("[12/12] Generating Figure A3: Photograph Association Matrix (Cramer's V)...")

photo_display_labels <- c(
  "landscape"           = "Landscape",
  "car_crash"           = "Car Crash",
  "girl_with_cat"       = "Girl w/ Cat",
  "pregnant_woman"      = "Pregnant Woman",
  "vase"                = "Vase",
  "woman_breastfeeding" = "Breastfeeding",
  "metal_structure"     = "Metal Structure",
  "homeless_arguing"    = "Homeless Arguing",
  "fruits_vegetables"   = "Fruits & Veg",
  "sunset_over_sea"     = "Sunset",
  "weaver_at_loom"      = "Weaver",
  "folk_dance"          = "Folk Dance",
  "rope"                = "Rope",
  "butcher"             = "Butcher",
  "bark_of_tree"        = "Tree Bark",
  "famous_monument"     = "Monument",
  "scrapyard"           = "Scrapyard",
  "first_communion"     = "First Communion",
  "wounded_man"         = "Wounded Man",
  "snake"               = "Snake"
)

photos <- names(photo_display_labels)

calc_cramer <- function(x, y) {
  tbl <- table(x, y)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
  n <- sum(tbl)
  k <- min(nrow(tbl), ncol(tbl))
  sqrt(chi2 / (n * (k - 1)))
}

cramer_mat <- matrix(NA, nrow = 20, ncol = 20, dimnames = list(photos, photos))
for(i in 1:20) {
  for(j in 1:20) {
    if (i > j) { # lower triangle only
      cramer_mat[i, j] <- calc_cramer(model_data[[photos[i]]], model_data[[photos[j]]])
    }
  }
}

cramer_df <- as.data.frame(as.table(cramer_mat))
colnames(cramer_df) <- c("Photo1", "Photo2", "CramerV")
cramer_df <- cramer_df[!is.na(cramer_df$CramerV), ]

cramer_df$Label1 <- photo_display_labels[as.character(cramer_df$Photo1)]
cramer_df$Label2 <- photo_display_labels[as.character(cramer_df$Photo2)]
cramer_df$Label1 <- factor(cramer_df$Label1, levels = rev(photo_display_labels[photos]))
cramer_df$Label2 <- factor(cramer_df$Label2, levels = photo_display_labels[photos])

cramer_df$TextLabel <- sub("^0\\.", ".", sprintf("%.2f", cramer_df$CramerV))

p_fig_a3 <- ggplot(cramer_df, aes(x = Label2, y = Label1, fill = CramerV)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = TextLabel, color = CramerV > 0.28), size = 2.4, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "gray20")) +
  scale_fill_gradient(
    low = "#F4F7F9",
    high = COLOR_BLUE,
    limits = c(0, 0.42),
    breaks = seq(0, 0.40, by = 0.10),
    name = "Cramer's V"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 8),
    axis.text.y = element_text(face = "bold", size = 8),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12, lineheight = 1.15, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 9.5, lineheight = 1.15, margin = margin(b = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9.5),
    plot.margin = margin(t = 14, r = 10, b = 10, l = 10)
  ) +
  labs(
    title = "Bivariate Association Matrix across Photograph Judgments",
    subtitle = "Pairwise Cramer's V statistics across 20 photograph subjects evaluated in 4 categories (N = 1,211; diagonal omitted)"
  )

ggsave(here("Plots", "cramer-photos.png"), plot = p_fig_a3, width = 8.5, height = 7.2, units = "in", dpi = 300)

message("All publication figures successfully generated in Plots/.")
