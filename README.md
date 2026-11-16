# An Impure Gaze: Aesthetic, Moral, and Political Evaluation in the United States

This repository contains the complete reproducibility package for the article

> Clayton Childress and Omar Lizardo. "An Impure Gaze: Aesthetic, Moral, and
> Political Evaluation in the United States." *Socius* (forthcoming).

The study investigates the relationship between:

1. **Aesthetic judgment** — evaluative ratings of 20 diverse photographs,
   analyzed using **Multiple Correspondence Analysis (MCA)**.
2. **Aesthetic orientations** — how individuals believe art should be
   evaluated, analyzed using **Principal Component Analysis (PCA)** and
   **Hierarchical Clustering on Principal Components (HCPC)**.
3. **Cross-model interconnections** — correlations between MCA and PCA
   dimensions, bridging judgments with abstract orientations.

Every table and figure in the published manuscript can be regenerated
end-to-end from the data in this repository using the scripts below.

---

## Repository Structure

```text
├── dat/
│   └── cultdat_repro.dta      # Public analysis extract (43 columns, N = 1,258)
├── Scripts/
│   ├── prepare_data.R         # (Authors only) builds the extract from the full survey file
│   ├── recode.dat.R           # Recoding function: factors, reference levels, var.photo
│   ├── run_analysis.R         # Stage 1: MCA, PCA, HCPC, MANOVA, multinomial logit -> cache/*.rds
│   ├── generate_md_tables.R   # Stage 2: APA 7th markdown tables -> cache/*.md
│   └── generate_plots.R       # Stage 3: publication figures -> Plots/*.png
├── cache/                     # Cached model objects (.rds) and manuscript tables (.md)
├── Plots/                     # All 12 manuscript figures (300 DPI PNG)
├── renv.lock                  # Exact package versions (renv)
└── renv/                      # renv project library configuration
```

---

## Requirements

- **R >= 4.5** (tested on R 4.5.3, Linux x86_64)
- All package dependencies are locked with `renv`. From the project root:

```r
install.packages("renv")   # if needed
renv::restore()
```

This installs the exact package versions used to produce the published
results (FactoMineR, factoextra, nnet, emmeans, ggplot2/ggrepel, dplyr,
tidyr, haven, here, corrplot, patchwork).

---

## Reproduction Pipeline

Run the three pipeline scripts in order from the project root:

```bash
Rscript Scripts/run_analysis.R       # ~1 min: fits all models, caches to cache/
Rscript Scripts/generate_md_tables.R # ~5 s:   writes Tables 1-5 and A1-A3
Rscript Scripts/generate_plots.R     # ~2 min: writes Figures 1-9 and A1-A3
```

- `run_analysis.R` sources `Scripts/recode.dat.R`, reads
  `dat/cultdat_repro.dta`, defines the complete-case analysis sample
  (N = 1,211), and caches all fitted objects (MCA, PCA, HCPC, the two
  MANOVA models, the multinomial logit, and cross-model correlations) to
  `cache/*.rds`. No model is ever fitted outside this script.
- `generate_md_tables.R` and `generate_plots.R` only read the cached
  objects; they never refit models. Tables are written to `cache/*.md`,
  figures to `Plots/*.png` (300 DPI, full-text-width equivalents).

### Script-to-manuscript mapping

| Manuscript asset | Output file | Producing script |
|---|---|---|
| Table 1 (Descriptive statistics) | `cache/table1_desc.md` | `generate_md_tables.R` |
| Table 2 (MANOVA, MCA space) | `cache/table2_manova_mca.md` | `generate_md_tables.R` |
| Table 3 (PCA-MCA correlations) | `cache/table3_pca_mca_corr.md` | `generate_md_tables.R` |
| Table 4 (MANOVA, PCA space) | `cache/table4_manova_pca.md` | `generate_md_tables.R` |
| Table 5 (HCPC cluster profiles) | `cache/table5_hcpc_clusters.md` | `generate_md_tables.R` |
| Table A1 (Question wording) | `cache/tableA1_questions.md` | `generate_md_tables.R` |
| Table A2 (Photo rating distributions) | `cache/tableA2_photos.md` | `generate_md_tables.R` |
| Table A3 (Multinomial estimates) | `cache/tableA3_multinom_reg.md` | `generate_md_tables.R` |
| Figure 1 (Active modalities cloud) | `Plots/cat-cloud.png` | `generate_plots.R` |
| Figure 2 (Supplementary categories cloud) | `Plots/sup-cloud.png` | `generate_plots.R` |
| Figure 3 (MCA sociodemographic cleavages) | `Plots/mca-ellipses-cleavages.png` | `generate_plots.R` |
| Figure 4 (PCA item correlations) | `Plots/corr.png` | `generate_plots.R` |
| Figure 5 (Pairwise PCA projections) | `Plots/pca-pairwise-projections.png` | `generate_plots.R` |
| Figure 6 (Ideology marginal effects) | `Plots/polpred.png` | `generate_plots.R` |
| Figure 7 (Individuals by HCPC cluster) | `Plots/pca-hcpc-clusters.png` | `generate_plots.R` |
| Figure 8 (Cluster probabilities by demographics) | `Plots/pca-multinom-demographics-probabilities.png` | `generate_plots.R` |
| Figure 9 (Cluster probability age curves) | `Plots/pca-multinom-age-curves.png` | `generate_plots.R` |
| Figure A1 (Numeric variable correlations) | `Plots/varcortrue.png` | `generate_plots.R` |
| Figure A2 (MCA/PCA scree plots) | `Plots/eig.png` | `generate_plots.R` |
| Figure A3 (Cramer's V across photographs) | `Plots/cramer-photos.png` | `generate_plots.R` |

### Reproducibility notes

- **Determinism.** All models and statistics are fully deterministic; the
  eight markdown tables regenerate byte-identically. `generate_plots.R`
  sets a seed, but `ggrepel`'s label-placement solver is not seed-stable,
  so Figures 1-2 reproduce only up to label positions; all point
  coordinates and statistics are exact. All other figures are
  pixel-identical across runs.
- **Package caveat.** `nnet` >= 7.3-20 no longer registers
  `summary.multinom` as an S3 method; the table script calls
  `nnet:::summary.multinom()` explicitly.

---

## Data

`dat/cultdat_repro.dta` contains **only** the raw columns consumed by the
analysis pipeline (43 of the 180 columns in the full survey file). The full
survey file is not distributed. Authors can regenerate the extract with
`Rscript Scripts/prepare_data.R` from the private source file.

All values are numeric codes; the recoding into labeled factors happens in
`Scripts/recode.dat.R`. Question wording for every measure is provided in
Tables A1 and A2 of the published manuscript and its appendix.

### Sociodemographic inputs

| Column | Content | Recoded factor |
|---|---|---|
| `race` | Ethnoracial identification (string codes) | `race.f` (White, Black, Asian, Hispanic, Mixed White, Mixed Other) |
| `age2` | Age in years | `age.f` (8 ordinal groups), `age` (continuous) |
| `educ` | Educational attainment, 1-7 | `educ.f` (High School or Less ... Prof./Graduate Degree) |
| `parented` | Parent's education, 1-7 | `peduc.f` (Less than High School ... Prof./Graduate Degree) |
| `income` | Household income bracket, 1-12 | `inc.f` (Less than 10K ... More than 150K) |
| `urban_rural` | Community size, 1-7 | `city.f` (Small/Midsized Town, Big City) |
| `child_arts` | Childhood arts exposure, 1-7 | `arts.f` (Low/Moderate/High) |
| `poli` | Political ideology, 1-7 | `pol.f` (Moderate, Liberal, Conservative) |
| `social` | Social ideology, 1-7 | `spol.f` (Soc. Lib./Mod./Con.) |
| `economic` | Economic ideology, 1-7 | `epol.f` (Econ. Lib./Mod./Con.) |
| `attend_service` | Religious service attendance, 1-7 | `relig.f` (Not/Somewhat/Very Religious) |
| `gender` | Gender identity code | `gend.f` (Man = reference, Woman, Nonbinary/Other) |
| `sexuality` | Sexual identity code | `sex.f` (Straight = reference, LGBTQ) |

### Photograph evaluation items (MCA indicators)

The 20 items `landscape` through `snake` (in questionnaire order) record
evaluations of 20 photographs, each coded 1 = Beautiful, 2 = Interesting,
3 = Meaningless, 4 = Ugly (recoded to factors `Be`, `In`, `Me`, `Ug` in
`recode.dat.R`). Item order: landscape, car_crash, girl_with_cat,
pregnant_woman, vase, woman_breastfeeding, metal_structure, homeless_arguing,
fruits_vegetables, sunset_over_sea, weaver_at_loom, folk_dance, rope, butcher,
bark_of_tree, famous_monument, scrapyard, first_communion, wounded_man, snake.

`recode.dat.R` also computes `var.photo`, the row-wise variance across the
20 ratings (a measure of taste heterogeneity).

### Abstract aesthetic-orientation items (PCA indicators)

| Column | Item (see manuscript Table A1 for wording) | Recoded name |
|---|---|---|
| `bestartpolitical` | The best art has a clear political point of view (1-7) | `artpol_y` |
| `bestartmoral` | The best art has a clear moral point of view (1-7) | `artmoral_y` |
| `bestartnotpolitical` | The best art doesn't pick political sides and makes us think about politics in new ways (1-7) | `artpol_n` |
| `bestartnotmoral` | The best art makes us think about what is right and wrong in new ways (1-7) | `artmoral_n` |
| `polarized` | I avoid engaging with art by creators endorsing political views I do not support (1-7) | `avoidpol_y` |
| `cancelledartists` | It's wrong to appreciate art by people who act poorly or hold objectionable beliefs (1-7) | `cancel_y` |
| `uniquetastes` | I like it when my tastes are different or unique from other people (1-7) | `distinction_y` |
| `enjoyabledifficult` | Enjoy difficult/challenging art (1-7) | `aesthetic_y` |
| `comedy` | Goal of the best comedy (1-7) | `serious_y` |
| `ironic` | Frequency of ironic cultural consumption (1-5) | `ironic_y` |

---

## Model specifications

- **MCA**: 20 photograph items active; 12 sociodemographic factors as
  supplementary categories (`FactoMineR::MCA`, `quali.sup = 21:32`).
- **PCA**: 10 aesthetic-orientation items standardized and active;
  sociodemographics as supplementary (`FactoMineR::PCA`).
- **HCPC**: hierarchical clustering on the first 3 PCA dimensions
  (`FactoMineR::HCPC`, `nb.clust = 4, kk = Inf`), yielding the four-cluster
  typology: Pure Aestheticists (reference), Selective Autonomists, Consistent
  Anti-Autonomists, Moralized Engaged Humanists.
- **MANOVA**: Pillai's trace models of MCA dimensions (2) and PCA dimensions
  (3) on the full sociodemographic battery.
- **Multinomial logit**: cluster membership on age (quadratic), income
  (quadratic), race, education, parent's education, city size, childhood arts
  exposure, social and economic ideology, religiosity, gender identity, and
  sexual identity (`nnet::multinom`); marginal predictions via `emmeans`.

## Software citation

The pipeline was run with R 4.5.3 and the package versions recorded in
`renv.lock`. Citations (versions in parentheses):

- R Core Team (2026). *R: A Language and Environment for Statistical
  Computing*. R Foundation for Statistical Computing, Vienna, Austria.
  https://www.R-project.org/
- Lê, Sébastien, Julie Josse, and François Husson (2008). "FactoMineR: An R
  Package for Multivariate Analysis." *Journal of Statistical Software* 25(1):
  1–18. (FactoMineR 2.15)
- Wickham, Hadley, Romain François, Lionel Henry, Kirill Müller, and Davis
  Vaughan (2023). "dplyr: A Grammar of Data Manipulation." *Journal of
  Statistical Software* 104(4): 1–30. (dplyr 1.2.1)
- Wickham, Hadley, Davis Vaughan, and Maximilian Girlich (2023). "tidyr: Tidy
  Messy Data." *Journal of Statistical Software* 108(5): 1–24.
  (tidyr 1.3.2)
- Wickham, Hadley, Evan Miller, and Danny Smith (2025). *haven: Import and
  Export 'SPSS', 'Stata' and 'SAS' Files*. (haven 2.5.5)
- Müller, Kirill (2025). *here: A Simpler Way to Find Your Files*.
  (here 1.0.2)
- Kassambara, Alboukadel, and Fabian Mundt (2026). *factoextra: Extract and
  Visualize the Results of Multivariate Data Analyses*. (factoextra 2.0.0)
- Venables, William N., and Brian D. Ripley (2002). *Modern Applied Statistics
  with S*, 4th ed. New York: Springer. (nnet 7.3-20)
- Lenth, Russell V., and Julia Piaskowski (2026). *emmeans: Estimated
  Marginal Means, aka Least-Squares Means*. (emmeans 2.0.3)
- Wickham, Hadley (2016). *ggplot2: Elegant Graphics for Data Analysis*.
  New York: Springer. (ggplot2 4.0.3)
- Slowikowski, Kamil (2026). *ggrepel: Automatically Position Non-Overlapping
  Text Labels with 'ggplot2'*. (ggrepel 0.9.8)
- Pedersen, Thomas Lin (2025). *patchwork: The Composer of Plots*.
  (patchwork 1.3.2)
- Wei, Taiyun, and Viliam Simko (2024). "R package 'corrplot': Visualization
  of a Correlation Matrix." (corrplot 0.95)
- Neuwirth, Erich (2022). *RColorBrewer: ColorBrewer Palettes*.
  (RColorBrewer 1.1-3)
