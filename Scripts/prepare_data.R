#!/usr/bin/env Rscript
# Scripts/prepare_data.R
#
# Derives the public analysis extract (dat/cultdat_repro.dta) from the full
# survey data file (dat/cultdat.dta, kept private). The extract retains ONLY
# the raw columns consumed by recode.dat() and the analysis pipeline:
#
#   - Sociodemographics: race, age2, educ, parented, income, urban_rural,
#     child_arts, poli, social, economic, attend_service, gender, sexuality
#   - 20 photograph evaluation items (landscape ... snake), coded 1-4
#   - 10 abstract aesthetic-orientation items (bestart* ... comedy), coded 1-7
#
# All recoding (factor construction, reference levels, var.photo) happens in
# Scripts/recode.dat.R, so the extract needs no derived variables.
#
# Run once by the authors after an update to the raw survey file:
#   Rscript Scripts/prepare_data.R

suppressPackageStartupMessages({
  library(here)
  library(haven)
  library(dplyr)
})

message("[1/3] Reading full survey file (dat/cultdat.dta)...")
dat <- read_dta(here("dat", "cultdat.dta"))

message("[2/3] Subsetting to the columns used by the analysis pipeline...")
needed <- c(
  # Sociodemographic raw inputs (recoded into .f factors in recode.dat())
  "race", "age2", "educ", "parented", "income", "urban_rural", "child_arts",
  "poli", "social", "economic", "attend_service", "gender", "sexuality",
  # Abstract aesthetic-orientation items (PCA indicators)
  "bestartpolitical", "bestartmoral", "bestartnotpolitical", "bestartnotmoral",
  "polarized", "cancelledartists", "uniquetastes", "enjoyabledifficult",
  "ironic", "comedy",
  # Photograph evaluation items (MCA indicators): the contiguous block
  # landscape ... snake in the original questionnaire order
  names(dat)[match("landscape", names(dat)):match("snake", names(dat))]
)

stopifnot(all(needed %in% names(dat)))
dat_slim <- dat |> select(all_of(needed))

message(sprintf("[3/3] Writing %d columns x %d rows to dat/cultdat_repro.dta...",
                ncol(dat_slim), nrow(dat_slim)))
write_dta(dat_slim, here("dat", "cultdat_repro.dta"))
message("Done. The full file dat/cultdat.dta is no longer needed for reproduction.")
