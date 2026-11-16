# Scripts/recode.dat.R
#
# Data preparation for the analysis pipeline. recode.dat() reads the public
# analysis extract (dat/cultdat_repro.dta, produced by Scripts/prepare_data.R)
# and returns a data frame with all recoded factors used by run_analysis.R:
#
#   - Sociodemographic factors with explicit, non-alphabetical level ordering
#     and the reference categories used in the models (e.g. "Man", "Straight")
#   - The 20 photograph items factorized to Be/In/Me/Ug
#   - var.photo: row-wise photograph rating variance (vectorized)
#
# All factors are constructed with case_when() so the level ordering is
# explicit rather than alphabetical.

recode.dat <- function() {
  dat <- haven::read_dta(here::here("dat", "cultdat_repro.dta")) |>
    mutate(
      race.f = factor(case_when(
        race %in% c("1", "3") ~ "White",
        race == "2" ~ "Black",
        race %in% c("4", "6") ~ "Asian",
        race == "5" ~ "Hispanic",
        grepl("1,", race, fixed = TRUE) ~ "Mixed White",
        race == ""                      ~ NA_character_,
        TRUE                            ~ "Mixed Other"
      ), levels = c("White", "Black", "Asian", "Hispanic", "Mixed White", "Mixed Other")),

      age.f = factor(case_when(
        between(age2, 17, 21) ~ "Age (17-21)",
        between(age2, 22, 28) ~ "Age (22-28)",
        between(age2, 29, 35) ~ "Age (29-35)",
        between(age2, 36, 42) ~ "Age (36-42)",
        between(age2, 43, 49) ~ "Age (43-49)",
        between(age2, 50, 59) ~ "Age (50-59)",
        between(age2, 60, 69) ~ "Age (60-69)",
        age2 >= 70            ~ "Age (70+)"
      ), levels = c(
        "Age (17-21)", "Age (22-28)", "Age (29-35)", "Age (36-42)",
        "Age (43-49)", "Age (50-59)", "Age (60-69)", "Age (70+)"
      )),

      educ.f = factor(case_when(
        educ %in% c(1, 2, 3) ~ "High School or Less",
        educ %in% c(4, 5)    ~ "Some College",
        educ == 6            ~ "College Degree",
        educ == 7            ~ "Prof./Graduate Degree"
      ), levels = c("High School or Less", "Some College", "College Degree", "Prof./Graduate Degree")),

      peduc.f  = factor(case_when(
        parented %in% c(1, 2) ~ "Less than High School",
        parented == 3         ~ "High School",
        parented %in% c(4, 5) ~ "Some College",
        parented == 6         ~ "College Degree",
        parented == 7         ~ "Prof./Graduate Degree"
      ), levels = c("Less than High School", "High School", "Some College", "College Degree", "Prof./Graduate Degree")),

      inc.f = factor(case_when(
        income == 1  ~ "Less than 10K",
        income == 2  ~ "Between 10K and 19.9K",
        income == 3  ~ "Between 20K and 29.9K",
        income == 4  ~ "Between 30K and 39.9K",
        income == 5  ~ "Between 40K and 49.9K",
        income == 6  ~ "Between 50K and 59.9K",
        income == 7  ~ "Between 60K and 69.9K",
        income == 8  ~ "Between 70K and 79.9K",
        income == 9  ~ "Between 80K and 89.9K",
        income == 10 ~ "Between 90K and 99.9K",
        income == 11 ~ "Between 100K and 149.9K",
        income == 12 ~ "More than 150K",
        TRUE         ~ NA_character_
      ), levels = c(
        "Less than 10K", "Between 10K and 19.9K", "Between 20K and 29.9K",
        "Between 30K and 39.9K", "Between 40K and 49.9K", "Between 50K and 59.9K",
        "Between 60K and 69.9K", "Between 70K and 79.9K", "Between 80K and 89.9K",
        "Between 90K and 99.9K", "Between 100K and 149.9K", "More than 150K"
      )),

      city.f = factor(case_when(
        urban_rural < 3 ~ "Small Town",
        between(urban_rural, 3, 5) ~ "Midsized Town",
        urban_rural > 5 ~ "Big City"
      ), levels = c("Midsized Town", "Small Town", "Big City")),

      arts.f = factor(case_when(
        child_arts < 3 ~ "Low Exposure to Arts as Child",
        between(child_arts, 3, 5) ~ "Moderate Exposure to Arts as Child",
        child_arts > 5 ~ "High Exposure to Arts as Child"
      ), levels = c("Moderate Exposure to Arts as Child", "Low Exposure to Arts as Child", "High Exposure to Arts as Child")),

      pol.f = factor(case_when(
        poli < 2.5 ~ "Liberal",
        between(poli, 2.5, 5.5) ~ "Moderate",
        poli > 5.5 ~ "Conservative"
      ), levels = c("Moderate", "Liberal", "Conservative")),

      spol.f = factor(case_when(
        social < 3 ~ "Soc. Lib.",
        between(social, 3, 5) ~ "Soc. Mod.",
        social > 5 ~ "Soc. Con."
      ), levels = c("Soc. Mod.", "Soc. Lib.", "Soc. Con.")),

      epol.f = factor(case_when(
        economic < 3 ~ "Econ. Lib.",
        between(economic, 3, 5) ~ "Econ. Mod.",
        economic > 5 ~ "Econ. Con."
      ), levels = c("Econ. Mod.", "Econ. Lib.", "Econ. Con.")),

      relig.f = factor(case_when(
        attend_service < 3 ~ "Not Religious",
        between(attend_service, 3, 5) ~ "Somewhat Religious",
        attend_service > 5 ~ "Very Religious"
      ), levels = c("Somewhat Religious", "Not Religious", "Very Religious"))
    ) |>
    mutate(
      # Reference categories: "Man" and "Straight" (used in multinomial models)
      gend.f = factor(gender, levels = c(2, 1, 3), labels = c("Man", "Woman", "Nonbinary/Other")),
      sex.f = factor(sexuality, levels = c(0, 1), labels = c("Straight", "LGBTQ"))
    ) |>
    mutate(
      # Row-wise photograph rating variance via matrix operations
      var.photo = {
        m <- as.matrix(pick(landscape:snake))
        rowSums((m - rowMeans(m))^2) / (ncol(m) - 1)
      }
    ) |>
    mutate(
      across(landscape:snake, ~factor(., labels = c("Be", "In", "Me", "Ug"))),
      # Standardized renames of the PCA items
      artpol_y = bestartpolitical,
      artmoral_y = bestartmoral,
      artpol_n = bestartnotpolitical,
      artmoral_n = bestartnotmoral,
      avoidpol_y = polarized,
      cancel_y = cancelledartists,
      distinction_y = uniquetastes,
      aesthetic_y = enjoyabledifficult,
      ironic_y = ironic,
      serious_y = comedy,
      age = age2,
      peduc = parented,
      arts = child_arts,
      city = urban_rural,
      relig = attend_service
    ) |>
    as.data.frame()

  return(dat)
}
