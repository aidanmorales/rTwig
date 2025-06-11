#' Download Twigs
#'
#' @description Download twig measurements from an external database, including
#'  additional, verified measurements from contributors.
#'
#' @param database Twig database to return. Can be "twigs", "twigs_index",
#'  "raw", for the raw twig measurements, or "all" for all databases.
#'
#' @return Returns a data frame
#' @export
#'
#' @examples
#'
#' download_twigs()
#'
download_twigs <- function(database = "twigs") {
  # Check inputs ---------------------------------------------------------------
  if (!is.character(database)) {
    message <- paste(
      "`database` must be character, not ",
      class(database),
      "."
    )
    abort(message, class = "data_format_error")
  }

  inform("Downloading Twig Measurements")

  # Build URL to twig database
  url <- paste0(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCGozChHsWpgd6h1-w7fdCyu",
    "xXm6l_FfetgB7y8zWxwcfZwnnjvFp_sZW9HJLa9LVudGtoIdOD_OBO/pub?gid=1587869300",
    "&single=true&output=csv"
  )

  # Download twig measurements csv and standardise units
  twigs <- tidytable::fread(url, showProgress = FALSE) %>%
    mutate(
      radius_mm = case_when(
        .data$units == "in" ~ .data$diameter * 25.4 / 2,
        .data$units == "cm" ~ .data$diameter * 10 / 2,
        .data$units == "mm" ~ .data$diameter / 2
      )
    ) %>%
    relocate("radius_mm", .before = "diameter") %>%
    select(-"diameter", -"units")

  if (database == "raw") {
    return(twigs)
  } else {
    # Species measurements
    species <- twigs %>%
      group_by("scientific_name") %>%
      summarize(
        radius_mm = mean(.data$radius_mm),
        min = min(.data$radius_mm),
        max = max(.data$radius_mm),
        n = length(.data$radius_mm),
        std = stats::sd(.data$radius_mm),
        .groups = "drop"
      ) %>%
      mutate(cv = .data$std / .data$radius_mm) %>%
      arrange(.data$scientific_name)

    # Genus measurements
    genus <- species %>%
      separate(
        .data$scientific_name,
        into = c("genus", "species"),
        sep = " ",
        remove = FALSE
      ) %>%
      mutate(std2 = .data$std * .data$n) %>%
      group_by("genus") %>%
      summarize(
        radius_mm2 = mean(.data$radius_mm),
        min = min(.data$min),
        max = max(.data$max),
        std = sum(.data$std2) / sum(.data$n),
        n2 = sum(.data$n)
      ) %>%
      mutate(
        cv = .data$std / .data$radius_mm2,
        scientific_name = paste(.data$genus, "spp.", sep = " ")
      ) %>%
      rename(
        n = "n2",
        radius_mm = "radius_mm2"
      ) %>%
      select(-"genus") %>%
      relocate("scientific_name", .before = "radius_mm")

    # Combine measurements
    twigs_summary <- bind_rows(genus, species) %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      distinct() %>%
      arrange(.data$scientific_name)

    if (database == "twigs") {
      return(twigs_summary)
    }

    # Create categorical twig index
    twigs_index <- twigs_summary %>%
      mutate(
        size_index = case_when(
          .data$radius_mm <= 1 ~ "slender",
          .data$radius_mm > 1 & .data$radius_mm <= 2 ~ "moderately slender",
          .data$radius_mm > 2 & .data$radius_mm <= 2.5 ~ "moderately stout",
          .data$radius_mm > 2.5 ~ "stout",
        ),
        index = case_when(
          .data$size_index == "slender" ~ 1,
          .data$size_index == "moderately slender" ~ 2,
          .data$size_index == "moderately stout" ~ 3,
          .data$size_index == "stout" ~ 4
        )
      ) %>%
      group_by("size_index") %>%
      summarize(
        radius_mm_new = mean(.data$radius_mm),
        n = n(),
        min = min(.data$radius_mm),
        max = max(.data$radius_mm),
        std = stats::sd(.data$radius_mm),
        .groups = "drop"
      ) %>%
      rename("radius_mm" = "radius_mm_new") %>%
      mutate(cv = .data$std / .data$radius_mm) %>%
      arrange(.data$radius_mm) %>%
      mutate(across(where(is.numeric), ~ round(., 2)))


    if (database == "twigs_index") {
      return(twigs_index)
    }

    if (database == "all") {
      return(list(
        raw = twigs,
        twigs = twigs_summary,
        twigs_index = twigs_index
      ))
    }
  }
}
