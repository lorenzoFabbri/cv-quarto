# -------------------------------------------------------------------------
# Utilities
# -------------------------------------------------------------------------

# Small helper for arranging with dynamic column name
safe_arrange_desc <- function(df, col_desc, ...) {
  if (is.null(col_desc) || !(col_desc %in% names(df))) {
    return(df)
  }
  dplyr::arrange(df, dplyr::desc(.data[[col_desc]]), ...)
}

escape_latex <- function(s) {
  s <- as.character(s)
  s <- ifelse(is.na(s), "", s)
  # Handle backslash first
  s <- gsub("\\\\", "\\\\textbackslash{}", s, perl = TRUE)
  s <- gsub("%", "\\\\%", s, perl = TRUE)
  s <- gsub("&", "\\\\&", s, perl = TRUE)
  s <- gsub("\\$", "\\\\$", s, perl = TRUE)
  s <- gsub("#", "\\\\#", s, perl = TRUE)
  s <- gsub("_", "\\\\_", s, perl = TRUE)
  s <- gsub("\\{", "\\\\{", s, perl = TRUE)
  s <- gsub("\\}", "\\\\}", s, perl = TRUE)
  s <- gsub("\\^", "\\\\^{}", s, perl = TRUE)
  s <- gsub("~", "\\\\textasciitilde{}", s, perl = TRUE)
  s
}

# Safe date formatter
# - Returns:
#  * "Present" only if year explicitly equals "Present" (case-insensitive)
#  * "" (empty string) if no meaningful information is provided
#  * "Mon YYYY" or "YYYY" otherwise
safe_orcid_date <- function(month, year) {
  m <- if (!is.null(month)) as.character(month) else ""
  y <- if (!is.null(year)) as.character(year) else ""
  m <- ifelse(is.na(m), "", trimws(m))
  y <- ifelse(is.na(y), "", trimws(y))

  # Return "Present" only when explicitly present in data
  if (tolower(y) == "present") {
    return("Present")
  }
  # If both missing, return empty
  if (y == "" && m == "") {
    return("")
  }
  # If year only, return year
  if (m == "") {
    return(y)
  }

  # Try lubridate parsing
  parsed <- tryCatch(
    lubridate::my(paste0(m, "/", y)),
    error = function(e) NA,
    warning = function(w) NA
  )

  if (inherits(parsed, "POSIXt") || inherits(parsed, "Date")) {
    return(format(as.Date(parsed), "%b %Y"))
  }

  # Try numeric month
  month_num <- suppressWarnings(as.integer(m))
  if (!is.na(month_num) && month_num >= 1 && month_num <= 12) {
    dt <- tryCatch(
      as.Date(sprintf("%s-%02d-01", y, month_num)),
      error = function(e) NA
    )
    if (!is.na(dt)) {
      return(format(as.Date(dt), "%b %Y"))
    }
  }

  # Fallback: "Mon YYYY" or "YYYY"
  if (m != "") paste(trimws(m), y) else y
}

# -------------------------------------------------------------------------
# Generic mapping + normalization helpers
# -------------------------------------------------------------------------

# - df: tibble/data.frame resulting from binding summaries (clean_names applied is expected)
# - mapping: named list mapping standard names to original (cleaned) df column names
# Returns tibble with standard columns: what, start, end, dates, location, details (list column where present)
map_orcid_fields <- function(df, mapping) {
  # Ensure df is a tibble
  df <- tibble::as_tibble(df)

  # Safe accessor helper: mapping values are expected to be
  #                       the cleaned column names (or original names then cleaned)
  col_or_na <- function(d, nm) {
    if (!is.null(nm) && nm %in% names(d)) d[[nm]] else rep(NA_character_, nrow(d))
  }

  what_col <- col_or_na(df, mapping$what)
  org_col <- col_or_na(df, mapping$org)
  city_col <- col_or_na(df, mapping$city)
  country_col <- col_or_na(df, mapping$country)

  start_month_col <- col_or_na(df, mapping$start_month)
  start_year_col <- col_or_na(df, mapping$start_year)
  end_month_col <- col_or_na(df, mapping$end_month)
  end_year_col <- col_or_na(df, mapping$end_year)

  details_col <- if (!is.null(mapping$details) && mapping$details %in% names(df)) {
    df[[mapping$details]]
  } else {
    rep(NA_character_, nrow(df))
  }

  n <- nrow(df)
  starts <- purrr::pmap_chr(list(start_month_col, start_year_col), safe_orcid_date)
  ends <- purrr::pmap_chr(list(end_month_col, end_year_col), safe_orcid_date)

  # Build human-friendly dates:
  dates <- vapply(seq_len(n), function(i) {
    s <- starts[i]
    e <- ends[i]
    s_empty <- is.na(s) || s == ""
    e_empty <- is.na(e) || e == ""
    if (s_empty && e_empty) {
      ""
    } else if (s_empty && !e_empty) {
      e
    } else if (!s_empty && e_empty) {
      s
    } else {
      paste0(s, " - ", e)
    }
  }, character(1))

  # Build location (avoid empty parenthesis)
  location <- vapply(seq_len(n), function(i) {
    org <- ifelse(is.na(org_col[i]) || org_col[i] == "", "", org_col[i])
    city <- ifelse(is.na(city_col[i]) || city_col[i] == "", "", city_col[i])
    country <- ifelse(is.na(country_col[i]) || country_col[i] == "", "", country_col[i])
    if (org == "" && city == "" && country == "") {
      ""
    } else if (city == "" && country == "") {
      org
    } else {
      paste0(
        org, " (",
        ifelse(city == "", "", city),
        ifelse(city == "" || country == "", "", ", "),
        ifelse(country == "", "", country),
        ")"
      )
    }
  }, character(1))

  # Normalize details into a list column
  details_list <- vector("list", length = n)
  for (i in seq_len(n)) {
    d_i <- details_col[[i]]
    if (is.list(d_i) && length(d_i) > 0) {
      details_list[[i]] <- unlist(d_i)
    } else if (is.vector(d_i) && length(d_i) > 1) {
      details_list[[i]] <- as.character(d_i)
    } else if (!is.na(d_i) && nzchar(as.character(d_i))) {
      details_list[[i]] <- as.character(d_i)
    } else {
      details_list[[i]] <- character(0)
    }
  }

  tibble::tibble(
    what = as.character(what_col),
    start = starts,
    end = ends,
    dates = dates,
    location = location,
    details = details_list
  )
}

# -------------------------------------------------------------------------
# Rendering helpers (LaTeX macros)
# -------------------------------------------------------------------------

cvevents <- function(tbl,
                     what = "what",
                     when = "dates",
                     where = "location",
                     details = "details",
                     delim = "\n") {
  command_start <- "\\cvevent"
  n <- nrow(tbl)
  out <- character(n)

  for (i in seq_len(n)) {
    when_i <- as.character(tbl[[when]][i])
    what_i <- as.character(tbl[[what]][i])
    where_i <- as.character(tbl[[where]][i])

    # Details processing
    items <- character(0)
    detail_cell <- tbl[[details]][[i]]
    if (length(detail_cell) > 0) {
      items <- as.character(detail_cell)
    }

    # Escape items
    if (length(items) > 0) {
      items_escaped <- vapply(items, escape_latex, character(1), USE.NAMES = FALSE)
    } else {
      items_escaped <- character(0)
    }

    if (length(items_escaped) == 0) {
      content <- ""
    } else {
      content <- paste0(
        "\\begin{itemize}[leftmargin=*]\n",
        paste0("  \\item ", items_escaped, collapse = "\n"),
        "\n\\end{itemize}"
      )
    }

    content_braced <- paste0("{", content, "}")

    out[i] <- paste0(
      command_start,
      "{", escape_latex(when_i), "}",
      "{", escape_latex(what_i), "}",
      "{", escape_latex(where_i), "}",
      content_braced
    )
  }

  cat(out, sep = "\n")
}

cvshortevents <- function(tbl,
                          what = "what",
                          when = "dates",
                          where = "location") {
  n <- nrow(tbl)
  out <- character(n)

  for (i in seq_len(n)) {
    when_i <- as.character(tbl[[when]][i])
    what_i <- as.character(tbl[[what]][i])
    where_i <- as.character(tbl[[where]][i])
    out[i] <- paste0(
      "\\cvshortevent",
      "{", escape_latex(when_i), "}",
      "{", escape_latex(what_i), "}",
      "{", escape_latex(where_i), "}"
    )
  }

  cat(out, sep = "\n")
}

# -------------------------------------------------------------------------
# High-level "render" function to reduce duplication in cv.qmd
#  - fetch_fun: a function that accepts orcid=... and returns a structure
#  - orcid: ORCID id
#  - summaries_path: character vector indexing into the returned list
#                     to reach the summaries list
#  - mapping: see map_orcid_fields
#  - arrange_desc_cols: optional character vector of column names
#                       present in cleaned df to arrange descending
#  - short: logical; if TRUE use cvshortevents, otherwise cvevents
#  - details_override: optional list where each element is a character vector
#                       of details for the corresponding row
# -------------------------------------------------------------------------
render_orcid_section <- function(fetch_fun,
                                 orcid,
                                 summaries_path,
                                 mapping,
                                 arrange_desc_cols = NULL,
                                 short = TRUE,
                                 details_override = NULL) {
  raw <- fetch_fun(orcid = orcid)

  # Drill down to summaries using summaries_path
  node <- raw
  for (p in summaries_path) {
    if (is.null(node[[p]])) {
      stop(
        sprintf(
          "Could not find path component '%s' in ORCID response. Check summaries_path.", p
        ),
        call. = FALSE
      )
    }
    node <- node[[p]]
  }

  # If node is a list of containers that include a `summaries` list column, try to unnest summaries
  # but prefer the path to point directly to the summaries list. If node is a list of lists of summaries,
  # bind_rows should handle consistent structures
  df <- dplyr::bind_rows(node)

  # If the data frame contains a column named "summaries" (list-column), unnest it
  if ("summaries" %in% names(df)) {
    df <- tidyr::unnest(df, cols = "summaries")
  }

  # Clean names for safe selection
  df <- janitor::clean_names(df)

  # If arrange columns provided, use them
  if (!is.null(arrange_desc_cols) && length(arrange_desc_cols) > 0) {
    # Convert provided names to cleaned names
    cleaned_cols <- vapply(arrange_desc_cols, janitor::make_clean_names, character(1))
    cleaned_cols <- cleaned_cols[cleaned_cols %in% names(df)]
    if (length(cleaned_cols) > 0) {
      # Arrange by the first cleaned column descending (sensible default)
      df <- dplyr::arrange(df, dplyr::desc(.data[[cleaned_cols[1]]]))
    }
  }

  # Map to standard columns
  norm <- map_orcid_fields(df, mapping)

  # If the caller provided an explicit details_override, attach/override the details column
  if (!is.null(details_override)) {
    if (!is.list(details_override)) {
      stop(
        "details_override must be a list where each element is a character vector (or empty character()).",
        call. = FALSE
      )
    }
    n_rows <- nrow(norm)
    if (length(details_override) >= n_rows) {
      norm$details <- details_override[seq_len(n_rows)]
    } else {
      # Pad with empty vectors for missing entries
      norm$details <- c(
        details_override,
        rep(list(character(0)), n_rows - length(details_override))
      )
    }
  }

  # Render
  if (isTRUE(short)) {
    cvshortevents(norm, what = "what", when = "dates", where = "location")
  } else {
    cvevents(norm, what = "what", when = "dates", where = "location", details = "details")
  }
}
