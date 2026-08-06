#' Name Conversion Module
#'
#' Unified search across YJSnumbers, Strains, alt_yjs, and alt_names tables.
#' Exact match for YJS numbers and Strain IDs, fuzzy match for names.
#' Results displayed in tabbed layout with inline expandable detail rows.
#'
#' @param id Module namespace ID
#' @param main_conn DB connection

# Global cache populated once when the module starts
.nameconv_cache <- NULL

nameconv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      td.details-control {
        cursor: pointer;
        font-weight: bold;
        font-size: 1.2em;
        color: var(--bs-primary, #0d6efd);
        text-align: center;
        width: 30px;
      }
      td.details-control:hover { color: var(--bs-link-hover-color, #0a58ca); }
      tr.child-row td {
        padding: 12px 16px;
        background-color: var(--bs-tertiary-bg, #f8f9fa);
      }
      .child-detail { max-width: 700px; }
      .child-detail .detail-row {
        display: flex;
        padding: 2px 0;
      }
      .child-detail .detail-label {
        flex: 0 0 160px;
        font-weight: 600;
        color: var(--bs-secondary-color, #6c757d);
        text-align: right;
        padding-right: 12px;
      }
      .child-detail .detail-value { flex: 1; }
      .child-detail .detail-section {
        font-weight: 600;
        margin-top: 8px;
        margin-bottom: 4px;
        padding-bottom: 2px;
        border-bottom: 1px solid #dee2e6;
      }
    ")),
    tags$script(HTML(sprintf(
      "$(document).on('keypress', '#%s', function(e) {
         if (e.which === 13) {
           e.preventDefault();
           $(this).trigger('change');
           var btn = $('#%s');
           setTimeout(function() { btn.click(); }, 100);
         }
       });",
      ns("search_input"), ns("search_btn")
    ))),
    tags$div(
      class = "mb-4",
      tags$h5("Name Conversion", class = "mb-3"),
      tags$div(
        class = "d-flex align-items-center gap-2",
        tags$div(
          style = "flex: 1; min-width: 0;",
          textInput(ns("search_input"), NULL, width = "100%",
                    placeholder = "Search by YJS number, strain ID, or name...")
        ),
        actionButton(ns("search_btn"), "Search", icon = icon("search"),
                     class = "btn-primary",
                     style = "height: 38px; margin-top: -19px; display: flex; align-items: center; gap: 0.4em; white-space: nowrap;")
      ),
      tags$small(class = "text-muted",
                 "Exact match for YJS/Strain IDs. Partial match for names.",
                 tags$br(),
                 "Examples: AAA, BAM, SACE_GAL, RM11, YJS1359")
    ),
    uiOutput(ns("results"))
  )
}

nameconv_server <- function(id, db_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    search_data <- reactiveVal(NULL)

    if (is.null(.nameconv_cache)) {
      .nameconv_cache <<- local({

        yjs_numbers <- DBI::dbGetQuery(
          db_conn,
          "SELECT * FROM yjs_numbers"
        )

        strains <- DBI::dbGetQuery(
          db_conn,
          "SELECT * FROM strains"
        )

        alt_names <- DBI::dbGetQuery(
          db_conn,
          "SELECT * FROM alt_names"
        )

        alt_yjs <- DBI::dbGetQuery(
          db_conn,
          "SELECT * FROM alt_yjs"
        )

        # ---------- primary indexes ----------

        yjs_by_id <- split(
          yjs_numbers,
          yjs_numbers$yjs_number
        )

        strain_by_id <- split(
          strains,
          strains$strain
        )

        # ---------- alt name maps ----------

        alt_names_by_strain <- tapply(
          alt_names$alt_name,
          alt_names$strain,
          paste,
          collapse = ", "
        )

        alt_name_lookup <- split(
          alt_names$strain,
          toupper(alt_names$alt_name)
        )

        # ---------- old yjs maps ----------

        old_yjs_by_current <- tapply(
          alt_yjs$old_yjs_number,
          alt_yjs$yjs_number,
          paste,
          collapse = ", "
        )

        old_yjs_lookup <- split(
          alt_yjs$yjs_number,
          alt_yjs$old_yjs_number
        )
        # ---------- linked samples ----------

        yjs_by_strain <- split(
          yjs_numbers,
          yjs_numbers$id_strain
        )

        linked_yjs_by_strain <- lapply(
          yjs_by_strain,
          function(df) paste(df$yjs_number, collapse = ", ")
        )

        # ---------- fuzzy indexes ----------

        yjs_name_index <- data.frame(
          yjs_number = yjs_numbers$yjs_number,
          sample_name = yjs_numbers$sample_name,
          norm = normalize_str(yjs_numbers$sample_name),
          stringsAsFactors = FALSE
        )

        strain_index <- data.frame(
          strain = strains$strain,
          norm = normalize_str(strains$strain),
          stringsAsFactors = FALSE
        )

        alt_name_index <- data.frame(
          strain = alt_names$strain,
          alt_name = alt_names$alt_name,
          norm = normalize_str(alt_names$alt_name),
          stringsAsFactors = FALSE
        )

        list(
          yjs_numbers = yjs_numbers,
          strains = strains,
          alt_names = alt_names,
          alt_yjs = alt_yjs,

          yjs_by_id = yjs_by_id,
          strain_by_id = strain_by_id,

          alt_names_by_strain = alt_names_by_strain,
          alt_name_lookup = alt_name_lookup,

          old_yjs_by_current = old_yjs_by_current,
          old_yjs_lookup = old_yjs_lookup,

          yjs_by_strain = yjs_by_strain,
          linked_yjs_by_strain = linked_yjs_by_strain,

          yjs_name_index = yjs_name_index,
          strain_index = strain_index,
          alt_name_index = alt_name_index
        )
      })}

    observeEvent(input$search_btn, {
      query_val <- trimws(input$search_input)
      if (nchar(query_val) == 0) {
        search_data(NULL)
        return()
      }
      result <- collect_search_results(query_val)
      search_data(result)
    })

    output$results <- renderUI({
      data <- search_data()
      if (is.null(data)) return(NULL)

      n_strains <- nrow(data$strains)
      n_samples <- nrow(data$samples)

      if (n_strains == 0 && n_samples == 0) {
        return(tags$div(
          class = "alert alert-warning mt-3",
          icon("exclamation-triangle"),
          paste0(" No match found for '", data$query, "'")
        ))
      }

      alert_ui <- lapply(data$alerts, function(msg) {
        tags$div(class = "alert alert-info mt-3 mb-1",
                 icon("arrow-right"), msg)
      })

      selected_tab <- if (n_strains > 0 && n_strains >= n_samples) {
        "strains"
      } else {
        "samples"
      }

      tagList(
        alert_ui,
        if (!is.null(data$fuzzy_header)) {
          tags$h6(class = "mt-3 mb-2 text-muted", data$fuzzy_header)
        },
        navset_card_tab(
          id = ns("results_tabs"),
          selected = selected_tab,
          nav_panel(
            title = paste0("Strains (", n_strains, ")"),
            value = "strains",
            if (n_strains > 0) {
              DT::DTOutput(ns("strains_table"))
            } else {
              tags$p(class = "text-muted mt-3",
                     "No matching strains.")
            }
          ),
          nav_panel(
            title = paste0("Samples (", n_samples, ")"),
            value = "samples",
            if (n_samples > 0) {
              DT::DTOutput(ns("samples_table"))
            } else {
              tags$p(class = "text-muted mt-3",
                     "No matching samples.")
            }
          )
        )
      )
    })

    output$strains_table <- DT::renderDT({
      data <- search_data()
      req(data, nrow(data$strains) > 0)
      build_strain_dt(data$strains)
    })

    output$samples_table <- DT::renderDT({
      data <- search_data()
      req(data, nrow(data$samples) > 0)
      build_sample_dt(data$samples)
    })
  })
}

# --- JS callback for child row toggle ---
# format_fn_body: JS function body that takes `d` (row data array)
# and returns HTML string for the child row content
child_row_callback <- function(format_fn_body) {
  DT::JS(paste0(
    "var formatDetail = ", format_fn_body, ";",
    "table.on('click', 'td.details-control', function() {",
    "  var td = $(this);",
    "  var tr = td.closest('tr');",
    "  var row = table.row(tr);",
    "  if (row.child.isShown()) {",
    "    row.child.hide();",
    "    td.html('+');",
    "  } else {",
    "    row.child(formatDetail(row.data())).show();",
    "    tr.next('tr').addClass('child-row');",
    "    td.html('\\u2212');",
    "  }",
    "});"
  ))
}

# --- Build strain DT with expandable child rows ---
build_strain_dt <- function(strains_df) {
  # Prepend empty column for expand toggle
  df <- cbind(` ` = rep("", nrow(strains_df)), strains_df)

  # Visible: toggle, strain, species, clade, eco_origin, country, Match Source
  # Hidden: geo_origin(7), continent(8), isolation(9), srr_id(10),
  #         alt_names(11), linked_yjs(12)
  hidden_cols <- which(names(df) %in%
    c("geo_origin", "continent", "isolation", "srr_id",
      "alt_names", "linked_yjs")) - 1  # 0-indexed

  format_fn <- paste0(
    "function(d) {",
    "  function row(label, val) {",
    "    var v = (val === null || val === '') ? '<span style=\"color:#adb5bd\">&mdash;</span>' : val;",
    "    return '<div class=\"detail-row\"><div class=\"detail-label\">' + label + '</div><div class=\"detail-value\">' + v + '</div></div>';",
    "  }",
    "  var h = '<div class=\"child-detail\">';",
    "  h += row('Geo Origin', d[7]);",
    "  h += row('Continent', d[8]);",
    "  h += row('Isolation', d[9]);",
    "  h += row('SRR ID', d[10]);",
    "  h += row('Alternative Names', d[11]);",
    "  if (d[12] && d[12] !== '') {",
    "    h += '<div class=\"detail-section\">Linked YJS Samples</div>';",
    "    h += '<div style=\"padding-left:172px\">' + d[12] + '</div>';",
    "  }",
    "  h += '</div>';",
    "  return h;",
    "}"
  )

  DT::datatable(
    df,
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    callback = child_row_callback(format_fn),
    options = list(
      dom = "tip",
      pageLength = 10,
      columnDefs = list(
        list(
          targets = 0,
          orderable = FALSE,
          className = "details-control",
          render = DT::JS(
            "function(data, type, row, meta) { return '+'; }"
          )
        ),
        list(targets = hidden_cols, visible = FALSE)
      )
    ),
    class = "compact stripe"
  )
}

# --- Build sample DT with expandable child rows ---
build_sample_dt <- function(samples_df) {
  df <- cbind(` ` = rep("", nrow(samples_df)), samples_df)

  # Visible: toggle, yjs_number, sample_name, species, id_strain,
  #          sample_type, ploidy, Match Source
  # Hidden: mating_type(8), strains_group(9), genotype(10),
  #         collection(11), old_yjs(12), strain_species(13),
  #         clade(14), eco_origin(15), geo_origin(16),
  #         country(17), isolation(18), alt_names(19)
  hidden_cols <- which(names(df) %in%
    c("mating_type", "strains_group", "genotype", "collection",
      "old_yjs", "strain_species", "clade", "eco_origin",
      "geo_origin", "country", "isolation", "alt_names")) - 1

  format_fn <- paste0(
    "function(d) {",
    "  function row(label, val) {",
    "    var v = (val === null || val === '') ? '<span style=\"color:#adb5bd\">&mdash;</span>' : val;",
    "    return '<div class=\"detail-row\"><div class=\"detail-label\">' + label + '</div><div class=\"detail-value\">' + v + '</div></div>';",
    "  }",
    "  var h = '<div class=\"child-detail\">';",
    "  h += row('Mating Type', d[8]);",
    "  h += row('Strains Group', d[9]);",
    "  h += row('Genotype', d[10]);",
    "  h += row('Collection', d[11]);",
    "  h += row('Old YJS Numbers', d[12]);",
    "  if (d[4] && d[4] !== '') {",
    "    h += '<div class=\"detail-section\">Linked Strain</div>';",
    "    h += row('Species', d[13]);",
    "    h += row('Clade', d[14]);",
    "    h += row('Eco Origin', d[15]);",
    "    h += row('Geo Origin', d[16]);",
    "    h += row('Country', d[17]);",
    "    h += row('Isolation', d[18]);",
    "    h += row('Alternative Names', d[19]);",
    "  }",
    "  h += '</div>';",
    "  return h;",
    "}"
  )

  DT::datatable(
    df,
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    callback = child_row_callback(format_fn),
    options = list(
      dom = "tip",
      pageLength = 10,
      columnDefs = list(
        list(
          targets = 0,
          orderable = FALSE,
          className = "details-control",
          render = DT::JS(
            "function(data, type, row, meta) { return '+'; }"
          )
        ),
        list(targets = hidden_cols, visible = FALSE)
      )
    ),
    class = "compact stripe"
  )
}

# --- Normalization: strip punctuation, lowercase ---
normalize_str <- function(x) {
  gsub("[^[:alnum:]]", "", tolower(x))
}

# --- Input classification ---
classify_input <- function(query) {
  q <- trimws(query)
  if (grepl("^YJS\\d+$", q, ignore.case = TRUE)) return("yjs")
  if (grepl("^(SACE_|XTRA_)?[A-Z]{3}$", q, ignore.case = TRUE)) return("strain")
  "freetext"
}

# --- Empty result template ---
empty_result <- function(query) {
  list(
    strains = data.frame(
      strain = character(0), species = character(0),
      clade = character(0), eco_origin = character(0),
      country = character(0), `Match Source` = character(0),
      geo_origin = character(0), continent = character(0),
      isolation = character(0), srr_id = character(0),
      alt_names = character(0), linked_yjs = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    samples = data.frame(
      yjs_number = character(0), sample_name = character(0),
      species = character(0), id_strain = character(0),
      sample_type = character(0), ploidy = character(0),
      `Match Source` = character(0),
      mating_type = character(0), strains_group = character(0),
      genotype = character(0), collection = character(0),
      old_yjs = character(0), strain_species = character(0),
      clade = character(0), eco_origin = character(0),
      geo_origin = character(0), country = character(0),
      isolation = character(0), alt_names = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    alerts = list(),
    query = query,
    fuzzy_header = NULL
  )
}

# --- Main search dispatcher (returns structured data) ---
collect_search_results <- function(query) {
  input_type <- classify_input(query)
  result <- empty_result(query)

  if (input_type == "yjs") {
    result <- search_yjs_exact(query, result)
  } else if (input_type == "strain") {
    result <- search_strain_exact(query, result)
  }

  if (nrow(result$strains) == 0 && nrow(result$samples) == 0) {
    result <- search_fuzzy(query, result)
  }

  result
}

# --- Exact YJS search ---
search_yjs_exact <- function(query, result) {
  q_upper <- toupper(trimws(query))
  yjs_data <- collect_yjs_data(q_upper)
  if (!is.null(yjs_data)) {
    result$samples <- rbind(result$samples, yjs_data$sample)
    if (!is.null(yjs_data$strain)) {
      result$strains <- rbind(result$strains, yjs_data$strain)
    }
    return(result)
  }

  cache <- get_nameconv_cache()
  matches <- cache$old_yjs_lookup[[q_upper]]

  if (!is.null(matches) && length(matches) > 0) {

    current_yjs <- matches[1]

    result$alerts <- c(
      result$alerts,
      list(
        paste0(
          "Redirected from old YJS number ",
          q_upper,
          " → ",
          current_yjs
        )
      )
    )

    yjs_data <- collect_yjs_data(current_yjs)

    if (!is.null(yjs_data)) {

      result$samples <- rbind(
        result$samples,
        yjs_data$sample
      )
      if (!is.null(yjs_data$strain)) {
        result$strains <- rbind(
          result$strains,
          yjs_data$strain
        )
      }
    }
  }
  result
}

# --- Exact Strain search ---
search_strain_exact <- function(query, result) {

  q_upper <- toupper(trimws(query))

  strain_data <- collect_strain_data(
    q_upper,
    "Exact match"
  )

  if (!is.null(strain_data)) {

    result$strains <- rbind(
      result$strains,
      strain_data$strain
    )

    if (nrow(strain_data$samples) > 0) {
      result$samples <- rbind(
        result$samples,
        strain_data$samples
      )
    }

    return(result)
  }

  cache <- get_nameconv_cache()

matches <- cache$alt_name_lookup[[q_upper]]

if (!is.null(matches) && length(matches) > 0) {

    resolved_strain <- matches[1]

    result$alerts <- c(
      result$alerts,
      list(
        paste0(
          "Found via alternative name '",
          q_upper,
          "' for strain ",
          resolved_strain
        )
      )
    )

    strain_data <- collect_strain_data(
      resolved_strain,
      "Via alt name"
    )

    if (!is.null(strain_data)) {

      result$strains <- rbind(
        result$strains,
        strain_data$strain
      )

      if (nrow(strain_data$samples) > 0) {
        result$samples <- rbind(
          result$samples,
          strain_data$samples
        )
      }
    }
  }

  result
}

# --- Fuzzy search ---
search_fuzzy <- function(query, result) {

  cache <- get_nameconv_cache()

  norm_query <- normalize_str(query)

  if (nchar(norm_query) == 0) {
    return(result)
  }

  max_results <- 50
  total_count <- 0

  # Fuzzy on YJSnumbers.sample_name
  yjs_all <- cache$yjs_name_index

  if (nrow(yjs_all) > 0) {

    hits <- yjs_all[
      grepl(norm_query, yjs_all$norm, fixed = TRUE),
      ,
      drop = FALSE
    ]

    for (i in seq_len(min(nrow(hits), max_results - total_count))) {

      yjs_data <- collect_yjs_data(
        hits$yjs_number[i],
        match_source = paste0(
          "Sample name '",
          hits$sample_name[i],
          "'"
        )
      )

      if (!is.null(yjs_data)) {

        result$samples <- rbind(
          result$samples,
          yjs_data$sample
        )

        if (!is.null(yjs_data$strain)) {

          if (!yjs_data$strain$strain %in%
                result$strains$strain) {

            result$strains <- rbind(
              result$strains,
              yjs_data$strain
            )
          }
        }

        total_count <- total_count + 1
      }
    }
  }

  # Fuzzy on Strains.strain
  if (total_count < max_results) {

    strains_all <- cache$strain_index

    if (nrow(strains_all) > 0) {

      hits <- strains_all[
        grepl(
          norm_query,
          strains_all$norm,
          fixed = TRUE
        ),
        ,
        drop = FALSE
      ]

      for (i in seq_len(
        min(
          nrow(hits),
          max_results - total_count
        )
      )) {

        if (!hits$strain[i] %in%
              result$strains$strain) {

          strain_data <- collect_strain_data(
            hits$strain[i],
            paste0(
              "Strain ID '",
              hits$strain[i],
              "'"
            )
          )

          if (!is.null(strain_data)) {

            result$strains <- rbind(
              result$strains,
              strain_data$strain
            )

            new_samples <- strain_data$samples[
              !strain_data$samples$yjs_number %in%
                result$samples$yjs_number,
              ,
              drop = FALSE
            ]

            if (nrow(new_samples) > 0) {

              result$samples <- rbind(
                result$samples,
                new_samples
              )
            }

            total_count <- total_count + 1
          }
        }
      }
    }
  }

  # Fuzzy on alt_names.alt_name
  if (total_count < max_results) {

    alt_all <- cache$alt_name_index

    if (nrow(alt_all) > 0) {

      hits <- alt_all[
        grepl(
          norm_query,
          alt_all$norm,
          fixed = TRUE
        ),
        ,
        drop = FALSE
      ]

      for (i in seq_len(nrow(hits))) {

        if (total_count >= max_results) {
          break
        }

        strain_id <- hits$strain[i]

        if (strain_id %in%
              result$strains$strain) {
          next
        }

        strain_data <- collect_strain_data(
          strain_id,
          paste0(
            "Alt name '",
            hits$alt_name[i],
            "'"
          )
        )

        if (!is.null(strain_data)) {

          result$strains <- rbind(
            result$strains,
            strain_data$strain
          )

          new_samples <- strain_data$samples[
            !strain_data$samples$yjs_number %in%
              result$samples$yjs_number,
            ,
            drop = FALSE
          ]

          if (nrow(new_samples) > 0) {

            result$samples <- rbind(
              result$samples,
              new_samples
            )
          }

          total_count <- total_count + 1
        }
      }
    }
  }

  total <- nrow(result$strains) +
           nrow(result$samples)

  if (total > 0) {

    result$fuzzy_header <- paste0(
      "Fuzzy matches (",
      nrow(result$strains),
      " strain",
      if (nrow(result$strains) != 1) "s",
      ", ",
      nrow(result$samples),
      " sample",
      if (nrow(result$samples) != 1) "s",
      ")"
    )
  }

  result
}

# --- Collect data for a single YJS number ---
collect_yjs_data <- function(yjs_number,
                             match_source = "Exact match") {

  cache <- get_nameconv_cache()

  yjs_row <- cache$yjs_by_id[[yjs_number]]

  if (is.null(yjs_row) || nrow(yjs_row) == 0) {
    return(NULL)
  }

  row <- yjs_row[1, ]

  strain_row <- NULL

  if (!is.na(row$id_strain) &&
      nchar(as.character(row$id_strain)) > 0) {

    strain_row <- cache$strain_by_id[[as.character(row$id_strain)]]

    if (!is.null(strain_row) && nrow(strain_row) > 0) {
      strain_row <- strain_row[1, ]
    } else {
      strain_row <- NULL
    }
  }

  # Alt YJS numbers
  old_yjs_str <- unname(
    cache$old_yjs_by_current[yjs_number]
  )

  if (is.na(old_yjs_str)) {
    old_yjs_str <- ""
  }

  # Alt names for linked strain
  alt_names_str <- ""

  strain_id <- row$id_strain

  if (!is.na(strain_id) &&
      nchar(as.character(strain_id)) > 0) {

    alt_names_str <- unname(
      cache$alt_names_by_strain[as.character(strain_id)]
    )

    if (is.na(alt_names_str)) {
      alt_names_str <- ""
    }
  }

  # Column order must match empty_result()$samples
  sample_df <- data.frame(
    yjs_number = row$yjs_number,
    sample_name = na_to_empty(row$sample_name),
    species = na_to_empty(row$species),
    id_strain = na_to_empty(row$id_strain),
    sample_type = na_to_empty(row$sample_type),
    ploidy = na_to_empty(row$ploidy),
    `Match Source` = match_source,
    mating_type = na_to_empty(row$mating_type),
    strains_group = na_to_empty(row$strains_group),
    genotype = na_to_empty(row$genotype),
    collection = na_to_empty(row$collection),
    old_yjs = old_yjs_str,
    strain_species = if (!is.null(strain_row))
      na_to_empty(strain_row$species) else "",
    clade = if (!is.null(strain_row))
      na_to_empty(strain_row$clade) else "",
    eco_origin = if (!is.null(strain_row))
      na_to_empty(strain_row$eco_origin) else "",
    geo_origin = if (!is.null(strain_row))
      na_to_empty(strain_row$geo_origin) else "",
    country = if (!is.null(strain_row))
      na_to_empty(strain_row$country) else "",
    isolation = if (!is.null(strain_row))
      na_to_empty(strain_row$isolation) else "",
    alt_names = alt_names_str,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  strain_df <- NULL

  if (!is.null(strain_row) &&
      !is.na(strain_row$strain) &&
      nchar(as.character(strain_row$strain)) > 0) {

    linked_str <- unname(
      cache$linked_yjs_by_strain[[as.character(strain_row$strain)]]
    )

    if (is.null(linked_str) || is.na(linked_str)) {
      linked_str <- ""
    }

    # Column order must match empty_result()$strains
    strain_df <- data.frame(
      strain = strain_row$strain,
      species = na_to_empty(strain_row$species),
      clade = na_to_empty(strain_row$clade),
      eco_origin = na_to_empty(strain_row$eco_origin),
      country = na_to_empty(strain_row$country),
      `Match Source` = match_source,
      geo_origin = na_to_empty(strain_row$geo_origin),
      continent = na_to_empty(strain_row$continent),
      isolation = na_to_empty(strain_row$isolation),
      srr_id = na_to_empty(strain_row$srr_id),
      alt_names = alt_names_str,
      linked_yjs = linked_str,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  list(
    sample = sample_df,
    strain = strain_df
  )
}

# --- Collect data for a single Strain ---
collect_strain_data <- function(strain_id,
                                match_source = "Exact match") {

  cache <- get_nameconv_cache()

  strain_row <- cache$strain_by_id[[as.character(strain_id)]]

  if (is.null(strain_row) || nrow(strain_row) == 0) {
    return(NULL)
  }

  sr <- strain_row[1, ]

  alt_names_str <- unname(
    cache$alt_names_by_strain[as.character(strain_id)]
  )

  if (is.na(alt_names_str)) {
    alt_names_str <- ""
  }

  linked_str <- cache$linked_yjs_by_strain[[as.character(strain_id)]]

  if (is.null(linked_str) || is.na(linked_str)) {
    linked_str <- ""
  }

  strain_df <- data.frame(
    strain = sr$strain,
    species = na_to_empty(sr$species),
    clade = na_to_empty(sr$clade),
    eco_origin = na_to_empty(sr$eco_origin),
    country = na_to_empty(sr$country),
    `Match Source` = match_source,
    geo_origin = na_to_empty(sr$geo_origin),
    continent = na_to_empty(sr$continent),
    isolation = na_to_empty(sr$isolation),
    srr_id = na_to_empty(sr$srr_id),
    alt_names = alt_names_str,
    linked_yjs = linked_str,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Linked samples with enriched detail columns
  yjs_rows <- cache$yjs_by_strain[[as.character(strain_id)]]

  if (!is.null(yjs_rows) && nrow(yjs_rows) > 0) {

    yjs_rows[is.na(yjs_rows)] <- ""

    yjs_rows$`Match Source` <- paste0(
      "Strain ",
      strain_id
    )

    # Add alt YJS for each sample
    yjs_rows$old_yjs <- ""

    old_yjs_map <- get_old_yjs_map(
      yjs_rows$yjs_number
    )

    yjs_rows$old_yjs <- unname(
      old_yjs_map[yjs_rows$yjs_number]
    )

    yjs_rows$old_yjs[is.na(yjs_rows$old_yjs)] <- ""

    # Add strain detail columns for child row display
    yjs_rows$strain_species <- na_to_empty(sr$species)
    yjs_rows$clade <- na_to_empty(sr$clade)
    yjs_rows$eco_origin <- na_to_empty(sr$eco_origin)
    yjs_rows$geo_origin <- na_to_empty(sr$geo_origin)
    yjs_rows$country <- na_to_empty(sr$country)
    yjs_rows$isolation <- na_to_empty(sr$isolation)
    yjs_rows$alt_names <- alt_names_str

    # Reorder to match expected column order
    samples_df <- yjs_rows[, c(
      "yjs_number",
      "sample_name",
      "species",
      "id_strain",
      "sample_type",
      "ploidy",
      "Match Source",
      "mating_type",
      "strains_group",
      "genotype",
      "collection",
      "old_yjs",
      "strain_species",
      "clade",
      "eco_origin",
      "geo_origin",
      "country",
      "isolation",
      "alt_names"
    ), drop = FALSE]

  } else {

    samples_df <- empty_result("")$samples
  }

  list(
    strain = strain_df,
    samples = samples_df
  )
}

# --- Helpers ---

get_nameconv_cache <- function() {
  if (is.null(.nameconv_cache)) {
    stop("Name conversion cache not initialized")
  }
  .nameconv_cache
}

na_to_empty <- function(x) {
  if (is.null(x) || is.na(x)) "" else as.character(x)
}

get_old_yjs_map <- function(yjs_numbers) {
  cache <- get_nameconv_cache()
  if (length(yjs_numbers) == 0) {
    return(character(0))
  }
  out <- cache$old_yjs_by_current[yjs_numbers]
  out[is.na(out)] <- ""
  out
}

add_old_yjs_column <- function(yjs_df) {
  if (nrow(yjs_df) == 0) return(yjs_df)
  old_map <- get_old_yjs_map(yjs_df$yjs_number)
  yjs_df$old_yjs <- unname(old_map[yjs_df$yjs_number])
  yjs_df$old_yjs[is.na(yjs_df$old_yjs)] <- ""
  yjs_df
}
