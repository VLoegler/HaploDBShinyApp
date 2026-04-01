#' Phylogenetic Tree Plot Module
#'
#' Renders phylogenetic trees using ggtree + ape.
#' v1: 3034 S. cerevisiae (circular) and 1060 B. bruxellensis (unrooted).
#' User can highlight strains, customize layout, and download plots.
#'
#' @param id Module namespace ID
#' @param user_info Reactive returning logged-in user data.frame

# -- Tree data loading (called once at startup) --

load_tree_datasets <- function() {
  list(
    sace_3034 = list(
      name = "3034 S. cerevisiae",
      tree = ape::read.tree("data/trees/sace_3034.nwk"),
      annotation = read.csv("data/trees/sace_3034_clades.csv", stringsAsFactors = FALSE),
      colors = read.csv("data/trees/sace_3034_colors.csv", stringsAsFactors = FALSE),
      default_layout = "circular",
      default_clade_point_size = 0.5,
      type = "sace"
    ),
    sace_1011 = list(
      name = "1011 S. cerevisiae",
      tree = ape::read.tree("data/trees/sace_1011.nwk"),
      annotation = read.csv("data/trees/sace_1011_clades.csv", stringsAsFactors = FALSE),
      colors = read.csv("data/trees/sace_1011_colors.csv", stringsAsFactors = FALSE),
      default_layout = "circular",
      default_clade_point_size = 1,
      type = "sace_1011"
    ),
    brbr_1060 = list(
      name = "1060 B. bruxellensis",
      tree = ape::read.tree("data/trees/brbr_1060.nwk"),
      annotation = read.csv("data/trees/brbr_1060_clusters.csv", stringsAsFactors = FALSE),
      colors = data.frame(
        Name = c("Admixed", "Beer", "Kombucha", "Teq/EtOH", "Wine 1", "Triploid group admixed W3/K", "Wine 3"),
        Color = c("gray70", "#FFB222", "#A3DC84", "#4B5E95", "#7C9BBE", "#F06154", "#ADD8E6"),
        stringsAsFactors = FALSE
      ),
      default_layout = "unrooted",
      default_clade_point_size = 1,
      type = "brbr"
    )
  )
}

#' Render a phylogenetic tree as a ggplot object
#'
#' @param dataset List with tree, annotation, colors, type
#' @param layout Character: "circular" or "unrooted"
#' @param color_clades Logical: color tips by clade/cluster
#' @param show_legend Logical
#' @param highlights Character vector of strain names to highlight
#' @param highlight_color Hex color for highlighted strains
#' @param highlight_size Numeric point size for highlights
#' @return A ggplot object
render_tree <- function(dataset, layout, color_clades, show_legend,
                        highlights, highlight_color, highlight_size,
                        label_size, branch_color, branch_width,
                        use_branch_length, clade_mode = "super_and_clades",
                        clade_point_size = 0.5,
                        highlight_use_clade_colors = FALSE) {
  tree <- dataset$tree
  ann <- dataset$annotation
  colors_df <- dataset$colors

  # Map layout names
  ggtree_layout <- if (layout == "unrooted") "ape" else layout

  if (dataset$type == "sace") {
    if (clade_mode == "super_only") {
      plotted_clade <- ann$SuperClade
      active_colors <- colors_df[colors_df$Level == "SuperClade", ]
      non_na_strains <- ann$StandardizedName[!is.na(ann$SuperClade)]
    } else if (clade_mode == "clades_only") {
      plotted_clade <- ann$Clade
      active_colors <- colors_df[colors_df$Level == "Clade", ]
      non_na_strains <- ann$StandardizedName[!is.na(ann$Clade)]
    } else {
      plotted_clade <- ann$SuperClade
      plotted_clade[is.na(plotted_clade)] <- ann$Clade[is.na(plotted_clade)]
      active_colors <- colors_df
      non_na_strains <- ann$StandardizedName[!is.na(ann$Clade) | !is.na(ann$SuperClade)]
    }
    tip_groups <- plotted_clade[match(tree$tip.label, ann$StandardizedName)]

    color_values <- setNames(c(active_colors$Color, "gray"), c(active_colors$Name, NA))

  } else if (dataset$type == "sace_1011") {
    tip_groups <- ann$clades[match(tree$tip.label, ann$STD_name)]
    non_na_strains <- ann$STD_name[!is.na(ann$clades) & ann$clades != "Unclustered"]
    color_values <- setNames(c(colors_df$hex_color, "gray"), c(colors_df$clade, NA))

  } else if (dataset$type == "brbr") {
    tip_groups <- ann$Cluster[match(tree$tip.label, ann$Isolate)]

    non_na_strains <- ann$Isolate[ann$Cluster != "Admixed"]

    color_values <- setNames(c(colors_df$Color, "gray"), c(colors_df$Name, NA))
  }

  if (dataset$type == "sace") {
    clade_levels <- active_colors$Name
  } else if (dataset$type == "sace_1011") {
    clade_levels <- colors_df$clade
  } else {
    clade_levels <- colors_df$Name
  }

  tip_df <- data.frame(
    label = tree$tip.label,
    clade_group = factor(tip_groups, levels = clade_levels),
    is_annotated = tree$tip.label %in% non_na_strains,
    is_highlight = tree$tip.label %in%
      (if (length(highlights) > 0 && !all(highlights == "")) highlights else character(0)),
    stringsAsFactors = FALSE
  )

  branch_len <- if (use_branch_length) "branch.length" else "none"
  p <- ggtree::ggtree(tree, layout = ggtree_layout, size = branch_width,
                       col = branch_color, branch.length = branch_len) %<+% tip_df

  if (color_clades) {
    p <- p + ggtree::geom_tippoint(
      ggplot2::aes(subset = is_annotated, color = clade_group),
      shape = 16, size = clade_point_size
    ) +
      ggplot2::scale_color_manual(values = color_values, na.translate = FALSE)
  }

  if (any(tip_df$is_highlight)) {
    if (highlight_use_clade_colors) {
      if (!color_clades) {
        p <- p + ggplot2::scale_color_manual(values = color_values, na.translate = FALSE)
      }
      p <- p + ggtree::geom_tippoint(
        ggplot2::aes(subset = is_highlight, color = clade_group),
        shape = 16, size = highlight_size
      )
    } else {
      p <- p + ggtree::geom_tippoint(
        ggplot2::aes(subset = is_highlight),
        shape = 16, size = highlight_size, color = highlight_color
      )
    }
    p <- p + ggtree::geom_tiplab(
      ggplot2::aes(subset = is_highlight, label = label),
      size = label_size / ggplot2::.pt, hjust = -1, color = highlight_color
    )
  }

  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = "right")
  }

  p + ggplot2::theme(
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
  )
}

# -- UI --

tree_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    card(
      card_header("Tree Controls"),
      selectInput(ns("tree_dataset"), "Tree Dataset",
                  choices = c("3034 S. cerevisiae" = "sace_3034",
                              "1011 S. cerevisiae" = "sace_1011",
                              "1060 B. bruxellensis" = "brbr_1060")),
      checkboxInput(ns("color_clades"), "Show Clades/Clusters", value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'sace_3034' && input['%s']", ns("tree_dataset"), ns("color_clades")),
        selectInput(ns("clade_mode"), "Clade Display",
                    choices = c("Super Clades only" = "super_only",
                                "Super Clades + Clades" = "super_and_clades",
                                "All Clades" = "clades_only"),
                    selected = "super_and_clades")
      ),
      checkboxInput(ns("show_legend"), "Show Legend", value = FALSE),
      tags$hr(),
      tags$label("Highlight Strains"),
      textAreaInput(ns("highlights"), NULL,
                    placeholder = "One strain per line or comma-separated",
                    rows = 4),
      checkboxInput(ns("highlight_use_clade_colors"), "Use Clade Colors for Highlights", value = FALSE),
      tags$hr(),
      actionButton(ns("generate_btn"), "Generate", class = "btn-primary w-100 mb-2"),
      downloadButton(ns("download_plot"), "Download Plot", class = "btn-outline-secondary w-100"),
      tags$hr(),
      bslib::accordion(
        id = ns("advanced_accordion"),
        open = FALSE,
        bslib::accordion_panel(
          "Advanced Parameters",
          colourpicker::colourInput(ns("highlight_color"), "Highlight Color", value = "#FF0000"),
          numericInput(ns("highlight_size"), "Highlight Point Size", value = 3, min = 0.5, max = 10, step = 0.5),
          numericInput(ns("label_size"), "Label Font Size (pt)", value = 8, min = 4, max = 24, step = 1),
          tags$hr(),
          numericInput(ns("clade_point_size"), "Clade Point Size", value = 0.5, min = 0.1, max = 5, step = 0.1),
          tags$hr(),
          selectInput(ns("layout"), "Layout",
                      choices = c("Circular" = "circular",
                                  "Unrooted" = "unrooted")),
          colourpicker::colourInput(ns("branch_color"), "Branch Color", value = "#808080"),
          numericInput(ns("branch_width"), "Branch Width", value = 0.1, min = 0.01, max = 2, step = 0.05),
          checkboxInput(ns("use_branch_length"), "Use Branch Length as Distance", value = TRUE),
          tags$hr(),
          numericInput(ns("img_width"), "Image Width (inches)", value = 10, min = 4, max = 30),
          numericInput(ns("img_height"), "Image Height (inches)", value = 10, min = 4, max = 30),
          selectInput(ns("download_format"), "Download Format",
                      choices = c("PNG" = "png", "PDF" = "pdf"))
        )
      )
    ),
    card(
      card_header("Phylogenetic Tree"),
      tags$style(HTML(sprintf(
        "#%s img { max-width: none !important; width: auto !important; }", ns("tree_plot")
      ))),
      plotOutput(ns("tree_plot"), height = "auto", width = "auto")
    )
  )
}

# -- Server --

tree_server <- function(id, user_info) {
  moduleServer(id, function(input, output, session) {

    # Load tree data once
    tree_data <- tryCatch(load_tree_datasets(), error = function(e) {
      message("Tree data not found: ", e$message)
      NULL
    })

    # Update layout default when dataset changes
    observeEvent(input$tree_dataset, {
      req(tree_data)
      ds <- tree_data[[input$tree_dataset]]
      updateSelectInput(session, "layout", selected = ds$default_layout)
      updateNumericInput(session, "clade_point_size",
                         value = ds$default_clade_point_size)
    })

    # Parse highlights
    get_highlights <- reactive({
      raw <- input$highlights
      if (is.null(raw) || trimws(raw) == "") return(character(0))
      h <- unlist(strsplit(raw, "[,\n]+"))
      trimws(h[nchar(trimws(h)) > 0])
    })

    # Rendered plot reactive
    tree_plot <- eventReactive(input$generate_btn, {
      req(tree_data, input$tree_dataset)
      ds <- tree_data[[input$tree_dataset]]
      render_tree(
        dataset = ds,
        layout = input$layout,
        color_clades = input$color_clades,
        show_legend = input$show_legend,
        highlights = get_highlights(),
        highlight_color = input$highlight_color,
        highlight_size = input$highlight_size,
        label_size = input$label_size,
        branch_color = input$branch_color,
        branch_width = input$branch_width,
        use_branch_length = input$use_branch_length,
        clade_mode = input$clade_mode %||% "super_and_clades",
        clade_point_size = input$clade_point_size,
        highlight_use_clade_colors = input$highlight_use_clade_colors
      )
    })

    output$tree_plot <- renderPlot({
      tree_plot()
    }, width = function() input$img_width * 72,
       height = function() input$img_height * 72)

    output$download_plot <- downloadHandler(
      filename = function() {
        ext <- input$download_format
        paste0("tree_plot_", input$tree_dataset, ".", ext)
      },
      content = function(file) {
        ggplot2::ggsave(
          file, plot = tree_plot(),
          width = input$img_width, height = input$img_height,
          device = input$download_format, dpi = 300
        )
      }
    )
  })
}
