# =============================================================================
# mod_region_settings.R — Region & figure settings module
# =============================================================================

#' Region & Figure Settings UI
#'
#' @param id module namespace ID
#' @export
mod_region_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    omics_banner(
      "Régions & Paramètres de figure",
      "Définissez les loci génomiques à visualiser et les options de rendu.",
      small = TRUE
    ),

    shiny::fluidRow(
      shiny::column(6,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("map-marker-alt")),
            shiny::tags$h5("Régions génomiques")
          ),
          bslib::navset_tab(
            bslib::nav_panel(
              shiny::tagList(shiny::icon("magic"), " S\u00e9lection guid\u00e9e"),
              shiny::div(class = "mt-2",

                # ---- Analyse ----
                shiny::div(
                  class = "d-flex gap-2 align-items-center mb-2",
                  shiny::actionButton(ns("btn_analyze_chroms"),
                    shiny::tagList(shiny::icon("dna"), " Analyser les chromosomes"),
                    class = "btn btn-secondary btn-sm"),
                  shiny::uiOutput(ns("chrom_index_badge"))
                ),
                shiny::uiOutput(ns("chrom_compat_alert")),

                # ---- S\u00e9lecteur chromosome ----
                shiny::div(id = ns("chrom_picker_block"),
                  shiny::selectInput(
                    ns("guided_chrom"),
                    shiny::tagList(shiny::icon("list"), " Chromosome / contig"),
                    choices  = c("— analyser d\u2019abord —" = ""),
                    selected = ""
                  ),
                  shiny::uiOutput(ns("chrom_size_info")),

                  # ---- Start / End ----
                  shiny::div(
                    class = "chrom-index-card",
                    shiny::fluidRow(
                      shiny::column(6,
                        shiny::numericInput(ns("guided_start"), "D\u00e9but (bp)", value = 1L,
                                            min = 1L, max = 1e12, step = 1L)
                      ),
                      shiny::column(6,
                        shiny::numericInput(ns("guided_end"), "Fin (bp)", value = 100000L,
                                            min = 2L, max = 1e12, step = 1L)
                      )
                    ),

                    # ---- Fen\u00eatres rapides ----
                    shiny::div(
                      class = "mb-2",
                      shiny::tags$small(class = "text-muted d-block mb-1", "Fen\u00eatre rapide :"),
                      shiny::div(
                        class = "d-flex gap-1 flex-wrap",
                        shiny::actionButton(ns("win_50k"),  "50 kb",  class = "btn btn-outline-secondary btn-sm region-chip"),
                        shiny::actionButton(ns("win_100k"), "100 kb", class = "btn btn-outline-secondary btn-sm region-chip"),
                        shiny::actionButton(ns("win_250k"), "250 kb", class = "btn btn-outline-secondary btn-sm region-chip"),
                        shiny::actionButton(ns("win_500k"), "500 kb", class = "btn btn-outline-secondary btn-sm region-chip"),
                        shiny::actionButton(ns("win_1m"),   "1 Mb",   class = "btn btn-outline-secondary btn-sm region-chip")
                      )
                    ),

                    shiny::uiOutput(ns("guided_region_preview")),
                    shiny::actionButton(ns("btn_add_guided_region"),
                      shiny::tagList(shiny::icon("plus"), " Ajouter cette r\u00e9gion"),
                      class = "btn btn-primary w-100 mt-1")
                  )
                ),

                # ---- Gene picker ----
                shiny::uiOutput(ns("gene_picker_block"))
              )
            ),
            bslib::nav_panel(
              shiny::tagList(shiny::icon("crosshairs"), " Saisie manuelle"),
              shiny::div(class = "mt-2",
                shiny::div(class = "alert alert-info p-2 mb-2",
                  shiny::icon("info-circle"),
                  shiny::HTML(" Format : <code>chr:start-end</code> &nbsp; Ex : <code>chr1:1000000-1250000</code>")
                ),
                shiny::textInput(ns("region_single"), "R\u00e9gion", placeholder = "chr1:1000000-1250000"),
                shiny::div(class = "d-flex gap-2 flex-wrap mb-1",
                  shiny::tags$small(class = "text-muted", "Exemples rapides :"),
                  shiny::actionLink(ns("ex_reg1"), "chr1:1000-20000"),
                  shiny::actionLink(ns("ex_reg2"), "chrX:100000-500000"),
                  shiny::actionLink(ns("ex_reg3"), "chr21:34600000-34700000")
                ),
                shiny::uiOutput(ns("region_validate_ui")),
                shiny::actionButton(ns("btn_add_region"),
                  shiny::tagList(shiny::icon("plus"), " Ajouter la r\u00e9gion"),
                  class = "btn btn-primary btn-sm mt-1")
              )
            ),
            bslib::nav_panel(
              shiny::tagList(shiny::icon("file-alt"), " Fichier BED"),
              shiny::div(class = "mt-2",
                shiny::div(class = "alert alert-info p-2 mb-2",
                  shiny::icon("info-circle"),
                  shiny::HTML(" Un fichier BED 3+ colonnes. Chaque ligne génère une figure séparée.")
                ),
                shiny::fileInput(ns("regions_bed"), "Fichier BED de régions (.bed)"),
                shiny::div(class = "d-flex gap-2",
                  shiny::actionButton(ns("btn_load_bed"),
                    shiny::tagList(shiny::icon("upload"), " Charger"),
                    class = "btn btn-primary btn-sm"),
                  shiny::downloadButton(ns("dl_regions_template"),
                    shiny::tagList(shiny::icon("download"), " Template BED"),
                    class = "btn btn-secondary btn-sm")
                )
              )
            )
          ),
          shiny::tags$hr(class = "divider"),
          shiny::tags$div(
            class = "flex-between mb-2",
            shiny::tags$h6(class = "mb-0",
              shiny::tagList(shiny::icon("list"), " Régions sélectionnées")
            ),
            shiny::actionButton(ns("btn_clear_regions"),
              shiny::tagList(shiny::icon("times"), " Effacer"),
              class = "btn btn-danger btn-sm")
          ),
          shiny::uiOutput(ns("regions_list_ui"))
        )
      ),
      shiny::column(6,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("image")),
            shiny::tags$h5("Paramètres de figure")
          ),
          shiny::selectInput(ns("output_format"), "Format de sortie",
                             choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                             selected = "png"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("width"), "Largeur (cm)", value = 38, min = 5, max = 200)
            ),
            shiny::column(6,
              shiny::numericInput(ns("dpi"), "Résolution (DPI)", value = 150, min = 72, max = 600)
            )
          ),
          shiny::textInput(ns("title"), "Titre de la figure", placeholder = "ex: Locus GENE1"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("fontsize"), "Taille de police", value = 14, min = 6, max = 40)
            ),
            shiny::column(6,
              shiny::numericInput(ns("track_label_fraction"), "Fraction étiquette",
                                  value = 0.1, min = 0.01, max = 0.5, step = 0.01)
            )
          ),
          shiny::selectInput(ns("track_label_h_align"), "Alignement étiquettes",
                             choices = c("Gauche" = "left", "Centre" = "center", "Droite" = "right")),
          shiny::checkboxInput(ns("decreasing_x_axis"), "Axe X décroissant (brin –)", value = FALSE),
          shiny::selectInput(ns("renderer"), "Renderer",
                             choices = c("pyGenomeTracks" = "pyGenomeTracks",
                                         "rGenomeTracks"  = "rGenomeTracks",
                                         "Les deux"       = "both"),
                             selected = "pyGenomeTracks"),
          shiny::div(class = "alert alert-warning p-2",
            shiny::icon("exclamation-triangle"),
            " pyGenomeTracks doit être installé dans l'environnement conda actif."
          ),
          shiny::textInput(ns("output_basename"), "Préfixe de sortie", value = "figure"),
          shiny::actionButton(ns("btn_save_settings"),
            shiny::tagList(shiny::icon("save"), " Enregistrer les paramètres"),
            class = "btn btn-primary w-100")
        )
      )
    )
  )
}

#' Region & Figure Settings server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @export
mod_region_settings_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with existing app state values
    shiny::observe({
      fs <- app_state$figure_settings
      if (!is.null(fs)) {
        shiny::updateSelectInput(session, "output_format", selected = fs$output_format %||% "png")
        shiny::updateNumericInput(session, "width",    value = fs$width    %||% 38)
        shiny::updateNumericInput(session, "dpi",      value = fs$dpi      %||% 150)
        shiny::updateTextInput(session, "title",       value = fs$title    %||% "")
        shiny::updateNumericInput(session, "fontsize", value = fs$fontsize %||% 14)
        shiny::updateSelectInput(session, "renderer",  selected = fs$renderer %||% "pyGenomeTracks")
        shiny::updateTextInput(session, "output_basename", value = fs$output_basename %||% "figure")
      }
    })

    # =========================================================================
    # Chromosome index (guided region selection)
    # =========================================================================

    # reactiveVal holding the last index result or NULL
    chrom_index_rv <- shiny::reactiveVal(NULL)

    # Helper: get file IDs from active tracks
    .active_file_ids <- function() {
      trks <- app_state$tracks %||% list()
      if (length(trks) == 0L) return(NULL)
      ids <- vapply(trks, function(t) t$file_id %||% "", character(1L))
      ids <- ids[!is.na(ids) & nchar(ids) > 0L & ids != "NULL"]
      if (length(ids) == 0L) NULL else ids
    }

    # Auto-refresh index when project or registry changes
    shiny::observe({
      proj <- app_state$project_config
      reg  <- app_state$registry
      if (is.null(proj) || is.null(reg) || nrow(reg) == 0L) {
        chrom_index_rv(NULL)
        return()
      }
      # Try cache first
      cache <- load_chrom_index_cache(proj)
      if (is_chrom_index_cache_valid(cache, reg)) {
        message("[genome_index] Using cached chrom index.")
        # Rebuild index result from cache
        idx_df <- tryCatch(
          as.data.frame(cache$index, stringsAsFactors = FALSE),
          error = function(e) NULL
        )
        if (!is.null(idx_df) && nrow(idx_df) > 0L) {
          chrom_index_rv(list(
            index      = idx_df,
            compat     = cache$compat %||% "ok",
            compat_msg = cache$compat_msg %||% "",
            per_file   = list()
          ))
          return()
        }
      }
    })

    # Manual / forced build
    shiny::observeEvent(input$btn_analyze_chroms, {
      proj <- app_state$project_config
      reg  <- app_state$registry
      if (is.null(proj)) {
        shiny::showNotification("Aucun projet actif.", type = "warning")
        return()
      }
      if (is.null(reg) || nrow(reg) == 0L) {
        shiny::showNotification("Registre vide — ajoutez des fichiers d'abord.", type = "warning")
        return()
      }

      active_ids <- .active_file_ids()
      ids_to_use <- if (!is.null(active_ids)) active_ids else NULL

      shiny::showNotification("Analyse des chromosomes en cours\u2026", id = "chrom_notif",
                              duration = NULL, type = "message")
      tryCatch({
        result <- build_project_chrom_index(proj, reg, active_file_ids = ids_to_use)
        chrom_index_rv(result)
        save_chrom_index_cache(result, reg, proj)
        shiny::removeNotification("chrom_notif")
        n <- if (!is.null(result$index)) nrow(result$index) else 0L
        shiny::showNotification(sprintf("%d chromosome(s) d\u00e9tect\u00e9(s).", n),
                                type = "message", duration = 4)
      }, error = function(e) {
        shiny::removeNotification("chrom_notif")
        shiny::showNotification(sprintf("Erreur analyse : %s", e$message), type = "error", duration = 8)
      })
    })

    # Update chromosome selectInput when index changes
    shiny::observe({
      res <- chrom_index_rv()
      if (is.null(res) || is.null(res$index) || nrow(res$index) == 0L) {
        shiny::updateSelectInput(session, "guided_chrom",
          choices = c("— analyser d\u2019abord —" = ""), selected = "")
        return()
      }
      idx <- res$index
      # Build labels: "Bd1 — 75.6 Mb — BigWig+GFF"
      sizes_str <- vapply(idx$length, function(l) {
        if (is.na(l) || l <= 0L) return("taille inconnue")
        if (l >= 1e9) sprintf("%.0f Gb", l / 1e9)
        else if (l >= 1e6) sprintf("%.1f Mb", l / 1e6)
        else if (l >= 1e3) sprintf("%.0f kb", l / 1e3)
        else sprintf("%d bp", as.integer(l))
      }, character(1L))
      labels <- paste0(idx$chrom, " \u2014 ", sizes_str,
                       " \u2014 ", gsub("\\+", " + ", idx$source_types))
      choices <- stats::setNames(idx$chrom, labels)
      shiny::updateSelectInput(session, "guided_chrom", choices = choices, selected = idx$chrom[1L])
    })

    # ---- Compatibility badge ----
    output$chrom_index_badge <- shiny::renderUI({
      res <- chrom_index_rv()
      if (is.null(res)) return(NULL)
      n <- if (!is.null(res$index)) nrow(res$index) else 0L
      cls <- switch(res$compat,
        "ok"      = "badge bg-success",
        "warning" = "badge bg-warning text-dark",
        "error"   = "badge bg-danger",
        "badge bg-secondary"
      )
      shiny::tags$span(class = cls, sprintf("%d chr", n))
    })

    output$chrom_compat_alert <- shiny::renderUI({
      res <- chrom_index_rv()
      if (is.null(res) || res$compat == "ok") return(NULL)
      alert_cls <- if (res$compat == "error") "alert alert-danger p-2 mt-1" else "alert alert-warning p-2 mt-1"
      icon_name  <- if (res$compat == "error") "times-circle" else "exclamation-triangle"
      shiny::div(class = alert_cls,
        shiny::icon(icon_name), " ", shiny::HTML(htmltools::htmlEscape(res$compat_msg)))
    })

    # ---- Chromosome size info ----
    output$chrom_size_info <- shiny::renderUI({
      res   <- chrom_index_rv()
      ch    <- input$guided_chrom
      if (is.null(res) || is.null(res$index) || is.null(ch) || ch == "") return(NULL)
      row <- res$index[res$index$chrom == ch, , drop = FALSE]
      if (nrow(row) == 0L) return(NULL)
      l <- row$length[1L]
      src <- row$length_source[1L]
      src_label <- switch(src,
        "bigwig_header"          = "BigWig header",
        "gff3_sequence_region"   = "GFF3 ##sequence-region",
        "inferred_from_features" = paste0("estim\u00e9 depuis ", row$n_features[1L], " features"),
        src
      )
      # compatibility badge per chromosome
      badges <- character(0)
      if (isTRUE(row$in_bigwig[1L])) badges <- c(badges, '<span class="badge bg-info text-dark me-1">BigWig</span>')
      if (isTRUE(row$in_gff[1L]))    badges <- c(badges, '<span class="badge bg-primary me-1">GFF/GTF</span>')
      if (isTRUE(row$in_text[1L]))   badges <- c(badges, '<span class="badge bg-secondary me-1">BED/BedGraph</span>')
      size_str <- if (!is.na(l) && l > 0L) {
        if (l >= 1e6) sprintf("%s bp (%.1f Mb)", format(as.integer(l), big.mark = "\u00a0"), l / 1e6)
        else          sprintf("%s bp", format(as.integer(l), big.mark = "\u00a0"))
      } else "taille inconnue"

      shiny::div(class = "chrom-size-info mb-2 p-2",
        shiny::HTML(paste(badges, collapse = "")),
        shiny::tags$small(class = "d-block text-muted mt-1",
          sprintf("Taille : %s \u2014 Source : %s", size_str, src_label)
        )
      )
    })

    # ---- Update start/end max when chromosome changes ----
    shiny::observe({
      res <- chrom_index_rv()
      ch  <- input$guided_chrom
      if (is.null(res) || is.null(res$index) || is.null(ch) || ch == "") return()
      row <- res$index[res$index$chrom == ch, , drop = FALSE]
      if (nrow(row) == 0L) return()
      chrom_len <- as.integer(row$length[1L])
      if (is.na(chrom_len) || chrom_len <= 0L) return()
      cur_end <- input$guided_end %||% 100000L
      new_end <- min(cur_end, chrom_len)
      shiny::updateNumericInput(session, "guided_start", max = chrom_len - 1L)
      shiny::updateNumericInput(session, "guided_end",   max = chrom_len, value = new_end)
    })

    # ---- Quick windows ----
    .apply_window <- function(size_bp) {
      s   <- max(1L, as.integer(input$guided_start %||% 1L))
      res <- chrom_index_rv()
      ch  <- input$guided_chrom %||% ""
      chrom_len <- if (!is.null(res) && !is.null(res$index) && ch != "") {
        row <- res$index[res$index$chrom == ch, , drop = FALSE]
        if (nrow(row) > 0L) as.integer(row$length[1L]) else NA_integer_
      } else NA_integer_
      e_raw <- s + as.integer(size_bp) - 1L
      e <- if (!is.na(chrom_len) && chrom_len > 0L) min(e_raw, chrom_len) else e_raw
      shiny::updateNumericInput(session, "guided_end", value = e)
    }
    shiny::observeEvent(input$win_50k,  { .apply_window(50000L)   })
    shiny::observeEvent(input$win_100k, { .apply_window(100000L)  })
    shiny::observeEvent(input$win_250k, { .apply_window(250000L)  })
    shiny::observeEvent(input$win_500k, { .apply_window(500000L)  })
    shiny::observeEvent(input$win_1m,   { .apply_window(1000000L) })

    # ---- Preview of the guided region ----
    output$guided_region_preview <- shiny::renderUI({
      ch <- input$guided_chrom %||% ""
      s  <- as.integer(input$guided_start %||% 1L)
      e  <- as.integer(input$guided_end   %||% 100000L)
      if (nchar(ch) == 0L || is.na(s) || is.na(e)) return(NULL)
      if (s >= e) {
        return(shiny::div(class = "alert alert-danger p-1 small mt-1",
          shiny::icon("times-circle"), " Le d\u00e9but doit \u00eatre \u003c la fin."))
      }
      region_str <- sprintf("%s:%d-%d", ch, s, e)
      width_bp   <- e - s
      width_str  <- if (width_bp >= 1e6) sprintf("%.2f Mb", width_bp / 1e6)
                    else if (width_bp >= 1e3) sprintf("%.1f kb", width_bp / 1e3)
                    else sprintf("%d bp", width_bp)

      # Validate against index
      ci  <- chrom_index_rv()
      val <- validate_region_against_index(region_str,
                                            if (!is.null(ci)) ci$index else NULL)
      val_ui <- if (val$status == "ok") {
        shiny::div(class = "text-success small", shiny::icon("check-circle"), " R\u00e9gion valide")
      } else if (val$status == "warning") {
        shiny::div(class = "alert alert-warning p-1 small mt-1",
          shiny::icon("exclamation-triangle"), " ", shiny::HTML(htmltools::htmlEscape(
            paste(val$messages, collapse = " | "))))
      } else {
        shiny::div(class = "alert alert-danger p-1 small mt-1",
          shiny::icon("times-circle"), " ", shiny::HTML(htmltools::htmlEscape(
            paste(val$messages, collapse = " | "))))
      }

      shiny::tagList(
        shiny::div(class = "region-preview-chip mt-1 mb-2",
          shiny::tags$code(region_str),
          shiny::tags$small(class = "text-muted ms-2", sprintf("(%s)", width_str))
        ),
        val_ui
      )
    })

    # ---- Add guided region ----
    shiny::observeEvent(input$btn_add_guided_region, {
      ch <- input$guided_chrom %||% ""
      s  <- as.integer(input$guided_start %||% 1L)
      e  <- as.integer(input$guided_end   %||% 100000L)
      if (nchar(ch) == 0L) {
        shiny::showNotification("S\u00e9lectionnez un chromosome.", type = "warning"); return()
      }
      if (is.na(s) || is.na(e) || s >= e) {
        shiny::showNotification("D\u00e9but doit \u00eatre < fin.", type = "warning"); return()
      }
      region_str <- sprintf("%s:%d-%d", ch, s, e)
      existing   <- app_state$regions %||% character(0)
      if (region_str %in% existing) {
        shiny::showNotification("Cette r\u00e9gion est d\u00e9j\u00e0 dans la liste.", type = "warning")
        return()
      }
      app_state$regions <- c(existing, region_str)
      shiny::showNotification(sprintf("R\u00e9gion ajout\u00e9e : %s", region_str),
                              type = "message", duration = 4)
    })

    # =========================================================================
    # Gene picker — full index, cache, annotation file selector, refresh
    # =========================================================================

    gene_index_rv       <- shiny::reactiveVal(NULL)
    selected_gff_rv     <- shiny::reactiveVal(NULL)
    gene_cache_status_rv <- shiny::reactiveVal("non indexé")

    # Reactive: GFF/GTF rows available in registry
    gff_rows_rv <- shiny::reactive({
      reg <- app_state$registry
      if (is.null(reg) || nrow(reg) == 0L) return(NULL)
      rows <- reg[tolower(reg$file_type_detected %||% "") %in%
                    c("gtf", "gff", "gff3"), , drop = FALSE]
      if (nrow(rows) == 0L) NULL else rows
    })

    # Auto-select first GFF when registry changes
    shiny::observe({
      gff_rows <- gff_rows_rv()
      if (is.null(gff_rows)) {
        selected_gff_rv(NULL)
        gene_index_rv(NULL)
        return()
      }
      cur <- shiny::isolate(selected_gff_rv())
      # Prefer an annotation file already used by active tracks.
      active_track_ids <- {
        trks <- app_state$tracks %||% list()
        if (length(trks) > 0L) {
          ids <- vapply(trks, function(t) t$file_id %||% "", character(1L))
          ids[!is.na(ids) & nchar(ids) > 0L]
        } else character(0)
      }
      preferred <- intersect(gff_rows$file_id, active_track_ids)
      default_id <- if (length(preferred) > 0L) preferred[1L] else gff_rows$file_id[1L]
      if (is.null(cur) || !cur %in% gff_rows$file_id) {
        selected_gff_rv(default_id)
      }
    })

    # Build gene index (with cache) for the selected GFF file
    .build_gene_index <- function(from_cache = TRUE) {
      gff_rows <- shiny::isolate(gff_rows_rv())
      if (is.null(gff_rows)) { gene_index_rv(NULL); return() }
      fid  <- shiny::isolate(selected_gff_rv())
      if (is.null(fid) || !fid %in% gff_rows$file_id) { gene_index_rv(NULL); return() }
      row   <- gff_rows[gff_rows$file_id == fid, , drop = FALSE]
      fpath <- row$stored_path[1L]
      if (!file.exists(fpath)) { gene_index_rv(NULL); return() }
      proj  <- shiny::isolate(app_state$project_config)

      # Try cache
      if (from_cache && !is.null(proj)) {
        cache <- load_gene_index_cache(fid, proj)
        if (is_gene_index_cache_valid(cache, fpath)) {
          gene_df <- tryCatch(as.data.frame(cache$genes, stringsAsFactors = FALSE),
                              error = function(e) NULL)
          if (!is.null(gene_df) && nrow(gene_df) > 0L) {
            message(sprintf("[genome_index] Gene index cache hit: %d genes from %s",
                            nrow(gene_df), basename(fpath)))
            gene_index_rv(gene_df)
            gene_cache_status_rv("index à jour (cache)")
            return()
          }
        }
      }

      # Build from file
      shiny::showNotification(
        shiny::tagList(shiny::icon("spinner"), " Indexation des g\u00e8nes en cours\u2026"),
        id = "gene_notif", duration = NULL, type = "message")
      tryCatch({
        gene_df <- inspect_gff_genes(fpath)
        if (!is.null(gene_df) && nrow(gene_df) > 0L && !is.null(proj)) {
          save_gene_index_cache(gene_df, fpath, fid, proj)
        }
        gene_index_rv(gene_df)
        gene_cache_status_rv("index reconstruit")
        shiny::removeNotification("gene_notif")
        n <- if (!is.null(gene_df)) nrow(gene_df) else 0L
        shiny::showNotification(
          sprintf("%s g\u00e8nes index\u00e9s depuis %s",
                  format(n, big.mark = "\u00a0"), basename(fpath)),
          type = "message", duration = 6)
      }, error = function(e) {
        shiny::removeNotification("gene_notif")
        shiny::showNotification(
          sprintf("Erreur indexation g\u00e8nes : %s", e$message),
          type = "error", duration = 8)
        gene_index_rv(NULL)
        gene_cache_status_rv("erreur")
      })
    }

    # Trigger rebuild when selected GFF changes
    shiny::observe({
      selected_gff_rv()  # reactive dependency
      shiny::isolate(.build_gene_index(from_cache = TRUE))
    })

    # Manual refresh (force rebuild, skip cache)
    shiny::observeEvent(input$btn_refresh_gene_index, {
      .build_gene_index(from_cache = FALSE)
    })

    # Annotation file selector change
    shiny::observeEvent(input$gff_src_file, {
      new_fid <- input$gff_src_file
      if (is.null(new_fid) || nchar(new_fid) == 0L) return()
      if (identical(new_fid, shiny::isolate(selected_gff_rv()))) return()
      selected_gff_rv(new_fid)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Populate gene selectize server-side to avoid sending huge lists to client
    shiny::observe({
      gi <- gene_index_rv()
      if (is.null(gi) || nrow(gi) == 0L) {
        shiny::updateSelectizeInput(session, "gene_picker_sel",
          choices = c("" = ""), selected = "", server = TRUE)
        return()
      }
      choices_vec <- build_gene_selectize_choices(gi)
      shiny::updateSelectizeInput(session, "gene_picker_sel",
        choices = choices_vec, selected = "", server = TRUE)
    })

    # Render gene picker UI block
    output$gene_picker_block <- shiny::renderUI({
      gff_rows <- gff_rows_rv()
      if (is.null(gff_rows)) return(NULL)

      gi      <- gene_index_rv()
      cur_gff <- selected_gff_rv() %||% gff_rows$file_id[1L]

      # Annotation file selector
      gff_choices <- stats::setNames(
        gff_rows$file_id,
        paste0(basename(gff_rows$stored_path), " (",
               toupper(gff_rows$file_type_detected), ")")
      )
      gff_selector <- shiny::selectInput(
        ns("gff_src_file"),
        shiny::tagList(shiny::icon("file-alt"), " Fichier d\u2019annotation"),
        choices  = gff_choices,
        selected = cur_gff
      )

      # Header: count badge + source filename
      if (!is.null(gi) && nrow(gi) > 0L) {
        src_file <- if ("source_file" %in% names(gi)) basename(gi$source_file[1L]) else ""
        cache_status <- gene_cache_status_rv() %||% "non indexé"
        header_content <- shiny::tagList(
          shiny::tags$span(
            class = "badge bg-success ms-2",
            format(nrow(gi), big.mark = "\u00a0"), " g\u00e8nes index\u00e9s"
          ),
          if (nchar(src_file) > 0L)
            shiny::tags$small(class = "text-muted d-block mt-1",
              shiny::icon("database"), " ", src_file,
              " — ", cache_status)
          else NULL
        )
        gene_picker_body <- shiny::tagList(
          shiny::selectizeInput(
            ns("gene_picker_sel"),
            "Choisir un g\u00e8ne",
            choices  = NULL,
            selected = "",
            options  = list(
              maxOptions  = 100L,
              placeholder = "ID, nom, locus_tag\u2026"
            )
          ),
          shiny::selectInput(ns("gene_flank"), "Flanquement",
            choices  = c("1 kb" = 1000, "5 kb" = 5000, "10 kb" = 10000,
                         "50 kb" = 50000, "100 kb" = 100000,
                         "Personnalisé" = "custom"),
            selected = 5000
          ),
          shiny::numericInput(ns("gene_flank_custom"),
            "Flanquement personnalisé (bp)",
            value = 5000L, min = 1L, max = 50000000L, step = 100L),
          shiny::actionButton(ns("btn_add_gene_region"),
            shiny::tagList(shiny::icon("plus"), " Ajouter r\u00e9gion autour du g\u00e8ne"),
            class = "btn btn-outline-primary btn-sm w-100 mt-1")
        )
      } else {
        header_content <- shiny::tags$small(
          class = "text-muted ms-2",
          if (is.null(gi))
            "Cliquez sur \u00ab\u202fRafra\u00eechir\u202f\u00bb pour indexer."
          else
            "Aucun g\u00e8ne trouv\u00e9 dans ce fichier."
        )
        gene_picker_body <- NULL
      }

      shiny::div(class = "gene-picker-card mt-3",
        shiny::div(class = "gene-picker-header mb-2",
          shiny::icon("dna"), " ",
          shiny::tags$strong("R\u00e9gion autour d\u2019un g\u00e8ne"),
          header_content
        ),
        gff_selector,
        shiny::actionButton(
          ns("btn_refresh_gene_index"),
          shiny::tagList(shiny::icon("sync"), " Rafra\u00eechir l\u2019index g\u00e8nes"),
          class = "btn btn-outline-secondary btn-sm mb-2"
        ),
        gene_picker_body
      )
    })

    shiny::observeEvent(input$btn_add_gene_region, {
      gi  <- gene_index_rv()
      sel <- as.integer(input$gene_picker_sel %||% "")
      if (is.null(gi) || is.null(sel) || is.na(sel) || sel < 1L || sel > nrow(gi)) {
        shiny::showNotification("S\u00e9lectionnez un g\u00e8ne.", type = "warning"); return()
      }
      flank <- if (identical(input$gene_flank, "custom")) {
        max(1L, as.integer(input$gene_flank_custom %||% 5000L))
      } else {
        as.integer(input$gene_flank %||% 5000L)
      }
      gene_row <- gi[sel, ]
      ch   <- gene_row$chrom
      s    <- max(1L, gene_row$start - flank)
      e_raw <- gene_row$end + flank
      chrom_len_known <- FALSE

      # Clamp to chromosome length if known
      ci  <- chrom_index_rv()
      if (!is.null(ci) && !is.null(ci$index)) {
        row_ch <- ci$index[ci$index$chrom == ch, , drop = FALSE]
        if (nrow(row_ch) > 0L) {
          chrom_len <- as.integer(row_ch$length[1L])
          if (!is.na(chrom_len) && chrom_len > 0L) {
            e_raw <- min(e_raw, chrom_len)
            chrom_len_known <- TRUE
          }
        }
      }
      if (!chrom_len_known) {
        shiny::showNotification(
          "Longueur du chromosome inconnue, fin non bornée par une taille officielle.",
          type = "warning", duration = 5)
      }
      region_str <- sprintf("%s:%d-%d", ch, s, e_raw)
      existing   <- app_state$regions %||% character(0)
      if (region_str %in% existing) {
        shiny::showNotification("D\u00e9j\u00e0 dans la liste.", type = "warning"); return()
      }
      app_state$regions <- c(existing, region_str)
      shiny::showNotification(
        sprintf("R\u00e9gion ajout\u00e9e : %s (\u00b1%s autour de %s)",
                region_str, format(flank, big.mark = " "), gene_row$name),
        type = "message", duration = 5)
    })

    # =========================================================================
    # Manual input (legacy)
    # =========================================================================

    # Exemples rapides
    shiny::observeEvent(input$ex_reg1, {
      shiny::updateTextInput(session, "region_single", value = "chr1:1000-20000")
    })
    shiny::observeEvent(input$ex_reg2, {
      shiny::updateTextInput(session, "region_single", value = "chrX:100000-500000")
    })
    shiny::observeEvent(input$ex_reg3, {
      shiny::updateTextInput(session, "region_single", value = "chr21:34600000-34700000")
    })

    output$region_validate_ui <- shiny::renderUI({
      reg <- trimws(input$region_single %||% "")
      if (nchar(reg) == 0) return(NULL)
      err <- if (!validate_region(reg)) sprintf("Format invalide : '%s'. Attendu : chr:debut-fin", reg) else NULL
      if (is.null(err)) {
        shiny::div(class = "text-success small mt-1",
          shiny::icon("check-circle"), sprintf(" Format valide : %s", reg))
      } else {
        shiny::div(class = "text-danger small mt-1",
          shiny::icon("times-circle"), sprintf(" %s", err))
      }
    })

    shiny::observeEvent(input$btn_add_region, {
      reg <- trimws(input$region_single %||% "")
      if (nchar(reg) == 0) return()
      err <- if (!validate_region(reg)) sprintf("Région invalide : '%s'", reg) else NULL
      if (!is.null(err)) {
        shiny::showNotification(sprintf("Région invalide : %s", reg), type = "warning")
        return()
      }
      existing <- app_state$regions %||% character(0)
      if (reg %in% existing) {
        shiny::showNotification("Cette région est déjà dans la liste.", type = "warning")
        return()
      }
      app_state$regions <- c(existing, reg)
      shiny::updateTextInput(session, "region_single", value = "")
    })

    shiny::observeEvent(input$btn_load_bed, {
      req(input$regions_bed)
      tryCatch({
        regions <- read_regions_bed(input$regions_bed$datapath)
        app_state$regions <- regions
        shiny::showNotification(sprintf("%d région(s) chargée(s).", length(regions)), type = "message")
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur BED : %s", e$message), type = "error")
      })
    })

    output$dl_regions_template <- shiny::downloadHandler(
      filename = function() "template_regions.bed",
      content  = function(file) {
        tpl <- "templates/input_files/template_regions.bed"
        if (file.exists(tpl)) file.copy(tpl, file)
        else writeLines("chr1\t1000\t5000\tregion_1\nchr1\t6000\t10000\tregion_2", file)
      }
    )

    output$regions_list_ui <- shiny::renderUI({
      regions <- app_state$regions %||% character(0)
      if (length(regions) == 0)
        return(shiny::p(class = "text-muted", shiny::em("Aucune région sélectionnée.")))
      items <- lapply(seq_along(regions), function(i) {
        shiny::tags$li(
          shiny::code(regions[i]),
          shiny::actionLink(ns(paste0("del_region_", i)),
            shiny::icon("times"),
            class = "ms-2 text-danger",
            style = "font-size:0.9em;")
        )
      })
      shiny::tags$ul(class = "list-unstyled", items)
    })

    # Suppression individuelle d'une région
    shiny::observe({
      regions <- app_state$regions %||% character(0)
      for (i in seq_along(regions)) {
        local({
          idx <- i
          shiny::observeEvent(input[[paste0("del_region_", idx)]], {
            regs <- app_state$regions %||% character(0)
            if (idx <= length(regs)) app_state$regions <- regs[-idx]
          }, ignoreNULL = TRUE, once = TRUE)
        })
      }
    })

    shiny::observeEvent(input$btn_clear_regions, {
      app_state$regions <- character(0)
    })

    shiny::observeEvent(input$btn_save_settings, {
      app_state$figure_settings <- list(
        output_format        = input$output_format,
        width                = input$width,
        dpi                  = input$dpi,
        title                = input$title %||% "",
        fontsize             = input$fontsize,
        track_label_fraction = input$track_label_fraction,
        track_label_h_align  = input$track_label_h_align,
        decreasing_x_axis    = isTRUE(input$decreasing_x_axis),
        renderer             = input$renderer,
        output_basename      = input$output_basename %||% "figure"
      )
      shiny::showNotification("Paramètres enregistrés.", type = "message")
    })
  })
}
