# =============================================================================
# R/core/ui_helpers.R — rGenomeTrackUI UI helper components
# Purely presentation: no reactive logic, no backend calls.
# =============================================================================

# ---------------------------------------------------------------------------
# status_badge(status)
# Returns a styled inline badge for a status string.
# ---------------------------------------------------------------------------
status_badge <- function(status, label = NULL, show_dot = TRUE) {
  status  <- tolower(as.character(status %||% "unknown"))
  display <- label %||% switch(status,
    "ok"        = "OK",
    "completed" = "Completed",
    "success"   = "Success",
    "running"   = "Running",
    "prepared"  = "Prepared",
    "idle"      = "Idle",
    "warning"   = "Warning",
    "partial"   = "Partial",
    "missing"   = "Missing",
    "error"     = "Error",
    "failed"    = "Failed",
    "disabled"  = "Disabled",
    "inactive"  = "Inactive",
    "enabled"   = "Enabled",
    "no_file_required" = "No file",
    "binary"    = "Binary",
    "text"      = "Text",
    toupper(substr(status, 1, 1))
  )
  css_class <- switch(status,
    "ok"        = ,
    "success"   = ,
    "completed" = ,
    "enabled"   = "rt-badge rt-badge-ok",
    "running"   = "rt-badge rt-badge-running",
    "prepared"  = "rt-badge rt-badge-prepared",
    "warning"   = ,
    "partial"   = "rt-badge rt-badge-warning",
    "missing"   = ,
    "error"     = ,
    "failed"    = "rt-badge rt-badge-error",
    "disabled"  = ,
    "inactive"  = ,
    "idle"      = "rt-badge rt-badge-muted",
    "binary"    = "rt-badge rt-badge-violet",
    "no_file_required" = "rt-badge rt-badge-muted",
    "rt-badge rt-badge-muted"
  )
  dot_html <- if (show_dot) shiny::tags$span(class = "badge-dot") else NULL
  shiny::tags$span(class = css_class, dot_html, display)
}

# ---------------------------------------------------------------------------
# info_card(title, value, subtitle, icon, color)
# Stat card — title as label, value as big number/text.
# color: "accent" | "blue" | "violet" | "success" | "warning" | "danger" | "primary"
# ---------------------------------------------------------------------------
info_card <- function(title, value, subtitle = NULL, icon_name = "circle",
                       color = "accent") {
  icon_html <- shiny::tags$div(
    class = paste("rt-metric-icon", color),
    shiny::icon(icon_name)
  )
  body_html <- shiny::tags$div(
    class = "rt-metric-body",
    shiny::tags$div(class = "rt-metric-value", value),
    shiny::tags$div(class = "rt-metric-label", title),
    if (!is.null(subtitle)) shiny::tags$div(class = "rt-metric-subtitle", subtitle)
  )
  shiny::tags$div(
    class = "rt-metric-card",
    icon_html,
    body_html
  )
}

# ---------------------------------------------------------------------------
# section_header(title, subtitle, icon_name)
# ---------------------------------------------------------------------------
section_header <- function(title, subtitle = NULL, icon_name = NULL) {
  icon_html <- if (!is.null(icon_name)) {
    shiny::tags$div(
      class = "section-icon",
      shiny::icon(icon_name)
    )
  }
  shiny::tags$div(
    class = "rt-section-header",
    icon_html,
    shiny::tags$div(
      shiny::tags$h4(title),
      if (!is.null(subtitle)) shiny::tags$p(class = "section-subtitle", subtitle)
    )
  )
}

# ---------------------------------------------------------------------------
# omics_banner(title, subtitle, status = NULL, right_content = NULL)
# Hero or section banner with animated omics background.
# ---------------------------------------------------------------------------
omics_banner <- function(title, subtitle = NULL, status = NULL,
                          right_content = NULL, small = FALSE) {
  status_html <- if (!is.null(status)) status_badge(status)

  title_tag <- if (small) shiny::tags$h3(title) else shiny::tags$h2(title)

  left <- shiny::tags$div(
    title_tag,
    if (!is.null(subtitle)) shiny::tags$p(class = "subtitle", subtitle),
    status_html
  )

  banner_class <- if (small) "section-banner" else "omics-hero"
  content_class <- if (small) "section-banner-content" else "omics-hero-content"

  shiny::tags$div(
    class = banner_class,
    shiny::tags$div(
      class = content_class,
      left,
      if (!is.null(right_content)) right_content
    )
  )
}

# ---------------------------------------------------------------------------
# workflow_stepper(steps, current)
# steps : character vector of step names
# current : integer index of current/active step
# done_until : integer — steps before current are marked done
# states : optional named list e.g. list(Inputs = "warn", Tracks = "done")
# nav_ids : optional inputIds for actionButton (one per step)
# Each step is an actionButton with class "workflow-step [state]" directly —
# no inner wrapper div.  CSS styles the button element directly.
# ---------------------------------------------------------------------------
workflow_stepper <- function(steps, current = 1L, done_until = NULL,
                              states = list(), nav_ids = NULL) {
  if (is.null(done_until)) done_until <- current - 1L

  n <- length(steps)

  step_items <- lapply(seq_len(n), function(i) {
    explicit <- states[[steps[[i]]]]
    state <- if (!is.null(explicit)) {
      explicit
    } else if (i < current || i <= done_until) {
      "done"
    } else if (i == current) {
      "active"
    } else {
      "pending"
    }

    idx_content <- if (state == "done") {
      shiny::icon("check")
    } else if (state == "warn") {
      shiny::icon("exclamation")
    } else {
      as.character(i)
    }

    btn_class <- paste(
      "workflow-step",
      switch(state,
        "done"   = "workflow-step-done",
        "active" = "workflow-step-active",
        "warn"   = "workflow-step-warn",
        "workflow-step-pending"
      )
    )

    label <- shiny::tagList(
      shiny::tags$span(class = "wf-idx", idx_content),
      shiny::tags$span(class = "wf-lbl", steps[[i]])
    )

    nav_id <- if (!is.null(nav_ids) && i <= length(nav_ids)) nav_ids[[i]] else NULL

    if (!is.null(nav_id)) {
      shiny::actionButton(nav_id, label, class = btn_class)
    } else {
      shiny::tags$div(class = btn_class, label)
    }
  })

  shiny::tags$div(class = "workflow-stepper", step_items)
}

# ---------------------------------------------------------------------------
# code_box(output_id, lang = "ini", title = NULL, allow_copy = TRUE)
# Wraps a shiny verbatimTextOutput/htmlOutput in a styled dark code block.
# ---------------------------------------------------------------------------
code_box <- function(output_id, lang = "ini", title = NULL,
                      allow_copy = TRUE, height = NULL) {
  copy_btn <- if (allow_copy) {
    shiny::tags$button(
      class = "btn-copy-code",
      `data-copy-target` = paste0("#", output_id),
      shiny::icon("copy"), " Copy"
    )
  }
  lang_label <- shiny::tags$span(class = "code-lang", toupper(lang))

  header <- shiny::tags$div(
    class = "rt-code-block-header",
    shiny::tags$div(
      class = "flex-row gap-8",
      lang_label,
      if (!is.null(title)) shiny::tags$span(class = "text-muted", " — ", title)
    ),
    shiny::tags$div(class = "code-actions", copy_btn)
  )

  style_attr <- if (!is.null(height)) paste0("max-height:", height, ";") else NULL
  content <- shiny::verbatimTextOutput(output_id)
  if (!is.null(style_attr)) {
    content <- shiny::tagAppendAttributes(content, style = style_attr)
  }

  shiny::tags$div(
    class = "rt-code-block",
    header,
    content
  )
}

# ---------------------------------------------------------------------------
# empty_state(title, message, action = NULL, icon_name = "inbox")
# ---------------------------------------------------------------------------
empty_state <- function(title, message, action = NULL, icon_name = "inbox") {
  shiny::tags$div(
    class = "rt-empty-state",
    shiny::tags$div(class = "empty-icon", shiny::icon(icon_name)),
    shiny::tags$h4(title),
    shiny::tags$p(message),
    if (!is.null(action)) action
  )
}

# ---------------------------------------------------------------------------
# rt_card(title, ..., icon_name = NULL, class = NULL)
# Wrapper for a clean rt-card with optional header.
# ---------------------------------------------------------------------------
rt_card <- function(title = NULL, ..., icon_name = NULL, footer = NULL,
                     card_class = NULL) {
  header <- if (!is.null(title)) {
    shiny::tags$div(
      class = "rt-card-header",
      if (!is.null(icon_name)) shiny::tags$span(class = "rt-card-icon", shiny::icon(icon_name)),
      shiny::tags$h5(title)
    )
  }
  shiny::tags$div(
    class = paste("rt-card", card_class),
    header,
    ...,
    footer
  )
}

# ---------------------------------------------------------------------------
# checklist_item(label, status, detail = NULL)
# status: "ok" | "warn" | "error" | "info"
# ---------------------------------------------------------------------------
checklist_item <- function(label, status = "info", detail = NULL) {
  icon_name <- switch(status,
    "ok"    = "check-circle",
    "warn"  = "exclamation-triangle",
    "error" = "times-circle",
    "info"  = "circle",
    "circle-o"
  )
  shiny::tags$li(
    shiny::tags$span(class = paste("check-icon", status), shiny::icon(icon_name)),
    shiny::tags$div(
      shiny::tags$span(label),
      if (!is.null(detail)) shiny::tags$div(style = "font-size:11px;color:var(--rt-muted);", detail)
    )
  )
}

# ---------------------------------------------------------------------------
# checklist_ui(items)
# items: list of checklist_item(...)
# ---------------------------------------------------------------------------
checklist_ui <- function(items) {
  shiny::tags$ul(class = "rt-checklist", items)
}

# ---------------------------------------------------------------------------
# download_card_btn(id, label, icon_name, desc = NULL)
# ---------------------------------------------------------------------------
download_card_btn <- function(id, label, icon_name = "download", desc = NULL) {
  shiny::tags$div(
    style = "margin-bottom:8px;",
    shiny::downloadButton(
      id,
      label = shiny::tagList(
        shiny::tags$span(class = "dl-icon", shiny::icon(icon_name)),
        shiny::tags$span(label),
        if (!is.null(desc)) shiny::tags$span(
          style = "font-size:10px;color:var(--rt-muted);margin-left:auto;",
          desc
        )
      ),
      class = "btn btn-download"
    )
  )
}

# ---------------------------------------------------------------------------
# path_block(path)
# Monospace path that can be clicked to copy (via JS)
# ---------------------------------------------------------------------------
path_block <- function(path) {
  shiny::tags$div(
    class = "rt-path-block",
    title = "Click to copy",
    path
  )
}

# ---------------------------------------------------------------------------
# format_ext_badges(extensions)
# extensions: character vector e.g. c(".bed", ".bedgraph")
# ---------------------------------------------------------------------------
format_ext_badges <- function(extensions) {
  if (is.null(extensions) || length(extensions) == 0) return(NULL)
  shiny::tagList(lapply(extensions, function(e) {
    shiny::tags$span(class = "ext-badge", e)
  }))
}

# ---------------------------------------------------------------------------
# track_type_badge(type)
# ---------------------------------------------------------------------------
track_type_badge <- function(type) {
  type_lc <- tolower(as.character(type %||% "unknown"))
  css <- switch(type_lc,
    "bedgraph"        = ,
    "bigwig"          = "track-type-badge track-type-signal",
    "gtf"             = ,
    "genes"           = "track-type-badge track-type-genes",
    "narrowpeak"      = ,
    "peaks"           = "track-type-badge track-type-peaks",
    "bedpe"           = ,
    "links"           = "track-type-badge track-type-links",
    "domains"         = "track-type-badge track-type-domains",
    "hic"             = ,
    "hic_matrix"      = "track-type-badge track-type-hic",
    "x_axis"          = ,
    "xaxis"           = "track-type-badge track-type-axis",
    "spacer"          = "track-type-badge track-type-spacer",
    "bed"             = "track-type-badge track-type-bed",
    "track-type-badge track-type-bed"
  )
  shiny::tags$span(class = css, type)
}

# ---------------------------------------------------------------------------
# dep_item_ui(label, ok, detail = NULL)
# A dependency status row: green check or red cross + label + optional detail.
# Used in mod_dashboard.R and mod_settings.R.
# ---------------------------------------------------------------------------
dep_item_ui <- function(label, ok, detail = NULL) {
  css_class <- if (isTRUE(ok)) "dep-item dep-ok" else "dep-item dep-error"
  icon_name <- if (isTRUE(ok)) "check-circle"    else "times-circle"
  shiny::tags$div(
    class = css_class,
    shiny::tags$span(class = "dep-icon", shiny::icon(icon_name)),
    shiny::tags$span(class = "dep-name", label),
    if (!is.null(detail))
      shiny::tags$span(class = "dep-ver", detail)
  )
}
#' French labels for DT without a runtime CDN request
#'
#' Supplying `language$url` makes DataTables issue an Ajax request.  Besides
#' making the UI depend on internet access, a failed request is reported as a
#' misleading "DataTables Ajax error".  Keep the small set of labels used by
#' the application local instead.
rt_dt_language <- function() {
  list(
    emptyTable = "Aucune donnée disponible",
    info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
    infoEmpty = "Affichage de 0 à 0 sur 0 entrée",
    infoFiltered = "(filtré de _MAX_ entrées au total)",
    lengthMenu = "Afficher _MENU_ entrées",
    loadingRecords = "Chargement…",
    processing = "Traitement…",
    search = "Rechercher :",
    zeroRecords = "Aucun résultat trouvé",
    paginate = list(first = "Premier", last = "Dernier", `next` = "Suivant", previous = "Précédent")
  )
}
