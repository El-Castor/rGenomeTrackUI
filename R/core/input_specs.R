# =============================================================================
# input_specs.R — Fonctions de chargement et validation des specs de formats
# =============================================================================

# Null-coalescing operator (standalone definition for when this file is sourced in isolation)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

# Cache interne
.input_specs_cache <- NULL

#' Charge les spécifications de formats depuis config/input_file_specs.yaml
#'
#' @param config_dir Chemin vers le dossier config (défaut: detect automatiquement)
#' @return Liste des specs par format
load_input_specs <- function(config_dir = NULL) {
  if (!is.null(.input_specs_cache)) return(.input_specs_cache)

  if (is.null(config_dir)) {
    # Détection depuis l'environnement Shiny ou le répertoire courant
    app_root <- tryCatch(
      dirname(getwd()),
      error = function(e) "."
    )
    candidates <- c(
      file.path(getwd(), "config", "input_file_specs.yaml"),
      file.path(dirname(getwd()), "config", "input_file_specs.yaml"),
      "config/input_file_specs.yaml"
    )
    spec_file <- candidates[file.exists(candidates)][1]
  } else {
    spec_file <- file.path(config_dir, "input_file_specs.yaml")
  }

  if (is.na(spec_file) || !file.exists(spec_file)) {
    warning("input_file_specs.yaml introuvable. Retour d'une liste vide.")
    return(list())
  }

  specs <- yaml::read_yaml(spec_file)
  specs <- specs$formats

  # Assigner l'identifiant de format à chaque entrée
  for (fmt in names(specs)) {
    specs[[fmt]]$id <- fmt
  }

  .input_specs_cache <<- specs
  specs
}

#' Remet à zéro le cache (utile pour les tests)
reset_input_specs_cache <- function() {
  .input_specs_cache <<- NULL
}

#' Retourne les noms de tous les formats supportés
#'
#' @param specs Liste de specs (charger avec load_input_specs si NULL)
#' @return character vector des ids de formats
get_supported_input_formats <- function(specs = NULL) {
  if (is.null(specs)) specs <- load_input_specs()
  names(specs)
}

#' Retourne la spec d'un format particulier
#'
#' @param format_id Identifiant du format (ex: "bed", "bigwig")
#' @param specs Liste de specs
#' @return Liste d'attributs du format, ou NULL si inconnu
get_format_spec <- function(format_id, specs = NULL) {
  if (is.null(specs)) specs <- load_input_specs()
  specs[[format_id]]
}

#' Retourne les formats compatibles avec un type de track
#'
#' @param track_type Identifiant du type de track (ex: "bigwig", "bed")
#' @param specs Liste de specs
#' @return character vector des ids de formats compatibles
get_formats_for_track_type <- function(track_type, specs = NULL) {
  if (is.null(specs)) specs <- load_input_specs()
  compatible <- c()
  for (fmt_id in names(specs)) {
    s <- specs[[fmt_id]]
    if (track_type %in% (s$associated_tracks %||% c())) {
      compatible <- c(compatible, fmt_id)
    }
  }
  compatible
}

#' Retourne les extensions valides pour un type de track
#'
#' @param track_type Identifiant du type de track
#' @param specs Liste de specs
#' @return character vector des extensions (ex: c(".bw", ".bigwig"))
get_extensions_for_track_type <- function(track_type, specs = NULL) {
  fmts <- get_formats_for_track_type(track_type, specs)
  exts <- c()
  for (fmt_id in fmts) {
    exts <- c(exts, specs[[fmt_id]]$extensions %||% c())
  }
  unique(exts)
}

#' Génère un texte d'aide HTML sur un format
#'
#' @param format_id Identifiant du format
#' @param specs Liste de specs
#' @return Chaîne HTML
render_format_help <- function(format_id, specs = NULL) {
  if (is.null(specs)) specs <- load_input_specs()
  spec <- specs[[format_id]]
  if (is.null(spec)) return("<em>Format inconnu.</em>")

  cols_html <- ""
  if (length(spec$columns) > 0) {
    required <- spec$required_columns %||% c()
    col_items <- vapply(spec$columns, function(col) {
      if (col %in% required) {
        paste0("<li><strong>", col, "</strong> <span class='text-success'>(requis)</span></li>")
      } else {
        paste0("<li>", col, " <span class='text-muted'>(optionnel)</span></li>")
      }
    }, character(1))
    cols_html <- paste0("<ul>", paste(col_items, collapse = ""), "</ul>")
  }

  notes_html <- ""
  if (length(spec$notes) > 0) {
    note_items <- vapply(spec$notes, function(n) paste0("<li>", n, "</li>"), character(1))
    notes_html <- paste0("<ul>", paste(note_items, collapse = ""), "</ul>")
  }

  example_html <- ""
  if (!is.null(spec$example) && nchar(trimws(spec$example)) > 0) {
    example_html <- paste0(
      "<h6>Exemple :</h6><pre class='bg-light p-2' style='font-size:0.8em'>",
      htmltools::htmlEscape(trimws(spec$example)),
      "</pre>"
    )
  }

  binary_badge <- if (isTRUE(spec$binary)) {
    "<span class='badge bg-warning text-dark'>Binaire</span>"
  } else {
    "<span class='badge bg-success'>Texte</span>"
  }

  exts_str <- paste(spec$extensions %||% c("N/A"), collapse = ", ")

  paste0(
    "<div class='format-help'>",
    "<p>", spec$description %||% "", "</p>",
    "<p><strong>Extensions :</strong> <code>", exts_str, "</code> &nbsp; ", binary_badge, "</p>",
    if (nchar(cols_html) > 0) paste0("<h6>Colonnes :</h6>", cols_html) else "",
    if (nchar(notes_html) > 0) paste0("<h6>Notes :</h6>", notes_html) else "",
    example_html,
    "</div>"
  )
}

#' Retourne le chemin vers le fichier template d'un format
#'
#' @param format_id Identifiant du format
#' @param templates_dir Chemin du dossier templates
#' @param specs Liste de specs
#' @return Chemin absolu ou NULL si inexistant
get_template_path <- function(format_id, templates_dir = "templates/input_files", specs = NULL) {
  if (is.null(specs)) specs <- load_input_specs()
  spec <- specs[[format_id]]
  if (is.null(spec)) return(NULL)
  fname <- spec$template_file
  if (is.null(fname) || is.na(fname)) return(NULL)
  path <- file.path(templates_dir, fname)
  if (file.exists(path)) path else NULL
}

#' Retourne le chemin vers le fichier exemple d'un format
#'
#' @param format_id Identifiant du format
#' @param examples_dir Chemin du dossier examples
#' @param specs Liste de specs
#' @return Chemin absolu ou NULL si inexistant
get_example_path <- function(format_id, examples_dir = "examples/input_files", specs = NULL) {
  if (is.null(specs)) specs <- load_input_specs()
  spec <- specs[[format_id]]
  if (is.null(spec)) return(NULL)
  fname <- spec$example_file
  if (is.null(fname) || is.na(fname)) return(NULL)
  path <- file.path(examples_dir, fname)
  if (file.exists(path)) path else NULL
}

#' Valide un fichier par rapport aux specs d'un format
#'
#' @param file_path Chemin du fichier à valider
#' @param format_id Identifiant du format attendu
#' @param original_name Nom original du fichier. Nécessaire pour les uploads
#'   Shiny dont le chemin temporaire ne conserve pas l'extension.
#' @param specs Liste de specs
#' @return Liste avec status ("ok" / "warning" / "error"), messages et preview (5 premières lignes)
validate_against_format_spec <- function(file_path, format_id, specs = NULL,
                                         original_name = NULL) {
  if (is.null(specs)) specs <- load_input_specs()

  result <- list(status = "ok", messages = c(), preview = NULL)

  # 1. Vérification existence fichier
  if (!file.exists(file_path)) {
    return(list(
      status = "error",
      messages = paste0("Fichier introuvable : ", file_path),
      preview = NULL
    ))
  }

  spec <- specs[[format_id]]
  if (is.null(spec)) {
    return(list(
      status = "warning",
      messages = paste0("Format '", format_id, "' inconnu, validation ignorée."),
      preview = NULL
    ))
  }

  # 2. Vérification extension
  extension_source <- if (!is.null(original_name) && nzchar(trimws(original_name))) {
    original_name
  } else {
    file_path
  }
  fext <- tolower(tools::file_ext(extension_source))
  valid_exts <- tolower(gsub("^\\.", "", spec$extensions %||% c()))
  if (length(valid_exts) > 0 && !fext %in% valid_exts) {
    result$status <- "warning"
    result$messages <- c(result$messages, paste0(
      "Extension inattendue '.", fext,
      "' pour le format ", spec$label,
      " (attendu : ", paste(spec$extensions, collapse = ", "), ")"
    ))
  }

  # 3. Fichiers binaires : validation par magic bytes uniquement
  if (isTRUE(spec$binary)) {
    fsize <- file.info(file_path)$size
    size_mb <- round(fsize / 1024^2, 1)
    result$messages <- c(result$messages, paste0("Taille : ", size_mb, " Mo"))

    # BigWig magic bytes : 0x888FFC26 (big-endian) ou 0x26FC8F88 (little-endian)
    if (fext %in% c("bw", "bigwig")) {
      tryCatch({
        con <- file(file_path, "rb")
        magic <- readBin(con, what = "raw", n = 4)
        close(con)
        magic_hex <- paste(as.character(magic), collapse = "")
        bw_le <- magic_hex == "26fc8f88"
        bw_be <- magic_hex == "888ffc26"
        if (bw_le || bw_be) {
          result$messages <- c(result$messages, "Signature BigWig valide.")
        } else {
          result$status <- "warning"
          result$messages <- c(result$messages,
            "Signature binaire inattendue — vérifiez que le fichier est un BigWig valide.")
        }
      }, error = function(e) {
        result$messages <<- c(result$messages, "Impossible de lire la signature binaire.")
      })
    } else {
      result$messages <- c(result$messages, "Format binaire : validation de contenu non applicable.")
    }
    return(result)
  }

  # 4. Lecture et preview (seulement les 50 premières lignes — jamais le fichier entier)
  fsize <- file.info(file_path)$size
  size_mb <- round(fsize / 1024^2, 1)
  result$messages <- c(result$messages, paste0("Taille : ", size_mb, " Mo"))

  tryCatch({
    raw_lines <- readLines(file_path, n = 50, warn = FALSE)
    data_lines <- raw_lines[!grepl("^#|^browser|^track|^\\s*$", raw_lines)]
    preview_lines <- head(data_lines, 5)
    result$preview <- preview_lines

    if (length(data_lines) == 0) {
      result$status <- "error"
      result$messages <- c(result$messages, "Fichier vide ou ne contient que des commentaires.")
      return(result)
    }

    # 5. Vérification du nombre minimal de colonnes
    min_cols <- spec$min_columns %||% 0
    if (min_cols > 0) {
      first_row <- strsplit(data_lines[1], "\t")[[1]]
      if (length(first_row) < min_cols) {
        result$status <- "error"
        result$messages <- c(result$messages, paste0(
          "Nombre de colonnes insuffisant : ", length(first_row),
          " colonne(s) détectée(s), minimum requis : ", min_cols,
          " pour le format ", spec$label, "."
        ))
      }
    }

  }, error = function(e) {
    result$status <<- "error"
    result$messages <<- c(result$messages, paste0("Erreur de lecture : ", conditionMessage(e)))
  })

  result
}

#' Construit un data.frame récapitulatif de tous les formats
#'
#' Utile pour l'affichage dans un DT::datatable
#'
#' @param specs Liste de specs
#' @return data.frame avec colonnes : Format, Label, Extensions, Binaire, Tracks, Description
format_specs_as_dataframe <- function(specs = NULL) {
  if (is.null(specs)) specs <- load_input_specs()

  rows <- lapply(names(specs), function(fmt_id) {
    s <- specs[[fmt_id]]
    data.frame(
      Format      = fmt_id,
      Label       = s$label %||% fmt_id,
      Extensions  = paste(s$extensions %||% "N/A", collapse = ", "),
      Binaire     = if (isTRUE(s$binary)) "Oui" else "Non",
      Tracks      = paste(s$associated_tracks %||% "—", collapse = ", "),
      Description = s$description %||% "",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
