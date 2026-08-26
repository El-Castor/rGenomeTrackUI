library(testthat)
library(withr)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "project_manager.R", "track_persistence.R")) {
  source(file.path("..", "..", "R", "core", f))
}

make_track <- function(id = "track_a", color = "#38bdf8", height = 2) {
  list(
    track_id = id,
    track_name = "D0",
    track_type = "bigwig",
    file_path = "",
    enabled = TRUE,
    order = 1L,
    params = list(color = color, height = height, scale_mode = "shared_by_group")
  )
}

test_that("project tracks are saved and reloaded from config/tracks.json", {
  tmp <- withr::local_tempdir()
  project <- create_project("PersistTracks", "Bd21", root_dir = tmp)
  tracks <- list(make_track(color = "#123456", height = 3))

  path <- save_project_tracks(project, tracks)
  expect_true(file.exists(path))
  expect_match(path, "config/tracks\\.json")

  loaded <- load_project_tracks(project)
  expect_length(loaded, 1)
  expect_equal(loaded[[1]]$track_id, "track_a")
  expect_equal(loaded[[1]]$params$color, "#123456")
  expect_equal(as.numeric(loaded[[1]]$params$height), 3)
})

test_that("update_track_by_id preserves id and updates parameters", {
  tracks <- standardize_tracks(list(make_track()))
  updated <- tracks[[1]]
  updated$params$color <- "#abcdef"
  updated$params$height <- 4
  out <- update_track_by_id(tracks, "track_a", updated)

  expect_length(out, 1)
  expect_equal(out[[1]]$track_id, "track_a")
  expect_equal(out[[1]]$params$color, "#abcdef")
  expect_equal(as.numeric(out[[1]]$params$height), 4)
})

test_that("track templates save globally and can be applied to new tracks", {
  tmp <- withr::local_tempdir()
  template_path <- file.path(tmp, "track_templates.json")

  tmpl <- save_track_template(
    make_track(color = "#f59e0b", height = 2.5),
    template_name = "BigWig orange signal",
    template_description = "Reusable signal style",
    keep_file_path = FALSE,
    path = template_path
  )

  expect_true(file.exists(template_path))
  templates <- load_track_templates(template_path)
  expect_length(templates, 1)
  expect_equal(templates[[1]]$template_name, "BigWig orange signal")
  expect_equal(templates[[1]]$track$file_path, "")

  applied <- apply_track_template(tmpl, file_path = "/data/new.bw", track_name = "D3")
  expect_true(grepl("^track_", applied$track_id))
  expect_equal(applied$track_name, "D3")
  expect_equal(applied$file_path, "/data/new.bw")
  expect_equal(applied$params$color, "#f59e0b")
})

test_that("delete_track_template removes template by id", {
  tmp <- withr::local_tempdir()
  template_path <- file.path(tmp, "track_templates.json")
  tmpl <- save_track_template(make_track(), "To delete", path = template_path)

  delete_track_template(tmpl$template_id, path = template_path)
  templates <- load_track_templates(template_path)
  expect_length(templates, 0)
})

test_that("track set templates save selected tracks and reapply with new ids", {
  tmp <- withr::local_tempdir()
  set_path <- file.path(tmp, "track_set_templates.json")
  tracks <- standardize_tracks(list(
    make_track("track_d0", "#38bdf8", 2),
    make_track("track_d3", "#8b5cf6", 2),
    list(track_id = "track_genes", track_name = "Genes", track_type = "gtf",
         file_path = "", enabled = TRUE, order = 3L,
         params = list(height = 1.5, style = "UCSC"))
  ))

  set <- save_track_set_template(
    tracks,
    selected_track_ids = c("track_d0", "track_d3", "track_genes"),
    name = "ATAC D0 D3 Genes",
    description = "time course",
    keep_file_paths = FALSE,
    path = set_path
  )

  expect_true(file.exists(set_path))
  sets <- load_track_set_templates(set_path)
  expect_length(sets, 1)
  expect_equal(sets[[1]]$n_tracks, 3)
  expect_equal(length(sets[[1]]$tracks), 3)

  applied <- apply_track_set_template(set, current_tracks = list())
  expect_length(applied, 3)
  expect_false(any(vapply(applied, function(t) t$track_id, character(1)) %in% c("track_d0", "track_d3", "track_genes")))
  expect_equal(vapply(applied, function(t) t$order, integer(1)), 1:3)
  expect_equal(applied[[1]]$params$color, "#38bdf8")
})

test_that("delete_track_set_template removes saved set", {
  tmp <- withr::local_tempdir()
  set_path <- file.path(tmp, "track_set_templates.json")
  set <- save_track_set_template(
    list(make_track("track_d0")),
    selected_track_ids = "track_d0",
    name = "Delete set",
    path = set_path
  )
  delete_track_set_template(set$track_set_id, path = set_path)
  expect_length(load_track_set_templates(set_path), 0)
})

test_that("selection helpers map DT rows to stable track ids", {
  tracks <- standardize_tracks(list(
    make_track("track_d0"),
    make_track("track_d3"),
    make_track("track_d5")
  ))
  fake_input <- list(tracks_table_rows_selected = c(1L, 3L))

  expect_true(has_multi_selection(fake_input$tracks_table_rows_selected))
  expect_equal(get_selected_track_ids(fake_input, tracks), c("track_d0", "track_d5"))
  expect_true(is_empty(get_selected_track_ids(list(tracks_table_rows_selected = integer(0)), tracks)))
})

test_that("automatic colours use distinct assay families and time gradients", {
  named_track <- function(id, name, type = "bigwig") {
    track <- make_track(id)
    track$track_name <- name
    track$track_type <- type
    track
  }
  tracks <- list(
    named_track("atac_d0", "ATAC D0"),
    named_track("atac_d4", "ATAC D4"),
    named_track("cpg_d0", "CpG D0"),
    named_track("cpg_d4", "CpG D4"),
    named_track("axis", "Coordinates", "x_axis")
  )
  coloured <- assign_automatic_track_colours(tracks)
  colours <- vapply(coloured[1:4], function(track) track$params$color, character(1))

  expect_equal(track_timepoint(tracks[[1]]), 0)
  expect_equal(track_timepoint(tracks[[2]]), 4)
  expect_identical(track_colour_family(tracks[[1]]), "ATAC")
  expect_identical(track_colour_family(tracks[[3]]), "CPG")
  expect_false(identical(colours[[1]], colours[[2]]))
  expect_false(identical(colours[[1]], colours[[3]]))
  expect_false(identical(colours[[3]], colours[[4]]))
  expect_identical(coloured[[5]]$params$color, "#38bdf8")
})

test_that("display filters combine modality and time while preserving context", {
  tracks <- list(
    modifyList(make_track("atac_d0"), list(track_name = "ATAC D0")),
    modifyList(make_track("atac_d3"), list(track_name = "ATAC D3")),
    modifyList(make_track("cpg_d0"), list(track_name = "CpG D0")),
    list(track_id = "genes", track_name = "Genes", track_type = "gtf",
         enabled = TRUE, params = list())
  )
  filtered <- filter_tracks_for_display(tracks, modalities = "ATAC", time_keys = "D3", keep_context = TRUE)
  expect_equal(vapply(filtered, function(track) isTRUE(track$enabled), logical(1)),
               c(FALSE, TRUE, FALSE, TRUE))

  without_context <- filter_tracks_for_display(tracks, modalities = "CPG", time_keys = "D0", keep_context = FALSE)
  expect_equal(vapply(without_context, function(track) isTRUE(track$enabled), logical(1)),
               c(FALSE, FALSE, TRUE, FALSE))
})

test_that("generic project cache reports hits and misses", {
  tmp <- withr::local_tempdir()
  cache_file <- file.path(tmp, "cache.rds")
  calls <- 0L
  first <- get_or_build_cache("key_a", cache_file, function() {
    calls <<- calls + 1L
    list(value = 1L)
  })
  second <- get_or_build_cache("key_a", cache_file, function() {
    calls <<- calls + 1L
    list(value = 2L)
  })

  expect_true(file.exists(cache_file))
  expect_false(isTRUE(attr(first, "cache_hit")))
  expect_true(isTRUE(attr(second, "cache_hit")))
  expect_equal(second$value, 1L)
  expect_equal(calls, 1L)
})

test_that("run state hash is stable and changes with region", {
  fs <- list(renderer = "pyGenomeTracks", signal_scale_mode = "shared_global_max_padded")
  tracks <- list(make_track("track_d0"))
  h1 <- hash_run_state("Bd1:1-100", "Bd1:1-100", tracks, fs, "pyGenomeTracks")
  h2 <- hash_run_state("Bd1:1-100", "Bd1:1-100", tracks, fs, "pyGenomeTracks")
  h3 <- hash_run_state("Bd1:200-300", "Bd1:200-300", tracks, fs, "pyGenomeTracks")

  expect_equal(h1, h2)
  expect_false(identical(h1, h3))
})

test_that("light file validation checks existence and readability only", {
  tmp <- withr::local_tempdir()
  bg <- file.path(tmp, "a.bg")
  writeLines("chr1\t0\t10\t1", bg)
  ok_track <- make_track("track_ok")
  ok_track$file_path <- bg
  missing_track <- make_track("track_missing")
  missing_track$file_path <- file.path(tmp, "missing.bg")

  expect_true(validate_files_light(list(ok_track)))
  expect_error(validate_files_light(list(missing_track)), "file not found")
})
