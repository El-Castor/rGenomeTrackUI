library(testthat)
library(withr)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "validators.R", "file_registry.R", "project_manager.R")) {
  source(file.path("..", "..", "R", "core", f))
}

# ---- validate_file_exists ---------------------------------------------------
test_that("validate_file_exists passes for real files", {
  tmp <- tempfile(); writeLines("x", tmp); on.exit(unlink(tmp))
  expect_true(validate_file_exists(tmp))
})

test_that("validate_file_exists fails for missing file", {
  expect_error(validate_file_exists("/nonexistent/path/file.bed"))
})

# ---- validate_bed_light -----------------------------------------------------
test_that("validate_bed_light accepts minimal BED3 content", {
  tmp <- tempfile(fileext = ".bed")
  writeLines(c("chr1\t1000\t5000", "chr2\t0\t100"), tmp)
  on.exit(unlink(tmp))
  expect_true(validate_bed_light(tmp))
})

test_that("validate_bed_light rejects non-numeric coords", {
  tmp <- tempfile(fileext = ".bed")
  writeLines(c("chr1\tstart\tend"), tmp)
  on.exit(unlink(tmp))
  expect_error(validate_bed_light(tmp))
})

# ---- detect_file_type -------------------------------------------------------
test_that("detect_file_type maps extensions correctly", {
  expect_equal(detect_file_type("sample.bedgraph"), "bedgraph")
  expect_equal(detect_file_type("sample.gtf"),      "gtf")
  expect_equal(detect_file_type("sample.narrowPeak"), "narrowPeak")
  expect_equal(detect_file_type("sample.bw"),       "bigwig")
  expect_equal(detect_file_type("sample.xyz"),      "unknown")
})

# ---- file registry ----------------------------------------------------------
test_that("add_file_to_registry creates a registry entry", {
  tmp_file <- tempfile(fileext = ".bedgraph")
  writeLines(c("chr1\t1000\t2000\t5.0"), tmp_file)
  on.exit(unlink(tmp_file))

  # create_project needs withr for temp dir
  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("RegTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "copy", track_type = "bedgraph")
  reg  <- load_file_registry(proj)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$file_type_detected[1], "bedgraph")
})

# ---- add_file_to_registry : copy mode stores in inputs/raw -----------------
test_that("add_file_to_registry copy mode places file in inputs/raw", {
  tmp_file <- tempfile(fileext = ".bed")
  writeLines(c("chr1\t100\t200", "chr2\t500\t600"), tmp_file)
  on.exit(unlink(tmp_file))

  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("CopyTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "copy", track_type = "bed",
                       original_name = "my_test.bed")
  reg <- load_file_registry(proj)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$linked_or_copied[1], "copied")
  expect_true(file.exists(reg$stored_path[1]))
  expect_true(grepl("inputs/raw", reg$stored_path[1], fixed = TRUE))
  expect_equal(reg$original_name[1], "my_test.bed")
})

# ---- add_file_to_registry : symlink mode stores in inputs/linked ------------
test_that("add_file_to_registry symlink mode creates link in inputs/linked", {
  tmp_file <- tempfile(fileext = ".bedgraph")
  writeLines(c("chr1\t100\t200\t3.5"), tmp_file)
  on.exit(unlink(tmp_file))

  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("LinkTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "link", track_type = "bedgraph",
                       original_name = "signal.bedgraph")
  reg <- load_file_registry(proj)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$linked_or_copied[1], "linked")
  expect_true(file.exists(reg$stored_path[1]))
  expect_true(grepl("inputs/linked", reg$stored_path[1], fixed = TRUE))
})

# ---- add_file_to_registry : auto-detect BigWig by extension -----------------
test_that("add_file_to_registry auto-detects bigwig from .bw extension", {
  # Cr\u00e9er un faux fichier .bw non-vide (pas de validation magic sur detect_file_type)
  tmp_file <- tempfile(fileext = ".bw")
  writeBin(as.raw(c(0x26, 0xfc, 0x8f, 0x88, 0x00)), tmp_file)  # magic LE BigWig + padding
  on.exit(unlink(tmp_file))

  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("BwTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "copy", original_name = "sample.bw")
  reg <- load_file_registry(proj)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$file_type_detected[1], "bigwig")
  expect_true(file.exists(reg$stored_path[1]))
})

# ---- add_file_to_registry : projet absent \u2014 erreur claire -------------------
test_that("add_file_to_registry errors if project path is invalid", {
  tmp_file <- tempfile(fileext = ".bed")
  writeLines("chr1\t100\t200", tmp_file)
  on.exit(unlink(tmp_file))

  fake_proj <- list(project_path = "/nonexistent_project_path_xyz")
  # Doit jeter une erreur (ensure_dir ne peut pas cr\u00e9er dans un chemin syst\u00e8me)
  expect_error(
    add_file_to_registry(fake_proj, tmp_file, mode = "copy"),
    regexp = NULL   # accepte n'importe quelle erreur
  )
})

# ---- add_file_to_registry : fichier vide \u2014 erreur claire ---------------------
test_that("add_file_to_registry errors on empty file", {
  tmp_file <- tempfile(fileext = ".bed")
  file.create(tmp_file)  # vide
  on.exit(unlink(tmp_file))

  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("EmptyTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  expect_error(
    add_file_to_registry(proj, tmp_file, mode = "copy"),
    regexp = "empty"
  )
})

# ---- add_file_to_registry : note transmise au registre ----------------------
test_that("add_file_to_registry stores notes correctly", {
  tmp_file <- tempfile(fileext = ".bed")
  writeLines("chr1\t100\t200", tmp_file)
  on.exit(unlink(tmp_file))

  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("NoteTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "copy",
                       notes = "ma note de test", original_name = "noted.bed")
  reg <- load_file_registry(proj)
  expect_equal(reg$notes[1], "ma note de test")
})

# ---- detect_file_type : cas BigWig ------------------------------------------
test_that("detect_file_type handles .bigwig extension", {
  expect_equal(detect_file_type("track.bigwig"), "bigwig")
  expect_equal(detect_file_type("track.bigWig"), "bigwig")
})

# ---- add_file_to_registry : tsv et json \u00e9crits --------------------------------
test_that("add_file_to_registry writes both tsv and json registry files", {
  tmp_file <- tempfile(fileext = ".bed")
  writeLines("chr1\t100\t200", tmp_file)
  on.exit(unlink(tmp_file))

  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("RegistryFiles", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "copy")

  tsv_path  <- file.path(proj$project_path, "inputs", "file_registry.tsv")
  json_path <- file.path(proj$project_path, "inputs", "file_registry.json")
  expect_true(file.exists(tsv_path))
  expect_true(file.exists(json_path))
})

# ---- detect_file_type : extensions utilisées par Parcourir auto-détection ---

test_that("detect_file_type maps .bedpe to links", {
  expect_equal(detect_file_type("interactions.bedpe"), "links")
})

test_that("detect_file_type maps .links to links", {
  expect_equal(detect_file_type("arcs.links"), "links")
})

test_that("detect_file_type maps .cool to hic_matrix", {
  expect_equal(detect_file_type("matrix.cool"), "hic_matrix")
})

test_that("detect_file_type maps .gff3 to gtf", {
  expect_equal(detect_file_type("genes.gff3"), "gtf")
})

test_that("detect_file_type maps .bg to bedgraph", {
  expect_equal(detect_file_type("signal.bg"), "bedgraph")
})

# ---- add_file_to_registry : auto-detect BigWig par extension .bigwig ---------
test_that("add_file_to_registry auto-detects bigwig from .bigwig extension", {
  tmp_file <- tempfile(fileext = ".bigwig")
  writeBin(as.raw(c(0x26, 0xfc, 0x8f, 0x88, 0x00)), tmp_file)  # magic LE BigWig + padding
  on.exit(unlink(tmp_file))

  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("BwExtTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "copy", original_name = "sample.bigwig")
  reg <- load_file_registry(proj)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$file_type_detected[1], "bigwig")
  expect_true(file.exists(reg$stored_path[1]))
})
