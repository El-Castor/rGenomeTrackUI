library(testthat)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "track_schema.R", "ini_generator.R")) {
  source(file.path("..", "..", "R", "core", f))
}

SCHEMA <- load_track_schema(testthat::test_path("..", "..", "config", "track_schema.yaml"))

make_tracks <- function() {
  list(
    list(track_id = "t1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_path = "/data/signal.bedgraph",
         params = list(color = "blue", height = 2)),
    list(track_id = "t2", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = "/data/genes.gtf",
         params = list(height = 4)),
    list(track_id = "t3", track_type = "x_axis", track_name = "X axis",
         enabled = TRUE, file_path = NULL,
         params = list())
  )
}

test_that("generate_tracks_ini returns a non-empty character string", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_type(ini, "character")
  expect_true(nchar(ini) > 0)
})

test_that("INI output contains expected section headers", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_match(ini, "\\[Signal\\]")
  expect_match(ini, "\\[Genes\\]")
  expect_match(ini, "\\[X axis\\]")
})

test_that("INI output contains file = for file-backed tracks", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_match(ini, "file = /data/signal.bedgraph")
  expect_match(ini, "file = /data/genes.gtf")
})

test_that("INI output contains file_type for bedgraph", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_match(ini, "file_type = bedgraph")
})

test_that("disabled tracks are excluded from INI", {
  tracks <- make_tracks()
  tracks[[2]]$enabled <- FALSE
  ini <- generate_tracks_ini(tracks, SCHEMA)
  expect_false(grepl("\\[Genes\\]", ini))
})

test_that("write_tracks_ini writes to disk", {
  tmp <- tempfile(fileext = ".ini")
  on.exit(unlink(tmp))
  write_tracks_ini(make_tracks(), SCHEMA, tmp)
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("\\[Signal\\]", content)))
})

test_that("GFF3 gene tracks are converted to GTF for pyGenomeTracks", {
  tmpdir <- tempfile("ini-gff3-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  gff3 <- file.path(tmpdir, "genes.gff3")
  writeLines(c(
    "##gff-version 3",
    "Bd1\tsrc\tgene\t100\t500\t.\t+\t.\tID=Bradi1g00010;Name=GeneA",
    "Bd1\tsrc\tmRNA\t100\t500\t.\t+\t.\tID=Bradi1g00010.1;Parent=Bradi1g00010",
    "Bd1\tsrc\texon\t100\t200\t.\t+\t.\tID=exon1;Parent=Bradi1g00010.1",
    "Bd1\tsrc\tCDS\t150\t450\t.\t+\t0\tID=cds1;Parent=Bradi1g00010.1"
  ), gff3)

  tracks <- list(
    list(track_id = "g1", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = gff3, params = list(title = "Genes"))
  )

  ini <- generate_tracks_ini(tracks, SCHEMA, work_dir = tmpdir)
  converted <- file.path(tmpdir, "genes.converted.gtf")
  expect_true(file.exists(converted))
  expect_match(ini, paste0("file = ", converted), fixed = TRUE)
  expect_match(ini, "file_type = gtf")
  expect_match(ini, "title = Genes")

  gtf <- readLines(converted)
  expect_true(any(grepl('gene_id "Bradi1g00010"', gtf, fixed = TRUE)))
  expect_true(any(grepl('transcript_id "Bradi1g00010"', gtf, fixed = TRUE)))
  expect_true(any(grepl("\texon\t", gtf, fixed = TRUE)))
})

test_that("GTF gene tracks keep the original file path", {
  tmpdir <- tempfile("ini-gtf-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  gtf <- file.path(tmpdir, "genes.gtf")
  writeLines('Bd1\tsrc\tgene\t100\t500\t.\t+\t.\tgene_id "g1"; transcript_id "g1"; gene_name "GeneA";', gtf)

  tracks <- list(
    list(track_id = "g1", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = gtf, params = list())
  )

  ini <- generate_tracks_ini(tracks, SCHEMA, work_dir = tmpdir)
  expect_match(ini, paste0("file = ", gtf), fixed = TRUE)
  expect_match(ini, "file_type = gtf")
})

test_that("signal tracks receive distinct automatic colors unless color is explicit", {
  tracks <- list(
    list(track_id = "s1", track_type = "bigwig", track_name = "D0",
         enabled = TRUE, file_path = "/data/d0.bw",
         params = list(color = "#1f77b4")),
    list(track_id = "s2", track_type = "bigwig", track_name = "D3",
         enabled = TRUE, file_path = "/data/d3.bw",
         params = list(color = "#1f77b4")),
    list(track_id = "s3", track_type = "bedgraph", track_name = "Manual",
         enabled = TRUE, file_path = "/data/manual.bg",
         params = list(color = "#123456"))
  )

  ini <- generate_tracks_ini(tracks, SCHEMA)
  expect_match(ini, "(?s)\\[D0\\].*color = #38bdf8", perl = TRUE)
  expect_match(ini, "(?s)\\[D3\\].*color = #8b5cf6", perl = TRUE)
  expect_match(ini, "(?s)\\[Manual\\].*color = #123456", perl = TRUE)
})

test_that("invalid pyGenomeTracks parameters are not emitted", {
  tracks <- list(
    list(track_id = "s1", track_type = "bigwig", track_name = "D0",
         enabled = TRUE, file_path = "/data/d0.bw",
         params = list(orientation = "normal", data_range_style = "inset")),
    list(track_id = "g1", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = "/data/genes.gtf",
         params = list(orientation = "normal"))
  )

  ini <- generate_tracks_ini(tracks, SCHEMA)
  expect_false(grepl("orientation = normal", ini, fixed = TRUE))
  expect_false(grepl("data_range_style", ini, fixed = TRUE))
})
