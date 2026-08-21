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

test_that("BED gene_rows zero uses pyGenomeTracks automatic row allocation", {
  bed_track <- list(
    track_id = "bed1", track_type = "bed", track_name = "Peaks",
    enabled = TRUE, file_path = "/data/peaks.bed",
    params = list(gene_rows = 0)
  )

  ini <- generate_tracks_ini(list(bed_track), SCHEMA)
  expect_match(ini, "\\[Peaks\\]")
  expect_false(grepl("gene_rows\\s*=", ini))

  bed_track$params$gene_rows <- 3
  ini_limited <- generate_tracks_ini(list(bed_track), SCHEMA)
  expect_match(ini_limited, "gene_rows = 3")
})

test_that("write_tracks_ini writes to disk", {
  tmp <- tempfile(fileext = ".ini")
  on.exit(unlink(tmp))
  tmp_gene <- tempfile(fileext = ".gtf")
  writeLines('chr1\tsrc\tgene\t1\t10\t.\t+\t.\tgene_id "g1"; transcript_id "g1"; gene_name "g1";', tmp_gene)
  tracks <- make_tracks()
  tracks[[2]]$file_path <- tmp_gene
  write_tracks_ini(tracks, SCHEMA, tmp)
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
         enabled = TRUE, file_path = gff3,
         params = list(title = "Genes", annotation_display_mode = "Gene blocks"))
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

test_that("GFF3 transcript/exon display mode uses annotation model features", {
  tmpdir <- tempfile("ini-gff3-model-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  gff3 <- file.path(tmpdir, "genes.gff3")
  writeLines(c(
    "##gff-version 3",
    "Bd1\tsrc\tgene\t100\t500\t.\t+\t.\tID=gene1;Name=GeneA",
    "Bd1\tsrc\tmRNA\t100\t500\t.\t+\t.\tID=tx1;Parent=gene1",
    "Bd1\tsrc\texon\t100\t200\t.\t+\t.\tID=exon1;Parent=tx1",
    "Bd1\tsrc\texon\t300\t400\t.\t+\t.\tID=exon2;Parent=tx1",
    "Bd1\tsrc\tCDS\t320\t380\t.\t+\t0\tID=cds1;Parent=tx1"
  ), gff3)

  tracks <- list(
    list(track_id = "g1", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = gff3,
         params = list(title = "Genes", annotation_display_mode = "Transcript/exon model",
                       annotation_render_format = "GTF",
                       display = "collapsed", gene_rows = 10, arrow_interval = 2))
  )

  ini <- generate_tracks_ini(tracks, SCHEMA, work_dir = tmpdir)
  converted <- file.path(tmpdir, "genes.converted.gtf")
  expect_true(file.exists(converted))
  expect_match(ini, "display = collapsed")
  expect_match(ini, "gene_rows = 10")
  expect_match(ini, "arrow_interval = 2")
  gtf <- readLines(converted)
  expect_equal(sum(grepl("\texon\t", gtf, fixed = TRUE)), 2L)
  expect_true(any(grepl("\tCDS\t", gtf, fixed = TRUE)))
  expect_true(any(grepl('transcript_id "tx1"', gtf, fixed = TRUE)))
})

test_that("GFF3 transcript/exon display mode can render BED12 blocks", {
  tmpdir <- tempfile("ini-gff3-bed12-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  gff3 <- file.path(tmpdir, "genes.gff3")
  writeLines(c(
    "##gff-version 3",
    "Bd1\tsrc\tgene\t100\t500\t.\t-\t.\tID=gene1;Name=GeneA",
    "Bd1\tsrc\tmRNA\t100\t500\t.\t-\t.\tID=tx1;Parent=gene1;Name=TxA",
    "Bd1\tsrc\texon\t100\t200\t.\t-\t.\tID=exon1;Parent=tx1",
    "Bd1\tsrc\texon\t300\t400\t.\t-\t.\tID=exon2;Parent=tx1",
    "Bd1\tsrc\tCDS\t320\t380\t.\t-\t0\tID=cds1;Parent=tx1"
  ), gff3)

  tracks <- list(
    list(track_id = "g1", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = gff3,
         params = list(title = "Genes", annotation_display_mode = "Transcript/exon model",
                       annotation_render_format = "BED12",
                       display = "stacked", gene_rows = 10, arrow_interval = 2))
  )

  ini <- generate_tracks_ini(tracks, SCHEMA, work_dir = tmpdir)
  converted <- file.path(tmpdir, "genes.converted.bed12")
  qc <- file.path(tmpdir, "annotation_track_QC.tsv")
  expect_true(file.exists(converted))
  expect_true(file.exists(qc))
  expect_match(ini, paste0("file = ", converted), fixed = TRUE)
  expect_match(ini, "file_type = bed")
  expect_match(ini, "# annotation_exons = 2", fixed = TRUE)
  expect_match(ini, "# display_blocks_used = exon", fixed = TRUE)
  bed <- strsplit(readLines(converted), "\t", fixed = TRUE)[[1]]
  expect_equal(length(bed), 12L)
  expect_equal(bed[[6]], "-")
  expect_equal(bed[[10]], "2")
})

test_that("GFF3 without exon can use CDS as display blocks", {
  tmpdir <- tempfile("ini-gff3-cds-bed12-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  gff3 <- file.path(tmpdir, "genes.gff3")
  writeLines(c(
    "##gff-version 3",
    "Bd1\tsrc\tgene\t100\t500\t.\t+\t.\tID=gene1;Name=GeneA",
    "Bd1\tsrc\tmRNA\t100\t500\t.\t+\t.\tID=tx1;Parent=gene1",
    "Bd1\tsrc\tCDS\t120\t200\t.\t+\t0\tID=cds1;Parent=tx1",
    "Bd1\tsrc\tCDS\t300\t360\t.\t+\t0\tID=cds2;Parent=tx1"
  ), gff3)

  tracks <- list(
    list(track_id = "g1", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = gff3,
         params = list(title = "Genes", annotation_display_mode = "Transcript/exon model",
                       annotation_render_format = "BED12",
                       use_CDS_as_exon_blocks = TRUE))
  )

  ini <- generate_tracks_ini(tracks, SCHEMA, work_dir = tmpdir)
  converted <- file.path(tmpdir, "genes.converted.bed12")
  expect_true(file.exists(converted))
  expect_match(ini, "# annotation_CDS = 2", fixed = TRUE)
  expect_match(ini, "# display_blocks_used = CDS", fixed = TRUE)
  bed <- strsplit(readLines(converted), "\t", fixed = TRUE)[[1]]
  expect_equal(bed[[10]], "2")
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

test_that("shared_by_group applies common min and max to signal tracks", {
  tmpdir <- tempfile("ini-scale-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg1 <- file.path(tmpdir, "a.bg")
  bg2 <- file.path(tmpdir, "b.bg")
  writeLines(c("chr1\t0\t10\t1", "chr1\t10\t20\t4"), bg1)
  writeLines(c("chr1\t0\t10\t2", "chr1\t10\t20\t8"), bg2)
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "A", enabled = TRUE, file_path = bg1,
         params = list(track_group = "ATAC-seq", scale_mode = "shared_by_group", color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "B", enabled = TRUE, file_path = bg2,
         params = list(track_group = "ATAC-seq", scale_mode = "shared_by_group", color = "#654321"))
  )
  ini <- generate_tracks_ini(tracks, SCHEMA, regions = "chr1:1-20")
  expect_match(ini, "(?s)\\[A\\].*min_value = 1.*max_value = 8", perl = TRUE)
  expect_match(ini, "(?s)\\[B\\].*min_value = 1.*max_value = 8", perl = TRUE)
  expect_match(ini, "# track_group = ATAC-seq", fixed = TRUE)
  expect_match(ini, "# scale_mode = shared_by_group", fixed = TRUE)
})

test_that("independent scale does not force min and max", {
  tmpdir <- tempfile("ini-independent-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg1 <- file.path(tmpdir, "a.bg")
  bg2 <- file.path(tmpdir, "b.bg")
  writeLines("chr1\t0\t10\t1", bg1)
  writeLines("chr1\t0\t10\t8", bg2)
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "A", enabled = TRUE, file_path = bg1,
         params = list(track_group = "ATAC-seq", scale_mode = "independent", color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "B", enabled = TRUE, file_path = bg2,
         params = list(track_group = "ATAC-seq", scale_mode = "independent", color = "#654321"))
  )
  ini <- generate_tracks_ini(tracks, SCHEMA, regions = "chr1:1-20")
  expect_false(grepl("min_value =", ini, fixed = TRUE))
  expect_false(grepl("max_value =", ini, fixed = TRUE))
})

test_that("manual scale respects user min and max", {
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "A", enabled = TRUE, file_path = "/data/a.bg",
         params = list(track_group = "ATAC-seq", scale_mode = "manual",
                       min_value = 0, max_value = 25, color = "#123456"))
  )
  ini <- generate_tracks_ini(tracks, SCHEMA)
  expect_match(ini, "min_value = 0", fixed = TRUE)
  expect_match(ini, "max_value = 25", fixed = TRUE)
})

test_that("robust shared scale calculates a common robust max", {
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "A", enabled = TRUE, file_path = "/data/a.bg",
         params = list(track_group = "ATAC-seq", scale_mode = "robust_shared_by_group",
                       robust_percentile = 99, color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "B", enabled = TRUE, file_path = "/data/b.bg",
         params = list(track_group = "ATAC-seq", scale_mode = "robust_shared_by_group",
                       robust_percentile = 99, color = "#654321"))
  )
  stats <- list(
    a = list(min = 0, max = 100, robust = 10),
    b = list(min = 0, max = 200, robust = 20)
  )
  ini <- generate_tracks_ini(tracks, SCHEMA, stats_by_track = stats)
  expect_match(ini, "(?s)\\[A\\].*min_value = 0.*max_value = 20", perl = TRUE)
  expect_match(ini, "(?s)\\[B\\].*min_value = 0.*max_value = 20", perl = TRUE)
})

test_that("global auto_per_track does not force common max and warns", {
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "D0", enabled = TRUE, file_path = "/data/a.bg",
         params = list(color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "D3", enabled = TRUE, file_path = "/data/b.bg",
         params = list(color = "#654321"))
  )
  fs <- list(apply_shared_scale_to_signal_tracks = TRUE, signal_scale_mode = "auto_per_track")
  ini <- generate_tracks_ini(tracks, SCHEMA, figure_settings = fs)
  expect_false(grepl("max_value =", ini, fixed = TRUE))
  expect_match(ini, "autoscaling par track", fixed = TRUE)
})

test_that("global shared_global_max writes same max to signal tracks", {
  tmpdir <- tempfile("ini-global-max-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg1 <- file.path(tmpdir, "a.bg")
  bg2 <- file.path(tmpdir, "b.bg")
  writeLines(c("chr1\t0\t10\t1", "chr1\t10\t20\t4"), bg1)
  writeLines(c("chr1\t0\t10\t2", "chr1\t10\t20\t8"), bg2)
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "D0", enabled = TRUE, file_path = bg1,
         params = list(color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "D3", enabled = TRUE, file_path = bg2,
         params = list(color = "#654321"))
  )
  fs <- list(apply_shared_scale_to_signal_tracks = TRUE, signal_scale_mode = "shared_global_max",
             shared_min_value = 0)
  ini <- generate_tracks_ini(tracks, SCHEMA, regions = "chr1:1-20", figure_settings = fs)
  expect_match(ini, "(?s)\\[D0\\].*min_value = 0.*max_value = 9.2", perl = TRUE)
  expect_match(ini, "(?s)\\[D3\\].*min_value = 0.*max_value = 9.2", perl = TRUE)
  expect_match(ini, "Mode d'echelle: shared_global_max", fixed = TRUE)
  expect_match(ini, "Raw global max detected: 8", fixed = TRUE)
  expect_match(ini, "Potential clipping: no", fixed = TRUE)
})

test_that("global shared_global_max_padded writes padded max to signal tracks", {
  tmpdir <- tempfile("ini-global-max-padded-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg1 <- file.path(tmpdir, "a.bg")
  bg2 <- file.path(tmpdir, "b.bg")
  writeLines(c("chr1\t0\t10\t10", "chr1\t10\t20\t20"), bg1)
  writeLines(c("chr1\t0\t10\t30", "chr1\t10\t20\t54"), bg2)
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "D0", enabled = TRUE, file_path = bg1,
         params = list(color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "D3", enabled = TRUE, file_path = bg2,
         params = list(color = "#654321"))
  )
  fs <- list(apply_shared_scale_to_signal_tracks = TRUE, signal_scale_mode = "shared_global_max_padded",
             shared_min_value = 0, signal_max_padding_factor = 1.15)
  ini <- generate_tracks_ini(tracks, SCHEMA, regions = "chr1:1-20", figure_settings = fs)
  expect_match(ini, "(?s)\\[D0\\].*min_value = 0.*max_value = 62.1", perl = TRUE)
  expect_match(ini, "(?s)\\[D3\\].*min_value = 0.*max_value = 62.1", perl = TRUE)
  expect_match(ini, "Raw global max detected: 54", fixed = TRUE)
  expect_match(ini, "Final max_value used: 62.1", fixed = TRUE)
})

test_that("global shared_global_quantile writes same quantile max", {
  tmpdir <- tempfile("ini-global-quantile-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg1 <- file.path(tmpdir, "a.bg")
  bg2 <- file.path(tmpdir, "b.bg")
  writeLines(c("chr1\t0\t10\t1", "chr1\t10\t20\t3"), bg1)
  writeLines(c("chr1\t0\t10\t5", "chr1\t10\t20\t9"), bg2)
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "D0", enabled = TRUE, file_path = bg1,
         params = list(color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "D3", enabled = TRUE, file_path = bg2,
         params = list(color = "#654321"))
  )
  fs <- list(apply_shared_scale_to_signal_tracks = TRUE, signal_scale_mode = "shared_global_quantile",
             shared_min_value = 0, shared_quantile = 0.5)
  ini <- generate_tracks_ini(tracks, SCHEMA, regions = "chr1:1-20", figure_settings = fs)
  expect_match(ini, "(?s)\\[D0\\].*min_value = 0.*max_value = 4.6", perl = TRUE)
  expect_match(ini, "(?s)\\[D3\\].*min_value = 0.*max_value = 4.6", perl = TRUE)
  expect_match(ini, "Quantile utilise: 0.5", fixed = TRUE)
  expect_match(ini, "Potential clipping: yes", fixed = TRUE)
})

test_that("global manual scale writes manual values to all signal tracks", {
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "D0", enabled = TRUE, file_path = "/data/a.bg",
         params = list(color = "#123456")),
    list(track_id = "b", track_type = "bedgraph", track_name = "D3", enabled = TRUE, file_path = "/data/b.bg",
         params = list(color = "#654321"))
  )
  fs <- list(apply_shared_scale_to_signal_tracks = TRUE, signal_scale_mode = "manual",
             manual_min_value = 2, manual_max_value = 42)
  ini <- generate_tracks_ini(tracks, SCHEMA, figure_settings = fs)
  expect_match(ini, "(?s)\\[D0\\].*min_value = 2.*max_value = 42", perl = TRUE)
  expect_match(ini, "(?s)\\[D3\\].*min_value = 2.*max_value = 42", perl = TRUE)
})

test_that("signal scaling cache key is stable and region-sensitive", {
  tmpdir <- tempfile("ini-scale-key-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg <- file.path(tmpdir, "a.bg")
  writeLines("chr1\t0\t10\t1", bg)

  key1 <- make_signal_scale_cache_key(bg, "chr1:1-10", "shared_global_max_padded", 0.99, 1.15)
  key2 <- make_signal_scale_cache_key(bg, "chr1:1-10", "shared_global_max_padded", 0.99, 1.15)
  key3 <- make_signal_scale_cache_key(bg, "chr1:20-30", "shared_global_max_padded", 0.99, 1.15)

  expect_equal(key1, key2)
  expect_false(identical(key1, key3))
})

test_that("signal scaling cache is reused for identical region and settings", {
  tmpdir <- tempfile("ini-scale-cache-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg <- file.path(tmpdir, "a.bg")
  cache_file <- file.path(tmpdir, "signal_scaling_cache.rds")
  writeLines(c("chr1\t0\t10\t10", "chr1\t10\t20\t20"), bg)
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "D0", enabled = TRUE, file_path = bg,
         params = list(color = "#123456"))
  )
  fs <- list(
    apply_shared_scale_to_signal_tracks = TRUE,
    signal_scale_mode = "shared_global_max_padded",
    shared_min_value = 0,
    signal_max_padding_factor = 1.1,
    signal_scaling_cache_file = cache_file
  )

  first <- apply_global_signal_scale(tracks, regions = "chr1:1-20", figure_settings = fs)
  second <- apply_global_signal_scale(tracks, regions = "chr1:1-20", figure_settings = fs)

  expect_true(file.exists(cache_file))
  expect_false(isTRUE(first$scale_info$cache_hit))
  expect_true(isTRUE(second$scale_info$cache_hit))
  expect_equal(first$scale_info$shared_max_value, second$scale_info$shared_max_value)
})

test_that("light prepare skips GFF annotation conversion and QC", {
  tmpdir <- tempfile("ini-light-prepare-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  gff3 <- file.path(tmpdir, "genes.gff3")
  writeLines(c(
    "##gff-version 3",
    "chr1\tsrc\tgene\t1\t100\t.\t+\t.\tID=g1;Name=G1",
    "chr1\tsrc\tmRNA\t1\t100\t.\t+\t.\tID=t1;Parent=g1",
    "chr1\tsrc\tCDS\t1\t50\t.\t+\t0\tID=cds1;Parent=t1"
  ), gff3)
  out <- prepare_annotation_file_for_track(
    gff3, "gtf", work_dir = tmpdir,
    display_mode = "Transcript/exon model",
    render_format = "GTF",
    light_prepare = TRUE
  )

  expect_equal(out$file_path, gff3)
  expect_equal(out$file_type, "gtf")
  expect_null(out$qc)
  expect_false(file.exists(build_converted_gtf_path(gff3, tmpdir)))
  expect_false(file.exists(annotation_qc_path(tmpdir)))
})

test_that("light prepare defers per-track signal statistics", {
  tmpdir <- tempfile("ini-light-signal-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bg <- file.path(tmpdir, "signal.bedgraph")
  writeLines(c("chr1\t0\t10\t2", "chr1\t10\t20\t8"), bg)
  tracks <- list(
    list(track_id = "s1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_path = bg,
         params = list(scale_mode = "shared_by_group", track_group = "ATAC-seq"))
  )

  ini <- generate_tracks_ini(
    tracks, SCHEMA, regions = "chr1:1-20",
    figure_settings = list(light_prepare = TRUE,
                           apply_shared_scale_to_signal_tracks = FALSE)
  )

  expect_false(grepl("min_value =", ini, fixed = TRUE))
  expect_false(grepl("max_value =", ini, fixed = TRUE))
})

test_that("spacer_before_genes is inserted before gene track", {
  gtf <- tempfile(fileext = ".gtf")
  writeLines('chr1\tsrc\tgene\t1\t10\t.\t+\t.\tgene_id "g1"; transcript_id "g1"; gene_name "g1";', gtf)
  tracks <- list(
    list(track_id = "a", track_type = "bedgraph", track_name = "D0", enabled = TRUE, file_path = "/data/a.bg",
         params = list(color = "#123456")),
    list(track_id = "g", track_type = "gtf", track_name = "genes", enabled = TRUE, file_path = gtf,
         params = list())
  )
  fs <- list(insert_spacer_before_genes = TRUE, spacer_before_genes_height = 0.4,
             apply_shared_scale_to_signal_tracks = TRUE, signal_scale_mode = "manual",
             manual_min_value = 0, manual_max_value = 10,
             gene_track_height = 1.5, gene_label_fontsize = 7, gene_rows = 2,
             gene_style = "UCSC", gene_display = "stacked")
  ini <- generate_tracks_ini(tracks, SCHEMA, figure_settings = fs)
  expect_true(regexpr("\\[spacer_before_genes\\]", ini) < regexpr("\\[genes\\]", ini))
  expect_match(ini, "\\[spacer_before_genes\\][\\s\\S]*file_type = spacer[\\s\\S]*height = 0.4", perl = TRUE)
  expect_match(ini, "(?s)\\[genes\\].*height = 1.5.*style = UCSC.*display = stacked.*gene_rows = 2.*fontsize = 7", perl = TRUE)
})
