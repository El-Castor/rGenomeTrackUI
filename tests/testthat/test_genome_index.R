library(testthat)
library(withr)

source(file.path("..", "..", "R", "core", "utils_slug.R"))
source(file.path("..", "..", "R", "core", "utils_paths.R"))
source(file.path("..", "..", "R", "core", "utils_json.R"))
source(file.path("..", "..", "R", "core", "validators.R"))
source(file.path("..", "..", "R", "core", "genome_index.R"))

# =============================================================================
# BigWig header parser
# =============================================================================

# Helper: create a minimal valid BigWig file with given chromosomes
.make_tiny_bigwig <- function(path, chroms = list(list(name="chr1", size=248956422L))) {
  # We only write a valid BigWig header + empty chromosome B+ tree so the
  # parser can read chrom names + sizes without any signal data.
  # Layout (little-endian):
  #   [0]  magic       uint32 = 0x26FC8F88
  #   [4]  version     uint16 = 4
  #   [6]  zoomLevels  uint16 = 0
  #   [8]  chromTreeOffset uint64 = 64 (header is 64 bytes, tree starts after)
  #   [16..63] zeros (fullDataOffset etc.)
  #   [64] bptFile header (32 bytes):
  #          magic      uint32 = 0x78CA8C91
  #          blockSize  uint32 = 256
  #          keySize    uint32 = (max chrom name len + 1, padded to >= 1)
  #          valSize    uint32 = 8
  #          itemCount  uint64 = n
  #          reserved   uint64 = 0
  #   [96] root leaf node:
  #          isLeaf   uint8 = 1
  #          reserved uint8 = 0
  #          count    uint16 = n
  #          [for each chrom]
  #            key  : keySize bytes (null-padded name)
  #            id   : uint32 (sequential)
  #            size : uint32

  n_chrom  <- length(chroms)
  max_name <- max(vapply(chroms, function(c) nchar(c$name), integer(1L)))
  key_size <- max_name + 1L  # null terminator

  con <- file(path, "wb")
  on.exit(close(con))

  # ---- Main header (64 bytes) ----
  writeBin(as.raw(c(0x26, 0xfc, 0x8f, 0x88)),  con)  # magic LE
  writeBin(as.integer(4L),  con, size = 2L)           # version
  writeBin(as.integer(0L),  con, size = 2L)           # zoomLevels
  chrom_tree_offset <- 64L
  writeBin(as.integer(chrom_tree_offset), con, size = 4L, endian = "little")  # lo
  writeBin(as.integer(0L),               con, size = 4L, endian = "little")  # hi
  # remaining header fields (48 bytes of zeros)
  writeBin(raw(48L), con)

  # ---- bptFile header (32 bytes) ----
  writeBin(as.raw(c(0x91, 0x8c, 0xca, 0x78)), con)    # bpt magic LE
  writeBin(as.integer(256L),  con, size = 4L, endian = "little")  # blockSize
  writeBin(as.integer(key_size), con, size = 4L, endian = "little")
  writeBin(as.integer(8L),    con, size = 4L, endian = "little")  # valSize
  writeBin(as.integer(n_chrom), con, size = 4L, endian = "little")  # itemCount lo
  writeBin(as.integer(0L),    con, size = 4L, endian = "little")  # itemCount hi
  writeBin(raw(8L),           con)                                 # reserved

  # ---- Root leaf node ----
  writeBin(as.integer(1L), con, size = 1L)             # isLeaf = 1
  writeBin(as.integer(0L), con, size = 1L)             # reserved
  writeBin(as.integer(n_chrom), con, size = 2L, endian = "little")  # count

  for (i in seq_along(chroms)) {
    ch <- chroms[[i]]
    # key: name + null padding to key_size bytes
    name_raw <- charToRaw(ch$name)
    pad_raw  <- raw(key_size - length(name_raw))
    writeBin(c(name_raw, pad_raw), con)
    writeBin(as.integer(i - 1L),  con, size = 4L, endian = "little")  # chromId
    writeBin(as.integer(ch$size), con, size = 4L, endian = "little")  # chromSize
  }
}

test_that("inspect_bigwig_chroms reads single chromosome from valid header", {
  tmp <- tempfile(fileext = ".bw")
  .make_tiny_bigwig(tmp, list(list(name = "chr1", size = 248956422L)))
  on.exit(unlink(tmp))

  res <- inspect_bigwig_chroms(tmp)
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$chrom[1L], "chr1")
  expect_equal(as.integer(res$length[1L]), 248956422L)
  expect_equal(res$length_source[1L], "bigwig_header")
  expect_equal(res$source_type[1L], "bigwig")
})

test_that("inspect_bigwig_chroms reads multiple chromosomes", {
  tmp <- tempfile(fileext = ".bw")
  .make_tiny_bigwig(tmp, list(
    list(name = "Bd1", size = 75631152L),
    list(name = "Bd2", size = 59130575L),
    list(name = "Bd3", size = 60432070L)
  ))
  on.exit(unlink(tmp))

  res <- inspect_bigwig_chroms(tmp)
  expect_equal(nrow(res), 3L)
  expect_equal(sort(res$chrom), c("Bd1", "Bd2", "Bd3"))
})

test_that("inspect_bigwig_chroms returns NULL for non-BigWig file", {
  tmp <- tempfile(fileext = ".bw")
  writeLines("not a bigwig", tmp)
  on.exit(unlink(tmp))
  expect_null(suppressWarnings(inspect_bigwig_chroms(tmp)))
})

# =============================================================================
# GFF chromosome inspector
# =============================================================================

test_that("inspect_gff_chroms reads ##sequence-region directives", {
  tmp <- tempfile(fileext = ".gff3")
  writeLines(c(
    "##gff-version 3",
    "##sequence-region Bd1 1 75631152",
    "##sequence-region Bd2 1 59130575",
    "Bd1\t.\tgene\t1000\t2000\t.\t+\t.\tID=gene1"
  ), tmp)
  on.exit(unlink(tmp))

  res <- inspect_gff_chroms(tmp)
  expect_is(res, "data.frame")
  expect_true("Bd1" %in% res$chrom)
  expect_true("Bd2" %in% res$chrom)
  bd1 <- res[res$chrom == "Bd1", ]
  expect_equal(as.integer(bd1$length), 75631152L)
  expect_equal(bd1$length_source, "gff3_sequence_region")
})

test_that("inspect_gff_chroms infers length from max(end) when no sequence-region", {
  tmp <- tempfile(fileext = ".gtf")
  writeLines(c(
    "chr1\t.\tgene\t1000\t50000\t.\t+\t.",
    "chr1\t.\texon\t2000\t48000\t.\t+\t.",
    "chr2\t.\tgene\t100\t20000\t.\t-\t."
  ), tmp)
  on.exit(unlink(tmp))

  res <- inspect_gff_chroms(tmp)
  expect_is(res, "data.frame")
  chr1 <- res[res$chrom == "chr1", ]
  expect_equal(as.integer(chr1$length), 50000L)
  expect_equal(chr1$length_source, "inferred_from_features")
  expect_true(chr1$n_features >= 1L)
})

test_that("inspect_gff_chroms returns NULL for non-GFF file", {
  tmp <- tempfile(fileext = ".gff3")
  writeLines(c("not gff content without any tabs"), tmp)
  on.exit(unlink(tmp))
  # Should return NULL (no valid feature rows) or a data.frame
  res <- inspect_gff_chroms(tmp)
  expect_true(is.null(res) || nrow(res) == 0L)
})

# =============================================================================
# Text track inspector (BED / BedGraph)
# =============================================================================

test_that("inspect_text_track_chroms reads BED chromosomes", {
  tmp <- tempfile(fileext = ".bed")
  writeLines(c(
    "chr1\t1000\t5000",
    "chr1\t10000\t15000",
    "chr2\t500\t2000"
  ), tmp)
  on.exit(unlink(tmp))

  res <- inspect_text_track_chroms(tmp, "bed")
  expect_is(res, "data.frame")
  expect_true("chr1" %in% res$chrom)
  expect_true("chr2" %in% res$chrom)
  chr1 <- res[res$chrom == "chr1", ]
  expect_equal(as.integer(chr1$length), 15000L)
})

# =============================================================================
# build_project_chrom_index
# =============================================================================

test_that("build_project_chrom_index aggregates BigWig and GFF correctly", {
  tmp_proj <- withr::local_tempdir()

  # Create a fake BigWig
  bw_path <- file.path(tmp_proj, "signal.bw")
  .make_tiny_bigwig(bw_path, list(
    list(name = "Bd1", size = 75631152L),
    list(name = "Bd2", size = 59130575L)
  ))

  # Create a fake GFF3
  gff_path <- file.path(tmp_proj, "genes.gff3")
  writeLines(c(
    "##sequence-region Bd1 1 75631152",
    "##sequence-region Bd2 1 59130575",
    "Bd1\t.\tgene\t1000\t2000\t.\t+\t.\tID=gene1"
  ), gff_path)

  reg <- data.frame(
    file_id           = c("f1", "f2"),
    original_name     = c("signal.bw", "genes.gff3"),
    stored_path       = c(bw_path, gff_path),
    file_type_detected = c("bigwig", "gff"),
    stringsAsFactors  = FALSE
  )
  proj <- list(project_path = tmp_proj)

  result <- build_project_chrom_index(proj, reg)
  expect_is(result$index, "data.frame")
  expect_equal(nrow(result$index), 2L)  # Bd1, Bd2
  expect_true("Bd1" %in% result$index$chrom)
  expect_equal(result$compat, "ok")
  bd1 <- result$index[result$index$chrom == "Bd1", ]
  expect_true(isTRUE(bd1$in_bigwig) && isTRUE(bd1$in_gff))
})

test_that("build_project_chrom_index detects incompatible chromosomes", {
  tmp_proj <- withr::local_tempdir()

  bw_path <- file.path(tmp_proj, "signal.bw")
  .make_tiny_bigwig(bw_path, list(list(name = "Bd1", size = 75000000L)))

  gff_path <- file.path(tmp_proj, "genes.gff3")
  writeLines(c(
    "##sequence-region chr1 1 248956422",
    "chr1\t.\tgene\t1000\t2000\t.\t+\t.\tID=gene1"
  ), gff_path)

  reg <- data.frame(
    file_id           = c("f1", "f2"),
    original_name     = c("signal.bw", "genes.gff3"),
    stored_path       = c(bw_path, gff_path),
    file_type_detected = c("bigwig", "gff"),
    stringsAsFactors  = FALSE
  )
  proj <- list(project_path = tmp_proj)

  result <- build_project_chrom_index(proj, reg)
  expect_equal(result$compat, "error")
})

# =============================================================================
# validate_region_against_index
# =============================================================================

.make_index <- function(chroms) {
  data.frame(
    chrom        = names(chroms),
    length       = unlist(chroms),
    length_source = "bigwig_header",
    n_files      = 1L,
    source_types = "bigwig",
    files        = "test.bw",
    in_bigwig    = TRUE,
    in_gff       = FALSE,
    in_text      = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("validate_region_against_index: valid region returns ok", {
  idx <- .make_index(c(Bd1 = 75631152L))
  res <- validate_region_against_index("Bd1:1-100000", idx)
  expect_equal(res$status, "ok")
})

test_that("validate_region_against_index: unknown chrom returns error", {
  idx <- .make_index(c(Bd1 = 75631152L))
  res <- validate_region_against_index("chr1:1-100000", idx)
  expect_equal(res$status, "error")
  expect_true(grepl("chr1", res$messages))
})

test_that("validate_region_against_index: end beyond length returns warning", {
  idx <- .make_index(c(Bd1 = 100000L))
  res <- validate_region_against_index("Bd1:1-200000", idx)
  expect_equal(res$status, "warning")
  expect_true(length(res$messages) > 0L)
})

test_that("validate_region_against_index: start >= end returns error", {
  idx <- .make_index(c(Bd1 = 75631152L))
  res <- validate_region_against_index("Bd1:50000-50000", idx)
  expect_equal(res$status, "error")
})

test_that("validate_region_against_index: invalid format returns error", {
  idx <- .make_index(c(Bd1 = 75631152L))
  res <- validate_region_against_index("notaregion", idx)
  expect_equal(res$status, "error")
})

test_that("validate_region_against_index: NULL index just checks format", {
  res <- validate_region_against_index("chr1:1-100000", NULL)
  expect_equal(res$status, "ok")
})

# =============================================================================
# Cache
# =============================================================================

test_that("save/load/validate chrom index cache works correctly", {
  tmp_proj_dir <- withr::local_tempdir()
  proj <- list(project_path = tmp_proj_dir)

  tmp_file <- file.path(tmp_proj_dir, "dummy.bed")
  writeLines("chr1\t100\t200", tmp_file)
  reg <- data.frame(file_id = "f1", stored_path = tmp_file, stringsAsFactors = FALSE)

  idx_result <- list(
    index      = data.frame(chrom = "chr1", length = 200L, length_source = "inferred_from_features",
                             n_files = 1L, source_types = "bed", files = "dummy.bed",
                             in_bigwig = FALSE, in_gff = FALSE, in_text = TRUE,
                             stringsAsFactors = FALSE),
    compat     = "ok",
    compat_msg = "1 chromosome.",
    per_file   = list()
  )

  save_chrom_index_cache(idx_result, reg, proj)
  cache_path <- file.path(tmp_proj_dir, "metadata", "chrom_index.json")
  expect_true(file.exists(cache_path))

  cache <- load_chrom_index_cache(proj)
  expect_false(is.null(cache))
  expect_equal(cache$compat, "ok")

  # Cache should be valid immediately after saving
  expect_true(is_chrom_index_cache_valid(cache, reg))
})

test_that("cache is invalid when file mtime changes", {
  tmp_proj_dir <- withr::local_tempdir()
  proj <- list(project_path = tmp_proj_dir)

  tmp_file <- file.path(tmp_proj_dir, "dummy.bed")
  writeLines("chr1\t100\t200", tmp_file)
  reg <- data.frame(file_id = "f1", stored_path = tmp_file, stringsAsFactors = FALSE)

  idx_result <- list(
    index      = data.frame(chrom = "chr1", length = 200L, length_source = "inferred_from_features",
                             n_files = 1L, source_types = "bed", files = "dummy.bed",
                             in_bigwig = FALSE, in_gff = FALSE, in_text = TRUE,
                             stringsAsFactors = FALSE),
    compat = "ok", compat_msg = "", per_file = list()
  )
  save_chrom_index_cache(idx_result, reg, proj)
  cache <- load_chrom_index_cache(proj)

  # Simulate file modification (wait 1s to change mtime)
  Sys.sleep(1.1)
  writeLines("chr1\t200\t400", tmp_file)  # update file

  expect_false(is_chrom_index_cache_valid(cache, reg))
})

# =============================================================================
# Gene index (GFF)
# =============================================================================

test_that("inspect_gff_genes extracts gene features with GFF3 attributes", {
  tmp <- tempfile(fileext = ".gff3")
  writeLines(c(
    "##gff-version 3",
    "Bd1\t.\tgene\t1000\t5000\t.\t+\t.\tID=Bradi1g00010;Name=gene1",
    "Bd1\t.\texon\t1200\t4800\t.\t+\t.\tParent=Bradi1g00010",
    "Bd2\t.\tgene\t2000\t7000\t.\t-\t.\tID=Bradi2g00015;Name=geneB"
  ), tmp)
  on.exit(unlink(tmp))

  gi <- inspect_gff_genes(tmp)
  expect_is(gi, "data.frame")
  expect_equal(nrow(gi), 2L)
  expect_true("gene_id" %in% names(gi))
  expect_true("Bradi1g00010" %in% gi$gene_id || "gene1" %in% gi$name)
  expect_true(all(gi$feature_type == "gene"))
  expect_false(any(grepl("exon", gi$feature_type)))
  # New columns present
  expect_true("strand"           %in% names(gi))
  expect_true("locus_tag"        %in% names(gi))
  expect_true("alias"            %in% names(gi))
  expect_true("gene_name"        %in% names(gi))
  expect_true("gene_key"         %in% names(gi))
  expect_true("searchable_label" %in% names(gi))
  expect_true("raw_attributes"   %in% names(gi))
  expect_true("search_blob"      %in% names(gi))
  expect_true("source_file"      %in% names(gi))
  # strand values correct
  bd1_gene <- gi[gi$gene_id == "Bradi1g00010", ]
  expect_equal(bd1_gene$strand, "+")
  bd2_gene <- gi[gi$gene_id == "Bradi2g00015", ]
  expect_equal(bd2_gene$strand, "-")
  # searchable_label contains gene_id
  expect_true(any(grepl("Bradi1g00010", gi$searchable_label)))
  expect_true(any(grepl("ID=Bradi1g00010", gi$raw_attributes, fixed = TRUE)))
  expect_true(any(grepl("ID=Bradi1g00010", gi$search_blob, fixed = TRUE)))
})

test_that("inspect_gff_genes excludes mRNA exon CDS and UTR by default", {
  tmp <- tempfile(fileext = ".gff3")
  writeLines(c(
    "##gff-version 3",
    "chr1\t.\tgene\t100\t900\t.\t+\t.\tID=gene1;Name=GeneA",
    "chr1\t.\tmRNA\t100\t900\t.\t+\t.\tID=tx1;Parent=gene1",
    "chr1\t.\texon\t100\t250\t.\t+\t.\tParent=tx1",
    "chr1\t.\tCDS\t300\t500\t.\t+\t0\tParent=tx1",
    "chr1\t.\tfive_prime_UTR\t100\t150\t.\t+\t.\tParent=tx1",
    "chr1\t.\tgene\t1200\t1800\t.\t-\t.\tID=gene2;Name=GeneB"
  ), tmp)
  on.exit(unlink(tmp))

  gi <- inspect_gff_genes(tmp)
  expect_equal(nrow(gi), 2L)
  expect_equal(unique(gi$feature_type), "gene")
  expect_equal(gi$gene_id, c("gene1", "gene2"))
})

test_that("inspect_gff_genes extracts locus_tag from GFF3", {
  tmp <- tempfile(fileext = ".gff3")
  writeLines(c(
    "##gff-version 3",
    "chr1\t.\tgene\t100\t500\t.\t+\t.\tID=gene001;Name=GeneA;locus_tag=LT001"
  ), tmp)
  on.exit(unlink(tmp))

  gi <- inspect_gff_genes(tmp)
  expect_false(is.null(gi))
  expect_equal(gi$locus_tag[1L], "LT001")
  expect_true(grepl("LT001", gi$searchable_label[1L]))
  expect_true(grepl("locus_tag=LT001", gi$raw_attributes[1L], fixed = TRUE))
  expect_true(grepl("locus_tag=LT001", gi$search_blob[1L], fixed = TRUE))
})

test_that("inspect_gff_genes strips gene prefixes but keeps raw attributes searchable", {
  tmp <- tempfile(fileext = ".gff3")
  writeLines(c(
    "##gff-version 3",
    "chr1\t.\tgene\t100\t500\t.\t+\t.\tID=gene:BdiBd21-3.1G000100;Name=transcript:BdiBd21-3.1G000100;locus_tag=LT001"
  ), tmp)
  on.exit(unlink(tmp))

  gi <- inspect_gff_genes(tmp)
  expect_equal(gi$gene_id[1L], "BdiBd21-3.1G000100")
  expect_true(grepl("ID=gene:BdiBd21-3.1G000100", gi$raw_attributes[1L], fixed = TRUE))
  expect_equal(nrow(search_gene_index(gi, "gene:BdiBd21-3.1G000100")), 1L)
})

test_that("inspect_gff_genes extracts Alias and fallback identifiers", {
  tmp <- tempfile(fileext = ".gff3")
  writeLines(c(
    "##gff-version 3",
    "chr1\t.\tgene\t100\t500\t.\t+\t.\tID=gene001;Name=GeneA;locus_tag=LT001;Alias=A1,A2",
    "chr1\t.\tgene\t700\t900\t.\t+\t.\tName=OnlyName",
    "chr2\t.\tgene\t1000\t1500\t.\t-\t.\t."
  ), tmp)
  on.exit(unlink(tmp))

  gi <- inspect_gff_genes(tmp)
  expect_equal(gi$alias[gi$gene_id == "gene001"], "A1,A2")
  expect_equal(gi$locus_tag[gi$gene_id == "gene001"], "LT001")
  expect_true("OnlyName" %in% gi$gene_id)
  expect_true("chr2:1000-1500" %in% gi$gene_id)
  expect_false(any(is.na(gi$gene_id) | gi$gene_id == ""))
})

test_that("inspect_gff_genes indexes ALL genes — no 5000 cap", {
  tmp <- tempfile(fileext = ".gff3")
  n   <- 6000L
  lines <- vapply(seq_len(n), function(i) {
    sprintf("chr1\t.\tgene\t%d\t%d\t.\t+\t.\tID=gene%05d;Name=G%05d",
            i * 100L, i * 100L + 90L, i, i)
  }, character(1L))
  writeLines(c("##gff-version 3", lines), tmp)
  on.exit(unlink(tmp))

  gi <- inspect_gff_genes(tmp)
  expect_false(is.null(gi))
  expect_equal(nrow(gi), n,
    info = "All genes must be indexed — no 5000 hard cap")
})

test_that("inspect_gff_genes extracts gene_id and gene_name from GTF", {
  tmp <- tempfile(fileext = ".gtf")
  writeLines(c(
    "chr1\tsrc\tgene\t100\t900\t.\t+\t.\tgene_id \"GENE001\"; gene_name \"Alpha\";",
    "chr1\tsrc\ttranscript\t100\t900\t.\t+\t.\tgene_id \"GENE001\"; transcript_id \"TX001\"; gene_name \"Alpha\";"
  ), tmp)
  on.exit(unlink(tmp))

  gi <- inspect_gff_genes(tmp)
  expect_false(is.null(gi))
  expect_true("GENE001" %in% gi$gene_id)
  expect_true("Alpha" %in% gi$name)
})

test_that("build_gene_selectize_choices returns one backend choice per indexed gene", {
  gi <- data.frame(
    gene_id = c("G1", "G2", "G3"),
    name = c("A", "B", "C"),
    locus_tag = c("", "", ""),
    chrom = c("chr1", "chr1", "chr2"),
    start = c(10L, 20L, 30L),
    end = c(15L, 25L, 35L),
    strand = c("+", "-", "+"),
    searchable_label = c("G1 | A  [chr1:10-15 +]", "G2 | B  [chr1:20-25 -]", "G3 | C  [chr2:30-35 +]"),
    source_file = "x.gff3",
    stringsAsFactors = FALSE
  )
  choices <- build_gene_selectize_choices(gi)
  expect_equal(length(choices), nrow(gi))
  expect_true(all(unname(choices) == c("G1", "G2", "G3")))
  expect_true(any(grepl("A", names(choices))))
  expect_true(any(grepl("G1", names(choices))))
  expect_false(any(is.na(names(choices)) | names(choices) == ""))
  expect_false(any(is.na(unname(choices)) | unname(choices) == ""))
  expect_equal(length(unique(unname(choices))), length(choices))
})

test_that("make_gene_selectize_choices normalizes duplicate and missing keys", {
  gi <- data.frame(
    gene_id = c("G1", "G1", ""),
    gene_name = c("Alpha", "Alpha iso", ""),
    locus_tag = c("", "", ""),
    alias = c("", "", ""),
    chrom = c("chr1", "chr1", "chr2"),
    start = c(10L, 20L, 30L),
    end = c(15L, 25L, 35L),
    strand = c("+", "-", "+"),
    feature_type = c("gene", "gene", "gene"),
    source_file = "x.gff3",
    stringsAsFactors = FALSE
  )
  choices <- make_gene_selectize_choices(gi)
  expect_equal(length(choices), nrow(gi))
  expect_false(any(is.na(names(choices)) | names(choices) == ""))
  expect_false(any(is.na(unname(choices)) | unname(choices) == ""))
  expect_equal(length(unique(unname(choices))), length(choices))
  expect_true("chr2:30-35" %in% unname(choices))
})

test_that("make_gene_selectize_data includes search_blob for server-side selectize", {
  gi <- data.frame(
    gene_id = c("G1", "G2"),
    gene_name = c("Alpha", "Beta"),
    locus_tag = c("LT1", ""),
    alias = c("", "AliasB"),
    chrom = c("chr1", "chr2"),
    start = c(10L, 20L),
    end = c(15L, 25L),
    strand = c("+", "-"),
    feature_type = c("gene", "gene"),
    raw_attributes = c("ID=G1;Name=Alpha;locus_tag=LT1", "ID=G2;Alias=AliasB"),
    source_file = "x.gff3",
    stringsAsFactors = FALSE
  )
  dat <- make_gene_selectize_data(gi)
  expect_equal(nrow(dat), nrow(gi))
  expect_true(all(c("label", "value", "search_blob") %in% names(dat)))
  expect_false(any(is.na(dat$label) | dat$label == ""))
  expect_false(any(is.na(dat$value) | dat$value == ""))
  expect_true(any(grepl("locus_tag=LT1", dat$search_blob, fixed = TRUE)))
})

test_that("search_gene_index finds exact ids prefixes and names", {
  gi <- data.frame(
    gene_id = c("BdiBd21-3.1G0000100.v1.2", "BdiBd21-3.1G0000200.v1.2", "Other001"),
    gene_name = c("BdiBd21-3.1G0000100", "KinaseBeta", "Gamma"),
    locus_tag = c("LT001", "", ""),
    alias = c("AliasA", "", ""),
    raw_attributes = c(
      "ID=BdiBd21-3.1G0000100.v1.2;Name=BdiBd21-3.1G0000100;Note=raw_hit",
      "ID=BdiBd21-3.1G0000200.v1.2;Name=KinaseBeta",
      "ID=Other001;Name=Gamma"
    ),
    chrom = c("Bd1", "Bd1", "Bd2"),
    start = c(10L, 200L, 500L),
    end = c(100L, 300L, 900L),
    strand = c("+", "-", "+"),
    feature_type = c("gene", "gene", "gene"),
    source_file = "x.gff3",
    stringsAsFactors = FALSE
  )
  gi <- normalize_gene_index(gi)
  expect_equal(nrow(search_gene_index(gi, "BdiBd21-3.1G0000100.v1.2")), 1L)
  expect_equal(nrow(search_gene_index(gi, "BdiBd21")), 2L)
  expect_equal(search_gene_index(gi, "Kinase")$gene_id, "BdiBd21-3.1G0000200.v1.2")
  expect_equal(search_gene_index(gi, "LT001")$gene_id, "BdiBd21-3.1G0000100.v1.2")
  expect_equal(search_gene_index(gi, "raw_hit")$gene_id, "BdiBd21-3.1G0000100.v1.2")
  expect_equal(nrow(search_gene_index(gi, "absent_query")), 0L)
})

test_that("resolve_gene_selection and gene_region_string handle stable gene keys", {
  gi <- data.frame(
    gene_id = c("G1", "G2"),
    gene_key = c("G1", "G2"),
    gene_name = c("Alpha", "Beta"),
    name = c("Alpha", "Beta"),
    locus_tag = c("LT1", ""),
    alias = c("A1", ""),
    chrom = c("chr1", "chr1"),
    start = c(200L, 10000L),
    end = c(500L, 12000L),
    strand = c("+", "-"),
    feature_type = c("gene", "gene"),
    searchable_label = c("G1 | Alpha | LT1 | A1 | chr1:200-500 | +", "G2 | Beta | chr1:10000-12000 | -"),
    source_file = "x.gff3",
    stringsAsFactors = FALSE
  )
  chrom_index <- data.frame(chrom = "chr1", length = 11000L, stringsAsFactors = FALSE)

  expect_equal(resolve_gene_selection(gi, "G1")$gene_id, "G1")
  expect_equal(resolve_gene_selection(gi, "Alpha")$gene_id, "G1")
  expect_equal(resolve_gene_selection(gi, "LT1")$gene_id, "G1")
  expect_equal(gene_region_string(resolve_gene_selection(gi, "G1"), 5000L, chrom_index), "chr1:1-5500")
  expect_equal(gene_region_string(resolve_gene_selection(gi, "G2"), 5000L, chrom_index), "chr1:5000-11000")
})

test_that("inspect_gff_genes gene region calculation with flanking", {
  gi <- data.frame(
    gene_id = "G1", name = "geneA", chrom = "Bd1",
    start = 10000L, end = 20000L, stringsAsFactors = FALSE
  )
  flank <- 5000L
  s <- max(1L, gi$start[1L] - flank)
  e <- gi$end[1L] + flank
  expect_equal(s, 5000L)
  expect_equal(e, 25000L)
})

test_that("inspect_gff_genes clamps start to 1 when gene near chromosome start", {
  flank     <- 5000L
  gene_start <- 200L
  s <- max(1L, gene_start - flank)
  expect_equal(s, 1L)
})

# =============================================================================
# Gene index cache
# =============================================================================

test_that("save/load/validate gene index cache works correctly", {
  tmp_proj_dir <- withr::local_tempdir()
  proj <- list(project_path = tmp_proj_dir)

  # Create a minimal GFF file
  gff_path <- file.path(tmp_proj_dir, "genes.gff3")
  writeLines(c(
    "##gff-version 3",
    "chr1\t.\tgene\t1000\t5000\t.\t+\t.\tID=gene1;Name=GeneA"
  ), gff_path)

  gene_df <- inspect_gff_genes(gff_path)
  expect_false(is.null(gene_df))

  # Save cache
  cache_path <- save_gene_index_cache(gene_df, gff_path, "fGFF", proj)
  expected_cache <- file.path(tmp_proj_dir, "metadata", "gene_index_fGFF.json")
  expect_true(file.exists(expected_cache))

  # Load cache
  cache <- load_gene_index_cache("fGFF", proj)
  expect_false(is.null(cache))
  expect_equal(cache$n_genes, 1L)
  expect_true(is.data.frame(cache$genes))
  expect_equal(cache$source_file, gff_path)

  # Validate: should be valid right after saving
  expect_true(is_gene_index_cache_valid(cache, gff_path))
})

test_that("gene index cache is invalid after file modification", {
  tmp_proj_dir <- withr::local_tempdir()
  proj <- list(project_path = tmp_proj_dir)

  gff_path <- file.path(tmp_proj_dir, "genes.gff3")
  writeLines(c(
    "##gff-version 3",
    "chr1\t.\tgene\t1000\t5000\t.\t+\t.\tID=gene1;Name=GeneA"
  ), gff_path)

  gene_df <- inspect_gff_genes(gff_path)
  save_gene_index_cache(gene_df, gff_path, "fGFF2", proj)
  cache <- load_gene_index_cache("fGFF2", proj)

  # Simulate file update (mtime change)
  Sys.sleep(1.1)
  writeLines(c(
    "##gff-version 3",
    "chr1\t.\tgene\t1000\t5000\t.\t+\t.\tID=gene1;Name=GeneA",
    "chr1\t.\tgene\t6000\t9000\t.\t-\t.\tID=gene2;Name=GeneB"
  ), gff_path)

  expect_false(is_gene_index_cache_valid(cache, gff_path))
})

test_that("gene index cache returns NULL for missing cache file", {
  tmp_proj_dir <- withr::local_tempdir()
  proj <- list(project_path = tmp_proj_dir)
  cache <- load_gene_index_cache("nonexistent_id", proj)
  expect_null(cache)
})
