# ============================================================================
# Extract the PK parameter terms NCA Assistant uses from CDISC Controlled
# Terminology
# ============================================================================
# Source: CDISC SDTM Controlled Terminology, tab-delimited release file
# ("SDTM Terminology.txt") distributed by NCI Enterprise Vocabulary Services:
#   https://evs.nci.nih.gov/ftp1/CDISC/SDTM/
# Download it, then run from the repository root:
#   Rscript cdisc/extract_pk_terms.R "path/to/SDTM Terminology.txt" 2026-03-27
# The second argument is the release date stated in the matching ODM file
# (FileOID "CDISC_CT.SDTM.<date>").
#
# Writes cdisc/pk_parameter_terms.csv (the terms for the codes in
# cdisc/pk_parameter_map.csv) and cdisc/ct_release.dcf (release metadata).
# Codelists: C85839 "PK Parameters Code" (PKPARMCD, values for PPTESTCD) and
# C85493 "PK Parameters" (PKPARM, values for PPTEST), paired by NCIt code.
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript cdisc/extract_pk_terms.R <SDTM Terminology.txt> <release date>")
src <- args[1]; release <- args[2]

ct <- read.delim(src, stringsAsFactors = FALSE, quote = "", check.names = FALSE)
codes <- ct[ct$`Codelist Code` == "C85839", c("Code", "CDISC Submission Value", "CDISC Definition")]
names_ <- ct[ct$`Codelist Code` == "C85493", c("Code", "CDISC Submission Value")]
terms <- merge(codes, names_, by = "Code")
names(terms) <- c("NCIt_code", "PPTESTCD", "definition", "PPTEST")

map <- read.csv(file.path("cdisc", "pk_parameter_map.csv"), stringsAsFactors = FALSE)
used <- unique(map$PPTESTCD[nzchar(map$PPTESTCD)])
missing <- setdiff(used, terms$PPTESTCD)
if (length(missing) > 0) stop("Codes in pk_parameter_map.csv not in this release: ", paste(missing, collapse = ", "))

out <- terms[terms$PPTESTCD %in% used, c("PPTESTCD", "PPTEST", "NCIt_code", "definition")]
out <- out[order(out$PPTESTCD), ]
write.csv(out, file.path("cdisc", "pk_parameter_terms.csv"), row.names = FALSE)

sha <- if (requireNamespace("digest", quietly = TRUE)) digest::digest(file = src, algo = "sha256") else unname(tools::md5sum(src))
write.dcf(data.frame(
  Standard = "CDISC SDTM Controlled Terminology",
  Release = release,
  Codelists = "C85839 PK Parameters Code (PKPARMCD, for PPTESTCD); C85493 PK Parameters (PKPARM, for PPTEST)",
  Source = "NCI EVS, https://evs.nci.nih.gov/ftp1/CDISC/SDTM/ (SDTM Terminology.txt)",
  Source_SHA256 = sha,
  Extracted = format(Sys.Date()),
  Terms = nrow(out),
  stringsAsFactors = FALSE), file.path("cdisc", "ct_release.dcf"))
cat("Wrote", nrow(out), "terms for release", release, "\n")
