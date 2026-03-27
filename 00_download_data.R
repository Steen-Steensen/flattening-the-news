# =============================================================================
# 00_download_data.R
# Downloads the two input files needed for this analysis from the DataverseNO
# public deposit:
#
#   Steen Steensen, 2026, NorwegianNewsTopics (2023): Topic Modeling of
#   Norwegian News, https://doi.org/10.18710/473JEF, DataverseNO, V1
#
# Run this script once before running scripts 01–04.
# Files are saved to the working directory (the project folder).
# If the files already exist they are not re-downloaded.
# =============================================================================

DATASET_DOI    <- "doi:10.18710/473JEF"
DATAVERSE_HOST <- "https://dataverse.no"

TARGET_FILES <- c(
  "1_NorwegianNewsTopics.csv",
  "3_NorwegianNewsTopics_topic_dictionary.csv"
)

# --- Check which files still need downloading --------------------------------
to_download <- TARGET_FILES[!file.exists(TARGET_FILES)]

if (length(to_download) == 0) {
  message("All data files already present. Nothing to download.")
  quit(save = "no", status = 0)
}

# --- Fetch file list from the DataverseNO API --------------------------------
api_url  <- paste0(DATAVERSE_HOST,
                   "/api/datasets/:persistentId/versions/:latest/files",
                   "?persistentId=", DATASET_DOI)

message("Fetching file list from DataverseNO...")
response <- tryCatch(
  readLines(url(api_url, open = "r"), warn = FALSE),
  error = function(e) stop("Could not reach DataverseNO API: ", e$message)
)
close(url(api_url, open = "r"))   # tidy up connection
raw_json <- paste(response, collapse = "")

# --- Parse file IDs (base-R JSON parsing — no jsonlite needed) ---------------
# Extract all "dataFile":{..."id":<n>,...,"filename":"<name>"} blocks
extract_files <- function(json) {
  # Pull out id + filename pairs using regex
  ids    <- regmatches(json, gregexpr('"id"\\s*:\\s*\\d+', json))[[1]]
  names_ <- regmatches(json, gregexpr('"filename"\\s*:\\s*"[^"]+"', json))[[1]]
  ids    <- as.integer(sub('.*:(\\d+)', '\\1', ids))
  names_ <- sub('.*:"([^"]+)"', '\\1', names_)
  if (length(ids) != length(names_))
    stop("Could not parse file list from API response.")
  setNames(ids, names_)
}

file_ids <- tryCatch(
  extract_files(raw_json),
  error = function(e) {
    stop("Failed to parse API response. ",
         "Please download the files manually from https://doi.org/10.18710/473JEF\n",
         "Original error: ", e$message)
  }
)

# --- Download each missing file ----------------------------------------------
for (fname in to_download) {
  fid <- file_ids[fname]
  if (is.na(fid)) {
    stop("File '", fname, "' not found in the DataverseNO deposit. ",
         "Please download it manually from https://doi.org/10.18710/473JEF")
  }
  dl_url <- paste0(DATAVERSE_HOST, "/api/access/datafile/", fid)
  message("Downloading ", fname, " ...")
  tryCatch(
    download.file(dl_url, destfile = fname, mode = "wb", quiet = FALSE),
    error = function(e) stop("Download failed for '", fname, "': ", e$message)
  )
  message("  Saved to: ", normalizePath(fname))
}

message("\nDone. All required data files are present.")
