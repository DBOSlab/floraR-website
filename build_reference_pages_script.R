# scripts/build_reference_pages.R

suppressPackageStartupMessages({
  library(fs)
  library(tools)
  library(devtools)
  library(roxygen2)
  library(knitr)
  library(purrr)
})

# --- Configuration ----------------------------------------------------------
pkg_name <- "refloraR"                                   # package name
pkg_dir  <- "/Users/domingoscardoso/Library/Mobile Documents/com~apple~CloudDocs/Publications_Bioinformatics/refloraR_package_and_website/refloraR"
ref_dir  <- path("reference")                             # output dir (relative to wd)

# --- Helpers ----------------------------------------------------------------
safe_filename <- function(x) gsub("[^A-Za-z0-9._-]", "-", x)

yaml_quote <- function(x) {
  if (is.null(x) || length(x) == 0) return("''")
  x <- as.character(x)
  x <- gsub("'", "''", x, fixed = TRUE)
  sprintf("'%s'", x)
}

rd_tag <- function(x) attr(x, "Rd_tag")

rd_render <- function(x) {
  if (length(x) == 0) return("")
  if (is.list(x) && is.null(rd_tag(x))) {
    return(paste0(vapply(x, rd_render, character(1)), collapse = ""))
  }
  tag <- rd_tag(x)
  if (is.null(tag)) return(paste0(unlist(x), collapse = ""))
  if (tag == "TEXT") return(paste0(unlist(x), collapse = ""))
  children <- unname(as.list(x))
  rend_children <- function() paste0(vapply(children, rd_render, character(1)), collapse = "")
  switch(tag,
         "\\code" = paste0("`", rend_children(), "`"),
         "\\samp" = paste0("`", rend_children(), "`"),
         "\\kbd"  = paste0("`", rend_children(), "`"),
         "\\emph" = paste0("*", rend_children(), "*"),
         "\\strong" = paste0("**", rend_children(), "**"),
         "\\bold" = paste0("**", rend_children(), "**"),
         "\\pkg"  = rend_children(),
         "\\doi"  = paste0("https://doi.org/", rend_children()),
         "\\href" = {
           url <- rd_render(children[[1]])
           txt <- if (length(children) >= 2) rd_render(children[[2]]) else url
           paste0("[", trimws(txt), "](", trimws(url), ")")
         },
         "\\link" = rend_children(),
         "\\itemize" = {
           vals <- vapply(children, rd_render, character(1))
           paste0("\n", paste(paste0("- ", vals), collapse = "\n"))
         },
         "\\enumerate" = {
           vals <- vapply(children, rd_render, character(1))
           paste0("\n", paste(paste0(seq_along(vals), ". ", vals), collapse = "\n"))
         },
         "\\item" = rend_children(),
         rend_children()
  )
}

rd_get_section <- function(rd, tag) {
  for (el in rd) if (identical(rd_tag(el), tag)) return(el)
  NULL
}

rd_section_md <- function(rd, tag, title_md) {
  sec <- rd_get_section(rd, tag)
  if (is.null(sec)) return("")
  content <- rd_render(sec)
  content <- gsub("\n{3,}", "\n\n", content)
  paste0("### ", title_md, "\n\n", trimws(content), "\n\n")
}

rd_arguments_table <- function(rd) {
  sec <- rd_get_section(rd, "\\arguments")
  if (is.null(sec)) return("")
  rows <- lapply(sec, function(it) {
    if (!identical(rd_tag(it), "\\item")) return(NULL)
    nm <- if (length(it) >= 1) rd_render(it[[1]]) else ""
    ds <- if (length(it) >= 2) rd_render(it[[2]]) else ""
    nm <- gsub("[\r\n]+", " ", nm)
    ds <- gsub("[\r\n]+", " ", ds)
    nm <- gsub("\\s{2,}", " ", trimws(nm))
    ds <- gsub("\\s{2,}", " ", trimws(ds))
    c(nm, ds)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return("")
  lines <- vapply(rows, function(r) sprintf("| %s | %s |", r[1], r[2]), character(1))
  paste(
    "### Arguments\n\n",
    "| Argument | Description |\n|---|---|\n",
    paste(lines, collapse = "\n"),
    "\n\n",
    sep = ""
  )
}

collect_rcode <- function(x) {
  if (length(x) == 0) return("")
  tag <- rd_tag(x)
  if (is.null(tag)) return(paste0(unlist(x), collapse = ""))
  if (tag %in% c("RCODE", "Rcode", "VERB", "CODE")) return(paste0(unlist(x), collapse = ""))
  kids <- unname(as.list(x))
  if (tag %in% c("\\dontrun", "\\donttest", "\\dontshow", "\\if", "\\ifelse")) {
    return(paste0(vapply(kids, collect_rcode, character(1)), collapse = ""))
  }
  paste0(vapply(kids, collect_rcode, character(1)), collapse = "")
}

rd_examples_md <- function(rd) {
  sec <- rd_get_section(rd, "\\examples")
  if (is.null(sec)) return("")
  code <- collect_rcode(sec)
  code <- gsub("\r?\n", "\n", code)
  code <- trimws(code)
  if (!nzchar(code)) return("")
  paste0("### Examples\n\n```r\n", code, "\n```\n\n")
}

extract_description_txt <- function(rd) {
  sec <- rd_get_section(rd, "\\title")
  if (is.null(sec)) return("")
  trimws(rd_render(sec))
}

# --- Prepare ----------------------------------------------------------------
dir_create(ref_dir)

# Refresh Rd files if none exist
man_dir <- path(pkg_dir, "man")
if (!dir_exists(man_dir) || length(dir_ls(man_dir, glob = "*.Rd", type = "file")) == 0) {
  roxygen2::roxygenize(pkg_dir, roclets = c("rd"))
}

# Load pkg (so exports are known)
devtools::load_all(pkg_dir, quiet = TRUE)
exports <- getNamespaceExports(pkg_name)

# --- Build alias -> Rd mapping ---------------------------------------------
rd_files <- dir_ls(man_dir, glob = "*.Rd", type = "file")
rd_files <- rd_files[!grepl("-package[.]Rd$", rd_files)]
rd_objs  <- set_names(
  lapply(rd_files, function(f) {
    tryCatch(suppressWarnings(tools::parse_Rd(f, encoding = "UTF-8")), error = function(e) NULL)
  }),
  rd_files
)
rd_objs <- rd_objs[!vapply(rd_objs, is.null, logical(1))]

alias_map <- new.env(parent = emptyenv())
for (f in names(rd_objs)) {
  rd <- rd_objs[[f]]
  al <- tryCatch(unlist(tools:::.Rd_get_metadata(rd, "alias")), error = function(e) character())
  if (length(al)) {
    for (a in al) if (is.null(alias_map[[a]])) alias_map[[a]] <- f
  }
}

resolve_rd <- function(symbol) {
  if (!is.null(alias_map[[symbol]])) return(alias_map[[symbol]])
  candidate <- path(man_dir, paste0(symbol, ".Rd"))
  if (file_exists(candidate)) return(candidate)
  NA_character_
}

# --- Generate per-function .qmd --------------------------------------------
for (fun in sort(unique(exports))) {
  rdfile <- resolve_rd(fun)
  outfile <- path(ref_dir, paste0(safe_filename(fun), ".qmd"))

  if (is.na(rdfile)) {
    cat(
      sprintf(
        "---
title: %s
description: %s
toc: true
toc-depth: 3
---

> Documentation for `%s()` was not found. Ensure the function is exported and documented.
",
        yaml_quote(fun), yaml_quote(sprintf("Reference for `%s()`.", fun)), fun
      ),
      file = outfile
    )
    next
  }

  rd <- rd_objs[[rdfile]]
  desc_plain <- extract_description_txt(rd)

  front <- sprintf(
    paste0(
      "---
",
      "title: %s
",
      "description: %s
",
      "toc: true
",
      "toc-depth: 3
",
      "---

"
    ),
    yaml_quote(fun), yaml_quote(desc_plain)
  )

  body <- paste0(
    "```{r}
#| eval: false
", pkg_name, "::", fun, "()
```

",
    rd_section_md(rd, "\\description", "Description"),
    rd_section_md(rd, "\\details", "Details"),
    rd_arguments_table(rd),
    rd_section_md(rd, "\\value", "Value"),
    rd_examples_md(rd)
  )

  writeLines(c(front, body), con = outfile, useBytes = TRUE)
}

# --- Build index.qmd --------------------------------------------------------
index_lines <- c(
  "---",
  "title: \"Function Reference\"",
  "format:",
  "  html:",
  "    toc: true",
  "    toc-depth: 3",
  "---",
  "",
  "<style>",
  "  body {",
  "    background-image: url('/figures/reflora_bg.png');",
  "    background-repeat: repeat-y;",
  "    background-size: cover;",
  "    background-attachment: fixed;",
  "    background-position: top center;",
  "    position: relative;",
  "    z-index: 0;",
  "  }",
  "  .background-overlay {",
  "    position: fixed;",
  "    top: 0;",
  "    left: 0;",
  "    width: 100%;",
  "    height: 100%;",
  "    background-color: rgba(255, 255, 255, 0.85);",
  "    z-index: -1;",
  "  }",
  "</style>",
  ""
)

by_group <- split(sort(unique(exports)), setNames(rep(pkg_name, length(unique(exports))), unique(exports)))

for (grp in sort(names(by_group))) {
  index_lines <- c(index_lines, sprintf("## %s", grp), "")
  for (fun in sort(by_group[[grp]])) {
    qmd <- paste0(safe_filename(fun), ".qmd")
    index_lines <- c(index_lines, sprintf("- [`%s()`](/reference/%s)", fun, qmd))
    index_lines <- c(index_lines, "")
  }
  index_lines <- c(index_lines, "")
}

writeLines(index_lines, path(ref_dir, "index.qmd"))

message("✔ Reference pages written to ", ref_dir)
