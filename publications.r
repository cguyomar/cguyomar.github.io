create_pub_listing <- function(bib_file, author = "Guyomar") {
  bib <- strsplit(paste(readLines(bib_file), collapse = "\n"), "\n@")[[1]]
  bib[1] <- substr(bib[1],start = 2,stop = nchar(bib[1]))
  articles <- lapply(
    X = paste0("@", bib[bib != ""]),
    FUN = function(ibib) {
      f <- tempfile()
      on.exit(unlink(f))
      writeLines(ibib, f)
      article <- tail(
        head(
          system(
            command = paste("pandoc", f, "--standalone", "--from=bibtex", "--to=markdown"),
            intern = TRUE
          ),
          -2
        ),
        -3
      )
      authors <- sub(".*- family: ", "", grep("family:", article, value = TRUE))
      given <- sub(".*given: ", "", grep("given", article, value = TRUE))
      given.first <-paste( sapply(given,substring,1,1),".", sep="")
      authors.string <- paste(given.first, authors, collapse = ", ")
      authors.string <- sprintf("  description: %s", authors.string)
      
      
      if (isTRUE(grepl("first", grep("note:", article, value = TRUE)))) {
        first <- "  first: '*As first or co-first*'"
      } else {
        first <- sprintf("  first: '%s'", paste(rep("&emsp;", 3), collapse = ""))
      }
      position <- sprintf("  position: '%s/%s'", grep(author, authors), length(authors))
      article <- c(
        article,
        sub("  container-title: (.*)", "  subtitle: '*\\1*'", grep("  container-title:", article, value = TRUE)),
        paste0(sub("  issued: ", "  date: \"", grep("  issued:", article, value = TRUE)),'"'),
        sub("  doi: ", "  path: https://doi.org/", grep("doi:", article, value = TRUE)),
        position,
        first,
        authors.string
      )
      article
    }
  )
  writeLines(text = unlist(articles), con = sub("\\.bib$", ".yml", bib_file))
  
  
  yaml_text <- c(
    "---",
    "title: 'Publications'",
    "page-layout: full",
    "title-block-banner: true",
    "image: /assets/images/social-profile.png",
    "date-format: 'YYYY'",
    "listing:",
    "  contents:",
    "    - publications.yml",
    "  page-size: 20",
    "  sort: 'date desc'",
    "  type: default",
    "  categories: false",
    "  sort-ui: [date, title, subtitle, first]",
    "  filter-ui: [date, title, subtitle]",
    "  fields: [date, title, subtitle, description, first]",
    "  field-display-names:",
    "    date: Issued",
    "    subtitle: Journal",
    "    description: Authors",
    "    position: Rank",
    "    first: 'First'",
    "---"
  )
  
  writeLines(
    text = sprintf(
      yaml_text
      
    ),
    con = sub("\\.bib$", ".qmd", bib_file)
  )
}

create_pub_listing(bib_file = "publications.bib")
