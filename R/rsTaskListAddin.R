#' Extract TODO strings from the current editor file
#'
#' Extracts the strings and displays them in the Viewer pane
#'
#' @export
todos <- function() {
  context <- rstudioapi::getSourceEditorContext()
  lines <- context$contents

  todos <- stringi::stri_match_all_regex(lines, "\\s*# TODO (.*)$")

  # There's at most 1 TODO per line, so take all captures of length 1
  pairs <- lapply(seq_along(todos), function(i) {
    if (! all(is.na(todos[[i]]))) {
      list(txt=todos[[i]][2], ix=i)
    }
  })

  dat <- as.data.frame(do.call(rbind, Map(as.list, pairs)))
  dat$ix <- paste(basename(context$path), as.integer(dat$ix), sep=":")
  viewertable(dat)
}

# See also
# rstudioapi::as.document_position(c(10,1))
# rstudioapi::setCursorPosition()

#' Display a table in the Viewer pane with CSS resembling the Environment pane
viewertable <- function(dat) {
  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")

  html.head <- paste("<head>",
                     # '<link rel="stylesheet" type="text/css" href="mystyle.css"/>',
                     "<style>
                     table {
                     width: 100%;
                     border: 0px;
                     border-collapse: collapse;
                     font-family: Monaco;
                     font-size: 9pt !important;
                     line-height: 1.45;
                     }

                     td {
                     border: 0px;
                     border-bottom: 1px solid #f0f0f0;
                     text-align: left;
                     text-overflow: ellipsis;
                     }</style>",
                     "</head>",
                     sep="\n")

  html.table <- paste(print(xtable::xtable(dat),
                            border="0",
                            include.rownames=FALSE,
                            include.colnames=FALSE,
                            type="html", file=htmlFile),
                      collapse="\n")
  html.body <- paste("<body>", html.table, "</body>")

  write(paste(html.head, html.body, sep="\n"), file=htmlFile)

  rstudioapi::viewer(htmlFile)
}
