# Read HTML file
html_content <- readLines(
  "../docs/Annex2.html",
  warn = FALSE,
  encoding = "UTF-8")

# Combine the lines to recreate the HTML content
html_content <- paste(html_content, collapse = "\n")

# Replace a value using gsub
replacement_value <- "Gráfico "
html_content <- gsub(
  "Figura&nbsp;",
  replacement_value,
  html_content)
# Write updated HTML content to file
writeLines(html_content, "../docs/Annex2.html", useBytes = TRUE)