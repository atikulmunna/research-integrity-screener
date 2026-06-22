# Shared helpers: generate PDFs on the fly so tests need no binary fixtures.

make_text_pdf <- function(text_per_page) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  for (txt in text_per_page) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, txt)
  }
  grDevices::dev.off()
  path
}

make_blank_pdf <- function(pages = 1) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  for (p in seq_len(pages)) {
    graphics::plot.new()
    graphics::rect(0.2, 0.2, 0.8, 0.8)  # image-like content, no text layer
  }
  grDevices::dev.off()
  path
}
