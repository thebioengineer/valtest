
#' Support PDF snapshotting contents
#' @rdname expect_snapshot_file
#' @param old,new Paths to old and new snapshot files.
#' @export
compare_pdf_content <- function(old, new){
  old <- read_pdf_raw(old)
  new <- read_pdf_raw(new)
  identical(old, new)
}

#' serialize pdf contents as image
#'
#' serialize pdf contents as image
#'
#' @param path path to pdf to have contents read
#' @returns pdf contents as serialized image
#'
#' @importFrom magick image_read_pdf image_append image_write
#' @importFrom brio read_file_raw
#'
#' @noRd
#' @keywords internal
read_pdf_raw <- function(path){
  tmp_image <- tempfile(fileext = ".png")
  pdf_to_png(path, tmp_image)
  brio::read_file_raw(tmp_image)
}

pdf_to_png <- function(path, output){
  pdf_image <- magick::image_read_pdf(path = path,density = 72)
  magick::image_write(magick::image_append(pdf_image,stack = TRUE), path = output,format = "png")
}

