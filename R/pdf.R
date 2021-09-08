
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
#' @importFrom magick image_read_pdf image_append image_write image_info
#' @importFrom brio read_file_raw
#'
#' @noRd
#' @keywords internal
read_pdf_raw <- function(path){
  tmp_image <- pdf_to_png(path, stack = TRUE)
  brio::read_file_raw(tmp_image)
}

pdf_to_png <- function(path, output = tempfile(fileext = ".png"), stack = FALSE){
  pdf_image <- magick::image_read_pdf(path = path,density = 72)

  if(stack){
    pdf_image <- magick::image_append(pdf_image,stack = TRUE)
  }else{
    output <- file.path(
      dirname(output),
      paste0(
        tools::file_path_sans_ext(basename(output)),
        "_",
        seq_len(nrow(magick::image_info(pdf_image))),
        ".",
        tools::file_ext(output)
      )
    )
  }
  for(i in seq_len(nrow(magick::image_info(pdf_image)))){
    magick::image_write(pdf_image[i],path = output[i],format = "png")
  }
  return(output)
}
