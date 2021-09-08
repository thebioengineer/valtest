test_that("compare_pdf_content returns TRUE when PDF image files should match", {

  test_file_1 <- tempfile()
  test_file_2 <- tempfile()

  pdf(test_file_1)
  plot(c(1,2,3,4))
  dev.off()

  pdf(test_file_2)
  plot(c(1,2,3,4))
  dev.off()

  expect_true(test_file_1 != test_file_2)
  expect_true(compare_pdf_content(test_file_1, test_file_1))
  expect_true(compare_pdf_content(test_file_1, test_file_2))

})

test_that("compare_pdf_content returns FALSE when PDF image files shouldn't match", {

  test_file_1 <- tempfile()
  test_file_2 <- tempfile()

  pdf(test_file_1)
  plot(c(1,2,3,4))
  dev.off()

  pdf(test_file_2)
  plot(c(1,2,3,5))
  dev.off()

  expect_true(test_file_1 != test_file_2)
  expect_false(compare_pdf_content(test_file_1, test_file_2))

})

test_that("compare_pdf_content returns TRUE when Document pdf files should match", {

  template_rmd <- tempfile(fileext = ".Rmd")
  output_pdf_1 <- tempfile(fileext = ".pdf")
  output_pdf_2 <- tempfile(fileext = ".pdf")

  writeLines(c(
    "---",
    "title: \"Untitled\"",
    "date: \"9/7/2021\"",
    "output: pdf_document",
    "---",
    "",
    "## R Markdown",
    "",
    "This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.",
    "",
    "When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:",
    "",
    "```{r cars}",
    "summary(cars)",
    "```",
    "",
    "## Including Plots",
    "",
    "You can also embed plots, for example:",
    "",
    "```{r pressure, echo=FALSE}",
    "plot(pressure)",
    "```",
    "",
    "Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.",
    ""
    ), template_rmd)

  quiet <- capture.output({
    suppressMessages({
    rmarkdown::render(template_rmd, output_file = output_pdf_1)
    rmarkdown::render(template_rmd, output_file = output_pdf_2)
    })
  })

  expect_true(output_pdf_1 != output_pdf_2)
  expect_true(compare_pdf_content(output_pdf_1, output_pdf_1))
  expect_true(compare_pdf_content(output_pdf_1, output_pdf_2))

})


test_that("compare_pdf_content works in snapshotting", {

  generate_pdf_image <- function(file) {

    pdf(file = file)
    plot(c(1,2,3,4))
    dev.off()

    return(file)
  }

  expect_snapshot_file(
    generate_pdf_image(
      file = "test.pdf"
    ),
    compare = compare_pdf_content
  )

})
