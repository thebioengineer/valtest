test_that("compare_pdf_content returns TRUE when PDF image files should match - pdf()", {

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

test_that("compare_pdf_content returns TRUE when PDF image files should match - ggsave()", {

  test_file_1 <- tempfile(fileext = ".pdf")
  test_file_2 <- tempfile(fileext = ".pdf")

  tmp_gg <- ggplot2::ggplot(data.frame(x = c(1,2,3,4))) +
    ggplot2::geom_point(
      ggplot2::aes(x = x, y = x)
    )

  suppressMessages({
    ggplot2::ggsave(test_file_1, plot = tmp_gg)
    ggplot2::ggsave(test_file_2, plot = tmp_gg)
  })

  expect_true(compare_pdf_content(test_file_1, test_file_1))
  expect_true(compare_pdf_content(test_file_1, test_file_2))

})

test_that("compare_pdf_content returns FALSE when PDF image files shouldn't match - png()", {

  test_file_1 <- tempfile()
  test_file_2 <- tempfile()

  pdf(test_file_1)
  plot(c(1,2,3,4))
  dev.off()

  pdf(test_file_2)
  plot(c(1,2,3,5))
  dev.off()

  expect_false(compare_pdf_content(test_file_1, test_file_2))

})

test_that("compare_pdf_content returns FALSE when PDF image files shouldn't match - ggsave()", {

  test_file_1 <- tempfile(fileext = ".pdf")
  test_file_2 <- tempfile(fileext = ".pdf")

  tmp_gg <- ggplot2::ggplot(data.frame(x = c(1,2,3,4))) +
    ggplot2::geom_point(
      ggplot2::aes(x = x, y = x)
    )

  tmp_gg2 <- ggplot2::ggplot(data.frame(x = c(1,2,3,4,5))) +
    ggplot2::geom_point(
      ggplot2::aes(x = x, y = x)
    )
  suppressMessages({
    ggplot2::ggsave(test_file_1, plot = tmp_gg)
    ggplot2::ggsave(test_file_2, plot = tmp_gg2)
  })

  expect_false(compare_pdf_content(test_file_1, test_file_2))

})

test_that("compare_pdf_content returns TRUE when Document pdf files should match", {

  template_rmd <- tempfile(fileext = ".Rmd")
  output_pdf_1 <- tempfile(fileext = ".pdf")
  output_pdf_2 <- tempfile(fileext = ".pdf")

  writeLines(c(
    "---",
    "title: \"Untitled\"",
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

  expect_true(compare_pdf_content(output_pdf_1, output_pdf_1))
  expect_true(compare_pdf_content(output_pdf_1, output_pdf_2))

})

test_that("compare_pdf_content returns FALSE when Document pdf files shouldn't match", {

  template_rmd_1 <- tempfile(fileext = ".Rmd")
  template_rmd_2 <- tempfile(fileext = ".Rmd")
  output_pdf_1 <- tempfile(fileext = ".pdf")
  output_pdf_2 <- tempfile(fileext = ".pdf")

  template_rmd_text <- c(
    "---",
    "title: \"Untitled\"",
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
  )

  writeLines(template_rmd_text, template_rmd_1)

  writeLines(c(template_rmd_text,"## New Header","","New text line"), template_rmd_2)

  quiet <- capture.output({
    suppressMessages({
      rmarkdown::render(template_rmd_1, output_file = output_pdf_1)
      rmarkdown::render(template_rmd_2, output_file = output_pdf_2)
    })
  })

  expect_false(compare_pdf_content(output_pdf_1, output_pdf_2))

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
