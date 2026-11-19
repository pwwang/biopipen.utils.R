# Guard the reduction-resolution regression in RunSeuratUMAP: the resolved
# reduction must be written into the argument list that reaches RunUMAP(), not
# only used for the log line. Without that assignment RunUMAP() silently fell
# back to Seurat's default ("pca") and the embedding ignored the integration
# while the log reported it.

find_src <- function() {
  for (p in c(file.path("..", "..", "R", "scrna-clustering.R"),
              file.path("R", "scrna-clustering.R"))) {
    if (file.exists(p)) return(readLines(p, warn = FALSE))
  }
  character(0)
}

test_that("RunSeuratUMAP passes the resolved reduction to RunUMAP", {
  src <- find_src()
  skip_if(!length(src), "R/scrna-clustering.R not found from the test working directory")

  # isolate the main RunSeuratUMAP body, i.e. up to the RunUMAP call
  call_idx <- grep("do_call\\(RunUMAP", src)[1]
  expect_false(is.na(call_idx))

  assign_idx <- grep("^\\s*RunUMAPArgs\\$reduction\\s*<-\\s*reduction\\s*$", src)
  expect_true(length(assign_idx) > 0)
  expect_true(any(assign_idx < call_idx))

  # and it must happen after the resolution that uses integrated_new_reduction
  resolve_idx <- grep("integrated_new_reduction", src)
  expect_true(any(resolve_idx < assign_idx))
})
