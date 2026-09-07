workflow_lines <- function(name) {
  readLines(
    .kaefa_repo_file(".github", "workflows", name),
    warn = FALSE
  )
}

test_that("runtime workflows keep the public API contract in scope", {
  workflows <- c("R-CMD-check.yaml", "test-fast.yaml", "test-suite.yaml")

  for (workflow in workflows) {
    content <- workflow_lines(workflow)

    expect_true(sum(grepl('^      - "\\*\\*"$', content)) == 2L)
    expect_true(sum(grepl('^      - "!docs/\\*\\*"$', content)) == 2L)
    expect_true(sum(grepl('^      - "!\\*\\.md"$', content)) == 2L)
    expect_true(sum(grepl('^      - "!\\*\\*/\\*\\.md"$', content)) == 2L)
    expect_true(sum(grepl(
      '^      - "docs/product/kaefa-core-api-contract\\.md"$',
      content
    )) == 2L)
  }
})
