workflow_lines <- function(name) {
  readLines(
    .kaefa_repo_file(".github", "workflows", name),
    warn = FALSE
  )
}

workflow_event_paths <- function(content, event_name) {
  event_line <- match(paste0("  ", event_name, ":"), content)
  if (is.na(event_line)) {
    stop("Missing workflow event: ", event_name)
  }

  remaining <- content[seq.int(event_line + 1L, length(content))]
  paths_offset <- match("    paths:", remaining)
  if (is.na(paths_offset)) {
    stop("Missing paths list for workflow event: ", event_name)
  }

  entries <- content[seq.int(event_line + paths_offset + 1L, length(content))]
  entry_count <- rle(grepl('^      - "', entries))$lengths[1L]
  if (!grepl('^      - "', entries[1L])) {
    stop("Empty paths list for workflow event: ", event_name)
  }

  sub('^      - "(.*)"$', "\\1", entries[seq_len(entry_count)])
}

test_that("runtime workflows keep the public API contract in scope", {
  workflows <- c("R-CMD-check.yaml", "test-fast.yaml", "test-suite.yaml")
  expected_paths <- c(
    "**",
    "!docs/**",
    "!*.md",
    "!**/*.md",
    "docs/product/kaefa-core-api-contract.md"
  )

  for (workflow in workflows) {
    content <- workflow_lines(workflow)

    expect_identical(workflow_event_paths(content, "push"), expected_paths)
    expect_identical(
      workflow_event_paths(content, "pull_request"),
      expected_paths
    )
  }
})
