# run the entire workflow to generate all figures for the paper

# create directory for paper figures if it doesn't exist
if (!dir.exists("figures/paper-figures")) {
  dir.create("figures/paper-figures", recursive = TRUE)
}

# load workflow if it hasn't been run yet
if (exists("workflow_run")) {
  if (workflow_run) {
    # run the workflow if indicated
    source("code/workflow.R")
  } else {
    # do not run the workflow again
    message("Alles gut!")
  }
} else {
  # run the workflow by default
  source("code/workflow.R")
}
