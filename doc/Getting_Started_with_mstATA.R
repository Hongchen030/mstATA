## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----workflow-table, echo=FALSE-----------------------------------------------
library(knitr)

workflow_table <- data.frame(
  Step = c("Prepare item pool",
           "Specify MST structure",
           "Identify hierarchical requirements",
           "Translate specifications to linear model",
           "Execute assembly via solver",
           "Diagnose infeasible models",
           "Evaluate assembled panels"),
  Description = c(
    "Check attributes in the item pool.",
    "Create mstATA_design object.",
    "Understand test specifications.",
    "Create mstATA_constraint and mstATA_objective objects.",
    "Create mstATA_model and mstATA_panel objects.",
    "Check the feasibility of individual and combined specifications.",
    "Produce tables, plots, and analytical results."
  )
)

kable(workflow_table, align = "l")


