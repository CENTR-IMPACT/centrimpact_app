# Simplified workflow functions for centralized sidebar workflow display

# Stub for create_workflow_observers to prevent missing function errors
create_workflow_observers <- function(ns_workflow, ns, session) {
  # Placeholder: add observer logic here if needed
  invisible(NULL)
}

# Main workflow update function - simplified for single workflow state
update_workflow_step <- function(rv_workflow, stage, status, metric = NULL, session, ns = NULL) {
  # Validate that rv_workflow is a reactive values object
  # if (is.null(rv_workflow)) {
  #   logger::log_error("rv_workflow is NULL in update_workflow_step")
  #   return()
  # }

  # if (!inherits(rv_workflow, "reactivevalues")) {
  #   logger::log_error("rv_workflow is not a reactivevalues object: {class(rv_workflow)[1]}")
  #   return()
  # }

  # Update the single workflow state
  # tryCatch(
  #   {
  #     # Check if rv_workflow is a character
  #     if (is.character(rv_workflow)) {
  #       logger::log_error("rv_workflow is a character value, not a reactiveValues object")
  #       return()
  #     }

  #     isolate({
  #       rv_workflow$stage <- stage
  #       rv_workflow$status <- status
  #     })
  #   },
  #   error = function(e) {
  #     logger::log_error("Error updating workflow values: {conditionMessage(e)}")
  #   }
  # )

  # logger::log_info("Updating workflow stage: {stage}, status: {status}")

  # Update the main workflow UI in the sidebar
  # tryCatch(
  #   {
  #     update_main_workflow_ui(rv_workflow, session, ns)
  #   },
  #   error = function(e) {
  #     logger::log_error("Error updating main workflow UI: {conditionMessage(e)}")
  #   }
  # )
}

# Get status color helper function
get_status_color <- function(status) {
  # Safely handle NULL or empty status
  if (is.null(status) || length(status) == 0 || status == "" || is.na(status)) {
    return("#D3D3D3") # default gray
  }

  # Safely handle NULL or empty status
  if (is.null(status) || length(status) == 0 || status == "" || is.na(status)) {
    return("#D3D3D3") # default gray
  }

  # Ensure status is a character string
  status <- as.character(status)

  switch(status,
    "complete" = "#3B6B35",
    "in progress" = "#dbd770",
    "initiated" = "#dbd770",
    "error" = "#990000",
    "not started" = "#D3D3D366",
    "init" = "#D3D3D366",
    "#D3D3D366" # default gray
  )
}

# --- All workflow UI update functions commented out for debugging ---
# update_main_workflow_icons <- function(ns_workflow, session, ns = NULL) { }
# update_main_workflow_status <- function(ns_workflow, session, ns = NULL) { }
# update_main_workflow_ui <- function(ns_workflow, session, ns = NULL) { }
# update_alignment_workflow_icons <- function(ns_workflow, session, ns = NULL) { }
# update_indicators_workflow_icons <- function(ns_workflow, session, ns = NULL) { }
# update_dynamics_workflow_icons <- function(ns_workflow, session, ns = NULL) { }
# update_cascade_workflow_icons <- function(ns_workflow, session, ns = NULL) { }
# update_all_workflow_icons <- function(ns_workflow, session, ns = NULL) { }
# update_alignment_status_display <- function(status, session, ns = NULL, ns_workflow = NULL) { }
# update_main_data_status_display <- function(status, session, ns = NULL, ns_workflow = NULL) { }
# update_main_data_workflow_icons <- function(stage, status, session, ns = NULL, ns_workflow = NULL) { }
# debug_workflow_state <- function(ns_workflow, module_name) { }
