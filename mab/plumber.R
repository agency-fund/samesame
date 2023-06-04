library(plumber)
library(uuid)

ALLOWED_RESULTS <- c("success", "failure")

# status, exp_id, arm_id, successes, failures
exp_db_file <- "exp_db.rds"
exp_db <- list()
if (file.exists(exp_db_file)) {
  exp_db <- readRDS(exp_db_file)
}

#* @apiTitle Agency Fund APIs
#* @apiDescription APIs provided to Agency Fund LPs.
#* @apiVersion 0.1

#* Multi-armed bandit
#* @param op: `draw` from an experiment, `create`, `update`, or `delete` arms
#* @param exp_id: uniquely identifying an experiment.
#* @param arm_id: uniquely identifying an arm within an experiment.
#* @param result: `success` or `failure` when updating an arm.
#* @serializer unboxedJSON
#* @get /mab
function(op = "draw", exp_id = NA, arm_id = NA, result = NA) {
  resp <- list(status = "OK", messages = c())
  # create experiment and (at least one) arms
  if (op == "create") {
    
    # missing exp_id
    if (is.na(exp_id)) {
      exp_id <- paste0(
        "exp_", strsplit(UUIDgenerate(), split = "-", fixed = TRUE)[[1]][1])
      resp$status <- "WARN"
      resp$messages <- c(resp$messages,
                         paste("Missing exp_id, generating one:", exp_id))
    }
    
    # missing arm_id
    if (is.na(arm_id)) {
      arm_id <- paste0(
        "arm_", strsplit(UUIDgenerate(), split = "-", fixed = TRUE)[[1]][1])
      resp$status <- "WARN"
      resp$messages <- c(resp$messages,
                         paste("Missing arm_id, generating one:", arm_id))
    }
    
    # duplicate arm found within an experiment
    if (exp_id %in% names(exp_db)) {
      if (arm_id %in% names(exp_db[[exp_id]])) {
        resp$status <- "ERROR"
        resp[["messages"]] <- c(resp[["messages"]],
                                paste("Arm", arm_id, "within Experiment",
                                      exp_id, "already exists."))
        return(resp)
      }
    }

    # create experiment/arms
    exp_db[[exp_id]][[arm_id]] <<- list(successes = 0, failures = 0)
    resp$exp_id <- exp_id
    resp$arm_id <- arm_id
  } else if (op == "draw") {
    
    if (is.na(exp_id)) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages, "Must provide exp_id to draw.")
      return(resp)
    }
 
    this_exp <- exp_db[[exp_id]]

    if (is.null(this_exp)) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         paste("Experiment", exp_id, "does not exist."))
      return(resp)
    } else if (length(this_exp) == 0) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         paste("Experiment", exp_id, "has no arms."))
      return(resp)
      
    } else if (length(this_exp) == 1) {
      resp$status <- "WARN"
      resp$messages <- c(resp$messages,
                         paste("Experiment", exp_id, "has only one arm."))
    }
    mab <- do.call(rbind, this_exp)
    mab_draw <- apply(mab, 1,
                      function(x) rbeta(1, x[["successes"]] + 1,
                                        x[["failures"]] + 1))
    mab_max <- names(which.max(mab_draw))
    resp$op <- op
    resp$draw <- as.list(mab_draw)
    resp$choice <- mab_max
  } else if (op == "update") {
    # any missing required parameters
    if (is.na(result) || is.na(exp_id) || is.na(arm_id)) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         "Must provide exp_id, result, and arm_id.")
      return(resp)
    }
    # experiment doesn't exist
    if (is.null(exp_db[[exp_id]])) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         paste("Experiment", exp_id, "doesn't exist"))
      return(resp)
    # arm doesn't exist within experiment
    } else if (is.null(exp_db[[exp_id]][[arm_id]])) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         paste("Arm", arm_id, "doesn't exist within Experiment",
                               exp_id))
      return(resp)
    # result doesn't exist
    } else if (!result %in% ALLOWED_RESULTS) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         paste("Result", result, "doesn't exist within Arm",
                               arm_id, "-", names(exp_db[[exp_id]][[arm_id]])))
      return(resp)
    }
    if (result == "success") {
      exp_db[[exp_id]][[arm_id]][["successes"]] <<-
        exp_db[[exp_id]][[arm_id]][["successes"]] + 1
    } else if (result == "failure") {
      exp_db[[exp_id]][[arm_id]][["failures"]] <<-
        exp_db[[exp_id]][[arm_id]][["failures"]] + 1
    }
    resp$op <- op
    resp$exp_id <- exp_id
    resp$arm_id <- arm_id
    resp$results <- exp_db[[exp_id]][[arm_id]]
  } else if (op == "delete") {
    # any missing required parameters
    if (is.na(exp_id) || is.na(arm_id)) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         "Must provide exp_id and arm_id.")
      return(resp)
    }
    # experiment doesn't exist
    if (is.null(exp_db[[exp_id]])) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         "Experiment", exp_id, "doesn't exist")
      return(resp)
    # arm doesn't exist within experiment
    } else if (is.null(exp_db[[exp_id]][[arm_id]])) {
      resp$status <- "ERROR"
      resp$messages <- c(resp$messages,
                         "Arm", arm_id, "doesn't exist within Experiment",
                         exp_id)
      return(resp)
    }
    exp_db[[exp_id]][[arm_id]] <<- NULL
    resp$op <- op
    resp$exp_id <- exp_id
    resp$arm_id <- arm_id
    resp$results <- exp_db[[exp_id]][[arm_id]]
    return(resp)
  } else {
    resp$status <- "ERROR"
    resp$messages <- c(resp$messages, paste("No op", op))
    return(resp)
  }
  saveRDS(exp_db, exp_db_file)
  return(resp)
}