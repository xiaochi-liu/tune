# wrapper for executions that catches warnings and errors if they occur. See
# https://adv-r.hadley.nz/conditions.html
catcher <- function(expr) {
  signals <- list()
  add_cond <- function(cnd) {
    signals <<- append(signals, list(cnd))
    rlang::cnd_muffle(cnd)
  }
  handle_error <- function(e) {
    signals <<- append(signals, list(e))
  }

  res <-
    withCallingHandlers(
      expr = tryCatch(
        expr = expr,
        error = handle_error
      ),
      warning = add_cond
    )

  list(res = res, signals = signals)
}
