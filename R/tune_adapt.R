#' Model tuning via grid search
#'
#' `tune_adapt()` computes a set of perfomance metrics (e.g. accuracy or RMSE)
#'  for a pre-defined set of tuning parameters that correspond to a model or
#'  recipe across one or more resamples of the data.
#'
#' @param object A model workflow or recipe object.
#' @param formula A traditional model formula.
#' @param model A `parsnip` model specification (or `NULL` when `object` is a
#' workflow).
#' @param rs An `rset()` object. This argument __should be named__.
#' @param grid A data frame of tuning combinations or `NULL`. If used, this
#' argument __should be named__.
#' @param perf A `yardstick::metric_set()` or `NULL`. If used, this argument
#' __should be named__.
#' @param control An object used to modify the tuning process. If used, this
#' argument __should be named__.
#' @param ... Not currently used.
#' @return A tibble of results.
#'
#' @details
#'
#' Suppose there are _m_ tuning parameter combinations. `tune_adapt()` may not
#' require all _m_ model/recipe fits across each resample. For example:
#'
#' \itemize{
#'   \item In cases where a single model fit can be used to make predictions
#'         for different parameter values in the grid, only one fit is used.
#'         For example, for some boosted trees, of 100 iterations of boosting
#'         are requested, the model object for 100 iterations can be used to
#'         make predictions on iterations less than 100 (if all other
#'         parameters are equal).
#'   \item When the model is being tuned in conjunction with pre-processing
#;         and/or post-processing parameters, the minimum number of fits are
#'.        used. For example, if the number of PCA components in a recipe step
#'         are being tuned over three values (along with model tuning
#'         parameters), only three recipes are are trained. The alternative
#'         would be to re-train the same recipe multiple times for each model
#'         tuning parameter.
#' }
#'
#' The `foreach` package is used here. To execute the resampling iterations in
#' parallel, register a parallel backend function. See the documentation for
#' `foreach::foreach()` for examples.
#'
#' For the most part, warnings generated during training are shown as they occur
#' and are associated with a specific resample when `control(verbose = TRUE)`.
#' They are (usually) not aggregated until the end of processing.
#'
#' @section Parameter Grids:
#'
#' If no tuning grid is provided, a semi-random grid (via
#' `dials::grid_latin_hypercube()`) is created with 10 candidate parameter
#' combinations.
#'
#' @section Performance Metrics:
#'
#' If no metric set is provided, one is created:
#' \itemize{
#'   \item For regression models, the root mean squared error and coefficient
#'         of determination are computed.
#'   \item For classification, the log-likelihood and overall accuracy are
#'         computed.
#' }
#'
#' Note that the metrics also determine what type of predictions are estimated
#' during tuning. For example, in a classification problem, of metrics are used
#' that are all associated with hard class predictions, the classification
#' probabilities are not created.
#'
#' The out-of-sample estimates of these metrics are contained in a list column
#' called `.metrics`. This tibble contains a row for each metric and columns
#' for the value, the estimator type, and so on.
#'
#' A `summarize()` method can be used for these objects to collapse the results
#' over the resampled (to obtain the final resampling estimates per tuning
#' parameter combination).
#'
#' @section Obtaining Predictions:
#'
#' When `control(save_preds = TRUE)`, the output tibble contains a list column
#'  called `.predictions` that has the out-of-sample predictions for each
#'  parameter combination in the grid and each fold (which can be very large).
#'
#' The elements of the tibble are tibbles with columns for the tuning
#'  parameters, the row number from the original data object (`.row`), the
#'  outcome data (with the same name(s) of the original data), and any columns
#'  created by the predictions. For example, for simple regression problems
#'  generates a column called `.pred` and so on. As noted above, the prediction
#'  columns that are returned are determined by the type of metric(s) requested.
#'
#' This list column can be `unnested` using `tidyr::unnest()` or using the
#'  convenience function `collect_predictions()`.
#'
#' @section Extracting information:
#'
#' The `extract` control option will result in an additional function to be
#'  returned called `.extracts`. This is a list column that has tibbles
#'  containing the results of the user's function for each tuning parameter
#'  combination. This can enable returning each model and/or recipe object that
#'  is created during resampling. Note that this could result in a large return
#'  object, depending on what is returned.
#'
#' Note that the function given to the `extract` argument is evaluated on
#'  every model that is _fit_ (as opposed to every model that is _evaluated_).
#' As noted above, in some cases, model predictions can be derived for
#'  sub-models so that, in these cases, not every row in the tuning parameter
#'  grid has a separate R object associated with it.
#'
#' @export
tune_adapt <- function(object, ...) {
  UseMethod("tune_adapt")
}

#' @export
#' @rdname tune_adapt
tune_adapt.default <- function(object, ...) {
  stop("The first argument should be either a formula, recipe, or workflow.",
       call. = FALSE)
}

#' @export
#' @rdname tune_adapt
tune_adapt.recipe <- function(object, model, rs, grid = NULL,
                             perf = NULL, control = grid_control(), ...) {
  if (is_missing(model) || !inherits(model, "model_spec")) {
    stop("`model` should be a parsnip model specification object.", call. = FALSE)
  }

  wflow <-
    workflows::workflow() %>%
    workflows::add_recipe(object) %>%
    workflows::add_model(model)

  tune_adapt_workflow(wflow, rs = rs, grid = grid, perf = perf, control = control)
}

#' @export
#' @rdname tune_adapt
tune_adapt.formula <- function(formula, model, rs, grid = NULL,
                             perf = NULL, control = grid_control(), ...) {
  if (is_missing(model) || !inherits(model, "model_spec")) {
    stop("`model` should be a parsnip model specification object.", call. = FALSE)
  }

  wflow <-
    workflows::workflow() %>%
    workflows::add_formula(formula) %>%
    workflows::add_model(model)

  tune_adapt_workflow(wflow, rs = rs, grid = grid, perf = perf, control = control)
}

#' @export
#' @rdname tune_adapt
tune_adapt.workflow <- function(object, model = NULL, rs, grid = NULL,
                             perf = NULL, control = grid_control(), ...) {
  if (!is.null(model)) {
    stop("When using a workflow, `model` should be NULL.", call. = FALSE)
  }

  tune_adapt_workflow(object, rs = rs, grid = grid, perf = perf, control = control)
}

# ------------------------------------------------------------------------------

tune_adapt_workflow <- function(object, rs, grid = NULL, perf = NULL,
                                .control = adapt_control(), control = grid_control()) {
  check_rset(rs) # make sure at least burn-in + 1 resamples
  param_info <- param_set(object)
  check_object(object, check_dials = is.null(param_info))
  check_grid_control(control)
  perf <- check_perf(perf, object)
  perf_data <- perf_info(perf)
  perf_name <- perf_data$.metric[1]
  maximize <- perf_data$direction[perf_data$.metric == perf_name] == "maximize"

  B <- nrow(rs)
  p <- nrow(grid)
  chr_p <- format(1:p)

  cli::cli_alert_info("Evaluating {.control$burn_in} resamples across {nrow(grid)} parameters")
  res <-
    tune_grid(
      object,
      rs = rs %>% slice(1:.control$burn_in),
      grid = grid,
      perf = perf,
      control = control
    )

  on.exit(return(res))

  # modularize this and also report out current best
  keep_models <- anova_filter(res, grid, param_info, perf, ctrl = .control)
  if (nrow(keep_models) > 1) {
    cli::cli_alert_info("Retaining {chr_p[nrow(keep_models)]} of {chr_p[nrow(grid)]} parameters")
    current_grid <- keep_models
  } else {
    cli::cli_alert_success("One remaining model")
  }

  if (nrow(keep_models) > 1) {
    for (rsamps in (.control$burn_in + 1):B) {
      message("")
      message(cli::rule(left = crayon::bold(paste("Resample", rsamps, "of", B))))
      message("")

      new_res <-
        tune_grid(
          object,
          rs = rs %>% slice(rsamps),
          grid = current_grid,
          perf = perf,
          control = control
        )
      res <- bind_rows(res, new_res)
      keep_models <- anova_filter(res, current_grid, param_info, perf, ctrl = .control)
      if (nrow(keep_models) > 1) {
        cli::cli_alert_info("Retaining {chr_p[nrow(keep_models)]} of {chr_p[nrow(current_grid)]} parameters")
        current_grid <- keep_models
      } else {
        cli::cli_alert_success("One remaining model")
        break
      }
    }
  }

  on.exit()

  # add back in missing rows of rs and reorder
  reup_rs(rs, res)
}


#' @export
adapt_control <-
  function(verbose = TRUE,
           burn_in = 5,
           shuffle = FALSE,
           method = "means",
           alpha = 0.01) {
    list(
      verbose = verbose,
      burn_in = burn_in,
      shuffle = shuffle,
      method = method,
      alpha = alpha
    )
  }

names0 <- function(num, prefix = "x") {
  if (num < 1) {
    stop("`num` should be > 0", call. = FALSE)
  }
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

format_metrics <- function(x, grid, pset, perf, win_loss = FALSE) {
  perf_data <- tune:::perf_info(perf)
  perf_name <- perf_data$.metric[1]
  maximize <- perf_data$direction[perf_data$.metric == perf_name] == "maximize"
  y <-
    summarize(x) %>%
    dplyr::filter(.metric == perf_name) %>%
    inner_join(grid, by = pset$id)
  z <-
    collect_metrics(x) %>%
    dplyr::filter(.metric == perf_name) %>%
    dplyr::select(one_of(pset$id), matches("^id"), .estimate) %>%
    inner_join(grid, by = pset$id)

  models <- y %>% dplyr::select(one_of(pset$id), mean)
  p <- nrow(models)
  if (maximize) {
    models <- models %>% dplyr::arrange(desc(mean))
  } else {
    models <- models %>% dplyr::arrange(mean)
  }
  models <-
    models %>%
    dplyr::mutate(
      ..model = names0(p, "model_"),
      ..model = factor(..model, levels = ..model)
    ) %>%
    dplyr::select(-mean)
  ..models <- levels(models$..model)

  z <- full_join(z, models, by = pset$id)
  if (!win_loss) {
    scores <- z
  } else {
    model_pairs <-
      utils::combn(p, 2) %>%
      t()  %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      setNames(c("ind_1", "ind_2")) %>%
      mutate(
        model_1 = ..models[ind_1],
        model_2 = ..models[ind_2]
      )
    scores <-
      purrr::map2_df(model_pairs$model_1, model_pairs$model_2,
                     get_win_loss, dat = z, maximize) %>%
      mutate(
        model_1 = factor(model_1, levels = ..models),
        model_2 = factor(model_2, levels = ..models)
      )
  }
  scores
}


get_win_loss <- function(m1, m2, dat, maximize) {
  if (!maximize) {
    dat$.estimate <- -dat$.estimate
  }
  wide_dat <-
    dplyr::filter(dat, ..model == m1 | ..model == m2) %>%
    dplyr::select(..model, .estimate, matches("^id")) %>%
    tidyr::pivot_wider(names_from = ..model, values_from = .estimate, id_cols = matches("^id"))
  wins_1 <-
    sum(wide_dat[[m1]] > wide_dat[[m2]] ) +
    .5 * sum(wide_dat[[m1]] == wide_dat[[m2]])
  wins_2 <-
    sum(wide_dat[[m2]] > wide_dat[[m1]] ) +
    .5 * sum(wide_dat[[m1]] == wide_dat[[m2]])

  tibble(model_1 = m1, model_2 = m2, wins_1 = wins_1, wins_2 = wins_2)
}

# ------------------------------------------------------------------------------

mixed_model_call <- function(x, ...) {
  UseMethod("mixed_model_call")
}

mixed_model_call.rset <- function(x, ...) {
  rlang::call2(
    .fn = "lme",
    .ns = "nlme",
    .estimate ~ ..model,
    data = expr(stats),
    random = ~ 1 | id
  )
}

mixed_model_call.vfold_cv <- function(x, ...) {

  if (any(names(x) == "id2")) {
    # We may not have been given any of the second repeat yet
    num_reps <- length(unique(x$id))
  } else {
    num_reps <- 1
  }
  if (num_reps > 1) {
    cl <-
      rlang::call2(
        .fn = "lme",
        .ns = "nlme",
        .estimate ~ ..model,
        data = expr(stats),
        random = ~ 1 | id2 / id
      )
  } else {
    cl <-
      rlang::call2(
        .fn = "lme",
        .ns = "nlme",
        .estimate ~ ..model,
        data = expr(stats),
        random = ~ 1 | id
      )
  }
  cl
}

mixed_model_call.rolling_origin <- function(x, ...) {
  rlang::call2(
    .fn = "lme",
    .ns = "nlme",
    .estimate ~ ..model,
    data = expr(stats %>% dplyr::arrange(id)),
    random = ~ 1 | id,
    correlation = expr(nlme::corAR1())
  )
}


anova_filter <- function(x, grid, pset, metrics, ctrl) {
  stats <- format_metrics(x, grid, pset, metrics)
  model_set <- stats %>%
    select(one_of(pset$id), ..model) %>%
    distinct()

  # todo make apprpriate call based on resample type

  cl <- mixed_model_call(x)
  mean_mod <- try(nlme::lme(.estimate ~ ..model, data = stats, random = ~ 1 | id),
    silent = TRUE
  )
  if (!inherits(mean_mod, "try-error")) {

    sd_param  <-
      purrr::map_dbl(mean_mod$modelStruct$reStruct, ~ attr(summary(.x), "stdDev"))
    sd_param <- sd_param * mean_mod$sigma
    sd_param <- c(sd_param, mean_mod$sigma)
    var_param <- sd_param^2
    pct_var <- sum(var_param[-length(var_param)]) / sum(var_param) * 100
    cli::cli_alert_info("Resample-to-resample variation was {round(pct_var, 1)}% of total")

    retained <-
      summary(mean_mod)$tTable %>%
      tibble::as_tibble(rownames = "..model") %>%
      dplyr::select(..model, mean = Value, p_val = `p-value`) %>%
      dplyr::mutate(..model = gsub("..model", "", ..model, fixed = TRUE)) %>%
      dplyr::filter(..model != "(Intercept)") %>%
      dplyr::filter(p_val > ctrl$alpha) %>%
      dplyr::select(..model)
  } else {
    cli::cli_alert_warning("Mixed-effects model failed")
    mean_mod <- try(stats::lm(.estimate ~ ..model, data = stats), silent = TRUE)
    retained <-
      summary(mean_mod)$coefficients %>%
      tibble::as_tibble(rownames = "..model") %>%
      dplyr::select(..model, mean = Estimate, p_val = `Pr(>|t|)`) %>%
      dplyr::mutate(..model = gsub("..model", "", ..model, fixed = TRUE)) %>%
      dplyr::filter(..model != "(Intercept)") %>%
      dplyr::filter(p_val > ctrl$alpha) %>%
      dplyr::select(..model)
    if (!inherits(mean_mod, "try-error")) {
      stop("Could not fit an ANOVA model to these data")
    }
  }
  retained <- bind_rows(retained, tibble(..model = levels(stats$..model)[1]))

  res <-
    model_set %>%
    dplyr::mutate(..model = as.character(..model)) %>%
    dplyr::inner_join(retained, by = "..model")

  best_mod <-
    res %>%
    dplyr::filter(..model == levels(stats$..model)[1]) %>%
    dplyr::select(one_of(pset$id)) %>%
    as.data.frame() %>%
    format(digits = 4) %>%
    as.list()
  best_mod_chr <- paste0(names(best_mod), "=", unname(best_mod), collapse = ", ")
  cli::cli_alert_info("Current best: {best_mod_chr}")

  res %>% dplyr::select(-..model)
}
