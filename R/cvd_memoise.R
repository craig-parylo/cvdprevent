# Shared cache ----------------------------------------------------------------

#' Create a persistent cache for memoisation
#'
#' @description
#' Initialises a disk-based cache for storing memoised data across R sessions.
#' The cache is stored in a user-specific directory and configured to expire items after one week.
#'
#' @returns A `cachem::cache_disk` object
#' @noRd
m_cache <- cachem::cache_disk(
  dir = rappdirs::user_cache_dir("R_cvdprevent"),
  max_age = 60 * 60 * 24 * 7 # 1 week in seconds
)

# Helper to memoise a lookup with the shared cache
memoise_lookup <- function(fn) {
  memoise::memoise(fn, cache = m_cache)
}

#' Clear all cached results
#' @noRd
cvd_clear_cache <- function() {
  m_cache$reset()
  invisible(TRUE)
}

# Memoise core functions ------------------------------------------------------

#' Memoised safe API call
#'
#' Wraps safe_api_call() with memoisation, caching only successful results.
#'
#' @param req httr2 request object or URL string
#' @param process_fn function(parsed_json) -> processed object, or NULL
#' @param context character label for error tibble
#' @param timeout numeric seconds for request
#' @return list(success = TRUE, result = ...) or list(success = FALSE, tibble = ...)
#' @noRd
memoised_safe_api_call <- local({
  # Internal memoised function
  .memoised <- memoise::memoise(
    function(req, process_fn, context, timeout) {
      safe_api_call(
        req = req,
        process_fn = process_fn,
        context = context,
        timeout = timeout
      )
    },
    cache = m_cache
  )

  # Public wrapper
  function(req, process_fn = NULL, context = "safe_api_call", timeout = 30) {
    res <- .memoised(
      req = req,
      process_fn = process_fn,
      context = context,
      timeout = timeout
    )

    # If success, return cached result
    if (isTRUE(res$success)) {
      return(res)
    }

    # If failure, bypass cache and retry live
    safe_api_call(
      req = req,
      process_fn = process_fn,
      context = context,
      timeout = timeout
    )
  }
})

#' Forget cached result for a given request
#'
#' @param req httr2 request object or URL string
#' @param process_fn function used in memoised call
#' @param context context string
#' @param timeout timeout value
#' @noRd
forget_memoised_safe <- function(
  req,
  process_fn = NULL,
  context = "safe_api_call",
  timeout = 30
) {
  # Must use the same arguments as memoised_safe_api_call
  memoise::forget(
    memoised_safe_api_call,
    req = req,
    process_fn = process_fn,
    context = context,
    timeout = timeout
  )
}

# Memoise lookups -------------------------------------------------------------

delayedAssign(
  "m_get_valid_time_period_ids",
  memoise_lookup(get_valid_time_period_ids)
)
delayedAssign(
  "m_get_valid_system_level_id_for_time_period_id",
  memoise_lookup(get_valid_system_level_id_for_time_period_id)
)
delayedAssign(
  "m_get_valid_area_ids_for_time_period_id",
  memoise_lookup(get_valid_area_ids_for_time_period_id)
)
