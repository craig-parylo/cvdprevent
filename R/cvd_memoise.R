# Shared cache ----------------------------------------------------------------

#' Check if a persistent cache is OK to use
#'
#' @returns Logical. TRUE = persistent cache is OK to use. FALSE = persistent cache is not OK.
#'
#' @noRd
is_persistent_cache_ok <- function() {
  # allow user override
  if (nzchar(Sys.getenv("CVDPREVENT_PERSISTENT_CACHE"))) {
    return(TRUE)
  }

  # avoid persistent cache on CRAN or non-interactive sessions
  if (!interactive()) {
    return(FALSE)
  }
  if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) {
    return(TRUE)
  }
  FALSE
}

#' Create a persistent cache for memoisation
#'
#' @description
#' Initialises a disk-based cache for storing memoised data across R sessions.
#' The cache is stored in a user-specific directory and configured to expire items after one week.
#'
#' @return A `cachem::cache_disk` object
#' @import cachem
#' @import rappdirs
#' @noRd
# m_cache <- cachem::cache_disk(
#   dir = rappdirs::user_cache_dir("R_cvdprevent"),
#   max_age = 60 * 60 * 24 * 7 # 1 week in seconds
# )
m_cache <- if (is_persistent_cache_ok()) {
  cachem::cache_disk(
    dir = rappdirs::user_cache_dir("R_cvdprevent"),
    max_age = 60 * 60 * 24 * 7 # 1 week in seconds
  )
} else {
  # otherwise use in-memory cache set to invalidate after 15 minutes
  cachem::cache_mem(max_age = 60 * 15)
}

# Helper to memoise a lookup with the shared cache
memoise_lookup <- function(fn) {
  memoise::memoise(fn, cache = m_cache)
}

#' Clear package cache
#'
#' @description
#' Remove all entries from the memoise cache used by cvdprevent.
#'
#' @details
#' This function forces the package cache to be emptied. It is safe to call from interactive sessions, non-interactive checks and tests. Clearing the cache does not change any package options or remove the cache directory; it only removes the stored key/value entries so subsequent calls will re-query the API.
#'
#' Use this when you want to:
#' - force fresh API requests or recomputation during development
#' - clear stale or corrupted cache contents before running checks
#' - free disc space used by the cache
#'
#' @return Invisibly returns TRUE on success
#'
#' @examples
#' \dontrun{
#' # Clear cache
#' cvd_clear_cache()
#' }
#'
#' @export
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
delayedAssign(
  "m_get_valid_tag_ids",
  memoise_lookup(get_valid_tag_ids)
)
delayedAssign(
  "m_get_valid_indicator_ids_for_time_period_id_and_area_id",
  memoise_lookup(get_valid_indicator_ids_for_time_period_id_and_area_id)
)
delayedAssign(
  "m_get_valid_metric_ids_for_time_period_id_and_area_id",
  memoise_lookup(get_valid_metric_ids_for_time_period_id_and_area_id)
)
delayedAssign(
  "m_get_valid_indicator_ids_for_time_period_id_and_system_level_id",
  memoise_lookup(get_valid_indicator_ids_for_time_period_id_and_system_level_id)
)
delayedAssign(
  "m_get_valid_pathway_group_ids",
  memoise_lookup(get_valid_pathway_group_ids)
)
