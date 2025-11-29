# Changelog

## cvdprevent (development version)

## cvdprevent 0.2.4

CRAN release: 2025-11-11

## Version 0.2.4 (2025-11-11)

CRAN release: 2025-11-11

### Added

- None in this release.

### Changed

- [`cvd_clear_cache()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_clear_cache.md)
  is exported for end-users to access, for example to clear stale values
  from the cache.

### Fixed

- Cache defaults to in-memory when the package is used non-interatively,
  which should resolve CRAN test issues on certain builds. When used
  interactively (most use-cases) will use persistent disc-based cache
  that is more convenient for users.

### Deprecated

- None in this release.

## Version 0.2.3 (2025-11-05)

CRAN release: 2025-11-05

### Added

- New function
  [`cvd_external_resource()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_external_resource.md)
  to retrieve metadata for external resources linked to CVDPREVENT,
  including category, source, type, and tags.
- Support for inequality marker breakdowns via
  [`cvd_indicator_person_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_person_timeseries.md)
  with expanded documentation and return structure.
- `@return` documentation blocks added for all major functions to
  improve clarity and usability.

### Changed

- Improved documentation across all indicator-related functions,
  including clearer `@description`, `@details`, and `@seealso` sections.
- Enhanced validation logic for `metric_id`, `area_id`, and
  `time_period_id` parameters to ensure API compatibility.
- Refactored internal processing of nested API responses for better
  stability and readability.

### Fixed

- Corrected handling of empty or malformed API responses in
  [`cvd_indicator_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_group.md)
  and
  [`cvd_indicator_pathway_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_pathway_group.md).
- Resolved issue where system-level comparison functions failed to
  relocate nested `ComparisonData` correctly.

### Deprecated

- None in this release.

## cvdprevent 0.2.2

CRAN release: 2025-05-25

## cvdprevent 0.1.0

- Initial CRAN submission.
