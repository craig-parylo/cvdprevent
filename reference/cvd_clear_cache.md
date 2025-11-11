# Clear package cache

Remove all entries from the memoise cache used by cvdprevent.

## Usage

``` r
cvd_clear_cache()
```

## Value

Invisibly returns TRUE on success

## Details

This function forces the package cache to be emptied. It is safe to call
from interactive sessions, non-interactive checks and tests. Clearing
the cache does not change any package options or remove the cache
directory; it only removes the stored key/value entries so subsequent
calls will re-query the API.

Use this when you want to:

- force fresh API requests or recomputation during development

- clear stale or corrupted cache contents before running checks

- free disc space used by the cache

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear cache
cvd_clear_cache()
} # }
```
