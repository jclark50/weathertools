# =============================================================================
# Optional color helper (crayon only if installed)
# =============================================================================
.crayon_red <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) crayon::red(x) else x
}
