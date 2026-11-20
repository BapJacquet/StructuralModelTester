#!/usr/bin/env Rscript
# Regenerate man/figures used by README.md (run from package root).
# UI overview/canvas still come from a live browser screenshot of the
# interaction example after Run; this script refreshes the ggplot panels.
options(bitmapType = "cairo")
pkgload::load_all(".", quiet = TRUE)

outdir <- "man/figures"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a

save_png <- function(path, plot, w = 9, h = 5.25) {
  grDevices::png(path, width = w * 120, height = h * 120, res = 120, type = "cairo")
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
}

# --- Interaction demo: Age + Gender×Method → Score -----------------------------
csv <- StructuralModelTester:::smt_sample_path("interaction_demo.csv")
js <- StructuralModelTester:::smt_sample_path("interaction_demo_model.json")
stopifnot(nzchar(csv), nzchar(js))
dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
st <- StructuralModelTester:::model_from_json(
  paste(readLines(js, warn = FALSE), collapse = "\n")
)
res <- StructuralModelTester:::fit_pvalue_model(
  st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
)
fit_score <- res$fit[["Score"]] %||% res$fit[[1]]

p_age <- StructuralModelTester:::edge_relationship_plot(
  st, dat, "e3", show_fit = TRUE, fit = fit_score
)
p_ix <- StructuralModelTester:::edge_relationship_plot(
  st, dat, "e6", show_fit = TRUE, fit = fit_score
)
save_png(file.path(outdir, "readme-age-effect.png"), p_age)
save_png(file.path(outdir, "readme-interaction-effect.png"), p_ix)
message("Wrote age + interaction effect plots")

# --- Power / FDR curves (path demo, compact Monte Carlo) ----------------------
js2 <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
stopifnot(nzchar(js2))
st2 <- StructuralModelTester:::model_from_json(
  paste(readLines(js2, warn = FALSE), collapse = "\n")
)
pow <- StructuralModelTester:::run_confirmatory_power(
  st2,
  n_grid = c(50L, 100L, 150L, 200L, 300L),
  n_reps = 40L,
  alpha = 0.05,
  seed = 1L,
  mode = "pvalue"
)
p_pow <- StructuralModelTester:::power_analysis_plot(
  pow$power, recommended_n = pow$recommended_n
)
p_fdr <- StructuralModelTester:::power_fdr_plot(
  pow$fdr, recommended_n = pow$recommended_n
)
save_png(file.path(outdir, "readme-power.png"), p_pow, h = 5)
save_png(file.path(outdir, "readme-fdr.png"), p_fdr, h = 5)
message("Wrote power + FDR plots")
message("Done. Refresh overview/canvas via browser screenshot if the UI changed.")
