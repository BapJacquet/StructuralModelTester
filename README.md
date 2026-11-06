# Structural Model Tester

Jamovi-inspired R package for visually building and testing structural models with [lavaan](https://lavaan.ugent.be/).

Define variables as nodes, draw **fixed**, **confirmatory**, or **exploratory** links (plus **measurement** links for latents), map data columns to nodes, then run:

- **p-value framework** — fit the full model and optionally mark non-significant exploratory links  
- **AIC / BIC search** — compare subsets of non-fixed links (confirmatory, exploratory, measurement), including the null (intercept-only) model; only **fixed** links are always kept  


Excluded and included links update on the path diagram; estimates appear on the edges and in the results pane.

Works on **Linux, Windows, and macOS** (anywhere R runs).

## Requirements

- R 4.1 or newer  
- A modern web browser (the UI opens locally via Shiny)

## Install

From the package directory:

```r
install.packages(c("devtools", "lavaan", "visNetwork", "shiny", "readr", "jsonlite", "DT", "igraph"))
devtools::install(".")   # or: remotes::install_local(".", force = TRUE)
```

## Run

```r
library(StructuralModelTester)
run_app()
```

## Quick start

1. Demo data (`X`, `M`, `Y`, `Z1`, `Z2`) loads automatically.  
2. In **Variables**, click **＋** next to `X`, `M`, and `Y` to add observed nodes.  
3. Switch to **Model**. Select two nodes (Ctrl/Cmd-click), choose a link type, click **Connect selected**.  
   - Example: confirmatory `X → M`, `M → Y`; exploratory `X → Y`.  
4. Click **Run** (p-values or AIC/BIC).  
5. Inspect the canvas and the **Results** pane.

Unmapped nodes are treated as **latents**. Connect them to indicators with link type **Measurement**.

## Project files

Use **Save project** / **Open project** for JSON model graphs (nodes, links, mappings). Data is loaded separately via **Open data**.

## License

MIT
