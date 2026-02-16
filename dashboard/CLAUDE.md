# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JC Poultry Farm Dashboard — a Quarto-based analytics dashboard tracking financial and operational performance of a poultry farming business in Uganda. Data is sourced live from KoboToolbox via API and rendered as a static HTML dashboard deployed to GitHub Pages via GitHub Actions.

## Build Commands

```bash
# Render the dashboard (requires KOBO_TOKEN env var)
cd dashboard && quarto render index.qmd

# For local dev, set the token first:
# Option 1: Create dashboard/.Renviron with KOBO_TOKEN=your_token
# Option 2: export KOBO_TOKEN=your_token
```

Rendering requires R with the packages listed in `dashboard/DESCRIPTION`. Install packages manually with `install.packages()` — there is no renv lockfile.

## Deployment

GitHub Actions (`.github/workflows/render-deploy.yml`) handles automated deployment:
- **Daily** at 06:00 UTC (09:00 EAT)
- **On push** to `main` when `dashboard/**` files change
- **Manual** via the "Run workflow" button in GitHub Actions

The workflow pulls data from KoboToolbox using the `KOBO_TOKEN` GitHub Secret, renders the Quarto dashboard, and deploys to GitHub Pages. The Pages source must be set to "GitHub Actions" in repo settings.

## Architecture

### Data Pipeline

1. `jc_connect_kobo()` reads `KOBO_TOKEN` from environment, connects to KoboToolbox API, and pulls all transaction data
2. `jc_clean_data()` strips haven labels from KoboToolbox data, renames columns (`amount_manual` -> `Amount_UGX`, `batch` -> `Batch_Number`), and filters out capex/batch 3
3. `jc_batch_summary()` computes per-batch aggregates (revenue, cost, profit, mortality, FCR, etc.) used by SPC charts and summary tables
4. Historical P&L for Batches 1-3 comes from `data/JC Poultry Farm - Business Operating Model_v1_250323.xlsx`

### Key Files

- **`index.qmd`** — Unified 5-page dashboard (`format: dashboard`):
  - **Overview**: Farm-level KPI cards with conditional coloring, latest batch detail, two inline SPC charts
  - **Batch Details**: Dynamically generated tabs for every batch via `bslib::navset_card_tab()` — no hardcoding
  - **SPC Analysis**: 5 interactive plotly SPC control charts + summary tables with conditional coloring
  - **P&L Statement**: Full profit/loss across all batches (Excel for 1-3, API for 4+)
  - **Data**: Filterable transaction data table

- **`R-scripts/00-functions.R`** — All helper functions:
  - `jc_connect_kobo()` — KoboToolbox API connection
  - `jc_clean_data(df_raw)` — data cleaning/renaming
  - `jc_dashboard_kpis(data, batch_num)` — per-batch KPIs (bird count derived from data, not hardcoded)
  - `jc_batch_summary(data)` — cross-batch summary for SPC
  - `jc_spc_plotly(df, metric, ylab, title)` — interactive SPC charts with plotly
  - `jc_kpi_theme(value, good, warn)` — conditional color theming for value boxes
  - `jc_pnl_by_batch(data, batch_num)` — P&L category breakdown per batch
  - `jc_sparkline(values, color)` — mini sparkline charts

- **`_quarto.yml`** — Project config (output to `_site/`)
- **`DESCRIPTION`** — R package dependencies (used by GitHub Actions for caching)
- **`style.css`** — Custom styles for conditional coloring, badges, value boxes

### Data Model

Transaction records from KoboToolbox have these key fields:
- `account_type`: "revenues", "cogs", "opex" (capex is filtered out)
- `category_cogs` / `category_revenues` / `category_opex`: subcategory codes like `cogs_feeds`, `cogs_chicks_purchased`, `revenues_sales`, `opex_salaries_wages`
- `batch`: batch number (string)
- `amount_manual`: transaction amount in UGX
- `quantity`: bird count (used for mortality and cost-per-bird calculations)

After `jc_clean_data()`: `amount_manual` -> `Amount_UGX`, `batch` -> `Batch_Number`.

### Known Gotchas

- **Haven labels**: KoboToolbox data via `robotoolbox` returns `haven_labelled` columns. `jc_clean_data()` strips these for the main pipeline. `jc_pnl_by_batch()` also strips them internally since it receives raw `df_raw` data. Always use `as.character()` before `as.integer()` on batch numbers.
- **Quarto `!expr` in chunk options**: Values must be YAML-quoted when they contain curly braces, e.g. `#| title: !expr 'glue("Batch {x}")'`
- **`data_color()` in gt**: Color functions must handle `NA` — return a fallback color like `"#6c757d"` for missing values.

## Conventions

- Currency is Ugandan Shillings (UGX), formatted with `format(x, big.mark = ",")`
- Batch numbers are strings in the data pipeline; converted to integer for sorting/plotting in `jc_batch_summary()`
- R code uses tidyverse style with pipe operators (`|>` and `%>%` both used)
- KPI thresholds: Profit Margin green >= 20%, amber >= 10%, red < 10%; Mortality green <= 4%, amber <= 7%, red > 7%
- SPC charts use Shewhart method: mean +/- 3 standard deviations for control limits
- `R-scripts/01-data-acquisition.R` and `R-scripts/02-nice-shot-charts.R` are unrelated NBA analytics scripts, not part of the farm dashboard
