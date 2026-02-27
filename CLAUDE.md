# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

`expensifyR` is an R package containing a Shiny app for personal finance management. It helps users (especially expats) import bank statements from multiple banks/currencies, auto-categorise transactions using a nearest-neighbour classifier, and visualise spending and balances.

The app is hosted at https://pxzy9z-akashastrub.shinyapps.io/expensifyr/.

## Commands

All commands should be run from an R session with the package loaded.

```r
# Install dependencies
devtools::install_deps()

# Load package during development
devtools::load_all()

# Run the Shiny app
expensifyR::run_app()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-import_natwest.R")

# Regenerate documentation (NAMESPACE + man/ files)
devtools::document()

# Check the package
devtools::check()
```

## Architecture

### Data Flow

1. **Import**: Raw bank CSVs are processed by bank-specific `import_*()` functions (`R/import_*.R`), each outputting a **common data model (CDM)** — a dataframe with columns: `date`, `description`, `amount_chf`, `amount_dkk`, `amount_eur`, `amount_usd`, `amount_gbp`, `bank`. Non-applicable currency columns are set to `NA`.

2. **Currency conversion**: `convert_amount()` (`R/currency_converter.R`) fetches historical exchange rates via the `priceR` package and converts a source currency column to the target currency column.

3. **Classification**: `classify_subcategories()` (`R/classify_subcategories.R`) uses a Jaccard similarity (k=3 character shingles) nearest-neighbour approach against the existing master file to predict subcategories for new transactions.

4. **Master file**: The master CSV accumulates all historical transactions. Columns include the CDM fields plus `subcategory`, `category`, and `direction` (added via a join to the category dictionary CSV).

5. **Visualisation**: `plot_waterfall()` and `plot_balances()` produce `plotly` charts. `transform_master_for_waterfall()` prepares the master data for the waterfall.

### Filename Convention (Critical)

The Shiny app dispatches to the correct import function by parsing the **uploaded filename**. Files must follow this naming scheme:

- `{bank}_{currency}_{anything}.csv` — e.g. `natwest_gbp_jan2024.csv`, `revolut_eur_jan2024.csv`
- For two-word banks: `{bank1}_{bank2}_{currency}_{anything}.csv` — e.g. `boa_credit_usd_jan2024.csv`, `boa_debit_usd_jan2024.csv`, `capitalone_debit_usd_jan2024.csv`

The app uses `stringr::str_split(filename, "_")` to extract `bank` and `currency`, then calls `expensifyR::import_{bank}(path, currency)` dynamically via `eval(parse(...))`.

### Supported Banks

| Function | Bank label in master | Currency |
|---|---|---|
| `import_natwest` | NatWest | GBP |
| `import_revolut` | Revolut {CURRENCY} | Any |
| `import_danske` | DanskeBank | DKK |
| `import_santander` | Santander | — |
| `import_boa_debit` | BoA Debit | USD |
| `import_boa_credit` | BoA Credit | USD |
| `import_capitalone_debit` | CapitalOne Debit | USD |
| `import_chase` | Chase Credit | USD |

### Adding a New Bank

1. Create `R/import_{bank}.R` with a function that takes `(path, currency)` and returns a CDM dataframe.
2. Add `@export` roxygen tag and run `devtools::document()` to update `NAMESPACE`.
3. Ensure the filename convention above is followed so the app dispatcher picks it up.

### Key Files

- `R/app.R` — entire Shiny UI + server in `run_app()`; handles personal vs. shared expenses, anti-join deduplication against old master, category dict dropdown
- `R/classify_subcategories.R` — Jaccard/shingle NLP classifier; requires subcategories with >2 observations in master to be used for training
- `R/currency_converter.R` — wraps `priceR::historical_exchange_rates()`
- `data/` — demo CSV files for master, category dict, and bank statements

### Tests

Tests live in `tests/testthat/`. Fixtures (small test CSVs) go in `tests/testthat/fixtures/`. Currently only `import_natwest` has a test; new import functions should follow the same pattern.
