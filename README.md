# Insurance Modeling Backend

Express/SQLite backend for analyzing the Kaggle Agency Performance dataset.

## Dataset

`data/dataset_a.csv` — 213,328 rows, 49 columns, annual data from 2005–2015.  
Each row is an **Agency × Product × Product Line × State × Year** observation.  
Imported into SQLite as the `agency_performance` table in `insurance.db`.

---

## Setup

### 1. Install dependencies

```bash
npm install
```

### 2. Import the CSV into SQLite

```bash
npm run import
```

This creates `insurance.db`, drops any existing `agency_performance` table, and imports all rows. Progress is printed every 10,000 rows.

### 3. Start the server

```bash
npm start
```

Or with auto-reload during development:

```bash
npm run dev
```

Server runs on `http://localhost:3000` by default. Set `PORT` env var to override.

---

## Endpoints

### `GET /api/health`

```
http://localhost:3000/api/health
```

Returns `{ "status": "ok" }`.

---

### `GET /api/options`

```
http://localhost:3000/api/options
```

Returns distinct dropdown values for `STATE_ABBR`, `PROD_LINE`, `PROD_ABBR`, `AGENCY_ID`, `PRIMARY_AGENCY_ID`, and `VENDOR`. Each list is sorted ascending with `"All"` prepended.

---

### `GET /api/agency-summary`

Query params:

| Param             | Description                                        |
|-------------------|----------------------------------------------------|
| `metrics`         | **Required.** Comma-separated list of metric names |
| `state`           | Filter by state abbreviation (e.g. `OH`)           |
| `prodLine`        | Filter by product line (e.g. `PL`)                 |
| `prodAbbr`        | Filter by product abbreviation                     |
| `agencyId`        | Filter by agency ID                                |
| `primaryAgencyId` | Filter by primary agency ID                        |
| `vendor`          | Filter by vendor name                              |

Omit a filter param or pass `All` to include all values.

Returns yearly aggregated data, summary statistics, the SQL template executed, and SQL params for each requested metric.

**Response shape (abbreviated):**

```json
{
  "filters": { "state": "OH", "prodLine": "PL", ... },
  "matchingRecordCount": 4827,
  "results": [
    {
      "metric": "WRTN_PREM_AMT",
      "displayName": "Written Premium",
      "unit": "currency",
      "aggregation": "SUM",
      "validRecordCount": 4827,
      "sqlTemplate": "SELECT STAT_PROFILE_DATE_YEAR AS year, SUM(WRTN_PREM_AMT) AS value ...",
      "sqlParams": ["OH", "PL"],
      "data": [{ "year": 2005, "value": 12345.67 }, ...],
      "stats": { "countYears": 11, "firstYear": 2005, "lastYear": 2015, "mean": ..., "cagr": ..., ... }
    }
  ]
}
```

- `matchingRecordCount` — total rows matching the active filters.
- `validRecordCount` — per-metric count of rows usable for that metric's calculation (see metric notes below).

**Metric calculation notes:**

- **`LOSS_RATIO`** is a calculated metric: `SUM(PRD_INCRD_LOSSES_AMT) / SUM(PRD_ERND_PREM_AMT)` aggregated per year. This avoids distortion from the raw `LOSS_RATIO` column, which contains sentinel values and row-level outliers. `validRecordCount` counts rows where both component columns are non-null and earned premium is non-zero.
- **`RETENTION_RATIO`, `LOSS_RATIO_3YR`, `GROWTH_RATE_3YR`** are averaged from the dataset's raw ratio columns after excluding sentinel values `99997`, `99998`, and `99999`.

#### Example — single metric

```
http://localhost:3000/api/agency-summary?state=OH&metrics=WRTN_PREM_AMT
```

#### Example — multiple metrics with filters

```
http://localhost:3000/api/agency-summary?state=OH&prodLine=PL&metrics=WRTN_PREM_AMT,LOSS_RATIO,RETENTION_RATIO
```

#### Available metrics

**Financial / insurance**

`WRTN_PREM_AMT`, `PRD_ERND_PREM_AMT`, `PRD_INCRD_LOSSES_AMT`, `NB_WRTN_PREM_AMT`, `PREV_WRTN_PREM_AMT`, `RETENTION_POLY_QTY`, `POLY_INFORCE_QTY`, `PREV_POLY_INFORCE_QTY`, `RETENTION_RATIO`, `LOSS_RATIO`, `LOSS_RATIO_3YR`, `GROWTH_RATE_3YR`, `ACTIVE_PRODUCERS`

**Quote / bind activity**

`CL_BOUND_CT_MDS`, `CL_QUO_CT_MDS`, `CL_BOUND_CT_SBZ`, `CL_QUO_CT_SBZ`, `CL_BOUND_CT_eQT`, `CL_QUO_CT_eQT`, `PL_BOUND_CT_ELINKS`, `PL_QUO_CT_ELINKS`, `PL_BOUND_CT_PLRANK`, `PL_QUO_CT_PLRANK`, `PL_BOUND_CT_eQTte`, `PL_QUO_CT_eQTte`, `PL_BOUND_CT_APPLIED`, `PL_QUO_CT_APPLIED`, `PL_BOUND_CT_TRANSACTNOW`, `PL_QUO_CT_TRANSACTNOW`
