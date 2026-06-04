const express = require('express');
const cors = require('cors');
const sqlite3 = require('sqlite3').verbose();
const path = require('path');

const app = express();
const PORT = process.env.PORT || 3000;
const DB_PATH = path.join(__dirname, 'insurance.db');

const ALLOWED_ORIGINS = new Set([
  'http://localhost:3000',
  'http://localhost:8000',
  'http://127.0.0.1:5500',
  'https://anthonywinney.com',
  'https://www.anthonywinney.com',
]);

app.use(cors({
  origin: (origin, callback) => {
    if (!origin || ALLOWED_ORIGINS.has(origin)) {
      callback(null, true);
    } else {
      callback(new Error(`CORS blocked: ${origin}`));
    }
  },
}));
app.use(express.json());

const db = new sqlite3.Database(DB_PATH, err => {
  if (err) {
    console.error('Failed to connect to database:', err.message);
    process.exit(1);
  }
  console.log(`Connected to: ${DB_PATH}`);
});

// ---------------------------------------------------------------------------
// Metric definitions
// ---------------------------------------------------------------------------

const METRIC_META = {
  // Financial / insurance
  WRTN_PREM_AMT:        { displayName: 'Written Premium',                   unit: 'currency', aggregation: 'SUM', exclude99999: false },
  PRD_ERND_PREM_AMT:    { displayName: 'Earned Premium',                    unit: 'currency', aggregation: 'SUM', exclude99999: false },
  PRD_INCRD_LOSSES_AMT: { displayName: 'Incurred Losses',                   unit: 'currency', aggregation: 'SUM', exclude99999: false },
  NB_WRTN_PREM_AMT:     { displayName: 'New Business Written Premium',      unit: 'currency', aggregation: 'SUM', exclude99999: false },
  PREV_WRTN_PREM_AMT:   { displayName: 'Previous Written Premium',          unit: 'currency', aggregation: 'SUM', exclude99999: false },
  RETENTION_POLY_QTY:   { displayName: 'Retained Policy Count',             unit: 'count',    aggregation: 'SUM', exclude99999: false },
  POLY_INFORCE_QTY:     { displayName: 'Policies In Force',                 unit: 'count',    aggregation: 'SUM', exclude99999: false },
  PREV_POLY_INFORCE_QTY:{ displayName: 'Previous Policies In Force',        unit: 'count',    aggregation: 'SUM', exclude99999: false },
  RETENTION_RATIO:      { displayName: 'Retention Ratio',                   unit: 'ratio',    aggregation: 'AVG', exclude99999: true  },
  LOSS_RATIO:           { displayName: 'Loss Ratio',                        unit: 'ratio',    aggregation: 'CALCULATED', formula: 'SUM(PRD_INCRD_LOSSES_AMT) / SUM(PRD_ERND_PREM_AMT)' },
  LOSS_RATIO_3YR:       { displayName: '3-Year Loss Ratio',                 unit: 'ratio',    aggregation: 'AVG', exclude99999: true  },
  GROWTH_RATE_3YR:      { displayName: '3-Year Growth Rate',                unit: 'ratio',    aggregation: 'AVG', exclude99999: true  },
  ACTIVE_PRODUCERS:     { displayName: 'Active Producers',                  unit: 'count',    aggregation: 'SUM', exclude99999: false },
  // Quote / bind activity — displayName generated from key
  CL_BOUND_CT_MDS:        { unit: 'count', aggregation: 'SUM', exclude99999: false },
  CL_QUO_CT_MDS:          { unit: 'count', aggregation: 'SUM', exclude99999: false },
  CL_BOUND_CT_SBZ:        { unit: 'count', aggregation: 'SUM', exclude99999: false },
  CL_QUO_CT_SBZ:          { unit: 'count', aggregation: 'SUM', exclude99999: false },
  CL_BOUND_CT_eQT:        { unit: 'count', aggregation: 'SUM', exclude99999: false },
  CL_QUO_CT_eQT:          { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_BOUND_CT_ELINKS:     { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_QUO_CT_ELINKS:       { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_BOUND_CT_PLRANK:     { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_QUO_CT_PLRANK:       { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_BOUND_CT_eQTte:      { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_QUO_CT_eQTte:        { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_BOUND_CT_APPLIED:    { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_QUO_CT_APPLIED:      { unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_BOUND_CT_TRANSACTNOW:{ unit: 'count', aggregation: 'SUM', exclude99999: false },
  PL_QUO_CT_TRANSACTNOW:  { unit: 'count', aggregation: 'SUM', exclude99999: false },
};

function toTitleCase(str) {
  return str
    .replace(/_/g, ' ')
    .replace(/\w\S*/g, w => w.charAt(0).toUpperCase() + w.slice(1).toLowerCase());
}

function getDisplayName(metric) {
  return METRIC_META[metric].displayName || toTitleCase(metric);
}

// ---------------------------------------------------------------------------
// Filter helpers
// ---------------------------------------------------------------------------

const FILTER_COLUMN_MAP = {
  state:           'STATE_ABBR',
  prodLine:        'PROD_LINE',
  prodAbbr:        'PROD_ABBR',
  agencyId:        'AGENCY_ID',
  primaryAgencyId: 'PRIMARY_AGENCY_ID',
  vendor:          'VENDOR',
};

function buildWhereClause(filters) {
  const conditions = [];
  const params = [];
  for (const [key, col] of Object.entries(FILTER_COLUMN_MAP)) {
    const val = filters[key];
    if (val && val !== 'All') {
      conditions.push(`${col} = ?`);
      params.push(val);
    }
  }
  return { conditions, params };
}

// ---------------------------------------------------------------------------
// Stats
// ---------------------------------------------------------------------------

function computeStats(data, unit) {
  const empty = {
    countYears: 0, firstYear: null, lastYear: null,
    mean: null, min: null, max: null,
    firstValue: null, latestValue: null,
    cagr: null, trendSlope: null,
  };

  if (!data || data.length === 0) return empty;

  const sorted = [...data].sort((a, b) => a.year - b.year);
  const valid = sorted.filter(d => d.value !== null && !isNaN(d.value));
  if (valid.length === 0) return empty;

  const values = valid.map(d => d.value);
  const firstYear = sorted[0].year;
  const lastYear = sorted[sorted.length - 1].year;
  const firstValue = sorted[0].value;
  const latestValue = sorted[sorted.length - 1].value;

  const mean = values.reduce((s, v) => s + v, 0) / values.length;
  const min = Math.min(...values);
  const max = Math.max(...values);

  let cagr = null;
  if (unit !== 'ratio' && firstValue > 0 && latestValue > 0 && lastYear !== firstYear) {
    cagr = Math.pow(latestValue / firstValue, 1 / (lastYear - firstYear)) - 1;
  }

  let trendSlope = null;
  if (valid.length >= 2) {
    const n = valid.length;
    const sumX  = valid.reduce((s, d) => s + d.year, 0);
    const sumY  = valid.reduce((s, d) => s + d.value, 0);
    const sumXY = valid.reduce((s, d) => s + d.year * d.value, 0);
    const sumX2 = valid.reduce((s, d) => s + d.year * d.year, 0);
    const denom = n * sumX2 - sumX * sumX;
    if (denom !== 0) {
      trendSlope = (n * sumXY - sumX * sumY) / denom;
    }
  }

  return {
    countYears: sorted.length,
    firstYear, lastYear,
    mean, min, max,
    firstValue, latestValue,
    cagr, trendSlope,
  };
}

// ---------------------------------------------------------------------------
// DB helpers
// ---------------------------------------------------------------------------

function dbGet(sql, params) {
  return new Promise((resolve, reject) => {
    db.get(sql, params, (err, row) => err ? reject(err) : resolve(row));
  });
}

function dbAll(sql, params) {
  return new Promise((resolve, reject) => {
    db.all(sql, params, (err, rows) => err ? reject(err) : resolve(rows));
  });
}

// ---------------------------------------------------------------------------
// Routes
// ---------------------------------------------------------------------------

app.get('/api/health', (_req, res) => {
  res.json({ status: 'ok' });
});

const NUMERIC_SORT_COLS = new Set(['AGENCY_ID', 'PRIMARY_AGENCY_ID']);

app.get('/api/options', async (_req, res) => {
  const columns = ['STATE_ABBR', 'PROD_LINE', 'PROD_ABBR', 'AGENCY_ID', 'PRIMARY_AGENCY_ID', 'VENDOR'];
  try {
    const queries = columns.map(col => {
      const orderBy = NUMERIC_SORT_COLS.has(col) ? `CAST(${col} AS REAL)` : col;
      return dbAll(
        `SELECT DISTINCT ${col} AS value FROM agency_performance WHERE ${col} IS NOT NULL ORDER BY ${orderBy} ASC`,
        []
      ).then(rows => ({ [col]: ['All', ...rows.map(r => r.value)] }));
    });
    const results = await Promise.all(queries);
    res.json(Object.assign({}, ...results));
  } catch (err) {
    console.error('[/api/options] DB error:', err.message);
    res.status(500).json({ error: 'Database error fetching options' });
  }
});

app.get('/api/agency-summary', async (req, res) => {
  const { state, prodLine, prodAbbr, agencyId, primaryAgencyId, vendor, metrics: metricsParam } = req.query;

  if (!metricsParam || metricsParam.trim() === '') {
    return res.status(400).json({
      error: 'Missing required query parameter: metrics. Provide a comma-separated list of metric names.',
    });
  }

  const requestedMetrics = metricsParam.split(',').map(m => m.trim()).filter(Boolean);
  const invalid = requestedMetrics.filter(m => !METRIC_META[m]);
  if (invalid.length > 0) {
    return res.status(400).json({
      error: `Invalid metric(s): ${invalid.join(', ')}. Valid metrics are: ${Object.keys(METRIC_META).join(', ')}`,
    });
  }

  const filters = {
    state:           state           || 'All',
    prodLine:        prodLine        || 'All',
    prodAbbr:        prodAbbr        || 'All',
    agencyId:        agencyId        || 'All',
    primaryAgencyId: primaryAgencyId || 'All',
    vendor:          vendor          || 'All',
  };

  const { conditions, params } = buildWhereClause(filters);
  const baseWhere = conditions.length > 0 ? `WHERE ${conditions.join(' AND ')}` : '';

  let matchingRecordCount;
  try {
    const row = await dbGet(`SELECT COUNT(*) AS count FROM agency_performance ${baseWhere}`, params);
    matchingRecordCount = row.count;
  } catch (err) {
    console.error('[/api/agency-summary] count error:', err.message);
    return res.status(500).json({ error: 'Database error fetching record count' });
  }

  const results = [];
  for (const metric of requestedMetrics) {
    const { aggregation, unit, exclude99999 } = METRIC_META[metric];
    const displayName = getDisplayName(metric);

    const metricConditions = [...conditions];
    const metricParams = [...params];
    let sqlTemplate;
    let validCountSQL;

    if (metric === 'LOSS_RATIO') {
      const whereClause = metricConditions.length > 0 ? `WHERE ${metricConditions.join(' AND ')}` : '';
      sqlTemplate =
        `SELECT STAT_PROFILE_DATE_YEAR AS year, ` +
        `SUM(PRD_INCRD_LOSSES_AMT) / NULLIF(SUM(PRD_ERND_PREM_AMT), 0) AS value ` +
        `FROM agency_performance ${whereClause} ` +
        `GROUP BY STAT_PROFILE_DATE_YEAR ORDER BY STAT_PROFILE_DATE_YEAR`;

      const validConditions = [
        ...metricConditions,
        'PRD_INCRD_LOSSES_AMT IS NOT NULL',
        'PRD_ERND_PREM_AMT IS NOT NULL',
        'PRD_ERND_PREM_AMT != 0',
      ];
      validCountSQL = `SELECT COUNT(*) AS count FROM agency_performance WHERE ${validConditions.join(' AND ')}`;
    } else {
      if (exclude99999) {
        metricConditions.push(`${metric} NOT IN (99997, 99998, 99999)`);
      }
      const metricWhere = metricConditions.length > 0 ? `WHERE ${metricConditions.join(' AND ')}` : '';
      sqlTemplate =
        `SELECT STAT_PROFILE_DATE_YEAR AS year, ${aggregation}(${metric}) AS value ` +
        `FROM agency_performance ${metricWhere} ` +
        `GROUP BY STAT_PROFILE_DATE_YEAR ORDER BY STAT_PROFILE_DATE_YEAR`;

      const validCountConditions = [...metricConditions, `${metric} IS NOT NULL`];
      validCountSQL = `SELECT COUNT(*) AS count FROM agency_performance WHERE ${validCountConditions.join(' AND ')}`;
    }

    let validRecordCount;
    try {
      const countRow = await dbGet(validCountSQL, metricParams);
      validRecordCount = countRow.count;
    } catch (err) {
      console.error(`[/api/agency-summary] valid count error for ${metric}:`, err.message);
      return res.status(500).json({ error: `Database error counting valid records for: ${metric}` });
    }

    let data;
    try {
      data = await dbAll(sqlTemplate, metricParams);
    } catch (err) {
      console.error(`[/api/agency-summary] query error for ${metric}:`, err.message);
      return res.status(500).json({ error: `Database error querying metric: ${metric}` });
    }

    const stats = computeStats(data, unit);

    results.push({ metric, displayName, unit, aggregation, validRecordCount, sqlTemplate, sqlParams: metricParams, data, stats });
  }

  console.log(`[/api/agency-summary] filters=${JSON.stringify(filters)} metrics=[${requestedMetrics.join(',')}] matchingRecordCount=${matchingRecordCount}`);

  res.json({ filters, matchingRecordCount, results });
});

// ---------------------------------------------------------------------------
// Start
// ---------------------------------------------------------------------------

app.listen(PORT, () => {
  console.log(`Server running on http://localhost:${PORT}`);
});
