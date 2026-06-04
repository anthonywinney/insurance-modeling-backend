const fs = require('fs');
const path = require('path');
const csv = require('csv-parser');
const sqlite3 = require('sqlite3').verbose();

const CSV_PATH = path.join(__dirname, 'data', 'dataset_a.csv');
const DB_PATH = path.join(__dirname, 'insurance.db');

const TEXT_COLUMNS = new Set([
  'PROD_ABBR', 'PROD_LINE', 'STATE_ABBR',
  'VENDOR_IND', 'VENDOR',
]);

function openDatabase() {
  return new Promise((resolve, reject) => {
    const db = new sqlite3.Database(DB_PATH, err => {
      if (err) return reject(err);
      console.log(`Connected to: ${DB_PATH}`);
      resolve(db);
    });
  });
}

function closeDatabase(db) {
  return new Promise((resolve, reject) => {
    db.close(err => {
      if (err) return reject(err);
      console.log('Database connection closed.');
      resolve();
    });
  });
}

function runPragmas(db) {
  return new Promise((resolve, reject) => {
    db.exec('PRAGMA journal_mode = WAL; PRAGMA synchronous = NORMAL;', err =>
      err ? reject(err) : resolve()
    );
  });
}

function dropTable(db) {
  return new Promise((resolve, reject) => {
    db.run('DROP TABLE IF EXISTS agency_performance', err =>
      err ? reject(err) : resolve()
    );
  });
}

function getColumns() {
  return new Promise((resolve, reject) => {
    let resolved = false;
    const fileStream = fs.createReadStream(CSV_PATH);
    const parser = csv();

    parser.on('headers', headers => {
      if (!resolved) {
        resolved = true;
        fileStream.destroy();
        resolve(headers);
      }
    });

    fileStream.on('error', err => { if (!resolved) reject(err); });
    parser.on('error', err => { if (!resolved) reject(err); });

    fileStream.pipe(parser);
  });
}

function createTable(db, columns) {
  const defs = columns
    .map(col => `${col} ${TEXT_COLUMNS.has(col) ? 'TEXT' : 'REAL'}`)
    .join(',\n  ');
  const sql = `CREATE TABLE agency_performance (\n  ${defs}\n)`;
  return new Promise((resolve, reject) => {
    db.run(sql, err => err ? reject(err) : resolve());
  });
}

function importRows(db, columns) {
  return new Promise((resolve, reject) => {
    const placeholders = columns.map(() => '?').join(', ');
    const insertSQL = `INSERT INTO agency_performance (${columns.join(', ')}) VALUES (${placeholders})`;

    db.run('BEGIN TRANSACTION', err => {
      if (err) return reject(err);

      const stmt = db.prepare(insertSQL, err => {
        if (err) return reject(err);
      });

      let rowCount = 0;

      const stream = fs.createReadStream(CSV_PATH).pipe(csv());

      stream.on('data', row => {
        const values = columns.map(col => {
          const raw = row[col];
          if (raw === '' || raw === undefined || raw === null) return null;
          if (!TEXT_COLUMNS.has(col)) {
            const num = Number(raw);
            return isNaN(num) ? raw : num;
          }
          return raw;
        });
        stmt.run(values);
        rowCount++;
        if (rowCount % 10000 === 0) {
          console.log(`  ${rowCount.toLocaleString()} rows imported...`);
        }
      });

      stream.on('end', () => {
        stmt.finalize(err => {
          if (err) {
            db.run('ROLLBACK');
            return reject(err);
          }
          db.run('COMMIT', err => {
            if (err) return reject(err);
            console.log(`\nImport complete. Total rows imported: ${rowCount.toLocaleString()}`);
            resolve(rowCount);
          });
        });
      });

      stream.on('error', err => {
        db.run('ROLLBACK');
        reject(err);
      });
    });
  });
}

async function main() {
  const db = await openDatabase();
  await runPragmas(db);

  console.log('Dropping existing agency_performance table...');
  await dropTable(db);

  console.log('Reading CSV columns...');
  const columns = await getColumns();
  console.log(`Found ${columns.length} columns.`);

  console.log('Creating agency_performance table...');
  await createTable(db, columns);

  console.log('Importing rows...');
  await importRows(db, columns);

  await closeDatabase(db);
}

main().catch(err => {
  console.error('Import failed:', err.message);
  process.exit(1);
});
