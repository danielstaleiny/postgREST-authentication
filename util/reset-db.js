const { config } = require('dotenv')
const { spawnSync } = require('child_process')
config()

const COMPOSE_PROJECT_NAME =
  process.env.COMPOSE_PROJECT_NAME || 'postgrest_starter-kit'
const POSTGRES_USER = process.env.POSTGRES_USER
const POSTGRES_PASSWORD = process.env.POSTGRES_PASSWORD
const SUPER_USER = process.env.SUPER_USER
const SUPER_USER_PASSWORD = process.env.SUPER_USER_PASSWORD

const DB_HOST = process.env.DB_HOST
const DB_NAME = process.env.DB_NAME
const PG = `${COMPOSE_PROJECT_NAME}_db_1`

const psql_version = spawnSync('psql', ['--version'])
const have_psql =
  psql_version.stdout && psql_version.stdout.toString('utf8').trim().length > 0

module.exports = function resetdb() {
  let pg
  if (have_psql) {
    var env = Object.create(process.env)
    env.PGPASSWORD = SUPER_USER_PASSWORD
    pg = spawnSync(
      'psql',
      [
        '-h',
        'localhost',
        '-U',
        SUPER_USER,
        DB_NAME,
        '-f',
        process.env.PWD + '/db/src/sample_data/reset.sql',
      ],
      { env: env }
    )
  } else {
    pg = spawnSync('docker', [
      'exec',
      PG,
      'psql',
      '-U',
      SUPER_USER,
      DB_NAME,
      '-f',
      'docker-entrypoint-initdb.d/sample_data/reset.sql',
    ])
  }
  if (pg.status !== 0) {
    throw new Error(
      `Could not reset database in rest tests. Error = ${pg.stderr.toString()}`
    )
  }
}
