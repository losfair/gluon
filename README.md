# gluon

WIP. Launch selfhosted apps in one click with mvSQLite and Fly.io.

## Project structure

- `gluon-fe/` - Frontend built on Next.js. Can be deployed independently or on Vercel.
- `gluon-statekeeper/` - Background service that syncs state between Gluon and external systems (Fly.io, mvSQLite Admin API, etc.). Written in Haskell.

