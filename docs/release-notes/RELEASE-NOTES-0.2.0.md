# Happening 0.2.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds embedded GeoIP data for country detection, dashboard improvements, and various bug fixes.

## New Features

### Embedded GeoIP Data
- Country detection now works out of the box with no external dependencies
- IP-to-country data from [ipverse/country-ip-blocks](https://github.com/ipverse/country-ip-blocks) is compiled into the binary
- External data directory still supported for fresher data (takes priority over embedded)

### Privacy Policy Template
- Added `/privacy` endpoint with a customizable privacy policy page

### Dashboard Improvements
- Improved display formatting and layout
- Tracking parameters (utm_*, fbclid, gclid, etc.) are now stripped from URLs for cleaner analytics

## Bug Fixes

- Fixed CSRF token validation on login form
- Base URL is now persisted in database (survives restarts without `-u` flag)

## CI/CD

- Added GitHub Actions workflows for CI and releases
- Automated binary builds for releases

## Documentation

- Fixed incorrect environment variable documentation: use `ACME_STAGING=true` for staging certificates (production is the default)

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ACME_EMAIL` | Email for Let's Encrypt notifications |
| `ACME_STAGING` | Set to "true" for staging certificates (default: production) |
| `ACME_CERT_PATH` | Custom certificate storage path |
| `ACME_RENEWAL_DAYS` | Days before expiry to trigger renewal (default: 30) |

## Upgrade Notes

This is a drop-in replacement for 0.1.0. No database migrations required.
