# Happening 0.1.0 Release Notes

**Release Date:** January 2026

## Summary

Initial release of Happening, a privacy-focused, self-hosted web analytics platform.

## Features

- **Privacy-first analytics** - No cookies, no personal data collection
- **Self-hosted** - Full control over your data
- **Automatic HTTPS** - Let's Encrypt certificates via ACME TLS-ALPN-01
- **Lightweight tracking script** - Minimal impact on page load
- **Real-time dashboard** - See visitors as they happen
- **Simple setup** - Single binary, interactive or CLI setup

## Dashboard Metrics

- Page views and unique visitors
- Session tracking with bounce rate
- Top pages and referrers
- Device, browser, and OS breakdown
- Country breakdown (embedded [ipverse/country-ip-blocks](https://github.com/ipverse/country-ip-blocks) data, works out of the box)
- Daily traffic charts
- Real-time visitor count

## CLI Options

- `-p, --port PORT` - HTTP server port (default: 8080)
- `-u, --url URL` - Public base URL for tracking snippets
- `-d, --database PATH` - SQLite database location
- `-s, --slynk-port PORT` - Enable Slynk REPL server

### Setup Subcommand

```bash
happening setup -a admin -P password -u https://analytics.example.com -e admin@example.com
```

## Environment Variables

- `ACME_EMAIL` - Email for Let's Encrypt notifications
- `ACME_PRODUCTION` - Set to "true" for production certificates
- `TRUST_PROXY` - Configure trusted proxy IPs for X-Forwarded-For
- `DEBUG` - Set to "true" for verbose error messages

## Installation

```bash
tar xzf happening-0.1.0-linux-amd64.tar.gz
./happening setup -a admin -P yourpassword -u https://your-domain.com
./happening
```

## Security

- CSRF protection on all forms
- Secure session management with CSPRNG tokens
- HTTP to HTTPS automatic redirect
- Client timestamp validation

## Known Limitations

- Linux x86_64 only
- SQLite backend (single-server deployment)
