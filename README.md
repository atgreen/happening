# Happening

A privacy-focused, self-hosted web analytics platform written in Common Lisp.

## Features

- **Privacy-first** - No cookies, no personal data collection
- **Self-hosted** - Your data stays on your server
- **Automatic HTTPS** - Built-in Let's Encrypt certificate management via [pure-tls](https://github.com/atgreen/pure-tls)
- **Lightweight** - Single binary, SQLite database, runs on free-tier cloud VMs (AWS, GCP, Azure, etc.)
- **Simple setup** - Interactive wizard or command-line options

## Quick Start

### Building

```sh
make
```

### Setup

Interactive setup wizard:
```sh
./happening setup
```

Or non-interactive:
```sh
./happening setup \
  -a admin \
  -P yourpassword \
  -u https://analytics.example.com \
  -e admin@example.com
```

### Running

```sh
# HTTP only (development)
./happening -p 8080

# HTTPS with automatic Let's Encrypt certificates
./happening -u https://analytics.example.com
```

## Configuration

### Command Line Options

| Option | Description |
|--------|-------------|
| `-p, --port PORT` | HTTP server port (default: 8080) |
| `-u, --url URL` | Public base URL (enables HTTPS if https://) |
| `-d, --database PATH` | SQLite database path |
| `-s, --slynk-port PORT` | Start Slynk server for remote REPL |

### Environment Variables

| Variable | Description |
|----------|-------------|
| `ACME_EMAIL` | Contact email for Let's Encrypt account |
| `ACME_PRODUCTION` | Set to `true` for production certificates (default: staging) |
| `ACME_CERT_PATH` | Custom certificate storage path |
| `ACME_RENEWAL_DAYS` | Days before expiry to trigger renewal (default: 30) |

Environment variables can also be set in a `.env` file.

### HTTPS Configuration

When you specify an `https://` URL with `-u`, Happening automatically:

1. Obtains a Let's Encrypt certificate using TLS-ALPN-01 challenge
2. Serves HTTPS on port 443
3. Renews certificates automatically before expiry

**Requirements:**
- Port 443 must be accessible from the internet
- Domain must resolve to your server's IP address

**Example deployment:**
```sh
# First time setup
./happening setup -a admin -P secret123 \
  -u https://analytics.example.com \
  -e admin@example.com

# Run with production certificates
ACME_EMAIL=admin@example.com \
ACME_PRODUCTION=true \
./happening -u https://analytics.example.com
```

## Tracking Script

After setup, add this script to your website:

```html
<script defer src="https://analytics.example.com/js/script.js"
        data-api="https://analytics.example.com/api/event"></script>
```

## Development

### Prerequisites

- SBCL (Steel Bank Common Lisp)
- Make

### Building from Source

```sh
make clean && make
```

### Running with Slynk

```sh
./happening -p 8080 -s 4005
```

Then connect from Emacs with `M-x sly-connect`.

## License

MIT License

Copyright (c) 2025 Anthony Green
