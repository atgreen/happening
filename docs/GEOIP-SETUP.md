# GeoIP Setup

Happening can show visitor countries using the [ipverse/country-ip-blocks](https://github.com/ipverse/country-ip-blocks) database. This is optional - if the data isn't present, country information simply won't be collected.

## Setup

Clone the country-ip-blocks repository into Happening's data directory:

```bash
cd /path/to/happening
git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks
```

Restart Happening. You should see a log message like:
```
Loaded 180000 IP ranges from 249 countries
```

## Updating

The ipverse database is updated regularly. To get the latest data:

```bash
cd data/country-ip-blocks
git pull
```

Then restart Happening to reload the data.

You can automate this with a cron job:
```bash
# Weekly update (Sundays at 3am)
0 3 * * 0 cd /path/to/happening/data/country-ip-blocks && git pull
```

Note: Happening loads the data at startup, so you'll need to restart after pulling updates.

## Privacy Notes

- IP addresses are **never stored** - they're only used momentarily to look up the country
- All lookups happen locally - no external API calls
- Country data is stored as 2-letter ISO codes (e.g., "US", "GB", "DE")
- Private/internal IPs (localhost, 10.x.x.x, 192.168.x.x) won't match any country

## How It Works

On startup, Happening:
1. Checks if `data/country-ip-blocks/country/` exists
2. Reads all `ipv4-aggregated.txt` and `ipv6-aggregated.txt` files
3. Builds sorted in-memory indices for fast binary search lookups

Memory usage with packed arrays:
- IPv4: ~9 bytes per range (~1.6 MB for ~180,000 ranges)
- IPv6: ~33 bytes per range (~3 MB for ~90,000 ranges)
- Total: ~5 MB typical

## Troubleshooting

**"GeoIP data not found" message**

The repository wasn't found. Check:
- Directory exists at `./data/country-ip-blocks/`
- The `country/` subdirectory exists with country folders inside

**Countries showing as "Unknown"**

This happens for:
- Events recorded before GeoIP data was loaded
- Private/internal IP addresses (localhost, 10.x.x.x, 192.168.x.x, fc00::/7)
- IPs not covered by any country's allocation
