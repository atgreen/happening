# GeoIP Setup

Happening includes embedded IP-to-country data from [ipverse/country-ip-blocks](https://github.com/ipverse/country-ip-blocks), so country lookup works out of the box.

## Default Behavior

When Happening starts, it will:
1. Check if an external ipverse repository exists at `data/country-ip-blocks/`
2. If found, use the external data (more up-to-date)
3. Otherwise, use the embedded data (included in the binary)

You should see a log message like:
```
Loaded 174665 IPv4 + 67683 IPv6 ranges for 237 countries (3.6 MB) [embedded]
```

Or with external data:
```
Loaded 174665 IPv4 + 67683 IPv6 ranges for 237 countries (3.6 MB) [external]
```

## Updating Country Data

The embedded data is from when Happening was built. To get the latest IP allocations:

### Option 1: Use External Repository (Recommended for Production)

Clone the ipverse repository for up-to-date data:

```bash
cd /path/to/happening
git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks
```

To update periodically:
```bash
cd data/country-ip-blocks
git pull
```

Then restart Happening. You can automate this with a cron job:
```bash
# Weekly update (Sundays at 3am)
0 3 * * 0 cd /path/to/happening/data/country-ip-blocks && git pull
```

### Option 2: Rebuild with Updated Embedded Data

If you're building from source and want to update the embedded data:

```bash
# Clone/update ipverse data
git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks

# Generate new embedded data
make generate-geoip

# Rebuild Happening
make clean && make
```

## Privacy Notes

- IP addresses are **never stored** - they're only used momentarily to look up the country
- All lookups happen locally - no external API calls
- Country data is stored as 2-letter ISO codes (e.g., "US", "GB", "DE")
- Private/internal IPs (localhost, 10.x.x.x, 192.168.x.x) won't match any country

## How It Works

On startup, Happening:
1. Checks if `data/country-ip-blocks/country/` exists (external data)
2. If not, uses embedded data compiled into the binary
3. Builds sorted in-memory indices for fast binary search lookups

Memory usage with packed arrays:
- IPv4: ~9 bytes per range (~1.6 MB for ~175,000 ranges)
- IPv6: ~33 bytes per range (~2.2 MB for ~68,000 ranges)
- Total: ~3.6 MB typical

## Troubleshooting

**"GeoIP data not available" message**

The embedded data wasn't included at build time. Rebuild with:
```bash
git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks
make generate-geoip
make clean && make
```

**Countries showing as "Unknown"**

This happens for:
- Events recorded before GeoIP data was loaded
- Private/internal IP addresses (localhost, 10.x.x.x, 192.168.x.x, fc00::/7)
- IPs not covered by any country's allocation

**Want the latest data without rebuilding?**

Just clone the external repository - it takes priority over embedded data:
```bash
git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks
```
