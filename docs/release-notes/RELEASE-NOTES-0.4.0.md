# Happening 0.4.0 Release Notes

**Release Date:** April 2026

## Summary

This release upgrades pure-tls with important security fixes and migrates to the tuition v2 TUI API.

## Security

### pure-tls 1.11.1

This release includes pure-tls 1.11.0 and 1.11.1, which address multiple security issues:

- **Trust-anchor verification** — Certificate chain validation now requires cryptographic signature verification, not just issuer-name equality, preventing forged intermediates from satisfying the anchor check.
- **TLS 1.3 downgrade detection** (CL-SEC-2026-0112, HIGH) — Detects and rejects downgrade sentinel bytes in ServerHello per RFC 8446 Section 4.1.3.
- **Session ID echo validation** (CL-SEC-2026-0113, MEDIUM) — Validates legacy_session_id_echo in ServerHello matches the value sent in ClientHello.
- **CSPRNG for ticket age** (CL-SEC-2026-0114, MEDIUM) — Replaced CL:RANDOM with ironclad CSPRNG for ticket_age_add generation, preventing ticket age correlation attacks.
- **Wildcard hostname validation** — Rejects known multi-label public suffixes (e.g., *.co.uk) that were previously accepted.
- **Constant-time comparison fixes** — Fixed ct-equal-mask false equality for certain length differences; constant-time comparison in PKCS#1 v1.5 signature verification and ML-KEM-768 decapsulation.
- **Post-handshake message handling** — Correct handling of TLS 1.3 message fragmentation and coalescing across records.
- **PSK session resumption** — Fixed double-parse and selected-identity truthiness bugs in PSK extension handling.

## New Features

### Tuition v2 API Migration

Migrated the setup wizard TUI to tuition v2, which provides a cleaner API for key handling, modifier detection, and view rendering.

## Dependencies

- Updated pure-tls to 1.11.1 (20260418)
- Updated tuition to v2 API (20260402)
- Updated clingon to 20260331
- Updated cl-selfupdate to 20260412
- Updated serapeum to 20260213

## Breaking Changes

None.

## Upgrade Notes

Drop-in replacement for 0.3.0. No database migrations required.
