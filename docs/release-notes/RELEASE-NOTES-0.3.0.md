# Happening 0.3.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds self-update support, favicon support for the dashboard, and various improvements.

## New Features

### Self-Update Support
- New `happening update` command to check for and apply updates from GitHub
- Use `happening update --check` to check for updates without applying
- Automatic platform detection for downloading the correct binary
- Atomic replacement with rollback on failure

### Favicon Support
- Added favicon.ico and favicon.svg to the dashboard
- Browser tabs now display the Happening icon
- Favicons are embedded in the binary and served with appropriate caching headers

## Bug Fixes

- Fixed duplicate "v" in version display (was showing "vv0.2.0" instead of "v0.3.0")

## Dependencies

- Added cl-selfupdate for self-update functionality
- Updated drakma from 2.0.9 to 2.0.10
- Updated pure-tls to latest version
- Updated serapeum to latest version
- Use pure-tls/cl+ssl-compat instead of OpenSSL-based cl+ssl

## CI/CD

- Improved release workflow to properly install ocicl during builds
- Build environment now automatically sets up ocicl from latest release

## Upgrade Notes

This is a drop-in replacement for 0.2.0. No database migrations required.

After upgrading, you can use `happening update` for future updates.
