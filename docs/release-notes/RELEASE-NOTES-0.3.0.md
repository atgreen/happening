# Happening 0.3.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds favicon support for the dashboard and updates dependencies.

## New Features

### Favicon Support
- Added favicon.ico and favicon.svg to the dashboard
- Browser tabs now display the Happening icon
- Favicons are embedded in the binary and served with appropriate caching headers

## Dependencies

- Updated drakma from 2.0.9 to 2.0.10
- Updated pure-tls to latest version
- Updated serapeum to latest version

## CI/CD

- Improved release workflow to properly install ocicl during builds
- Build environment now automatically sets up ocicl from latest release

## Upgrade Notes

This is a drop-in replacement for 0.2.0. No database migrations required.
