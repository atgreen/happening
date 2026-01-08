# Product Requirements Document: Happening
## A Self-Hosted Web Analytics Platform

**Version:** 1.0
**Date:** January 2026
**Status:** Draft

---

## 1. Executive Summary

Happening is a lightweight, privacy-focused, self-hosted web analytics platform built in Common Lisp. It provides website owners with essential traffic metrics while respecting user privacy and offering complete data ownership.

### Core Value Proposition
- **Privacy-first**: No cookies required, GDPR-compliant by design
- **Self-hosted**: Full data ownership, no third-party dependencies
- **Zero dependencies**: Embedded SQLite database, single binary deployment
- **Lightweight**: Minimal tracking script (~1KB), fast page loads
- **Simple**: Focus on essential metrics without overwhelming dashboards

---

## 2. Problem Statement

Existing analytics solutions like Google Analytics are:
- Privacy-invasive and require cookie consent banners
- Complex with features most users never need
- Cloud-dependent with data stored on third-party servers
- Heavy, impacting page load performance

Website owners need a simple, privacy-respecting way to understand their traffic without compromising user experience or data sovereignty.

---

## 3. Target Users

### Primary
- **Independent developers** running personal sites, blogs, or side projects
- **Small businesses** wanting analytics without privacy concerns
- **Privacy-conscious organizations** needing GDPR/CCPA compliance

### Secondary
- **Agencies** managing multiple client sites
- **Open source projects** tracking documentation usage

---

## 4. Functional Requirements

### 4.1 Tracking Script

| ID | Requirement | Priority |
|----|-------------|----------|
| TS-01 | Lightweight JavaScript snippet (<1KB gzipped) | Must |
| TS-02 | No cookies or local storage required | Must |
| TS-03 | Collect: page URL, referrer, viewport size, timestamp | Must |
| TS-04 | Collect: user agent (for browser/OS/device detection) | Must |
| TS-05 | Generate anonymous visitor hash (IP + UA, not stored raw) | Must |
| TS-06 | Track page visibility changes for accurate time-on-page | Should |
| TS-07 | Support custom events (optional, disabled by default) | Could |
| TS-08 | Respect Do Not Track (DNT) browser setting | Must |

### 4.2 Data Collection API

| ID | Requirement | Priority |
|----|-------------|----------|
| DC-01 | HTTP endpoint to receive tracking events | Must |
| DC-02 | Validate and sanitize all incoming data | Must |
| DC-03 | Rate limiting per IP to prevent abuse | Must |
| DC-04 | CORS support for cross-origin requests | Must |
| DC-05 | Batch event ingestion for high-traffic sites | Should |
| DC-06 | Async processing to minimize response latency | Should |

### 4.3 Core Metrics

| ID | Metric | Description | Priority |
|----|--------|-------------|----------|
| CM-01 | Page Views | Total page loads | Must |
| CM-02 | Unique Visitors | Distinct visitor hashes per period | Must |
| CM-03 | Sessions | Grouped visits with 30-min timeout | Must |
| CM-04 | Bounce Rate | Single-page sessions / total sessions | Must |
| CM-05 | Session Duration | Average time between first and last event | Must |
| CM-06 | Pages per Session | Average page views per session | Must |
| CM-07 | Top Pages | Most viewed URLs | Must |
| CM-08 | Top Referrers | Traffic sources | Must |
| CM-09 | Geographic Location | Country/region from IP (optional) | Should |
| CM-10 | Device Breakdown | Desktop/mobile/tablet | Must |
| CM-11 | Browser Breakdown | Chrome/Firefox/Safari/etc. | Must |
| CM-12 | OS Breakdown | Windows/macOS/Linux/iOS/Android | Should |

### 4.4 Dashboard

| ID | Requirement | Priority |
|----|-------------|----------|
| DB-01 | Real-time visitor count | Must |
| DB-02 | Configurable date range selector | Must |
| DB-03 | Time series charts for traffic trends | Must |
| DB-04 | Comparison with previous period | Should |
| DB-05 | Filterable by page, referrer, device, etc. | Should |
| DB-06 | Responsive design (mobile-friendly) | Must |
| DB-07 | Dark mode support | Could |
| DB-08 | Export data as CSV/JSON | Should |

### 4.5 Multi-Site Support

| ID | Requirement | Priority |
|----|-------------|----------|
| MS-01 | Support multiple websites per installation | Must |
| MS-02 | Unique site ID for each tracked domain | Must |
| MS-03 | Per-site dashboard views | Must |
| MS-04 | Aggregate view across all sites | Could |

### 4.6 Authentication & Access Control

| ID | Requirement | Priority |
|----|-------------|----------|
| AC-01 | Admin authentication (username/password) | Must |
| AC-02 | Secure session management | Must |
| AC-03 | Password reset functionality | Should |
| AC-04 | Optional: shareable public dashboard links | Could |
| AC-05 | Optional: read-only user accounts | Could |

---

## 5. Non-Functional Requirements

### 5.1 Performance

| ID | Requirement | Target |
|----|-------------|--------|
| PF-01 | Tracking script load time | <50ms |
| PF-02 | Event ingestion latency | <100ms p99 |
| PF-03 | Dashboard page load | <500ms |
| PF-04 | Support concurrent visitors | 10,000+ per site |
| PF-05 | Data retention | Configurable (default: 2 years) |

### 5.2 Security

| ID | Requirement |
|----|-------------|
| SC-01 | HTTPS required for all endpoints |
| SC-02 | Input validation on all user-supplied data |
| SC-03 | SQL injection prevention (parameterized queries) |
| SC-04 | XSS prevention in dashboard |
| SC-05 | CSRF protection on state-changing operations |
| SC-06 | Secure password hashing (Argon2 or bcrypt) |

### 5.3 Privacy

| ID | Requirement |
|----|-------------|
| PR-01 | No personally identifiable information (PII) stored |
| PR-02 | IP addresses hashed immediately, never stored raw |
| PR-03 | No cross-site tracking capabilities |
| PR-04 | Data deletion API for compliance requests |
| PR-05 | Configurable data retention policies |

### 5.4 Deployment

| ID | Requirement |
|----|-------------|
| DP-01 | Single binary distribution (with embedded SQLite) |
| DP-02 | Docker image available |
| DP-03 | Zero external dependencies - fully self-contained |
| DP-04 | Configuration via environment variables or config file |
| DP-05 | Automatic database migrations |
| DP-06 | Database file location configurable (default: ./data/happening.db) |
| DP-07 | SQLite WAL mode enabled by default for concurrent read/write performance |

---

## 6. Technical Architecture

### 6.1 Technology Stack

| Component | Technology |
|-----------|------------|
| **Backend** | Common Lisp (SBCL) |
| **Web Framework** | Hunchentoot |
| **Database** | SQLite (embedded) |
| **ORM/Query Builder** | cl-dbi + sxql |
| **Frontend** | HTMX + minimal vanilla JS |
| **CSS** | Tailwind CSS or Pico CSS |
| **Charts** | Chart.js or uPlot |
| **Caching** | In-memory LRU cache |

### 6.2 System Components

```
┌─────────────────────────────────────────────────────────────────┐
│                        Client Websites                          │
│                    (JavaScript Tracking Script)                 │
└────────────────────────────┬────────────────────────────────────┘
                             │ HTTP POST /api/event
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Happening Server                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │
│  │   Event     │  │  Dashboard  │  │   Background Workers    │ │
│  │  Ingestion  │  │    API      │  │  (Aggregation, Cleanup) │ │
│  │    API      │  │             │  │                         │ │
│  └──────┬──────┘  └──────┬──────┘  └────────────┬────────────┘ │
│         │                │                      │               │
│         └────────────────┼──────────────────────┘               │
│                          ▼                                      │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    Data Access Layer                        ││
│  │                    (cl-dbi + sxql)                          ││
│  └─────────────────────────────────────────────────────────────┘│
└────────────────────────────────┬────────────────────────────────┘
                                 │
                                 ▼
                    ┌────────────────────────┐
                    │        SQLite          │
                    │   (Embedded Database)  │
                    └────────────────────────┘
```

### 6.3 Data Model

```sql
-- SQLite schema (WAL mode enabled for concurrent access)

sites
├── id (text, primary key)      -- nanoid or uuid string
├── domain (text, unique)
├── name (text)
├── created_at (integer)        -- unix timestamp
└── settings (text)             -- JSON string

events
├── id (integer, primary key autoincrement)
├── site_id (text, FK)
├── timestamp (integer)         -- unix timestamp ms
├── visitor_hash (text)         -- SHA256(IP + UA + daily salt)
├── session_id (text)
├── url (text)
├── referrer (text)
├── device_type (text)          -- 'desktop', 'mobile', 'tablet'
├── browser (text)
├── os (text)
├── country (text, nullable)
└── viewport_width (integer)
-- Index: (site_id, timestamp) for time-range queries
-- Index: (site_id, session_id) for session grouping

daily_stats (pre-aggregated for fast dashboard queries)
├── id (integer, primary key autoincrement)
├── site_id (text, FK)
├── date (text)                 -- ISO date 'YYYY-MM-DD'
├── page_views (integer)
├── unique_visitors (integer)
├── sessions (integer)
├── bounces (integer)
├── total_duration (integer)    -- seconds
└── page_stats (text)           -- JSON string
-- Unique: (site_id, date)

users
├── id (integer, primary key autoincrement)
├── username (text, unique)
├── password_hash (text)
├── created_at (integer)
└── last_login (integer)
```

---

## 7. API Specification

### 7.1 Event Ingestion

```
POST /api/event
Content-Type: application/json

{
  "site_id": "abc123",
  "url": "https://example.com/page",
  "referrer": "https://google.com",
  "viewport": { "width": 1920, "height": 1080 },
  "timestamp": 1704067200000
}

Response: 204 No Content
```

### 7.2 Dashboard API

```
GET /api/sites/:site_id/stats
Query params: start_date, end_date, granularity (hour|day|week|month)

Response:
{
  "period": { "start": "2026-01-01", "end": "2026-01-07" },
  "totals": {
    "page_views": 15420,
    "unique_visitors": 4832,
    "sessions": 5201,
    "bounce_rate": 0.42,
    "avg_duration": 185
  },
  "timeseries": [...],
  "top_pages": [...],
  "top_referrers": [...],
  "devices": {...},
  "browsers": {...}
}
```

---

## 8. User Interface Mockups

### Dashboard Overview
```
┌────────────────────────────────────────────────────────────────┐
│  Happening              [Site: example.com ▼]    [👤 Admin]    │
├────────────────────────────────────────────────────────────────┤
│  📅 Last 7 days ▼    [◀ Jan 1 - Jan 7, 2026 ▶]                │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐          │
│  │ 15,420   │ │  4,832   │ │   42%    │ │  3:05    │          │
│  │ Pageviews│ │ Visitors │ │ Bounce   │ │ Avg Time │          │
│  │ +12% ▲   │ │ +8% ▲    │ │ -3% ▼    │ │ +15% ▲   │          │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘          │
│                                                                │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │                                                         │  │
│  │     ╭─────────────────────────────────────────╮        │  │
│  │     │  Visitors & Pageviews Over Time         │        │  │
│  │     │     📈 (Time Series Chart)              │        │  │
│  │     ╰─────────────────────────────────────────╯        │  │
│  │                                                         │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                                │
│  ┌────────────────────────┐ ┌────────────────────────────┐    │
│  │ Top Pages              │ │ Top Referrers              │    │
│  │ /blog/post-1    2,340  │ │ google.com          1,203  │    │
│  │ /                1,892  │ │ twitter.com           892  │    │
│  │ /about            743  │ │ (direct)              654  │    │
│  │ /contact          412  │ │ github.com            321  │    │
│  └────────────────────────┘ └────────────────────────────┘    │
│                                                                │
│  ┌────────────────────────┐ ┌────────────────────────────┐    │
│  │ Devices          🥧    │ │ Browsers             🥧    │    │
│  │ ● Desktop    62%       │ │ ● Chrome      58%         │    │
│  │ ● Mobile     35%       │ │ ● Firefox     22%         │    │
│  │ ● Tablet      3%       │ │ ● Safari      15%         │    │
│  └────────────────────────┘ └────────────────────────────┘    │
└────────────────────────────────────────────────────────────────┘
```

---

## 9. Implementation Phases

### Phase 1: Foundation (MVP)
- Project setup with SBCL and Quicklisp
- Hunchentoot web server configuration
- Database schema and migrations
- Event ingestion API
- Basic dashboard with core metrics
- Single-user authentication

### Phase 2: Enhanced Analytics
- Session detection and duration tracking
- Bounce rate calculation
- Device/browser/OS detection
- Top pages and referrers
- Date range filtering

### Phase 3: Production Readiness
- Multi-site support
- Data aggregation workers
- Rate limiting and abuse prevention
- Docker packaging
- Documentation

### Phase 4: Polish
- Real-time visitor count
- Period comparisons
- Data export (CSV/JSON)
- Public dashboard links
- Performance optimizations

---

## 10. Success Metrics

| Metric | Target |
|--------|--------|
| Tracking script size | <1KB gzipped |
| Event ingestion latency | <100ms p99 |
| Dashboard load time | <500ms |
| Memory footprint | <256MB for 10k daily events |
| Setup time | <5 minutes with Docker |

---

## 11. Out of Scope (v1.0)

- A/B testing
- Funnel analysis
- Custom event tracking (deferred to v1.1)
- Team/organization accounts
- Email reports
- Mobile app
- Real-time streaming dashboard
- Geographic heatmaps

---

## 12. Open Questions

1. Should we support MaxMind GeoIP for location data, or keep it optional?
2. Preferred charting library: Chart.js (popular) vs uPlot (lightweight)?
3. Include optional Plausible/Umami import tools?

---

## 13. Appendix

### A. Competitive Analysis

| Feature | Google Analytics | Plausible | Umami | Happening |
|---------|-----------------|-----------|-------|-----------|
| Privacy-focused | ❌ | ✅ | ✅ | ✅ |
| Self-hosted | ❌ | ✅ | ✅ | ✅ |
| No cookies | ❌ | ✅ | ✅ | ✅ |
| Open source | ❌ | ✅ | ✅ | ✅ |
| Lightweight | ❌ | ✅ | ✅ | ✅ |
| Zero dependencies | ❌ | ❌ | ❌ | ✅ |
| Embedded SQLite | ❌ | ❌ | ❌ | ✅ |
| Common Lisp | ❌ | ❌ | ❌ | ✅ |

### B. References

- [Plausible Analytics](https://plausible.io) - Privacy-focused alternative
- [Umami](https://umami.is) - Simple, self-hosted analytics
- [Hunchentoot](https://edicl.github.io/hunchentoot/) - Common Lisp web server
- [cl-dbi](https://github.com/fukamachi/cl-dbi) - Database-independent interface for Common Lisp
- [sxql](https://github.com/fukamachi/sxql) - SQL query builder for Common Lisp
- [HTMX](https://htmx.org) - HTML-driven interactivity
