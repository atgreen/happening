;;; static-assets.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Embedded static assets for single-binary deployment

(in-package :happening)

;;; ----------------------------------------------------------------------------
;;; Compile-time asset loading
;;; ----------------------------------------------------------------------------
;;; Assets in the assets/ directory are loaded at compile time and embedded
;;; in the binary. This enables single-binary deployment without external files.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *embedded-assets* (make-hash-table :test 'equal)
    "Hash table storing embedded assets loaded at compile time.")

  (let ((assets-dir (merge-pathnames "assets/"
                                     (asdf:system-source-directory :happening))))
    ;; Load text assets from files
    (dolist (filename '("chart.min.js"))
      (let ((path (merge-pathnames filename assets-dir)))
        (when (probe-file path)
          (setf (gethash filename *embedded-assets*)
                (alexandria:read-file-into-string path)))))))

;;; ----------------------------------------------------------------------------
;;; Embedded CSS
;;; ----------------------------------------------------------------------------

(defparameter *embedded-style-css*
  "/* Happening Analytics - Dashboard Styles */

:root {
  --primary-color: #4f46e5;
  --primary-hover: #4338ca;
  --success-color: #10b981;
  --danger-color: #ef4444;
  --warning-color: #f59e0b;
  --text-color: #1f2937;
  --text-muted: #6b7280;
  --bg-color: #f9fafb;
  --card-bg: #ffffff;
  --border-color: #e5e7eb;
  --shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
  --shadow-lg: 0 4px 6px rgba(0, 0, 0, 0.1);
  --radius: 8px;
  --radius-lg: 12px;
}

* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  background-color: var(--bg-color);
  color: var(--text-color);
  line-height: 1.5;
}

a {
  color: var(--primary-color);
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* Buttons */
.btn-primary, .btn-secondary {
  display: inline-block;
  padding: 0.75rem 1.5rem;
  border-radius: var(--radius);
  font-weight: 500;
  cursor: pointer;
  border: none;
  font-size: 1rem;
  transition: all 0.2s;
}

.btn-primary {
  background-color: var(--primary-color);
  color: white;
}

.btn-primary:hover {
  background-color: var(--primary-hover);
  text-decoration: none;
}

.btn-secondary {
  background-color: white;
  color: var(--text-color);
  border: 1px solid var(--border-color);
}

.btn-secondary:hover {
  background-color: var(--bg-color);
  text-decoration: none;
}

/* Forms */
.form-group {
  margin-bottom: 1.25rem;
}

.form-group label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 500;
  color: var(--text-color);
}

.form-group input {
  width: 100%;
  padding: 0.75rem;
  border: 1px solid var(--border-color);
  border-radius: var(--radius);
  font-size: 1rem;
}

.form-group input:focus {
  outline: none;
  border-color: var(--primary-color);
  box-shadow: 0 0 0 3px rgba(79, 70, 229, 0.1);
}

.form-actions {
  display: flex;
  gap: 1rem;
  margin-top: 1.5rem;
}

/* Error messages */
.error-message {
  background-color: #fef2f2;
  border: 1px solid #fecaca;
  color: #dc2626;
  padding: 0.75rem 1rem;
  border-radius: var(--radius);
  margin-bottom: 1.5rem;
}

/* Setup and Login pages */
.setup-container, .login-container {
  min-height: 100vh;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 2rem;
}

.setup-card, .login-card {
  background: var(--card-bg);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-lg);
  padding: 2.5rem;
  width: 100%;
  max-width: 480px;
}

.setup-card h1, .login-card h1 {
  font-size: 1.75rem;
  margin-bottom: 0.5rem;
}

.setup-card h2 {
  font-size: 1.25rem;
  margin-top: 2rem;
  margin-bottom: 1rem;
  padding-top: 1.5rem;
  border-top: 1px solid var(--border-color);
}

.setup-card p, .login-card p {
  color: var(--text-muted);
  margin-bottom: 1.5rem;
}

.code-block {
  background: #1f2937;
  border-radius: var(--radius);
  padding: 1rem;
  overflow-x: auto;
  margin: 1rem 0;
}

.code-block code {
  color: #e5e7eb;
  font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
  font-size: 0.875rem;
  white-space: pre;
}

/* Dashboard */
.dashboard {
  max-width: 1400px;
  margin: 0 auto;
  padding: 1.5rem;
}

.dashboard-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 2rem;
  padding-bottom: 1rem;
  border-bottom: 1px solid var(--border-color);
}

.dashboard-header h1 {
  font-size: 1.5rem;
  font-weight: 600;
}

.dashboard-header nav {
  display: flex;
  gap: 1.5rem;
}

.site-selector {
  display: flex;
  align-items: center;
  gap: 1rem;
  color: var(--text-muted);
}

/* Date range */
.date-range {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 1.5rem;
}

.date-presets {
  display: flex;
  gap: 0.5rem;
}

.date-presets a {
  padding: 0.5rem 1rem;
  background: var(--card-bg);
  border: 1px solid var(--border-color);
  border-radius: var(--radius);
  color: var(--text-muted);
  font-size: 0.875rem;
}

.date-presets a:hover {
  border-color: var(--primary-color);
  color: var(--primary-color);
  text-decoration: none;
}

/* Realtime badge */
.realtime-badge {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 1.5rem;
  font-size: 0.875rem;
  color: var(--text-muted);
}

.pulse {
  width: 10px;
  height: 10px;
  background: var(--success-color);
  border-radius: 50%;
  animation: pulse 2s infinite;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

/* Stat cards */
.stat-cards {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}

.stat-card {
  background: var(--card-bg);
  border-radius: var(--radius-lg);
  padding: 1.5rem;
  box-shadow: var(--shadow);
}

.stat-value {
  font-size: 2rem;
  font-weight: 700;
  color: var(--text-color);
}

.stat-label {
  color: var(--text-muted);
  font-size: 0.875rem;
  margin-top: 0.25rem;
}

.stat-change {
  font-size: 0.875rem;
  margin-top: 0.5rem;
}

.stat-change.positive {
  color: var(--success-color);
}

.stat-change.negative {
  color: var(--danger-color);
}

/* Chart container */
.chart-container {
  background: var(--card-bg);
  border-radius: var(--radius-lg);
  padding: 1.5rem;
  box-shadow: var(--shadow);
  margin-bottom: 2rem;
  min-height: 300px;
}

/* Dashboard grid */
.dashboard-grid {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 1.5rem;
}

.dashboard-panel {
  background: var(--card-bg);
  border-radius: var(--radius-lg);
  padding: 1.5rem;
  box-shadow: var(--shadow);
}

.dashboard-panel.panel-wide {
  grid-column: span 2;
}

.dashboard-panel h3 {
  font-size: 1rem;
  font-weight: 600;
  margin-bottom: 1rem;
  color: var(--text-muted);
}

/* Tables */
.data-table {
  width: 100%;
  border-collapse: collapse;
}

.data-table th,
.data-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid var(--border-color);
}

.data-table th {
  font-weight: 500;
  color: var(--text-muted);
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.data-table td {
  font-size: 0.875rem;
}

.data-table tr:last-child td {
  border-bottom: none;
}

/* Sites page */
.sites-container {
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
}

.page-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 2rem;
}

.page-header h1 {
  font-size: 1.5rem;
}

.sites-list {
  display: grid;
  gap: 1rem;
}

.site-card {
  background: var(--card-bg);
  border-radius: var(--radius-lg);
  padding: 1.5rem;
  box-shadow: var(--shadow);
}

.site-card h3 {
  font-size: 1.125rem;
  margin-bottom: 0.25rem;
}

.site-domain {
  color: var(--text-muted);
  font-size: 0.875rem;
  margin-bottom: 1rem;
}

.site-actions {
  display: flex;
  gap: 0.75rem;
}

.site-actions .btn-secondary {
  padding: 0.5rem 1rem;
  font-size: 0.875rem;
}

.empty-state {
  text-align: center;
  padding: 3rem;
  color: var(--text-muted);
}

/* Footer */
.dashboard-footer {
  margin-top: 3rem;
  padding-top: 1.5rem;
  border-top: 1px solid var(--border-color);
  text-align: center;
  color: var(--text-muted);
  font-size: 0.875rem;
}

/* Responsive */
@media (max-width: 768px) {
  .dashboard-header {
    flex-direction: column;
    align-items: flex-start;
    gap: 1rem;
  }

  .date-range {
    flex-direction: column;
    align-items: flex-start;
    gap: 1rem;
  }

  .stat-cards {
    grid-template-columns: 1fr 1fr;
  }

  .dashboard-grid {
    grid-template-columns: 1fr;
  }

  .dashboard-panel.panel-wide {
    grid-column: span 1;
  }
}

@media (max-width: 480px) {
  .stat-cards {
    grid-template-columns: 1fr;
  }

  .setup-card, .login-card {
    padding: 1.5rem;
  }
}
"
  "Embedded style.css content")

;;; ----------------------------------------------------------------------------
;;; Embedded JavaScript
;;; ----------------------------------------------------------------------------

(defparameter *embedded-app-js*
  "/* Happening Analytics - Dashboard JavaScript */

document.addEventListener('DOMContentLoaded', function() {
  // Initialize charts if Chart.js is available and we have data
  if (typeof Chart !== 'undefined' && window.CHART_DATA) {
    initTrafficChart();
  }
  // Start auto-refresh for dashboard
  if (document.querySelector('.dashboard')) {
    startAutoRefresh();
  }
});

function initTrafficChart() {
  var ctx = document.getElementById('traffic-chart');
  if (!ctx) return;

  var data = window.CHART_DATA;
  var labels = data.map(function(d) { return d.date; });
  var pageviews = data.map(function(d) { return d.pageviews; });
  var visitors = data.map(function(d) { return d.visitors; });

  window.trafficChart = new Chart(ctx, {
    type: 'line',
    data: {
      labels: labels,
      datasets: [
        {
          label: 'Page Views',
          data: pageviews,
          borderColor: '#4f46e5',
          backgroundColor: 'rgba(79, 70, 229, 0.1)',
          fill: true,
          tension: 0.3
        },
        {
          label: 'Visitors',
          data: visitors,
          borderColor: '#10b981',
          backgroundColor: 'rgba(16, 185, 129, 0.1)',
          fill: true,
          tension: 0.3
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      interaction: {
        intersect: false,
        mode: 'index'
      },
      plugins: {
        legend: {
          position: 'top',
          align: 'end'
        }
      },
      scales: {
        x: {
          grid: {
            display: false
          }
        },
        y: {
          beginAtZero: true,
          grid: {
            color: '#e5e7eb'
          }
        }
      }
    }
  });
}

function startAutoRefresh() {
  // Refresh every 10 seconds
  setInterval(function() {
    // Extract site-id from URL
    var match = window.location.pathname.match(/\\/dashboard\\/([^/]+)/);
    if (!match) return;
    var siteId = match[1];
    var params = new URLSearchParams(window.location.search);
    var url = '/api/stats/' + siteId;
    if (params.toString()) url += '?' + params.toString();

    fetch(url, { credentials: 'same-origin' })
      .then(function(r) { return r.json(); })
      .then(function(stats) { updateDashboard(stats); })
      .catch(function(e) { console.error('Refresh failed:', e); });
  }, 10000);
}

function updateDashboard(stats) {
  // Update stat cards
  var cards = document.querySelectorAll('.stat-card');
  if (cards[0]) cards[0].querySelector('.stat-value').textContent = formatNumber(stats.pageviews);
  if (cards[1]) cards[1].querySelector('.stat-value').textContent = formatNumber(stats.visitors);
  if (cards[2]) cards[2].querySelector('.stat-value').textContent = formatPercent(stats['bounce-rate']);

  // Update realtime badge
  var realtime = document.querySelector('.realtime-badge span:last-child');
  if (realtime) realtime.textContent = stats.realtime + ' active now';

  // Update chart if available
  if (window.trafficChart && stats.daily) {
    window.trafficChart.data.labels = stats.daily.map(function(d) { return d.date; });
    window.trafficChart.data.datasets[0].data = stats.daily.map(function(d) { return d.pageviews; });
    window.trafficChart.data.datasets[1].data = stats.daily.map(function(d) { return d.visitors; });
    window.trafficChart.update();
  }

  // Update tables
  updateTable('.dashboard-panel:nth-child(1) tbody', stats['top-pages'], ['url', 'views']);
  updateTable('.dashboard-panel:nth-child(2) tbody', stats['top-referrers'], ['referrer', 'views']);
  updateTable('.dashboard-panel:nth-child(3) tbody', stats.devices, ['device_type', 'count']);
  updateTable('.dashboard-panel:nth-child(4) tbody', stats.browsers, ['browser', 'count']);
}

function updateTable(selector, data, keys) {
  var tbody = document.querySelector(selector);
  if (!tbody || !data) return;
  tbody.innerHTML = '';
  data.forEach(function(row) {
    var tr = document.createElement('tr');
    keys.forEach(function(key) {
      var td = document.createElement('td');
      var val = row[key];
      td.textContent = typeof val === 'number' ? formatNumber(val) : (val || '');
      tr.appendChild(td);
    });
    tbody.appendChild(tr);
  });
}

function formatNumber(n) {
  return n == null ? '0' : n.toLocaleString();
}

function formatPercent(n) {
  return ((n || 0) * 100).toFixed(1) + '%';
}
"
  "Embedded app.js content")

(defparameter *embedded-tracker-js*
  "!function(s,n,d){s&&n.sendBeacon((d.currentScript?.src?new URL(d.currentScript.src).origin:\"\")+\"/api/event\",JSON.stringify({site_id:s,url:location.href,referrer:d.referrer,viewport:{width:innerWidth},timestamp:Date.now()}))}(HAPPENING_SITE_ID,navigator,document)"
  "Embedded tracker.js content (minified)")

;;; ----------------------------------------------------------------------------
;;; Embedded asset handlers
;;; ----------------------------------------------------------------------------

(defun get-embedded-asset (filename)
  "Get an embedded asset by filename from the assets hash table."
  (gethash filename *embedded-assets*))

(defun embedded-asset-handler (content content-type &optional max-age)
  "Create a handler that serves embedded content."
  (lambda ()
    (setf (hunchentoot:content-type*) content-type)
    (when max-age
      (setf (hunchentoot:header-out :cache-control)
            (format nil "public, max-age=~D" max-age)))
    content))

(defun file-asset-handler (filename content-type &optional max-age)
  "Create a handler that serves content from the embedded assets hash table."
  (lambda ()
    (let ((content (get-embedded-asset filename)))
      (when content
        (setf (hunchentoot:content-type*) content-type)
        (when max-age
          (setf (hunchentoot:header-out :cache-control)
                (format nil "public, max-age=~D" max-age)))
        content))))

(defun make-embedded-dispatch-table ()
  "Create dispatch table entries for embedded assets."
  (list
   ;; CSS
   (hunchentoot:create-prefix-dispatcher
    "/css/style.css"
    (embedded-asset-handler *embedded-style-css* "text/css" 86400))
   ;; JavaScript
   (hunchentoot:create-prefix-dispatcher
    "/js/app.js"
    (embedded-asset-handler *embedded-app-js* "application/javascript" 86400))
   (hunchentoot:create-prefix-dispatcher
    "/js/tracker.js"
    (embedded-asset-handler *embedded-tracker-js* "application/javascript" 3600))
   ;; Chart.js (loaded from assets/ at compile time)
   (hunchentoot:create-prefix-dispatcher
    "/js/chart.min.js"
    (file-asset-handler "chart.min.js" "application/javascript" 86400))))
