/* Happening Analytics - Dashboard JavaScript */

document.addEventListener('DOMContentLoaded', function() {
  // Initialize charts if Chart.js is available and we have data
  if (typeof Chart !== 'undefined' && window.CHART_DATA) {
    initTrafficChart();
  }
});

function initTrafficChart() {
  var ctx = document.getElementById('traffic-chart');
  if (!ctx) return;

  var data = window.CHART_DATA;
  var labels = data.map(function(d) { return d.date; });
  var pageviews = data.map(function(d) { return d.pageviews; });
  var visitors = data.map(function(d) { return d.visitors; });

  new Chart(ctx, {
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
