const $ = require('jquery')
const Chart = require('chart.js');

const ctx = 'mainChart'

const chart = new Chart(ctx, {
  type: 'line',
  data: {
    labels: [],
    datasets: [{
      label: 'Block Propagation',
      data: [],
    }]
  },
  options: {
    scales: {
      xAxes: [{
        scaleLabel: {
          display: true,
          labelString: 'Node'
        }
      }],
      yAxes: [{
        ticks: {
          beginAtZero:true
        },
        scaleLabel: {
          display: true,
          labelString: 'Milliseconds'
        }
      }]
    },
    responsive: true,
    maintainAspectRatio: false
  }
})

const params = new URLSearchParams(window.location.search)

const height = params.get('height') || 10
const rank = params.get('rank') || 0

$.get('/api/block-propagation?height=' + height + '&rank=' + rank, function(resp) {
  const labels = []
  const data = []
  for (var i = 0; i < resp.length; i++) {
    labels.push(i)
    data.push(resp[i])
  }
  chart.data.labels = labels
  chart.data.datasets[0].data = data
  chart.update()
})
