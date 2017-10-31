const $ = require('jquery')
const Chart = require('chart.js');

const ctx = 'mainChart'

const chart = new Chart(ctx, {
  type: 'line',
  data: {
    labels: [],
    datasets: [{
      label: 'Average Block Latency',
      data: [],
      borderWidth: 1
    }]
  },
  options: {
    scales: {
      xAxes: [{
        scaleLabel: {
          display: true,
          labelString: 'Height'
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

const latest = params.get('latest') || 100
const frequency = parseInt(params.get('frequency')) || 2
const freeze = params.get('freeze') == 'true'

function update() {
  $.get('/api/avg-block-latency?latest=' + latest, function(resp) {
    const labels = []
    const data = []
    resp.forEach(function(elem) {
      labels.push(elem[0])
      data.push(elem[1])
    })
    chart.data.labels = labels
    chart.data.datasets[0].data = data
    chart.update()
  })
}

update()

if (!freeze) {
  setInterval(update, frequency * 1000)
}
