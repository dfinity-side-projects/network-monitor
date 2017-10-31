const $ = require('jquery')
const Chart = require('chart.js');

const ctx = 'mainChart'

const chart = new Chart(ctx, {
  type: 'bar',
  data: {
    labels: ["Red", "Blue", "Yellow", "Green", "Purple", "Orange"],
    datasets: [{
      label: 'Avg Block Latency',
      data: [12, 19, 3, 5, 2, 3],
      borderWidth: 1
    }]
  },
  options: {
    scales: {
      yAxes: [{
        ticks: {
          beginAtZero:true
        }
      }]
    }
  }
})

const params = new URLSearchParams(window.location.search)
const latest = params.get('latest')

setInterval(function() {
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
}, 5000)