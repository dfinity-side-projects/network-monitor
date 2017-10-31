const Chart = require('chart.js');

const ctx = "mainChart"

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
const last = params.get('last')

setInterval(function() {
  $.get('/api/avg-block-latency?last=' + last, function(resp) {
    const labels = []
    const data = []
    resp.forEach(function(elem) {
      labels.push(elem.height)
      data.push(elem.latency)
    })
    chart.data.labels = labels
    chart.data.datasets[0].data = data
  })
}, 5000)