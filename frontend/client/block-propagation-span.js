const $ = require('jquery')
const Chart = require('chart.js');

const ctx = 'mainChart'

const chart = new Chart(ctx, {
  type: 'line',
  data: {
    labels: [],
    datasets: []
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

let query = '/api/block-propagation?type=percentage'

const params = new URLSearchParams(window.location.search)

const height = params.get('height')
const rank = params.get('rank')
const span = params.get('span')
const frequency = parseInt(params.get('frequency')) || 2
const freeze = params.get('freeze') == 'true'

if (height) query += '&height=' + height
if (rank) query += '&rank=' + rank
if (span) query += '&span=' + span

function update() {
  $.get(query, function(resp) {
    const labels = []
    const data25 = []
    const data50 = []
    const data75 = []
    const data99 = []
    for (let i = 0; i < resp.length; i++) {
      labels.push(i)
      data25.push(resp[i]['0.25'])
      data50.push(resp[i]['0.5'])
      data75.push(resp[i]['0.75'])
      data99.push(resp[i]['0.99'])
    }
    chart.data.labels = labels
    chart.data.datasets = [
      {
        label: "25%",
        data: data25,
        backgroundColor: '#ff6384'
      },
      {
        label: "50%",
        data: data50,
        backgroundColor: '#36a2eb'
      },
      {
        label: "75%",
        data: data75,
        backgroundColor: '#cc65fe'
      },
      {
        label: "99%",
        data: data99,
        backgroundColor: '#ffce56'
      }
    ]
    chart.update()
  })
}

update()

if (!height && !freeze) {
  setInterval(update, frequency * 1000)
}