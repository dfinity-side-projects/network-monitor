var express = require('express');
var router = express.Router();

/* GET home page. */
router.get('/', function(req, res, next) {
  res.render('index', { metrics: {
    'Average Block Latency': 'avg-block-latency',
    'Block Propagation': 'block-propagation'
  } });
});

router.get('/:metric', function(req, res, next) {
  res.render('metric', { metric: req.params['metric'] });
});

module.exports = router;
