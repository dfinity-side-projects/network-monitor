var express = require('express');
var router = express.Router();

/* GET home page. */
router.get('/:metric', function(req, res, next) {
  res.render('metric', { metric: req.params['metric'] });
});

module.exports = router;
