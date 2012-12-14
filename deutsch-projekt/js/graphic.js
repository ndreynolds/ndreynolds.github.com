$(function() {

  var popoverTpl = _.template($('#popover-inner').html());

  var partyData, 
      timer, 
      current = 'sept1930.csv';

  var totals = {
    'sept1930.csv': {
      votes: 34956471,
      seats: 577
    },
    'juli1932.csv': {
      votes: 36882354,
      seats: 608
    }
  };

  var width = 443,
      height = 345,
      radius = Math.min(width, height) / 2;

  var color = d3.scale.ordinal()
    .range(["#af8748", "#d70029", "#8b0000", "#1857a3",
            "#555555", "#d0743c", "#ff8c00", "#1f7d50",
            "#933b58", "#facc2a", "#3fa0c4"]);

  var arc = d3.svg.arc()
    .outerRadius(radius - 10)
    .innerRadius(radius - 70);

  var pie = d3.layout.pie()
    .sort(null)
    .value(function(d) { return d.stimmen; });

  var svg = d3.select("#graph")
    .append("svg")
      .attr("width", width)
      .attr("height", height)
    .append("g")
      .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

  d3.json("parties.json", function(data) {
    partyData = data;
  });

  d3.csv(current, function(data) {

    data.forEach(function(d) {
      d.stimmen = +d.stimmen;
    });

    var g = svg.selectAll(".arc")
        .data(pie(data))
      .enter().append("g")
        .attr("class", "arc");

    g.append("path")
      .attr("d", arc)
      .style("fill", function(d) { return color(d.data.partei); })
      .on('mouseover', onMouseover)
      .on('mouseout', onMouseout)
      .each(function(d) { this._current = d; });

    addText(g);
  });

  function addText(sel) {
    sel.append("text")
      .attr("transform", function(d) { return "translate(" + arc.centroid(d) + ")"; })
      .attr("dy", ".35em")
      .style("text-anchor", "middle")
      .on('mouseover', onMouseover)
      .text(function(d, i) {
        return i < 7 ? d.data.partei : "";
      });
  }

  function onMouseover(obj) {
    clearTimeout(timer);

    var data = partyData[obj.data.partei];
    data.votes = obj.data.stimmen;
    data.votesPercent = (data.votes / totals[current].votes * 100).toFixed(2);
    data.seats = obj.data.sitze;
    data.seatsPercent = (data.seats / totals[current].seats * 100).toFixed(2);
    
    $('.popover').html(popoverTpl(data)).show();
  }

  function onMouseout() {
    timer = setTimeout(function() {
      $('.popover').hide();
    }, 100);
  }

  function change(datasetUrl) {
    var path = svg.selectAll("path");
    svg.selectAll("text").remove();
    current = datasetUrl;
    d3.csv(datasetUrl, function(data) {
      path = path.data(pie(data));
      path.transition().duration(750).attrTween("d", arcTween); // redraw the arcs
      setTimeout(function() {
        addText(svg.selectAll(".arc").data(pie(data)));
      }, 1000);
    });
  }

  function arcTween(a) {
    var i = d3.interpolate(this._current, a);
    this._current = i(0);
    return function(t) {
      return arc(i(t));
    };
  }

  $('#graph').on('mousemove', function(e) {
    $('.popover').css({ 'top': e.pageY + 10, 'left': e.pageX + 10 });
  });

  $('.toggle-btn').click(function() {
    if ($(this).data('url') !== current)
      change($(this).data('url'));
    $('.toggle-btn').removeClass('active');
    $(this).addClass('active');
    return false;
  });

  $('a.impressum').click(function() {
    $('.impressum-popover').show();
    $('.wrapper').css('opacity', 0.3);
  });

  $('.impressum-popover .close').click(function() {
    $('.impressum-popover').hide();
    $('.wrapper').css('opacity', 1.0);
  });

  setTimeout(function() {
    change('juli1932.csv');
    $('.toggle-btn').removeClass('active');
    $('.toggle-btn[data-url="juli1932.csv"]').addClass('active');
  }, 2000);
});
