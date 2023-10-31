$('.carousel').on('slid.bs.carousel', function () {
    // Find the currently active plotly element within this carousel
    var activePlot = $(this).find('.carousel-item.active .js-plotly-plot');

    if (activePlot.length > 0) {
      var plotId = activePlot.attr('id');
      var plot = document.getElementById(plotId);
      Plotly.relayout(plot, {autosize: true});
    }
  });