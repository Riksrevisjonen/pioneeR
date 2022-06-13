(function($) {

$(document).ready(function() {

  // We want to disable the default behaviour of dropdown menus in Bootstrap so
  // that the dropdown does not immediately close when the user selects an input
  // value
  $(document).on('click', '#uopts_menu', function(e) {
     e.stopPropagation();
  });

  // Change the analysis tab based on if we have time series data or not
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'hasyear') {
      if (event.value === true) {
        $('#malmquist-tab').parent('li').show();
        $('#pioneeranalysis-tab').parent('li').hide();
      } else if (event.value === false) {
        $('#malmquist-tab').parent('li').hide();
        $('#pioneeranalysis-tab').parent('li').show();
      }
    }
  });

});

})(jQuery);
