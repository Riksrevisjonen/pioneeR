(function($) {

$(document).ready(function() {

  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'hasyear') {
      if (event.value === true) {
        $('#malmquist-tab').parent('li').show();
        $('#pioneer_analysis-tab').parent('li').hide();
      } else if (event.value === false) {
        $('#malmquist-tab').parent('li').hide();
        $('#pioneer_analysis-tab').parent('li').show();
      }
    }
  });

});

})(jQuery);
