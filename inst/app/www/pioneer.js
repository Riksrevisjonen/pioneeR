(function($) {

$(document).ready(function() {

  document.querySelector('[data-value="malmquist"]').closest('li').style.display = 'none';
  document.querySelector('[data-value="pioneeranalysis"]').closest('li').style.display = 'none';

  document.addEventListener('click', function(e) {
    if (e.target.id === 'uopts_menu') {
      e.target.stopPropagation();
    }
  });

  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'hasyear') {
      let el_m = document.querySelector('[data-value="malmquist"]').closest('li');
      let el_a = document.querySelector('[data-value="pioneeranalysis"]').closest('li');
      if (e.value === true) {
        el_m.style.display = 'block';
        el_a.style.display = 'none';
      } else if (e.value === false) {
        el_m.style.display = 'none';
        el_a.style.display = 'block';
      }
    }
  });

});

})(jQuery);
