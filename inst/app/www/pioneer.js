(function($) {

$(document).ready(function() {

  document.querySelector('[data-value="malmquist"]').closest('li').style.display = 'none';
  document.querySelector('[data-value="pioneeranalysis"]').closest('li').style.display = 'none';

  document.addEventListener('click', function(e) {
    if (e.target.id === 'uopts_menu') {
      e.target.stopPropagation();
    }
  });

  $('#plot_dea').mousemove(function(e) {
    // Use scrollWidth and scrollHeight on the html element to get document width and height
    let el = document.getElementById('plot_dea_tooltip');
    let docX = document.documentElement.scrollWidth;
    let docY = document.documentElement.scrollHeight;
    // If we are far from the right edge, position with left, else position with right
    if ((docX - e.pageX) > 200) {
      el.style.left = (e.pageX + 5) + "px";
      el.style.right = null;
    } else {
      el.style.left = null;
      el.style.right = (docX - e.pageX + 5) + "px";
    }
    // If we are far from the bottom, position with top, else position with bottom
    if ((docY - e.pageY) > 200) {
      el.style.top = (e.pageY + 5) + "px";
      el.style.bottom = null;
    } else {
      el.style.top = null;
      el.style.bottom = (docY - e.pageY + 5) + "px";
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
