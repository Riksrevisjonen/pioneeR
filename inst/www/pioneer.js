(function($) {

$(document).ready(function() {

  let hiddenTabs = ['pioneeranalysis', 'bootstrap', 'malmquist', 'pioneer_compare'];
  for (let i = 0; i < hiddenTabs.length; i++) {
    console.log(`[data-value="${hiddenTabs[i]}"]`);
    document.querySelector(`[data-value="${hiddenTabs[i]}"]`).closest('li').style.display = 'none';
  }

  document.addEventListener('click', function(e) {
    if (e.target.id === 'uopts_menu') {
      e.target.stopPropagation();
    }
  });

  const positionTooltip = function(e, id) {
    // Use scrollWidth and scrollHeight on the html element to get document width and height
    let el = document.getElementById(id);
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
  }

  $('#plot_dea').mousemove(function(e) {
    positionTooltip(e, 'plot_dea_tooltip');
  });

  $('#dea_salter_plot').mousemove(function(e) {
    positionTooltip(e, 'plot_salter_tooltip');
  });

  Shiny.addCustomMessageHandler('toggle_compare', function(msg) {
    let el = document.querySelector('[data-value="pioneer_compare"]').closest('li');
    el.style.display = msg === true ? 'block' : 'none';
  });

  Shiny.addCustomMessageHandler('disable_run_bootstrap', function(msg) {
    let el = document.getElementById('run_boot');
    if (msg === true) {
      el.setAttribute('disabled', '');
    } else {
      el.removeAttribute('disabled');
    }
  });

  $(document).on('click', '[data-app-delete-id]', function(e) {
    let id = e.target.getAttribute('data-app-delete-id');
    let parent = e.target.closest('div.row');
    parent.remove();
    Shiny.onInputChange('delete_mod_id', { id: id, nounce: Math.random() });
  });

  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'hasyear') {
      let el_m = document.querySelector('[data-value="malmquist"]').closest('li');
      let el_a = document.querySelector('[data-value="pioneeranalysis"]').closest('li');
      let el_b = document.querySelector('[data-value="bootstrap"]').closest('li');
      if (e.value === true) {
        el_m.style.display = 'block';
        el_a.style.display = 'none';
        el_b.style.display = 'none';
      } else if (e.value === false) {
        el_m.style.display = 'none';
        el_a.style.display = 'block';
        el_b.style.display = 'block';
      }
    }
  });

});

})(jQuery);
