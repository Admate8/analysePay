
document.addEventListener('DOMContentLoaded', function() {

  new fullpage('#fullpage', {
    autoScrolling: true,
    scrollHorizontally: true
  });

  // Prevent the user from scrolling before the Analyse button is clicked
  fullpage_api.setAllowScrolling(false, 'down');
  fullpage_api.setKeyboardScrolling(false, 'down');

  $(document).on('click', '#commit_input_data', function() {
    fullpage_api.setAllowScrolling(true);
    fullpage_api.setKeyboardScrolling(true);
  });

  // Observe the buttons to navigate the pages
  $(document).on('click', '#commit_input_data', function() {
    fullpage_api.moveSectionDown();
  });
  $(document).on('click', '#move_down', function() {
    fullpage_api.moveSectionDown();
  });
  $(document).on('click', '#move_up', function() {
    fullpage_api.moveSectionUp();
  });
  $(document).on('click', '#move_top', function() {
    fullpage_api.moveTo(1);
  });


  /*
  $(document).on('show.bs.collapse', '#1-accordion, #2-accordion', function(e) {
    Shiny.setInputValue('accordion_opened', e.currentTarget.id);
  }); */

/* Make sure that the first accordion is closed upon initialisation */
  $(document).on('shiny:sessioninitialized', function() {
    $('#1-accordion').collapse('hide');
  });
});

/* Send the input if the first accordion is expanded - happens only once each session */
(function() {
  var openedAccordions = {};
  var initialLoadComplete = false;

  // Wait for Shiny to be fully loaded
  $(document).on('shiny:sessioninitialized', function() {
    // Set a flag after initial load
    setTimeout(function() {
      initialLoadComplete = true;
    }, 2000); // Small delay to ensure all initial renders are complete
  });

  $(document).on('shown.bs.collapse', '#1-accordion', function(e) {
    var accordionId = e.currentTarget.id;

    // Only process if initial load is complete and this is a new open
    if (initialLoadComplete && !openedAccordions[accordionId]) {
      openedAccordions[accordionId] = true;
      Shiny.setInputValue('accordion_first_time_open', accordionId);
    }
  });
})();

