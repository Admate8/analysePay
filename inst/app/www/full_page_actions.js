
document.addEventListener('DOMContentLoaded', function() {

  let isButtonClicked = false;

  new fullpage('#fullpage', {
    autoScrolling: true,
    scrollHorizontally: true,

    // Prevent the user from scrolling down when on the first page until the button is clicked
    afterLoad: function(origin, destination, direction) {

      // Check if the user is on the first page
      if (destination.index === 0 && !isButtonClicked) {
        // Prevent scrolling down
        fullpage_api.setAllowScrolling(false, 'down');
        fullpage_api.setKeyboardScrolling(false, 'down');
        // Disable the navigation down button
        $('#move_down').prop('disabled', true);
      } else {
        // Allow scrolling in all directions otherwise
        fullpage_api.setAllowScrolling(true);
        fullpage_api.setKeyboardScrolling(true);
        $('#move_down').prop('disabled', false);
      }
    },

    afterRender: function () {
      // arrow-left
      const arrow_left = document.querySelector(".fp-controlArrow.fp-prev");
      arrow_left.innerHTML = `<i class="fa-solid fa-caret-left"></i>`;
      // arrow-right
      const arrow_right = document.querySelector(".fp-controlArrow.fp-next");
      arrow_right.innerHTML = `<i class="fa-solid fa-caret-right"></i>`;
    }
  });

  $(document).on('click', '#commit_input_data', function() {
    isButtonClicked = false;

    // Allow scrolling in all directions after the button is clicked
    fullpage_api.setAllowScrolling(true);
    fullpage_api.setKeyboardScrolling(true);
  });

  // Fix the slide navigation buttons
  $(document).on('click', '.fp-controlArrow.fp-prev', function() {
    fullpage_api.moveSlideLeft();
  });
    $(document).on('click', '.fp-controlArrow.fp-next', function() {
    fullpage_api.moveSlideRight();
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


  // Render a hint when the accordion-sl is open for the first time
  var accordionSlOpenedFirst = false;
  // Get the value of the currently open panel
  $(document).on('show.bs.collapse', '#1-accordion, #2-accordion', function(e) {
    var panelValue = $(e.target).closest('.accordion-item').attr('data-value');

    if (panelValue === "accordion-sl" && !accordionSlOpenedFirst) {
        accordionSlOpenedFirst = true;
        Shiny.setInputValue('sl_accordion_opened', true);
    }
  });


  // Remove the number if clicked on the input box
   /*$(document).on('shiny:connected', function(event) {

    const inputIds = [ '1-expend_num_input_food', '1-expend_num_input_drinks', '1-expend_num_input_clothing', '1-expend_num_input_housing', '1-expend_num_input_household', '1-expend_num_input_health', '1-expend_num_input_transport', '1-expend_num_input_comms', '1-expend_num_input_recreation', '1-expend_num_input_education', '1-expend_num_input_restaurants', '1-expend_num_input_misc', '1-expend_num_input_other' ];
    inputIds.forEach(id => { document.querySelector(`#${id}`).addEventListener("focus", function (e) { e.target.value = ''; }); });
   });*/
});

//Send the input if the first accordion is expanded - happens only once each session
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

  $(document).on('shown.bs.collapse', '[id$="-accordion"]', function(e) {
    var accordionId = e.currentTarget.id;

    // Only process if initial load is complete and this is a new open
    if (initialLoadComplete && !openedAccordions[accordionId]) {
      openedAccordions[accordionId] = true;
      Shiny.setInputValue('accordion_first_time_open', true);
    }
  });
})();

