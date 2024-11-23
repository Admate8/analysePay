
document.addEventListener('DOMContentLoaded', function() {
  new fullpage('#fullpage', {
    autoScrolling: true,
    scrollHorizontally: true
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
});

