function adjustButtonWidth() {
  var buttons = document.querySelectorAll('.button-container .btn');
  var maxWidth = 0;

  // Determine the width of the largest button without fixed width for an accurate measure
  buttons.forEach(function (button) {
    button.style.width = '';  // Reset to natural width
    var width = button.offsetWidth;
    if (width > maxWidth) {
      maxWidth = width;
    }
  });

  var viewportWidth = window.innerWidth;

  // Check if the maxWidth of the button is more than 90% of the viewport width
  if (maxWidth > viewportWidth * 0.9) {
    maxWidth = viewportWidth * 0.9;
    buttons.forEach(function (button) {
      button.style.whiteSpace = 'normal';
    });
  } else {
    buttons.forEach(function (button) {
      button.style.whiteSpace = 'nowrap';
    });
  }

  // Set the width of all buttons to the calculated width
  buttons.forEach(function (button) {
    button.style.width = maxWidth + 'px';
  });
}

// Adjust button widths on page load
document.addEventListener('DOMContentLoaded', adjustButtonWidth);

// Adjust button widths on window resize
window.addEventListener('resize', adjustButtonWidth);  