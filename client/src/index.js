var Elm = require('./Main.elm');

var node = document.getElementById('main');

var app = Elm.Main.embed(node);


/** PORTS **/

/* Set the Page Title */
app.ports.setPageTitle.subscribe(function(pageTitle) {
  if (pageTitle !== "") {
    var suffix = " : Southern Exposure Seed Exchange";
    document.title = pageTitle + suffix;
  }
});

/* Scroll to Top of Element if it's not in view */
app.ports.scrollToSelector.subscribe(function(selector) {
  $selector = $(selector);
  if ($selector.length > 0) {
    var elementTop = $selector.offset().top;
    if (elementTop < $(window).scrollTop()) {
      $('html, body').animate({ scrollTop: elementTop }, 500);
    }
  }
  $(':focus').blur();
});

/* Collapse the Mobile Menus */
app.ports.collapseMobileMenus.subscribe(function() {
  $('.navbar-collapse.show').collapse('hide');
});
