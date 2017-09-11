var Elm = require('./Main.elm');

const authTokenKey = 'authToken';
const authUserIdKey = 'authUserId';


/** FLAGS **/
var [userId, token] = getAuthData();
function getAuthData() {
  var token = localStorage.getItem(authTokenKey);
  var userId = localStorage.getItem(authUserIdKey);
  if (userId !== null) {
    userId = parseInt(userId);
    if (isNaN(userId)) {
      userId = null;
    }
  }
  return [userId, token];
}


/** ELM **/
var node = document.getElementById('main');
var app = Elm.Main.embed(
  node,
  {
    authToken: token,
    authUserId: userId,
  }
);


/** SUBSCRIPTIONS **/

window.addEventListener('storage', function(e) {
  if ((e.key === authTokenKey || e.key === null) && e.oldValue !== e.newValue) {
    if (e.newValue === null) {
      /* Send a Logged Out Message When Another Tab Deletes the Auth Token */
      app.ports.loggedOut.send(null);
    } else {
      /* Send a Logged In Message When Another Tab Sets the Auth Token */
      var [userId, token] = getAuthData();
      app.ports.loggedIn.send({
        userId: userId,
        token: token,
      });
    }
  }
});


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


/* Store the User ID & Token in Local Storage */
app.ports.storeAuthDetails.subscribe(function(authDetails) {
  var [token, userId] = authDetails;
  localStorage.setItem(authUserIdKey, userId);
  localStorage.setItem(authTokenKey, token);
});

/* Remove the Stored User ID & Token if they Exist in Local Storage */
app.ports.removeAuthDetails.subscribe(function() {
  localStorage.removeItem(authUserIdKey);
  localStorage.removeItem(authTokenKey);
});
