const { Elm } = require('./Main.elm');

const authUserIdKey = 'authUserId';
const cartTokenKey = 'cartSessionToken';
const cartItemCountKey = 'cartItemCount';

// TODO: Select between test or production key
const stripeApiKey = 'pk_test_F6Mr5XLKEDMn4rUsmsv5aqvr';


/** FLAGS **/
var cartToken = localStorage.getItem(cartTokenKey);
var cartItemCount = localStorage.getItem(cartItemCountKey);
var userId = getAuthData();


/** ELM **/
var app = Elm.Main.init({
  flags: {
      authUserId: userId,
      cartSessionToken: cartToken,
      cartItemCount: intOrNull(cartItemCount),
    },
  },
);


/** STRIPE **/
var stripeHandler = StripeCheckout.configure({
  key: stripeApiKey,
  locale: 'auto',
  name: 'Southern Exposure',
  image: '/static/img/logos/sese.png',
  zipCode: true,
  token: function(token) {
    /** STRIPE SUBSCRIPTION **/
    app.ports.stripeTokenReceived.send(token.id)
  }
});


/** ANALYTICS **/
// TODO: Reduce ID duplication - stated here & in webpack config
const GA_MEASUREMENT_ID = 'UA-5070189-1';
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', GA_MEASUREMENT_ID, { 'send_page_view': false });



/** SUBSCRIPTIONS **/

/* Changes to Stored Auth Details */
window.addEventListener('storage', function(e) {
  if ((e.key === authUserIdKey || e.key === null) && e.oldValue !== e.newValue) {
    if (e.newValue === null) {
      /* Send a Logged Out Message When Another Tab Deletes the User ID */
      app.ports.loggedOut.send(null);
    } else {
      /* Send a Logged In Message When Another Tab Sets the User ID */
      var userId = getAuthData();
      app.ports.loggedIn.send(userId);
    }
  }
});

/* Changes to Stored Cart Session */
window.addEventListener('storage', function(e) {
  if ((e.key === cartTokenKey) && e.oldValue !== e.newValue) {
    if (e.newValue !== null) {
       app.ports.newCartSessionToken.send(e.newValue);
    }
  }
});

/* Changes to Cart Item Count */
window.addEventListener('storage', function(e) {
  if ((e.key === cartItemCountKey) && e.oldValue !== e.newValue) {
    if (e.newValue !== null) {
      var itemCount = parseInt(e.newValue);
      if (!isNaN(itemCount)) {
        app.ports.cartItemCountChanged.send(itemCount);
      }
    }
  }
});


/** PORTS **/

/* Scroll to Top of Element if it's not in view */
app.ports.scrollToSelector.subscribe(function(selector) {
  requestAnimationFrame(function() {
    var $selector = $(selector);
    if ($selector.length > 0) {
      var elementTop = $selector.offset().top;
      if (elementTop < $(window).scrollTop()) {
        $('html, body').animate({ scrollTop: elementTop }, 300);
      }
    }
    $(':focus').blur();
  });
});

/* Collapse the Mobile Menus */
app.ports.collapseMobileMenus.subscribe(function() {
  $('.navbar-collapse.show').collapse('hide');
});


/* Store the User ID in Local Storage */
app.ports.storeAuthDetails.subscribe(function(authDetails) {
  var userId = authDetails;
  localStorage.setItem(authUserIdKey, userId);
});

/* Remove the Stored User ID from Local Storage */
app.ports.removeAuthDetails.subscribe(function() {
  localStorage.removeItem(authUserIdKey);
});

/* Store the Cart Session Token in Local Storage */
app.ports.storeCartSessionToken.subscribe(function(token) {
  localStorage.setItem(cartTokenKey, token);
});

/* Remove the Cart Session Token from Local Storage */
app.ports.removeCartSessionToken.subscribe(function() {
  localStorage.removeItem(cartTokenKey);
});

/* Store the Number of Items in the Cart */
app.ports.setCartItemCount.subscribe(function(itemCount) {
  localStorage.setItem(cartItemCountKey, itemCount);
});


/* Open the Stripe Checkout Popup */
app.ports.collectStripeToken.subscribe(function(portData) {
  var [customerEmail, checkoutTotal] = portData;
  stripeHandler.open({
    email: customerEmail,
    amount: checkoutTotal,
  });
  window.addEventListener('popstate', function() {
    stripeHandler.close();
  });
});

/* Log Page Change with Google Analytics */
app.ports.logPageView.subscribe(function(portData) {
  var [url, title] = portData;
  gtag('config', GA_MEASUREMENT_ID, { 'page_path': url, 'page_title': title });
});

/* Log Purchases with Google Analytics */
app.ports.logPurchase.subscribe(function(purchaseData) {
  gtag('event', 'purchase', purchaseData);
});


/** UTILITIES **/

/* Parse an Int or return null */
function intOrNull(intString) {
  var maybeInt = parseInt(intString);
  if (isNaN(maybeInt)) {
    return null;
  }
  return maybeInt;
}

/* Return the User ID if stored, or `null` otherwise. */
function getAuthData() {
  var userId = localStorage.getItem(authUserIdKey);
  return intOrNull(userId);
}
