=========
CHANGELOG
=========

v1.03.14
=========

* Add Admin pages for listing, creating, & editing Product Sales.
* Bump copyright year in footer.


v1.03.13
=========

* Add configurable setting for the text in the Order Confirmation email before
  the Order Summary tables.
* Re-enable the checkout process, adding the ability to dynamically disable
  checkouts with custom messages instead of the previous system of code changes
  and hardcoded messages. A new Settings admin page allows managers to set
  these values.


v1.03.12
=========

* Temporarily stop accepting orders again. We will add this functionality to
  the Admin site so we don't need code changes to do this in the future.


v1.03.11
=========

* Properly render Store Credit errors on the Checkout page.
* Add Admin page for adding, editing, & deleting the available Shipping
  Methods.
* Fix cleanup of expired anonymous carts.
* Temporarily remove the Priority Shipping option from the Checkout page.
* Re-enable ability to place orders.


v1.03.10
=========

* Temporarily stop accepting new orders because of overwhelming order volume
  due to the novel coronavirus.


v1.03.09
=========

* Tweak wording in Order Confirmation emails & add an "Organic" column to the
  Product table.
* Add a "View on Avalara" button to the Edit Customer admin page if the
  Customer has an associated Avalara Customer.
* Tweak Priority Shipping text on the Checkout page.


v1.03.08
=========

* Fix empty Cart page after resetting password.
* Fix case-insensitive searching on the Product & Page admin pages.
* Allow ``<iframe>`` elements in HTML content of Pages, Products, & Categories.
* Log in users if they try to register with an existing email address and valid
  password during checkout.
* Change Slug & Title for Fundraisers page.
* Add hover text to the Product Variant dropdowns.
* Add heading to Icon Legends on search & category pages.
* Allow the asynchronous workers to finish processing before stopping the
  job-processing pool.
* Add timestamps to the server's log files.
* Fix a bug causing enqueued jobs to be dropped without processing.
* Add login form to the Checkout page instead of redirecting user's to the
  login page. The form includes a "Forgot Password" link & can be used to log
  in before submitting the order if desired.
* Fix re-submitting the Checkout form when a Stripe error is present.
* Add Admin page for adding, editing, & deleting Surcharges.


v1.03.07
=========

* Add an Order History table to the Edit Customer page in the Admin.
* Add a link to the Edit Customer page on the Admin's View Order page.
* Reduce variant quantities when an order is placed.
* Add a "Search Keywords" field to the Products New/Edit Admin Pages, allowing
  admins to specify additional words that can be matched when searching from
  the frontend.
* Replace "Limited Availability" badges in Add to Cart forms with "OoS" text in
  dropdowns.
* Reduce size of Sub-Category images.
* Add links to relevant Stripe pages on the Customer & Order Admin pages.
* Add additional breakpoints for prettified weights(3oz, 6oz, 2.5lbs).
* Add a search input to the Pages list in the Admin.
* Fix checkout shipping error message to say we only ship to United States,
  instead of North America.


v1.03.06
=========

* Improve error message when loading Checkout page with no items in Cart.
* Disable Grammarly on all textarea elements.
* Fix issue with loading Checkout page for anonymous Customers.


v1.03.05
=========

* Fix Checkout validation when billing details are not required.


v1.03.04
=========

* Default the State/Province dropdown fields in the address forms to a blank
  option instead of the first item in the list(e.g., Alabama).
* Don't create Tax line items when no tax is due.
* Fix Avalara refund amounts when completely refunding an Order with multiple
  refunds.
* Fix encoding of Full/Percentage Avalara refunds.
* Fix server errors when synchronous email sending fails, adding a retry
  mechanism.
* Fix SQL errors caused by password reset uniqueness.
* Fix 404 errors when attempting to load the homepage at ``/index.html``
  instead of an empty path.
* Fix a bug causing Address updates of only phone numbers from taking effect.


v1.03.03
=========

* Fix case-insensitive validation of unique emails.
* Fix double charging for tax when Avalara is in testing mode.
* Revert the changes to the Order Placed & Password Reset emails so they send
  synchronously. There seems to be a bug where emails are not getting sent out
  asynchronously and these are the most important emails we send.
* Fix a bug causing HTML entities in Page names to be rendered incorrectly.


v1.03.02
=========

* Add a note about seasonal item shipping times to the Order Placed email.
* Align the ordercount function for the StoneEdge integration with the
  downloadorders function, removing the filter for already-processed Orders.


v1.03.01
=========

* Improve the error message shown when the client receives an unexpected
  response from the server, suggesting to customers that they try refreshing
  the page to load the latest website code.
* Fix a bug causing errors in parsing of the Avalara Location Code during
  server configuration/initialization.
* Fix a bug preventing enqueued Avalara Transaction Creation tasks from being
  voided when the Avalara integration is in testing mode.


v1.03.00
=========

* Fix a bug causing the generic error message from appearing on the Checkout
  page.
* Fix a bug causing the sending of Order Placed emails to fail when an Order
  included a discount.
* Fix a bug causing off-by-one errors when exporting batches of orders to
  StoneEdge.
* Add an integration with the Avalara AvaTax API along with configuration
  options for disabling the integration, only reporting orders for the testing
  period, or using the integration for all tax functionality. This is
  integrated with the asynchronous task queue, so that we can process Orders &
  enqueue the submission of tax transactions when we cannot reach the Avalara
  service.
* Remove Product Variants from Carts when they are disabled.
* Remove the Member Number form from the Checkout page & Checkout API routes.
* Modify the StoneEdge integration so that Order exporting doesn't filter based
  on the Status field. Previously, we would not export Orders that were already
  exported by filtering out Orders without the PaymentReceived or OrderReceived
  statuses.
* Add a pool of asynchronous workers to the server for queueing & offloading
  non-critical tasks such as sending emails & resizing/optimizing images,
  allowing pages to return faster.
* Increase the font-size of the "Already have an account?" link on the
  Anonymous Checkout page.
* Add additionally environment logging during server startup.
* Log any uncaught exceptions encountered during route processing to the
  server's log file.


v1.02.02
=========

* Rename the ``Login Details`` section of the anonymous Checkout page to
  ``Create an Account`` and include a link to the Login page for Customers that
  already have an account.
* Add server logging for any Stripe, Avalara, or Email Authentication errors.


v1.02.01
=========

* Fix a bug causing Category names to render with HTML-encoded entities like
  ``&amp;``.
* Fix a bug causing errors in the Edit Category admin form from removing the
  opaque overlay.
* Add the ``rel=noopener`` HTML attribute to external links to prevent security
  vulnerabilities.


v1.02.00
=========

* Add a script to update product prices from a CSV file.
* Add a ``skip link`` allowing users with screen readers to skip the navigation
  elements and begin reading at the main content of the page.
* Add additional accessibility attributes to UI elements, improving the
  feedback given to disabled users using assistive technologies.
* Display a pop-up lightbox for showing large versions of Product images when
  clicking images on the Product Details page.
* Include a ``From:`` name in all emails instead of only the address.
* BCC the customer service email address when sending Order Confirmation
  emails.
* Fix a bug preventing the Homepage's Carousel from rotating.
* Fix a bug causing the Media Directory configuration from being properly
  parsed.


v1.01.00
=========

* Add script to update descriptions for products from a CSV file.
* Add a translucent "Processing" overlay with a spinner to the Checkout page
  after the Customer clicks the Place Order button(for free checkouts) or after
  they have entered their creditcard details.
* Improve the "Loading" text that is shown while navigating between pages.
  Instead of simple text, it is now a translucent overlay over the entire
  window with a large spinner. It will now appear when navigating between
  different Categories.
* Change the Login, Registration, & Password Reset pages so that
  the casing of email addresses is ignored. Instead, it will attempt processing
  against the first Customer with a case-insensitive matching email.
* Add a script to merge & export Customer accounts with email addresses that
  differ only in their casing. Emails from popular domains simply have the
  Customer merged(along with Addresses, Orders, & Reviews) while less popular
  domains have Customer emails exported so we can contact them.
* Fix D.C. & Outlying Territories of the United States being missing from the
  State dropdowns.
* Set the ``description`` meta tag, describing the current page to search
  engine crawlers & social networks.
* Add an ``Edit Homepage`` button to the Pages list on the Admin site.
* Add a ``slug`` column to the Pages table on the Admin site.
* Ensure that invalid URLs cause the prerendering server to return a 404 status
  code.
* Add a Coupons section to the Admin site, allowing administrators to view,
  edit, and create Coupons.
* Fix a bug causing the "Adding to Cart.." loading text to not be shown on
  mobile devices.


v1.00.00
=========

* Order the Admin's Products Table by SKU
* Remove the Site Map link from the Footer.
* Fix a bug in the order total calculation for Order Confirmation emails.
* Fix a bug allowing Customers to add Sold Out or Inactive Product Variants to
  their Cart via the Quick Order page.
* Remove the ``DataMigration`` & ``ImageMigration`` scripts.
* Automatically generate & serve a ``robots.txt`` file, varying the included
  directives depending on what environment we are deploying to.
* Trigger proper status codes when prerender pages for web bots/crawlers.
* Set Order Statuses to ``Processing`` after they have been exported to
  StoneEdge.
* Fix scrolling to anchor links on Static Pages.
* Show Loading/Success/Error text below the Add to Cart button when a Customer
  adds an item to their Cart. The message will automatically disappear after 10
  seconds.
* Fix URL of Product & Category Images in the ``og:image`` SEO meta attribute.
* Fix display of the mobile navigation menu's ``Log Out`` link.
* Fix display of prettified mass quantities in the Lot Size input of the Edit
  Product Page.
* Don't log SQL queries when running on the Production site.
* Fix broken Images on the Edit Category Admin page.
* Fix the Location URLs in the generated Sitemaps so they include the domain
  name.


v0.11.0
========

* Lots of small fixes & tweaks based on user feedback before rolling out to
  Production.
* Show a preview of a Variant's grams, as rendered by the frontend, on the Add
  & Edit Product pages.
* Move the "special shipping requirements" text from the Checkout Success page
  to below the Comments field on the Checkout page.


v0.10.0
========

* Include additional meta tags for providing page information to social
  networks and search engines.
* Expand the Structured Data provided to search engines about SESE & our
  products.
* Update to the latest Google Analytics tracking script.
* Expand the information in the Google Merchant Feed. Include sale information,
  category hierarchy, bundle status, brand names, & lot sizes.
* Include the Blog's Sitemap in the generated Sitemap Index.


v0.9.0
======

* Implement ZenCart's password hashing scheme for a migrated Customer's first
  login. This allows us to import passwords from ZenCart instead of requiring
  password resets for all Customers.
* Add fields for setting a product's Organic, Heirloom, Regional, & Small
  Grower statuses to the Add/Edit Product pages.
* Add field for setting a customer's store credit balance to the Edit Customer
  page.
* Show a live preview of the content on the Add Page & Edit Page admin pages.
* Sanitize text fields in the Admin forms to prevent Cross-Site Scripting
  vulnerabilities.
* Automatically scale & optimize new Category & Product images.


v0.8.0
======

* Add server route for integration with the StoneEdge Order Manager. Only
  endpoints/functions required for the Order Downloading functionality are
  implemented. Instead of combining the database querying and export
  generation, we split the export processing into two discrete steps - querying
  the database and transforming the Order data into what StoneEdge requires,
  and then generating the XML output for the transformed data.
* Store the issuer & last 4 digits of credit cards for exporting Orders to
  StoneEdge.


v0.7.0
======

* Improve the mobile responsiveness of all pages, especially the Product &
  Category pages and all tables that caused the page width to overflow on
  the old site.
* Automatically thumbnail Product & Category images in a variety of sizes.
  Provide image size hints so browsers can pull the smallest image sizes
  necessary.
* Support storing & rendering various types of Lot Sizes besides weights.
  E.g., mushroom plug counts, slip counts, or custom labels.
* Add support for browser autocompletion in Login, Registration, & Checkout
  form fields.


v0.6.0
======

* Don't collect a billing address if an Order is free.
* Allow priority shipping charges to have both a flat fee and percentage fee
  based on the Order sub-total. Default all priority charges to $5 + 5%.
* Show ``Free!`` for the prices of free Products, instead of no text in the Add
  to Cart forms.
* Don't collect contact addresses from Customers.
* Allow Customers to have an infinite number of Addresses.
* Show the links above the recent Order summary on the My Account page.
* Expand the Orders table on My Account page instead of navigating to a
  different page when Customers click the ``View All Orders`` button.
* Show the Checkout on a single page instead of 4 separate pages.
* Show the new Order's details on the Checkout Success page.


v0.5.0
======

* Add a button to the Quick Order page for adding additional form rows to the
  table.
* Instantly update the Cart counts in every tab & the Cart details in every
  Shopping Cart tab when adding or removing Products.
* Display Seasonal Item Surcharges on the Shopping Cart page for all Customers,
  as well as Tax & Shipping charges for registered Customers.
* Disable the ``Update`` button on the Shopping Cart page if the Customer
  hasn't changed any Quantities.
* Show dropdowns for selecting an SKUs instead of showing each SKU as a
  separate product on the Category, Search Results, & Product Pages.
* Store anonymous Customer Carts for 4 months.
* Move Add to Cart form on Product Details page from right side to under the
  product image.
* Show SKU under Add to Cart Form on the Product Details page.


v0.4.0
======

* Automatically log Customers in after they successfully reset their password.
* Send Link to Password Reset page instead of emailing Customers a new password.
* Password Reset page doesn't indicate if the email is registered.
* Add ``Register`` link to links in the Site Header when logged out.
* Put the Login Form first on the Log In page.
* Re-organize fields on the Create Account page.
* Remove the Company Name field from Contact Addresses.


v0.3.0
======

* Style the current page in the left sidebar's nav links.
* Reduce size of attribute icons in sidebar, reduce empty space between each.


v0.2.0
======

* Show Products in subcategories on a Category's Detail page.
* Change ``Search Descriptions`` checkbox on the Advanced Search page into a
  radio field with ``Titles`` and ``Titles & Descriptions`` options.
* Remove ``Include Subcategories`` checkbox on the Advanced Search page.
  Instead, subcategories are always included.
* Show used search terms & filters on the Search Results page.
* Combine the different SKUs of identical Products into a single Product(merge
  bulk Products into their normal Products).
* Change Pagination on pages with many results - always showing the first/last
  pages and 2 pages before & after the current page.
