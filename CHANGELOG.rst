=========
CHANGELOG
=========


v0.9.0
======

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
