=======================
SESE E-Commerce Website
=======================

.. image:: https://github.com/Southern-Exposure-Seed-Exchange/southernexposure.com/actions/workflows/main.yml/badge.svg
    :target: https://github.com/Southern-Exposure-Seed-Exchange/southernexposure.com/actions/workflows/main.yml


This is a rebuild of the current Southern Exposure Seed Exchange website as a
Single Page Application, using Haskell/Servant for the API Server & Elm for the
Frontend.

* `Roadmap <http://bugs.sleepanarchy.com/projects/sese-website/roadmap>`_
* `Issues <http://bugs.sleepanarchy.com/projects/sese-website/issues?sort=priority%3Adesc>`_
* `Staging Site <https://staging.southernexposure.com>`_


Quickstart
==========

You will need to have `Stack <https://haskellstack.org>`_ & either `NVM
<https://github.com/creationix/nvm>`_ or ``node``/``npm`` installed.

You can start the Client & Server in watch mode by running ``./manage.hs`` or
``./manage.hs watch``. This will rebuild the Client & Server when their source
files change, then hot update the Client code(refreshing open tabs if
necessary) & restart the Server.

To see all available commands, run ``./manage.hs help``.


License
========

GPL-3.0
