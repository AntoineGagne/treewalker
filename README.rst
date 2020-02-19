==========
treewalker
==========

.. image:: https://travis-ci.org/AntoineGagne/treewalker.svg?branch=master
    :target: https://travis-ci.org/AntoineGagne/treewalker

.. image:: http://img.shields.io/hexpm/v/treewalker.svg?style=flat
    :target: https://hex.pm/packages/treewalker

.. image:: https://img.shields.io/github/release/AntoineGagne/treewalker?color=brightgreen
    :target: https://github.com/AntoineGagne/treewalker/releases

.. image:: https://coveralls.io/repos/github/AntoineGagne/treewalker/badge.svg?branch=master
    :target: https://coveralls.io/github/AntoineGagne/treewalker?branch=master

:Author: `Antoine Gagné <gagnantoine@gmail.com>`_

.. contents::
    :backlinks: none

.. sectnum::

A web crawler in Erlang that respects ``robots.txt``.

Installation
============

This library is available on `hex.pm <https://hex.pm/packages/treewalker>`_.

Keep in mind that the library is not yet stable and its API may be subject to changes.

Usage
=====

.. code-block:: erlang

    %% This will add the specified crawler to the supervision tree
    {ok, _} = treewalker:add_crawler(example, #{scraper => example_scraper,
                                                fetcher => example_fetcher,
                                                max_depth => 3,
                                                link_filter => example_link_filter,
                                                store => example_store}),
    %% Starts crawling
    ok = treewalker:start_crawler(example),
    %% ...
    %% Stops the crawler
    %% The pending requests will be completed and dropped
    ok = treewalker:stop_crawler(example),

Options
=======

The following settings are available via the ``sys.config`` configuration:

.. code-block:: erlang

    {treewalker, [
                  %% The minimum delay to wait before retrying a failed request
                  {min_retry_delay, pos_integer()},
                  %% The maximum delay to wait before retrying a failed request
                  {max_retry_delay, pos_integer()},
                  %% The maximum amount of retries of a failed request
                  {max_retries, pos_integer()},
                  %% The maximum amount of delay before starting a request (in seconds)
                  {max_worker_delay, pos_integer()},
                  %% The maximum amount of concurrent workers making HTTP requests
                  {max_concurrent_worker, pos_integer()},
                  %% The user agent making the HTTP requests
                  {user_agent, binary()}]},

Development
===========

Running all the tests and linters
---------------------------------

You can run all the tests and linters with the ``rebar3`` alias:

.. code-block:: sh

    rebar3 check
