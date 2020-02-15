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

Keep in mind that this library is not yet ready for production use since it is
missing many core features such as rate limiting.

Development
===========

Running all the tests and linters
---------------------------------

You can run all the tests and linters with the ``rebar3`` alias:

.. code-block:: sh

    rebar3 check
