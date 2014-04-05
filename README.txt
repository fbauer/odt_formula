A python parser for the OpenFormula language
============================================

Warning: this code in unmaintained. It was a fun experiment to play
around with funcparserlib during the 2011 Christmas break, but
afterwards I lost interest. Expect bugs.

Installation
------------

There's no proper setup.py. You have to track down dependencies by
hand.
The code should work under Python 2.7 and maybe under 3.x as well.

Dependencies
------------

- funcparserlib
- lxml
- pytest

Overview
--------

formularparser.py implements an incomplete parser for the OpenFormula
language. It is a pure recursive decent parser based on the parser
combinators provided by funcparserlib.

The file examples.txt is part of the OpenFormula Draft specification
v1.2. It is used in test_formularparser.py as regression test suite.

