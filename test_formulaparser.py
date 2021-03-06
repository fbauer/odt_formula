"""
Copyright (c) 2012, Florian Bauer <fbauer.devel@gmail.com>
All rights reserved.

Licensed under the terms of the 2-clause BSD license.
See the file LICENSE.txt for details.

Tests for the code in formularparser.py
"""
import formulaparser as f
import pytest
import codecs
import pprint
# pprint in test_toplevel needs a higher recursion limit
import sys; sys.setrecursionlimit(2000)
testcases = [
    (u'0', u'number', (u'number', 0)),
    (u'0e-1', u"number", (u'number', 0)),
    (u'0e-1', u'number', (u'number', 0)),
    (u'0.1e-1', u'number', (u'number', 0.01)),
    (u'0.1e+1', u'number', (u'number', 1)),

    (u'IF', u"identifier", (u"identifier", u'IF')),
    (u'iF', u"identifier", (u"identifier", u'IF')),

    (u'0', u"expression", (u"expression", ('number', 0))),
    (u'0e-1',u"expression", (u"expression", ('number', 0))),
    (u'0e-1', u"expression", (u"expression", ('number', 0))),
    (u'0.1e-1', u"expression", (u"expression", ('number', 0.01))),
    (u'0.1e+1', u"expression", (u"expression", ('number', 1))),
    (u'.1e+1', u"expression", (u"expression", ('number', 1))),
    (u'0+0', u"expression",
     (u"expression",
      ('infixop',
       ('+',
        ('number', 0),
        ('number', 0))))),

    (u'0+1', u"expression",
     (u"expression",
      ('infixop',
       ('+',
        ('number', 0),
        ('number', 1))))),

    (u'0+"foo"', u"expression",
     (u"expression",
      ('infixop',
       ('+',
        ('number', 0),
        ('string', 'foo'))))),

    (u'(0+"foo")', u"expression",
     (u"expression",
      ('infixop',
       ('+',
        ('number', 0),
        ('string', 'foo'))))),

    (u'"foo"%', u"expression",
     (u"expression",
      ('postfixop',
       ('%',
        ('string', 'foo'))))),
   
   
    (u'0+"foo"%', u"expression",
     (u"expression",
      ('infixop',
       ('+',
        ('number', 0),
        ('postfixop',
         ('%',
          ('string', 'foo'))))))),

    (u'(0+"foo")%', u"expression",
     (u"expression",
      ('postfixop',
       ('%',
        ('infixop',
         ('+',
          ('number', 0),
          ('string', 'foo'))))))),

    (u'IF()', u"expression",
     (u"expression",
      ('function',
       ((u'identifier', u'IF'),
        (u'parameterlist', []))
       ))),

    (u'1-2-3', u"expression",
     (u"expression",
      (u'infixop',
       ('-',
        (u'infixop',
         ('-',
          (u'number', 1),
          (u'number', 2))),
        (u'number', 3))))),
   
    (u'(1-2)-3', u"expression",
     (u"expression",
      (u'infixop',
       ('-',
        (u'infixop',
         ('-',
          (u'number', 1),
          (u'number', 2))),
        (u'number', 3))))),

    (u'1-(2-3)', u"expression",
     (u"expression",
      (u'infixop',
       ('-',
        (u'number', 1),
        (u'infixop',
         ('-',
          (u'number', 2),
          (u'number', 3))))))),

     (u'1+(2+3)', u"expression",
      (u"expression",
       (u'infixop',
        ('+',
         (u'number', 1),
         (u'infixop',
          ('+',
           (u'number', 2),
           (u'number', 3))))))),

    (u'1-2*3', u"expression",
     (u"expression",
      (u'infixop',
       ('-',
        (u'number', 1),
        (u'infixop',
         ('*',
          (u'number', 2),
          (u'number', 3))))))),

    (u'(1-2)*3', u"expression",
     (u"expression",
      (u'infixop',
       ('*',
        (u'infixop',
         ('-',
          (u'number', 1),
          (u'number', 2))),
        (u'number', 3))))),

    (u'1--2', u"expression",
     (u"expression",
      (u'infixop',
       ('-',
        (u'number', 1),
        (u'prefixop',
         ('-',
          (u'number', 2))))))),

    (u'1^2^3', u"expression",
     (u"expression",
      ('infixop',
       (u'^',
        ('infixop',
         (u'^',
          (u'number', 1.0),
          (u'number', 2.0))),
        (u'number', 3.0))))),

    (u"SUM([.B4:.B5])", u"function",
     (u"function",
      ((u"identifier", "SUM"),
       (u"parameterlist",
        [(u"expression",
          (u"reference",
           (None,
            ((None, ('string', 'B'), (u'integer', 4)),
             (None, ('string', 'B'), (u'integer', 5))))))])))),
    
    (u'(4+5)+2', u"expression",
     (u'expression',
      ('infixop',
       (u'+',
        ('infixop',
         (u'+',
          (u'number', 4.0),
          (u'number', 5.0))),
        (u'number', 2.0))))),
    (u'(4 + 5)+2', u'expression',
     ('expression',
      ('infixop',
       (u'+',
        ('infixop',
         (u'+',
          (u'number', 4.0),
          (u'number', 5.0))),
        (u'number', 2.0))))),
    (u'(4 * 5)+2\n', u'expression',
     ('expression',
      ('infixop',
       (u'+',
        ('infixop',
         (u'*',
          (u'number', 4.0),
          (u'number', 5.0))),
        (u'number', 2.0))))),
    (u'("4" * "5")+2\n', u'expression',
     (u'expression',
      (u'infixop',
       (u'+',
        (u'infixop',
         (u'*',
          (u'string', u"4"),
          (u'string', u"5"))),
        (u'number', 2.0))))),
   
    (u'("4" & "5")+2\n', u'expression',
     (u'expression',
      ('infixop',
       (u'+',
        ('infixop',
         (u'&',
          (u'string', u"4"),
          (u'string', u"5"))),
        (u'number', 2.0))))),
   
    (u'IF()', u"function",
     (u"function",
      ((u'identifier', u'IF'),
       (u'parameterlist', [])))),

    (u'iF()', u"function",
     (u"function",
      ((u'identifier', u'IF'),
       (u'parameterlist', [])))),

    (u'iF(if())', u"function",
     (u"function",
      ((u'identifier', u'IF'),
       (u'parameterlist',
        [('expression',
          (u'function',
           ((u'identifier', u'IF'),
            (u'parameterlist', [])))),
         ])))),

    (u'iF(1)', u"function",
     (u"function",
      ((u'identifier', u'IF'),
       (u'parameterlist',
        [('expression', (u'number', 1.0))])))),

    (u'iF(1;2)', u"function",
     (u"function",
      ((u'identifier', u'IF'),
       (u'parameterlist',
        [('expression', (u'number', 1.0)),
         ('expression', (u'number', 2.0))])))),

    #0 parameters
    (u"", u"parameterlist", (u"parameterlist", [])),
    #2 empty parameters
    (u";", u"parameterlist", (u"parameterlist", [None, None])),
    
    (u";;", u"parameterlist", (u"parameterlist", [None, None, None])),
    
    (u"1", u"parameterlist",
     (u"parameterlist", [(u"expression", (u"number", 1))])),
    
    (u"1;", u"parameterlist",
     (u"parameterlist", [(u"expression", (u"number", 1)), None])),
    
    (u"1;;", u"parameterlist",
     (u"parameterlist", [(u"expression", (u"number", 1)), None, None])),

    (u"1; ;", u"parameterlist",
     (u"parameterlist", [(u"expression", (u"number", 1)), None, None])),

    (u";1", u"parameterlist",
     (u"parameterlist", [None, (u"expression", (u"number", 1))])),

    (u'=("4" & "5")+2\n', u'formula',
     (u'formula',
      (('intro', (u'=', None)),
       ('expression',
        ('infixop',
         (u'+',
          ('infixop',
           (u'&',
            ('string', u'4'),
            ('string', u'5'))),
          (u'number', 2.0))))))),
    
    (u"[.B4]", u"reference",
     (u"reference",
      (None,
       ((None, ('string', 'B'), (u'integer', 4)),
        None)))),
   
    (u"['Sheet1'.B4]", u"reference",
     (u"reference",
      (None,
       (((u'string', "Sheet1"), ('string', 'B'), (u'integer', 4)), None)))),
    
    (u"[.B4:.B5]", u"reference",
     (u"reference",
      (None,
       ((None, ('string', 'B'), (u'integer', 4)),
        (None, ('string', 'B'), (u'integer', 5)))))),
   
    (u"[.B:.B]", u"reference",(u"reference",
                  (None,
                   ((None, ('string', 'B'), None),
                    (None, ('string', 'B'), None))))),
    
    (u"[.4:.5]", u"reference",(u"reference",
                  (None,
                   ((None, None, (u'integer', 4)),
                    (None, None, (u'integer', 5)))))), 

   (u"['Sheet1'.B4:.B5]", u"reference",(u"reference",
                           (None,
                            (((u'string', "Sheet1"), ('string', 'B'), (u'integer', 4)),
                             (None, ('string', 'B'), (u'integer', 5)))))),
    
    (u"['Sheet1'.B:.B]", u"reference",(u"reference",
                          (None,
                           (((u'string', "Sheet1"), ('string', 'B'), None),
                            (None, ('string', 'B'), None))))),
    
    (u"['Sheet1'.4:.5]", u"reference",(u"reference",
                          (None,
                           (((u'string', "Sheet1"), None, (u'integer', 4)),
                            (None, None, (u'integer', 5)))))),

    (u"['Sheet1'.B4:'Sheet1'.B5]", u"reference",(u"reference",
                           (None,
                            (((u'string', "Sheet1"), ('string', 'B'), (u'integer', 4)),
                             ((u'string', "Sheet1"), ('string', 'B'), (u'integer', 5)))))),
    
    (u"['Sheet1'.B:'Sheet1'.B]", u"reference",(u"reference",
                          (None,
                           (((u'string', "Sheet1"), ('string', 'B'), None),
                            ((u'string', "Sheet1"), ('string', 'B'), None))))),
    
    (u"['Sheet1'.4:'Sheet1'.5]", u"reference",(u"reference",
                          (None,
                           (((u'string', "Sheet1"), None, (u'integer', 4)),
                            ((u'string', "Sheet1"), None, (u'integer', 5)))))),

    (u"[.B4:.B5]", u"parameterlist",
                    (u"parameterlist",
                    [(u"expression",
                      (u"reference",
                       (None,
                        ((None, ('string', 'B'), (u'integer', 4)),
                         (None, ('string', 'B'), (u'integer', 5))))) )])),
    
    (u"./openformula-testsuite.ods", u"iri",
     (u"iri", u"./openformula-testsuite.ods")),
    
    (u"'./openformula-testsuite.ods'#", u"source",
     (u"source", (u"iri", u"./openformula-testsuite.ods"))),
    
    (u"['./openformula-testsuite.ods'#'Sheet1'.B4]", u"reference",
     ('reference',
      (('source', (u'iri', u'./openformula-testsuite.ods')),
       ((('string', u'Sheet1'), (u'string', u'B'), (u'integer', 4)),
        None)))),
    ]
@pytest.mark.parametrize(('input', "parser", 'expected'),
                         testcases)
def test_parser(input, parser, expected):
    tag, value = expected
    result = getattr(f, parser).parse(input)
    print result
    assert result.tag == parser
    assert result.value == value
    
@pytest.mark.parametrize(('input'),
                         codecs.open('examples.txt', 'r', 'utf-8'),
                         )
def test_toplevel(input):
    result = f.formula.parse(input)
    print result
