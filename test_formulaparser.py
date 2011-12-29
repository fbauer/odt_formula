import formulaparser as f
import pytest
import codecs
import pprint
# FIXME
import sys; sys.setrecursionlimit(2000)
testcases = [
    (u'0', (u'number', 0)),
    (u'0e-1', (u'number', 0)),
    (u'0e-1', (u'number', 0)),
    (u'0.1e-1', (u'number', 0.01)),
    (u'0.1e+1', (u'number', 1)),

    (u'IF', (u"identifier", u'IF')),
    (u'iF', (u"identifier", u'IF')),

    (u'0', (u"expression", ('number', 0))),
    (u'0e-1',(u"expression", ('number', 0))),
    (u'0e-1', (u"expression", ('number', 0))),
    (u'0.1e-1', (u"expression", ('number', 0.01))),
    (u'0.1e+1', (u"expression", ('number', 1))),
    (u'.1e+1', (u"expression", ('number', 1))),
    (u'0+0', (u"expression",
              ('infixop',
               ('+',
                ('number', 0),
                ('number', 0))))),

    (u'0+1', (u"expression",
              ('infixop',
               ('+',
                ('number', 0),
                ('number', 1))))),

    (u'0+"foo"', (u"expression",
                  ('infixop',
                   ('+',
                    ('number', 0),
                    ('string', 'foo'))))),

    (u'(0+"foo")', (u"expression",
                    ('infixop',
                     ('+',
                      ('number', 0),
                      ('string', 'foo'))))),

    (u'"foo"%', (u"expression",
                 ('postfixop',
                  ('%',
                   ('string', 'foo'))))),


    (u'0+"foo"%', (u"expression",
                   ('infixop',
                    ('+',
                     ('number', 0),
                     ('postfixop',
                      ('%',
                       ('string', 'foo'))))))),

    (u'(0+"foo")%', (u"expression",
                     ('postfixop',
                      ('%',
                       ('infixop',
                        ('+',
                         ('number', 0),
                         ('string', 'foo'))))))),
    
    (u'IF()', (u"expression",
               ('function',
                ((u'identifier', u'IF'),
                 (u'parameterlist', []))
                ))),
    
    (u'1-2-3', (u"expression",
                (u'infixop',
                 ('-',
                  (u'infixop',
                   ('-',
                    (u'number', 1),
                    (u'number', 2))),
                  (u'number', 3))))),
    
    (u'(1-2)-3', (u"expression",
                  (u'infixop',
                   ('-',
                    (u'infixop',
                     ('-',
                      (u'number', 1),
                      (u'number', 2))),
                    (u'number', 3))))),
    
    (u'1-(2-3)', (u"expression",
                  (u'infixop',
                   ('-',
                    (u'number', 1),
                    (u'infixop',
                     ('-',
                      (u'number', 2),
                      (u'number', 3))))))),
    
     (u'1+(2+3)', (u"expression",
                  (u'infixop',
                   ('+',
                    (u'number', 1),
                    (u'infixop',
                     ('+',
                      (u'number', 2),
                      (u'number', 3))))))),
    
    (u'1-2*3', (u"expression",
                (u'infixop',
                 ('-',
                  (u'number', 1),
                  (u'infixop',
                   ('*',
                    (u'number', 2),
                    (u'number', 3))))))),
    
    (u'(1-2)*3', (u"expression",
                  (u'infixop',
                   ('*',
                    (u'infixop',
                     ('-',
                      (u'number', 1),
                      (u'number', 2))),
                    (u'number', 3))))),
    
    (u'1--2', (u"expression",
               (u'infixop',
                ('-',
                 (u'number', 1),
                 (u'prefixop',
                  ('-',
                   (u'number', 2))))))),
    
    (u'1^2^3', (u"expression",
                ('infixop',
                 (u'^',
                  ('infixop',
                   (u'^',
                    (u'number', 1.0),
                    (u'number', 2.0))),
                  (u'number', 3.0))))),
    
    (u"SUM([.B4:.B5])", (u"expression", ())),
    (u'(4+5)+2', ('expression',
                  ('infixop',
                   (u'+',
                    ('infixop',
                     (u'+',
                      (u'number', 4.0),
                      (u'number', 5.0))),
                    (u'number', 2.0))))),
    (u'(4 + 5)+2', ('expression',
                    ('infixop',
                     (u'+',
                      ('infixop',
                       (u'+',
                        (u'number', 4.0),
                        (u'number', 5.0))),
                      (u'number', 2.0))))),
    (u'(4 * 5)+2\n', ('expression',
                      ('infixop',
                       (u'+',
                        ('infixop',
                         (u'*',
                          (u'number', 4.0),
                          (u'number', 5.0))),
                        (u'number', 2.0))))),
    (u'("4" * "5")+2\n', ('expression',
                          ('infixop',
                           (u'+',
                            ('infixop',
                             (u'*',
                              (u'string', u"4"),
                              (u'string', u"5"))),
                            (u'number', 2.0))))),
    (u'("4" & "5")+2\n', ('expression',
                          ('infixop',
                           (u'+',
                            ('infixop',
                             (u'&',
                              (u'string', u"4"),
                              (u'string', u"5"))),
                            (u'number', 2.0))))),
    (u'IF()', (u"function",
               ((u'identifier', u'IF'),
                (u'parameterlist', [])))),
    
    (u'iF()', (u"function",
               ((u'identifier', u'IF'),
                (u'parameterlist', [])))),
    
    (u'iF(if())', (u"function",
                   ((u'identifier', u'IF'),
                    (u'parameterlist',
                     [('expression',
                       (u'function',
                        ((u'identifier', u'IF'),
                         (u'parameterlist', [])))),
                      ])))),
    
    (u'iF(1)', (u"function",
                ((u'identifier', u'IF'),
                 (u'parameterlist',
                  [('expression', (u'number', 1.0))])))),
    
    (u'iF(1;2)', (u"function",
                  ((u'identifier', u'IF'),
                   (u'parameterlist',
                    [('expression', (u'number', 1.0)),
                     ('expression', (u'number', 2.0))
                     ])))),

    #0 parameters
    (u"", (u"parameterlist", [])),
    #2 empty parameters
    (u";", (u"parameterlist", [None, None])),
    (u";;", (u"parameterlist", [None, None, None])),
    (u"1", (u"parameterlist", [(u"expression", (u"number", 1))])),
    (u"1;", (u"parameterlist", [(u"expression", (u"number", 1)), None])),
    (u"1;;", (u"parameterlist", [(u"expression", (u"number", 1)),
                                 None, None])),
    
    (u"1; ;", (u"parameterlist", [(u"expression", (u"number", 1)),
                                  None, None])),
    
    (u";1", (u"parameterlist", [None, (u"expression", (u"number", 1))])),
    (u"[.B4:.B5]", (u"parameterlist", ())),
    (u'=("4" & "5")+2\n', ('formula',
                           (('intro', (u'=', None)),
                            ('expression',
                             ('infixop',
                              (u'+',
                               ('infixop',
                                (u'&',
                                 ('string', u'4'),
                                 ('string', u'5'))),
                               (u'number', 2.0))))))),


    ]
@pytest.mark.parametrize(('input', 'expected'),
                         testcases)
def test_parser(input, expected):
    tag, value = expected
    result = getattr(f, tag).parse(input)
    print result
    assert result.tag == tag
    assert result.value == value
    
@pytest.mark.parametrize(('input'),
                         codecs.open('examples.txt', 'r', 'utf-8'),
                         )
def test_toplevel(input):
    result = f.formula.parse(input)
    pprint.pprint(result)
