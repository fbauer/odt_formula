import formulaparser as f
import pytest

@pytest.mark.parametrize(('input', 'expected'),
                         [(u'0', 0),
                          (u'0e-1', 0),
                          (u'0e-1', 0),
                          (u'0.1e-1', 0.01),
                          (u'0.1e+1', 1),
                          ])
def test_number(input, expected):
    result = f.number.parse(input)
    assert result.tag == 'number'
    assert result.value == expected

@pytest.mark.parametrize(('input', 'expected'),
                         [(u'0', ('number', 0)),
                          (u'0e-1',('number', 0)),
                          (u'0e-1', ('number', 0)),
                          (u'0.1e-1', ('number', 0.01)),
                          (u'0.1e+1', ('number', 1)),
                          (u'.1e+1', ('number', 1)),
                          (u'0+0', ('infixop',
                                    ('+',
                                     ('number', 0),
                                     ('number', 0)))),

                          (u'0+1', ('infixop',
                                    ('+',
                                     ('number', 0),
                                     ('number', 1)))),

                          (u'0+"foo"', ('infixop',
                                        ('+',
                                         ('number', 0),
                                         ('string', 'foo')))),

                          (u'(0+"foo")', ('infixop',
                                          ('+',
                                           ('number', 0),
                                           ('string', 'foo')))),

                          (u'"foo"%', ('postfixop',
                                       ('%',
                                        ('string', 'foo')))),


                          (u'0+"foo"%', ('infixop',
                                         ('+',
                                          ('number', 0),
                                          ('postfixop',
                                           ('%',
                                            ('string', 'foo')))))),

                          (u'(0+"foo")%', ('postfixop',
                                           ('%',
                                            ('infixop',
                                             ('+',
                                              ('number', 0),
                                              ('string', 'foo')))))),
                          (u'IF()', ('function',
                                     ((u'identifier', u'IF'),
                                      (u'parameterlist', []))
                                     )),
                          ])
def test_expression(input, expected):
    result = f.expression.parse(input)
    print result
    assert result.tag == 'expression'
    assert result.value == expected
    
@pytest.mark.parametrize(('input', 'expected'),
                         [(u'IF', u'IF'),
                          (u'iF', u'IF'),
                          ])
def test_identifier(input, expected):
    result = f.identifier.parse(input)
    print result
    assert result.tag == 'identifier'
    assert result.value == expected

@pytest.mark.parametrize(('input', 'expected'),
                         [(u'IF()', ((u'identifier', u'IF'),
                                      (u'parameterlist', []))),
                          (u'iF()', ((u'identifier', u'IF'),
                                      (u'parameterlist', []))),
                          (u'iF(if())', ((u'identifier', u'IF'),
                                         (u'parameterlist',
                                          [('expression',
                                            (u'function',
                                             ((u'identifier', u'IF'),
                                              (u'parameterlist', [])))),
                                           ]))),
                           (u'iF(1)', ((u'identifier', u'IF'),
                                       (u'parameterlist',
                                        [('expression', (u'number', 1.0))]))),
                          (u'iF(1;2)', ((u'identifier', u'IF'),
                                       (u'parameterlist',
                                        [('expression', (u'number', 1.0)),
                                         ('expression', (u'number', 2.0))
                                         ]))),
                          ])
def test_function(input, expected):
    result = f.function.parse(input)
    print result
    assert result.tag == 'function'
    assert result.value == expected

@pytest.mark.parametrize(('input', 'expected'),
                         #0 parameters
                         [(u"", []),
                          #2 empty parameters
                          (u";", [None, None]),
                          (u";;", [None, None, None]),
                          (u"1", [(u"expression", (u"number", 1))]),
                          (u"1;", [(u"expression", (u"number", 1)), None]),
                          (u"1;;", [(u"expression", (u"number", 1)), None, None]),                                                    
                          (u"1; ;", [(u"expression", (u"number", 1)), None, None]),                          
                          (u";1", [None, (u"expression", (u"number", 1))]),
                          ])
def test_parameterlist(input, expected):
    result = f.parameterlist.parse(input)
    print result
    assert result.tag == u'parameterlist'
    assert result.value == expected
