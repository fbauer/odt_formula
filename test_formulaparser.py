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
                          (u'0+0', ('infixop', ('+', ('number', 0), ('number', 0)))),
                          (u'0+1', ('infixop', ('+', ('number', 0), ('number', 1)))),
                          (u'0+"foo"', ('infixop', ('+', ('number', 0), ('string', 'foo')))),
                          (u'"foo"%', ('postfixop', ('%', ('string', 'foo')))),
                          (u'0+"foo"%', ('infixop',
                                         ('+',
                                          ('number', 0),
                                          ('postfixop',
                                           ('%',
                                            ('string', 'foo')))))),
                          ])
def test_expression(input, expected):
    result = f.expression.parse(input)
    print result
    assert result.tag == 'expression'
    assert result.value == expected 

