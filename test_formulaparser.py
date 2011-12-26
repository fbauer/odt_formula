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
 
