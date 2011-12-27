import funcparserlib.parser as p
import string
from collections import namedtuple
#dummy declarations, incorrect
whitespace = p.a(' ')
array = p.a('')

digitxml = p.a('1')
combiningcharxml = p.a('g')
automaticintersection = p.a('')
namedexpression  = p.a('')
error = p.a('')
iri = p.a('a')
Node = namedtuple('Node', ['tag', 'value']) 


def first(x):
    return x[0]

def tag(t):
    return lambda x: Node(t, x)

def oneof(ts):
    return reduce(lambda x, y: x|y, [p.a(t) for t in ts])

def qchar(x):
    return p.some(lambda s: s in x)

def qstring(x):
    return p.many(qchar(x)) >> join

def qname(x):
    return p.oneplus(qchar(x)) >> join

def join(x):
    if x is None:
        return ''
    return ''.join(i for i in x if i is not None)

letterxml = oneof(string.letters)
expression = p.forward_decl()
singlequoted = p.a("'") + p.oneplus(p.a("'") + p.a("'") | p.some(lambda x: x != "'")) + p.a("'")
spaces = p.skip(p.many(whitespace))
identifier = letterxml + (p.many(letterxml | digitxml | p.a('_') | p.a('.') | combiningcharxml) >> join) >> join

## Whitespace ::= #x20 | #x09 | #x0a | #x0d

whitespace = oneof(u'\x20\x09\x0a\x0d')

## Array ::= '{' MatrixRow ( RowSeparator MatrixRow )* '}'
## MatrixRow ::= Expression ( ';' Expression )*
## RowSeparator ::= '|'

rowseparator = p.a('|')
matrixrow = expression + p.many( p.a(';') +  expression)
array = p.a('{') + matrixrow + p.many(rowseparator + matrixrow) + p.a('}')

## Error ::= '#' [A-Z0-9]+ ([!?] | ('/' ([A-Z] | ([0-9] [!?]))))

error = (p.a('#') + qname(string.uppercase + string.digits) +
         (oneof('!?') | (p.a('/') +  (oneof(string.uppercase) |
                                      (oneof(string.digits) + oneof('!?'))))))

## QuotedLabel ::= SingleQuoted

quotedlabel = singlequoted

## AutomaticIntersection ::= QuotedLabel Whitespace* '!!' Whitespace* QuotedLabel

automaticintersection = quotedlabel + spaces + p.a('!!') + spaces + quotedlabel

## Reference ::= '[' Source? RangeAddress ']'
## RangeAddress ::=
## SheetLocatorOrEmpty '.' Column Row (':' '.' Column Row )? |
## SheetLocatorOrEmpty '.' Column ':' '.' Column |
## SheetLocatorOrEmpty '.' Row ':' '.' Row |
## SheetLocator '.' Column Row ':' SheetLocator '.' Column Row |
## SheetLocator '.' Column ':' SheetLocator '.' Column |
## SheetLocator '.' Row ':' SheetLocator '.' Row
## SheetLocatorOrEmpty ::= SheetLocator | /* empty */
## SheetLocator ::= SheetName ('.' SubtableCell)*
## SheetName ::= QuotedSheetName | '$'? [^\]\. #$']+
## QuotedSheetName ::= '$'? SingleQuoted | Error
## SubtableCell ::= ( Column Row ) | QuotedSheetName
## Column ::= '$'? [A-Z]+
## Row ::= '$'? [1-9] [0-9]*
## Source ::= "'" IRI "'" "#"
## CellAddress ::= SheetLocatorOrEmpty '.' Column Row /* Not used
## directly */

absref = p.maybe(p.a('$')) 

quotedsheetname = absref + singlequoted | error
column = absref + qname(string.uppercase)
row = absref + qchar('123456789') + qstring('0123456789') >> join
subtablecell = (column + row) | quotedsheetname

source = p.a("'") + iri + p.a("'") + p.a("#")
sheetname = quotedsheetname | absref + (p.oneplus(p.some(lambda x: x not in "]. #$'")) >> join)
# XXX cell address already matched by subtablecell, need non-greedy match there
sheetlocator = sheetname #+ p.many(p.a('.') + subtablecell)
sheetlocatororempty = p.maybe(sheetlocator)
celladdress = sheetlocatororempty + p.a('.') + column + row # Not used directly

rangeaddress = (sheetlocatororempty + p.a('.') + column + row + p.maybe(p.a(':') +  p.a('.') + column +  row ) |
                sheetlocatororempty + p.a('.') + column +  p.a(':') + p.a('.') + column |
                sheetlocatororempty + p.a('.') + row + p.a(':') + p.a('.') + row |
                sheetlocator + p.a('.') + column + row + p.a(':') + sheetlocator + p.a('.') + column + row |
                sheetlocator + p.a('.') + column + p.a(':') + sheetlocator + p.a('.') + column |
                sheetlocator + p.a('.') + row + p.a(':') + sheetlocator + p.a('.') + row
                )

reference = p.a('[') + p.maybe(source) +  rangeaddress +  p.a(']')

## NamedExpression ::= SimpleNamedExpression |
## SheetLocalNamedExpression | ExternalNamedExpression
## SimpleNamedExpression ::= Identifier |
## '$$' (Identifier | SingleQuoted)
## SheetLocalNamedExpression ::=
## QuotedSheetName '.' SimpleNamedExpression
## ExternalNamedExpression ::=
## Source '#' (SimpleNamedExpression | SheetLocalNamedExpression)

simplenamedexpression = (identifier | p.a('$$') +  (identifier | singlequoted))
sheetlocalnamedexpression = quotedsheetname + p.a('.') + simplenamedexpression
externalnamedexpression = source + p.a('#') +  (simplenamedexpression | sheetlocalnamedexpression)
namedexpression = (simplenamedexpression | sheetlocalnamedexpression | externalnamedexpression)

## ParameterList ::= /* empty */ |
## Parameter (Separator EmptyOrParameter )* |
## Separator EmptyOrParameter /* First param empty */
## (Separator EmptyOrParameter )*
## EmptyOrParameter ::= /* empty */ Whitespace* | Parameter
## Parameter ::= Expression
## Separator ::= ';'

parameter = expression
separator = p.a(';')
emptyorparameter = spaces | parameter
parameterlist = p.maybe(parameter + p.many(separator + emptyorparameter ) |
                        separator +  emptyorparameter + # /* first param empty */
                        (separator +  emptyorparameter ))

## FunctionName ::= Identifier
## Identifier ::= LetterXML (LetterXML | DigitXML |
## '_' | '.' | CombiningCharXML)*

functionname = identifier

## ReferenceOp ::= IntersectionOp | ReferenceConcatenationOp | RangeOp
## IntersectionOp ::= '!'
## ReferenceConcatenationOp ::= '~'
## RangeOp ::= ':'

intersectionop = p.a('!')
referenceconcatenationop = p.a('~')
rangeop = p.a(':')
referenceop = intersectionop | referenceconcatenationop | rangeop

## PrefixOp ::= '+' | '-'
## PostfixOp ::= '%'
## InfixOp ::= ArithmeticOp | ComparisonOp | StringOp | ReferenceOp
## ArithmeticOp ::= '+' | '-' | '*' | '/' | '^'
## ComparisonOp ::= '=' | '<>' | '<' | '>' | '<=' | '>='
## StringOp ::= '&'

prefixop = oneof('+-')
postfixop = p.a('%')

arithmeticop = p.a('+') | p.a('-') | p.a('*') | p.a('/') | p.a('^')
comparisonop = p.a('=') | p.a('<>') | p.a('<') | p.a('>') | p.a('<=') | p.a('>=')
stringop = p.a('&')
infixop = arithmeticop | comparisonop | stringop | referenceop

## ReferenceList ::= Reference (Whitespace* ReferenceConcatenationOp
## Whitespace* Reference)*

referencelist = reference + p.many(spaces + referenceconcatenationop + spaces + reference)

## String ::= '"' ([^"#x00] | '""')* '"'

char = p.some(lambda x: x not in '"\0')
string = (p.skip(p.a('"'))
          + (p.many((p.a('"') + p.a('"') >> join) | char) >> join)
          + p.skip(p.a('"'))) >> tag('string')

## Number ::= StandardNumber |
## '.' [0-9]+ ([eE] [-+]? [0-9]+)?
## StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?

digit = p.oneplus(p.some(lambda c: c.isdigit()))
digits = digit >> join
exponent = (p.maybe(oneof('eE') + p.maybe(oneof('-+')) + digits) >> join)
standardnumber = (digits + (p.maybe(p.a('.') + digits) >> join) + exponent) 
number = (standardnumber | (p.a('.') + digits + exponent) >> join) >> join >> float >> tag(u'number')

## Expression ::=
## Whitespace* (
## Number |
## String |
## Array |
## PrefixOp Expression |
## Expression PostfixOp |
## Expression InfixOp Expression |
## '(' Expression ')' |
## FunctionName Whitespace* '(' ParameterList ')' |
## Reference |
## QuotedLabel |
## AutomaticIntersection |
## NamedExpression |
## Error
## ) Whitespace*
## SingleQuoted ::= "'" ([^'] | "''")+ "'"

atoms = ((p.skip(p.a('(')) + expression + p.skip(p.a(')')) >> (lambda x: x.value)) |
         functionname + spaces + p.a('(') + parameterlist + p.a(')') |
         number |
         string |
         array |
         reference |
         quotedlabel |
         automaticintersection |
         namedexpression |
         error |
         prefixop +  expression
         )
        
def infix(tree):
    if not tree[1]:
        return first(tree)
    else:
        return tag('infixop')((tree[1][0][0], tree[0], tree[1][0][1]))

def postfix(tree):
    if not tree[1]:
        return first(tree)
    else:
        return tag('postfixop')((tree[1][0][0], tree[0]))

postfix_ex = (atoms + postfixop) >> postfix
infix_ex = ((postfix_ex | atoms) + p.many(infixop + (postfix_ex | atoms))) >> infix

expression.define(spaces + ((postfix_ex >> tag('expression') |
                             infix_ex >> tag('expression')
                             )
                            )
                  +  spaces 
                  )

## Formula ::= Intro? Expression
## Intro ::= '=' ForceRecalc?
## ForceRecalc ::= '='

force_recalc = p.a('=')
intro = p.a('=') + p.maybe(force_recalc)
formula = (p.maybe(intro) >> tag('intro')) + expression

if __name__ == '__main__':
    print string.parse('"a"')
    print formula.parse('="f"')
    print formula.parse('"f"')

    print digit.parse('123')
    print standardnumber.parse('1e3')
    print standardnumber.parse('123')
    print number.parse('1e3')
    print number.parse('.1e3')
    print string.parse('"a"')
    print string.parse('""""')

    print sheetname.parse('Sheet1.A1')
    print sheetlocator.parse('Sheet1.A1')
    print rangeaddress.parse('Sheet1.A1')
    print reference.parse('[.B2]')
    print reference.parse('[Sheet1.A1]')
    print expression.parse('1')
    print expression.parse('1+1')
    print formula.parse('=IF()')
    print formula.parse('=IF(5;TRUE();FALSE())')
