import funcparserlib.parser as p
import string
from collections import namedtuple


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

# entitities defined elsewhere
# these are not quite correct but sufficient for now
letterxml = oneof(string.letters + '$') 
combiningcharxml = oneof(string.letters)
digitxml = oneof(string.digits)
iri = p.many(letterxml)

expression = p.forward_decl()
singlequoted = p.a("'") + p.oneplus(p.a("'") + p.a("'") | p.some(lambda x: x != "'")) + p.a("'")

identifier = (letterxml +
              (p.many(letterxml |
                      digitxml |
                      p.a('_') |
                      p.a('.') |
                      combiningcharxml) >> join)
              >> join) >> string.upper >> tag(u'identifier')

## Whitespace ::= #x20 | #x09 | #x0a | #x0d

whitespace = oneof(u'\x20\x09\x0a\x0d')
spaces = p.skip(p.many(whitespace))

## Array ::= '{' MatrixRow ( RowSeparator MatrixRow )* '}'
## MatrixRow ::= Expression ( ';' Expression )*
## RowSeparator ::= '|'

rowseparator = p.a('|')
matrixrow = expression + p.many( p.a(';') +  expression)
array = p.a('{') + matrixrow + p.many(rowseparator + matrixrow) + p.a('}') >> tag('array')

## Error ::= '#' [A-Z0-9]+ ([!?] | ('/' ([A-Z] | ([0-9] [!?]))))

error = (p.a('#') + qname(string.uppercase + string.digits) +
         (oneof('!?') | (p.a('/') +  (oneof(string.uppercase) |
                                      (oneof(string.digits) + oneof('!?')))))) >> tag('error')

## QuotedLabel ::= SingleQuoted

quotedlabel = singlequoted >> tag(u'quotedlabel')

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

reference = p.a('[') + p.maybe(source) +  rangeaddress +  p.a(']') >> tag('reference')

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
separator = p.skip(p.a(';'))
def mkNone(x):
    if isinstance(x, p._Ignored):
        return None
    else:
        return x
emptyorparameter = p.maybe(p.skip(p.oneplus(whitespace)) | parameter) >> mkNone

def empty(x):
    if x == [None]:
        return []
    else:
        return x

def flat(x):
    return [x[0]] + x[1]

parameterlist = p.maybe(parameter + p.many(separator + emptyorparameter) |
                        #(separator >> (lambda x: None)) +
                        emptyorparameter + p.many(separator +  emptyorparameter ))>> flat >> empty >> tag(u'parameterlist')

## FunctionName ::= Identifier
## Identifier ::= LetterXML (LetterXML | DigitXML |
## '_' | '.' | CombiningCharXML)*

functionname = identifier

## ReferenceOp ::= IntersectionOp | ReferenceConcatenationOp | RangeOp
## IntersectionOp ::= '!'
## ReferenceConcatenationOp ::= '~'
## RangeOp ::= ':'

rangeop = p.a(':')
intersectionop = p.a('!')
referenceconcatenationop = p.a('~')

referenceop = intersectionop | referenceconcatenationop | rangeop

## PrefixOp ::= '+' | '-'
## PostfixOp ::= '%'
## InfixOp ::= ArithmeticOp | ComparisonOp | StringOp | ReferenceOp
## ArithmeticOp ::= '+' | '-' | '*' | '/' | '^'
## ComparisonOp ::= '=' | '<>' | '<' | '>' | '<=' | '>='
## StringOp ::= '&'


postfixop = p.a('%')
prefixop = oneof('+-')
pow_op = p.a('^')
add_op =  p.a('+') | p.a('-')
mul_op = p.a('*') | p.a('/')
arithmeticop = add_op | mul_op | pow_op
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

function = (functionname + spaces + p.skip(p.a('(')) + parameterlist + p.skip(p.a(')'))) >> tag(u'function')
atoms = spaces + ((p.skip(p.a('(')) + expression + p.skip(p.a(')')) >> (lambda x: x.value)) |
         function |
         number |
         string |
         array |
         reference |
         quotedlabel |
         automaticintersection |
         namedexpression |
         error
         ) + spaces

## >>> def eval_expr(z, list):
## ...     return reduce(lambda s, (f, x): f(s, x), list, z)

def infix(tree):
    z, tail = tree
    if not tail:
        return z
    else:
        return reduce(lambda s, (f, x): tag('infixop')((f, s, x)), tail, z)


def postfix(tree):
    z, tail = tree
    if not tail:
        return z
    else:
        return reduce(lambda s, f: tag('postfixop')((f, s)), tail, z)
    
def prefix(tree):
    tail, z = tree
    if not tail:
        return z
    else:
        return reduce(lambda s, f: tag('prefixop')((f, s)), tail, z)

#
# expression parsing + precedence
range_ex = (atoms + p.many(rangeop + atoms)) >> infix
intersection_ex = (range_ex + p.many(intersectionop + range_ex)) >> infix
referenceconcatenation_ex = (intersection_ex + p.many(referenceconcatenationop + intersection_ex)) >> infix
postfix_ex = (referenceconcatenation_ex + p.many(postfixop)) >> postfix
prefix_ex = (p.many(prefixop) + postfix_ex) >> prefix
factor = (prefix_ex + p.many(pow_op + prefix_ex)) >> infix
term = (factor+ p.many(mul_op + factor)) >> infix
infix_ex = (term + p.many(add_op + term)) >> infix
string_ex = (infix_ex + p.many(stringop + infix_ex)) >> infix
comparision_ex = (string_ex + p.many(comparisonop + string_ex)) >> infix

expression.define(spaces + (comparision_ex >> tag('expression'))
                  +  spaces)

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
    print formula.parse('=IF()+bar()*baz()')
    print formula.parse('=IF(5;TRUE();FALSE())')
