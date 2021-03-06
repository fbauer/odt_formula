"""
Copyright (c) 2012, Florian Bauer <fbauer.devel@gmail.com>
All rights reserved.

Licensed under the terms of the 2-clause BSD license.
See the file LICENSE.txt for details.

A parser for the odt formula language as specified in 
OpenDocument-v1.2-cd05-part2-editor-revision.pdf
"""
import funcparserlib.parser as p
import string
from collections import namedtuple
import re

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


## [84]   	Letter	   ::=   	 BaseChar | Ideographic
## [85]   	BaseChar	   ::=   	[#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131] | [#x0134-#x013E] | [#x0141-#x0148] | [#x014A-#x017E] | [#x0180-#x01C3] | [#x01CD-#x01F0] | [#x01F4-#x01F5] | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1] | #x0386 | [#x0388-#x038A] | #x038C | [#x038E-#x03A1] | [#x03A3-#x03CE] | [#x03D0-#x03D6] | #x03DA | #x03DC | #x03DE | #x03E0 | [#x03E2-#x03F3] | [#x0401-#x040C] | [#x040E-#x044F] | [#x0451-#x045C] | [#x045E-#x0481] | [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC] | [#x04D0-#x04EB] | [#x04EE-#x04F5] | [#x04F8-#x04F9] | [#x0531-#x0556] | #x0559 | [#x0561-#x0586] | [#x05D0-#x05EA] | [#x05F0-#x05F2] | [#x0621-#x063A] | [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE] | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5 | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D | [#x0958-#x0961] | [#x0985-#x098C] | [#x098F-#x0990] | [#x0993-#x09A8] | [#x09AA-#x09B0] | #x09B2 | [#x09B6-#x09B9] | [#x09DC-#x09DD] | [#x09DF-#x09E1] | [#x09F0-#x09F1] | [#x0A05-#x0A0A] | [#x0A0F-#x0A10] | [#x0A13-#x0A28] | [#x0A2A-#x0A30] | [#x0A32-#x0A33] | [#x0A35-#x0A36] | [#x0A38-#x0A39] | [#x0A59-#x0A5C] | #x0A5E | [#x0A72-#x0A74] | [#x0A85-#x0A8B] | #x0A8D | [#x0A8F-#x0A91] | [#x0A93-#x0AA8] | [#x0AAA-#x0AB0] | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9] | #x0ABD | #x0AE0 | [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28] | [#x0B2A-#x0B30] | [#x0B32-#x0B33] | [#x0B36-#x0B39] | #x0B3D | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61] | [#x0B85-#x0B8A] | [#x0B8E-#x0B90] | [#x0B92-#x0B95] | [#x0B99-#x0B9A] | #x0B9C | [#x0B9E-#x0B9F] | [#x0BA3-#x0BA4] | [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5] | [#x0BB7-#x0BB9] | [#x0C05-#x0C0C] | [#x0C0E-#x0C10] | [#x0C12-#x0C28] | [#x0C2A-#x0C33] | [#x0C35-#x0C39] | [#x0C60-#x0C61] | [#x0C85-#x0C8C] | [#x0C8E-#x0C90] | [#x0C92-#x0CA8] | [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9] | #x0CDE | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C] | [#x0D0E-#x0D10] | [#x0D12-#x0D28] | [#x0D2A-#x0D39] | [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30 | [#x0E32-#x0E33] | [#x0E40-#x0E45] | [#x0E81-#x0E82] | #x0E84 | [#x0E87-#x0E88] | #x0E8A | #x0E8D | [#x0E94-#x0E97] | [#x0E99-#x0E9F] | [#x0EA1-#x0EA3] | #x0EA5 | #x0EA7 | [#x0EAA-#x0EAB] | [#x0EAD-#x0EAE] | #x0EB0 | [#x0EB2-#x0EB3] | #x0EBD | [#x0EC0-#x0EC4] | [#x0F40-#x0F47] | [#x0F49-#x0F69] | [#x10A0-#x10C5] | [#x10D0-#x10F6] | #x1100 | [#x1102-#x1103] | [#x1105-#x1107] | #x1109 | [#x110B-#x110C] | [#x110E-#x1112] | #x113C | #x113E | #x1140 | #x114C | #x114E | #x1150 | [#x1154-#x1155] | #x1159 | [#x115F-#x1161] | #x1163 | #x1165 | #x1167 | #x1169 | [#x116D-#x116E] | [#x1172-#x1173] | #x1175 | #x119E | #x11A8 | #x11AB | [#x11AE-#x11AF] | [#x11B7-#x11B8] | #x11BA | [#x11BC-#x11C2] | #x11EB | #x11F0 | #x11F9 | [#x1E00-#x1E9B] | [#x1EA0-#x1EF9] | [#x1F00-#x1F15] | [#x1F18-#x1F1D] | [#x1F20-#x1F45] | [#x1F48-#x1F4D] | [#x1F50-#x1F57] | #x1F59 | #x1F5B | #x1F5D | [#x1F5F-#x1F7D] | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC] | #x1FBE | [#x1FC2-#x1FC4] | [#x1FC6-#x1FCC] | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB] | [#x1FE0-#x1FEC] | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126 | [#x212A-#x212B] | #x212E | [#x2180-#x2182] | [#x3041-#x3094] | [#x30A1-#x30FA] | [#x3105-#x312C] | [#xAC00-#xD7A3]
## [86]   	Ideographic	   ::=   	[#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]
## [87]   	CombiningChar	   ::=   	[#x0300-#x0345] | [#x0360-#x0361] | [#x0483-#x0486] | [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD] | #x05BF | [#x05C1-#x05C2] | #x05C4 | [#x064B-#x0652] | #x0670 | [#x06D6-#x06DC] | [#x06DD-#x06DF] | [#x06E0-#x06E4] | [#x06E7-#x06E8] | [#x06EA-#x06ED] | [#x0901-#x0903] | #x093C | [#x093E-#x094C] | #x094D | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983] | #x09BC | #x09BE | #x09BF | [#x09C0-#x09C4] | [#x09C7-#x09C8] | [#x09CB-#x09CD] | #x09D7 | [#x09E2-#x09E3] | #x0A02 | #x0A3C | #x0A3E | #x0A3F | [#x0A40-#x0A42] | [#x0A47-#x0A48] | [#x0A4B-#x0A4D] | [#x0A70-#x0A71] | [#x0A81-#x0A83] | #x0ABC | [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD] | [#x0B01-#x0B03] | #x0B3C | [#x0B3E-#x0B43] | [#x0B47-#x0B48] | [#x0B4B-#x0B4D] | [#x0B56-#x0B57] | [#x0B82-#x0B83] | [#x0BBE-#x0BC2] | [#x0BC6-#x0BC8] | [#x0BCA-#x0BCD] | #x0BD7 | [#x0C01-#x0C03] | [#x0C3E-#x0C44] | [#x0C46-#x0C48] | [#x0C4A-#x0C4D] | [#x0C55-#x0C56] | [#x0C82-#x0C83] | [#x0CBE-#x0CC4] | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD] | [#x0CD5-#x0CD6] | [#x0D02-#x0D03] | [#x0D3E-#x0D43] | [#x0D46-#x0D48] | [#x0D4A-#x0D4D] | #x0D57 | #x0E31 | [#x0E34-#x0E3A] | [#x0E47-#x0E4E] | #x0EB1 | [#x0EB4-#x0EB9] | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD] | [#x0F18-#x0F19] | #x0F35 | #x0F37 | #x0F39 | #x0F3E | #x0F3F | [#x0F71-#x0F84] | [#x0F86-#x0F8B] | [#x0F90-#x0F95] | #x0F97 | [#x0F99-#x0FAD] | [#x0FB1-#x0FB7] | #x0FB9 | [#x20D0-#x20DC] | #x20E1 | [#x302A-#x302F] | #x3099 | #x309A
## [88]   	Digit	   ::=   	[#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9] | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F] | [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF] | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F] | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]
## [89]   	Extender	   ::=   	#x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 | #x0E46 | #x0EC6 | #x3005 | [#x3031-#x3035] | [#x309D-#x309E] | [#x30FC-#x30FE]

digitsxml = re.compile(ur'[\u0030-\u0039\u0660-\u0669\u06F0-\u06F9\u0966-\u096F\u09E6-\u09EF\u0A66-\u0A6F\u0AE6-\u0AEF\u0B66-\u0B6F\u0BE7-\u0BEF\u0C66-\u0C6F\u0CE6-\u0CEF\u0D66-\u0D6F\u0E50-\u0E59\u0ED0-\u0ED9\u0F20-\u0F29]')

letters = re.compile(ur'[\u0041-\u005A\u0061-\u007A\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF\u0100-\u0131\u0134-\u013E\u0141-\u0148\u014A-\u017E\u0180-\u01C3\u01CD-\u01F0\u01F4-\u01F5\u01FA-\u0217\u0250-\u02A8\u02BB-\u02C1\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03CE\u03D0-\u03D6\u03DA\u03DC\u03DE\u03E0\u03E2-\u03F3\u0401-\u040C\u040E-\u044F\u0451-\u045C\u045E-\u0481\u0490-\u04C4\u04C7-\u04C8\u04CB-\u04CC\u04D0-\u04EB\u04EE-\u04F5\u04F8-\u04F9\u0531-\u0556\u0559\u0561-\u0586\u05D0-\u05EA\u05F0-\u05F2\u0621-\u063A\u0641-\u064A\u0671-\u06B7\u06BA-\u06BE\u06C0-\u06CE\u06D0-\u06D3\u06D5\u06E5-\u06E6\u0905-\u0939\u093D\u0958-\u0961\u0985-\u098C\u098F-\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09DC-\u09DD\u09DF-\u09E1\u09F0-\u09F1\u0A05-\u0A0A\u0A0F-\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32-\u0A33\u0A35-\u0A36\u0A38-\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8B\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2-\u0AB3\u0AB5-\u0AB9\u0ABD\u0AE0\u0B05-\u0B0C\u0B0F-\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32-\u0B33\u0B36-\u0B39\u0B3D\u0B5C-\u0B5D\u0B5F-\u0B61\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99-\u0B9A\u0B9C\u0B9E-\u0B9F\u0BA3-\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB5\u0BB7-\u0BB9\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C60-\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CDE\u0CE0-\u0CE1\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D28\u0D2A-\u0D39\u0D60-\u0D61\u0E01-\u0E2E\u0E30\u0E32-\u0E33\u0E40-\u0E45\u0E81-\u0E82\u0E84\u0E87-\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA-\u0EAB\u0EAD-\u0EAE\u0EB0\u0EB2-\u0EB3\u0EBD\u0EC0-\u0EC4\u0F40-\u0F47\u0F49-\u0F69\u10A0-\u10C5\u10D0-\u10F6\u1100\u1102-\u1103\u1105-\u1107\u1109\u110B-\u110C\u110E-\u1112\u113C\u113E\u1140\u114C\u114E\u1150\u1154-\u1155\u1159\u115F-\u1161\u1163\u1165\u1167\u1169\u116D-\u116E\u1172-\u1173\u1175\u119E\u11A8\u11AB\u11AE-\u11AF\u11B7-\u11B8\u11BA\u11BC-\u11C2\u11EB\u11F0\u11F9\u1E00-\u1E9B\u1EA0-\u1EF9\u1F00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2126\u212A-\u212B\u212E\u2180-\u2182\u3041-\u3094\u30A1-\u30FA\u3105-\u312C\uAC00-\uD7A3\u4E00-\u9FA5\u3007\u3021-\u3029]')
letterxml = p.some(letters.match)

combiningchars = re.compile(ur'[\u0300-\u0345\u0360-\u0361\u0483-\u0486\u0591-\u05A1\u05A3-\u05B9\u05BB-\u05BD\u05BF\u05C1-\u05C2\u05C4\u064B-\u0652\u0670\u06D6-\u06DC\u06DD-\u06DF\u06E0-\u06E4\u06E7-\u06E8\u06EA-\u06ED\u0901-\u0903\u093C\u093E-\u094C\u094D\u0951-\u0954\u0962-\u0963\u0981-\u0983\u09BC\u09BE\u09BF\u09C0-\u09C4\u09C7-\u09C8\u09CB-\u09CD\u09D7\u09E2-\u09E3\u0A02\u0A3C\u0A3E\u0A3F\u0A40-\u0A42\u0A47-\u0A48\u0A4B-\u0A4D\u0A70-\u0A71\u0A81-\u0A83\u0ABC\u0ABE-\u0AC5\u0AC7-\u0AC9\u0ACB-\u0ACD\u0B01-\u0B03\u0B3C\u0B3E-\u0B43\u0B47-\u0B48\u0B4B-\u0B4D\u0B56-\u0B57\u0B82-\u0B83\u0BBE-\u0BC2\u0BC6-\u0BC8\u0BCA-\u0BCD\u0BD7\u0C01-\u0C03\u0C3E-\u0C44\u0C46-\u0C48\u0C4A-\u0C4D\u0C55-\u0C56\u0C82-\u0C83\u0CBE-\u0CC4\u0CC6-\u0CC8\u0CCA-\u0CCD\u0CD5-\u0CD6\u0D02-\u0D03\u0D3E-\u0D43\u0D46-\u0D48\u0D4A-\u0D4D\u0D57\u0E31\u0E34-\u0E3A\u0E47-\u0E4E\u0EB1\u0EB4-\u0EB9\u0EBB-\u0EBC\u0EC8-\u0ECD\u0F18-\u0F19\u0F35\u0F37\u0F39\u0F3E\u0F3F\u0F71-\u0F84\u0F86-\u0F8B\u0F90-\u0F95\u0F97\u0F99-\u0FAD\u0FB1-\u0FB7\u0FB9\u20D0-\u20DC\u20E1\u302A-\u302F\u3099\u309A]')
combiningcharxml = p.some(combiningchars.match)
digitxml = p.some(digitsxml.match)


expression = p.forward_decl()
quote = p.skip(p.a("'"))
singlequoted =  quote + (p.oneplus(p.a("'") + p.a("'") | p.some(lambda x: x != "'")) >> join) + quote

identifier = (letterxml +
              (p.many(letterxml |
                      digitxml |
                      p.a('_') |
                      p.a('.') |
                      combiningcharxml) >> join)
              >> join) >> string.upper >> tag(u'identifier')
iri = p.oneplus(p.some(lambda x: x not in u"'#")) >> join >> tag(u'iri')
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

def isabs(tree):
    absref, ref = tree
    if absref:
        return tag('absref')(ref)
    else:
        return ref
absref = p.maybe(p.a('$')) 

quotedsheetname = (absref + singlequoted) >> isabs | error
column = (absref + (qname(string.uppercase) >> tag(u'string'))) >> isabs
row = (absref + ((qchar('123456789') + qstring('0123456789')) >> join >> int>> tag(u'integer'))) >> isabs
subtablecell = (column + row) | quotedsheetname

source = (quote + iri + quote + p.skip(p.a("#"))) >> tag("source")
sheetname = (quotedsheetname | absref + (p.oneplus(p.some(lambda x: x not in "]. #$'")) >> join)) >> tag('string')
# XXX cell address already matched by subtablecell, need non-greedy match there
sheetlocator = sheetname #+ p.many(p.a('.') + subtablecell)
sheetlocatororempty = p.maybe(sheetlocator)
celladdress = sheetlocatororempty + p.a('.') + column + row # Not used directly
dot = p.skip(p.a('.'))
colon = p.skip(p.a(':'))
def addrow(x):
    return (x[0], x[1], None)
def addcolumn(x):
    return (x[0], None, x[1])
rangeaddress = (
                ((sheetlocator + dot + column + row) >> tuple) + colon + ((sheetlocator + dot + column + row) >> tuple) |
                ((sheetlocator + dot + column) >> addrow) + colon + ((sheetlocator + dot + column) >> addrow) |
                ((sheetlocator + dot + row) >> addcolumn) + colon + ((sheetlocator + dot + row) >> addcolumn) |
                ((sheetlocatororempty + dot + column + row) >> tuple)
                + p.maybe(colon +  dot + ((column +  row) >> (lambda x: (None, x[0], x[1])) ) ) |
                ((sheetlocatororempty + dot + column) >> addrow) +
                colon + dot + (column >> (lambda x: (None, x, None))) |
                ((sheetlocatororempty + dot + row) >> addcolumn) +
                colon + dot + (row >> (lambda x: (None, None, x)))
 
                )

reference = p.skip(p.a('[')) + p.maybe(source) +  rangeaddress +  p.skip(p.a(']')) >> tag('reference')

## NamedExpression ::= SimpleNamedExpression |
## SheetLocalNamedExpression | ExternalNamedExpression
## SimpleNamedExpression ::= Identifier |
## '$$' (Identifier | SingleQuoted)
## SheetLocalNamedExpression ::=
## QuotedSheetName '.' SimpleNamedExpression
## ExternalNamedExpression ::=
## Source '#' (SimpleNamedExpression | SheetLocalNamedExpression)

simplenamedexpression = (identifier | p.a('$') + p.a('$') +  (identifier | singlequoted))
sheetlocalnamedexpression = quotedsheetname + p.a('.') + simplenamedexpression
externalnamedexpression = source + p.a('#') +  (simplenamedexpression | sheetlocalnamedexpression)
namedexpression = (simplenamedexpression | sheetlocalnamedexpression | externalnamedexpression) >> tag(u'namedexpression')

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
formula = ((p.maybe(intro) >> tag('intro')) + expression) >> tag('formula')

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
