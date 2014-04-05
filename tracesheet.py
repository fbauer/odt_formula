from lxml import etree
import string
import re
import codecs

tables = dict()

def counted(rows, attr):
    r_index = 0

    for row in rows:
        try:
            r_index += int(row.attrib[attr])
        except KeyError:
            r_index += 1
        yield r_index, row

class Cell(object):
    def __init__(self, text='', formula=''):
        self.text = text
        self.formula = Formula(formula)
    def __repr__(self):
        return "Cell(text=%r, formula=%r)" % (self.text, self.formula)
    
class Formula(object):
    def __init__(self, formula):
        self.formula = formula
        self.inputs = list(getvars(formula))
    def __repr__(self):
        return repr(self.formula)

def getvars(formula):
    for match in re.finditer(r'\[.+?\]', formula):
        s = match.group(0).replace('$', '')
        if not ':' in s:
            yield s
        else:
            a, b = s[1:-1].split(':')
            ma = re.match(r'\.([A-Z]+)(\d+)', a)
            mb = re.match(r'\.([A-Z]+)(\d+)', b)
            if ma and mb:
                xa, ya = ma.group(1), int(ma.group(2))
                xb, yb = mb.group(1), int(mb.group(2))
                if xa == xb:
                    for i in range(ya, yb+1):
                        yield '[.%s%d]' % (xa, i)
                else:
                    yield s
            else:
                yield s
        
with open("testsheet.xml") as inf:
    tree = etree.parse(inf)
    print dir(tree)
    for element in tree.getiterator():
        print element.tag    #print(etree.tostring(tree, pretty_print=True))
    sheets = tree.iterfind(".//{urn:oasis:names:tc:opendocument:xmlns:table:1.0}table")

    for sheet in sheets:
        tname = sheet.attrib["{urn:oasis:names:tc:opendocument:xmlns:table:1.0}name"]
        tables[tname] = table = dict()
        for r_index, row in counted(sheet.iterfind("{urn:oasis:names:tc:opendocument:xmlns:table:1.0}table-row"),
                                    "{urn:oasis:names:tc:opendocument:xmlns:table:1.0}number-rows-repeated"):
 
            for c_index, cell in counted(row.iterfind("{urn:oasis:names:tc:opendocument:xmlns:table:1.0}table-cell"),
                                         "{urn:oasis:names:tc:opendocument:xmlns:table:1.0}number-columns-repeated"):
                print cell.tag
                c_str = ('X' + string.uppercase)[c_index]
                p=cell.find("{urn:oasis:names:tc:opendocument:xmlns:text:1.0}p")
                formula_s = cell.attrib.get("{urn:oasis:names:tc:opendocument:xmlns:table:1.0}formula", "")
                # get rid of the namespace prefix
                # XXX only of: really supported
                formula_parts = formula_s.split(':')
                formula = ':'.join(formula_parts[1:])
                assert formula_parts[0] in  ('of', '')
                cell_data = Cell(text= p.text if p is not None else '',
                                 formula=formula)
                table["[.%s%i]" % (c_str, r_index)] = cell_data



formulas = dict((k, v) for (k, v) in tables['Sheet1'].iteritems() if v.formula.formula)
constants = dict((k, v) for (k, v) in tables['Sheet1'].iteritems() if not v.formula.formula)


for k, v in sorted(constants.iteritems()):
    print u"%s = %r" % (k, v.text or "None")
    
for k, v in sorted(formulas.iteritems()):
    print u"%s = %s %r" % (k, v.formula, v.formula.inputs)

def name(ins):
    return ins.replace('[', '').replace(']', '').replace('.', '')

def sanitize(ins):
    return re.escape(ins)

with codecs.open("tracesheet.dot", "wb", 'utf-8') as out:
    out.write(u'digraph deps {\n')
    for k, v in sorted(formulas.iteritems()):
        out.write(u'%s [label="%s\\n%s"]\n' % (name(k), k, sanitize(v.formula.formula)))
        for inp in v.formula.inputs:
            if inp in formulas:
                out.write(u"%s->%s\n" % (name(inp), name(k)))
            elif inp in constants:
                out.write(u'%s [label="%s\\n%s"]\n' % (name(inp), inp, sanitize(constants[inp].text)))
                out.write(u"%s->%s\n" % (name(inp), name(k)))
    out.write(u'}\n')
