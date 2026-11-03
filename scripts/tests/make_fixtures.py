"""Write the spreadsheet fixtures of test_spreadsheets.py: python3 -I make_fixtures.py DIR."""
import struct, sys, zipfile
from pathlib import Path

def record(kind, body): return struct.pack('<HH', kind, len(body)) + body

def biff2(cells):
    attributes = bytes(3)
    rows = max(r for r, _, _ in cells) + 1
    columns = max(c for _, c, _ in cells) + 1
    out = record(0x0009, struct.pack('<HH', 2, 0x0010)) + record(0x0042, struct.pack('<H', 1252))
    out += record(0x0000, struct.pack('<HHHH', 0, rows, 0, columns))
    for r, c, v in cells:
        at = struct.pack('<HH', r, c) + attributes
        if v is None: out += record(0x0001, at)
        elif isinstance(v, str): out += record(0x0004, at + bytes([len(v)]) + v.encode('cp1252'))
        else: out += record(0x0003, at + struct.pack('<d', v))
    return out + record(0x000A, b'')

MAIN = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
SHARED = f'<sst xmlns="{MAIN}"><si><t>a</t></si><si><t>b</t></si><si><t>c</t></si><si><t>x</t></si></sst>'
SHEET = (f'<worksheet xmlns="{MAIN}"><sheetData>'
         '<row r="1"><c r="A1" t="s"><v>0</v></c><c r="B1" t="s"><v>1</v></c><c r="C1" t="s"><v>2</v></c></row>'
         '<row r="2"><c r="A2"><v>1</v></c><c r="B2"><v>2.5</v></c><c r="C2" t="s"><v>3</v></c></row>'
         '<row r="3"><c r="B3" s="1"/><c r="C3"><v>3</v></c></row>'
         '<row r="4"/>'
         '</sheetData></worksheet>')

def xlsx(path):
    with zipfile.ZipFile(path, 'w', zipfile.ZIP_DEFLATED) as z:
        for name, text in [('xl/sharedStrings.xml', SHARED), ('xl/worksheets/sheet1.xml', SHEET)]:
            z.writestr(zipfile.ZipInfo(name, (1980, 1, 1, 0, 0, 0)), text)

def write(out):
    (out/'empty-cell.xls').write_bytes(biff2([(0, 0, 'a'), (0, 1, 'b'), (0, 2, 'c'),
                                             (1, 0, 1.0), (1, 1, 2.5), (1, 2, 'x'),
                                             (2, 0, None), (2, 1, 3.0)]))
    xlsx(out/'empty-cell.xlsx')

if __name__ == '__main__':
    write(Path(sys.argv[1]))
