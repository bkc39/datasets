import importlib.util, tempfile, unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIXTURES = HERE/'fixtures'

def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

importer = load('import_data', HERE.parent/'import-data.py')

class SpreadsheetReaders(unittest.TestCase):
    def test_xls_empty_cell_is_missing(self):
        self.assertEqual(importer.xls_records((FIXTURES/'empty-cell.xls').read_bytes()),
                         [['a', 'b', 'c'], ['1.0', '2.5', 'x'], [None, '3.0', None]])

    def test_xlsx_empty_cell_is_missing(self):
        self.assertEqual(importer.xlsx_records((FIXTURES/'empty-cell.xlsx').read_bytes()),
                         [['a', 'b', 'c'], ['1', '2.5', 'x'], [None, None, '3']])

    def test_missing_cells_stay_missing_when_typed(self):
        _, *records = importer.xlsx_records((FIXTURES/'empty-cell.xlsx').read_bytes())
        self.assertEqual(importer.uci_typed(records, set(), {2}), [[1, 2.5, 'x'], [None, None, '3']])

    def test_fixtures_are_the_generator_output(self):
        with tempfile.TemporaryDirectory() as out:
            load('make_fixtures', HERE/'make_fixtures.py').write(Path(out))
            for name in ('empty-cell.xls', 'empty-cell.xlsx'):
                self.assertEqual((Path(out)/name).read_bytes(), (FIXTURES/name).read_bytes(), name)

if __name__ == '__main__':
    unittest.main()
