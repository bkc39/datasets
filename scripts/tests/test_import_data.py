import copy, importlib.util, tempfile, unittest
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

class ImportState(unittest.TestCase):
    def test_imports_in_one_process_are_independent(self):
        def step_x(cfg): importer.add(cfg, 'x', ['a'], [[1]], 's')
        def step_y(cfg):
            cfg.citations['x'] = 'changed'
            importer.add(cfg, 'y', ['a'], [[2]], 's')
        with tempfile.TemporaryDirectory() as tmp:
            tmp = Path(tmp)
            def run(steps):
                cfg = importer.Config(package='p', out=tmp, provenance=tmp, private=tmp, cache=tmp,
                                      sources={'s': {'url': 'u', 'sha256': '0'*64, 'license': 'MIT', 'version': '1'}})
                registry = copy.deepcopy(importer.run_import(cfg, steps))
                return registry, (tmp/'registry.rktd').read_text()
            first = run([step_x])
            run([step_y])
            self.assertEqual(run([step_x]), first)
            self.assertEqual(sorted(first[0]), ['x'])

    def test_r_recipes_survive_repeated_imports(self):
        before = copy.deepcopy(importer.R_RECIPES)
        recipes = [(id, kw) for id, _, kw in importer.R_RECIPES if 'aliases' in kw or 'levels' in kw]
        self.assertTrue(recipes)
        for id, kw in recipes:
            header = list(kw.get('aliases', {}))
            first, second = (importer.r_recipe_columns(id, header, [], {}, kw) for _ in range(2))
            self.assertEqual(first, second, id)
            self.assertEqual(first[0], list(kw.get('aliases', {}).values()), id)
            self.assertNotIn('aliases', first[2], id)
            self.assertNotIn('levels', first[2], id)
        self.assertEqual(importer.R_RECIPES, before)

if __name__ == '__main__':
    unittest.main()
