from variable_parser.import_parser import ImportParser
import unittest
import sys

class TestImportParser(unittest.TestCase):
    def setup(self):
        pass

    def test_parse_package_socket(self):
        import_parser = ImportParser()
        variable = import_parser.parse_package('socket')
        self.assertTrue('htons' in variable.methods)
        self.assertTrue('IPV6_RTHDR' in variable.fields)
        self.assertFalse('__file__' in variable.fields)
        found_variable = False
        for variable in variable.variables:
            if variable.symbol == 'warnings':
                found_variable = True
                break
        self.assertTrue(found_variable)
        

    def test_import_module_test_module(self):
        import_parser = ImportParser()
        variable =import_parser.parse_package('test.test_package.test_module_a')
        self.assertEqual('test.test_package.test_module_a', variable.symbol)
        var = variable.variables[0]
        self.assertEqual('test_module_b_object', var.symbol)


    def test_import_module_name_not_found(self):
        import_parser = ImportParser()
        variable = import_parser.parse_package('thispackagenamedoesnotexist')
        self.assertEqual(None, variable)



        
