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


    def test_import_module_test_module(self):
        import_parser = ImportParser()
        variable = import_parser.parse_package('test.test_module')
        self.assertEqual('test.test_module', variable.symbol)
        self.assertTrue('TestClass' in variable.fields)


#    def test_import_module_test_package(self):
#        import_parser = ImportParser()
#        variable = import_parser.parse_package('test')
#        self.assertEqual('test', variable.symbol)
#        self.assertTrue('test_module' in variable.fields)
#        print variable.fields
#        print 'Finished test_package'
        
