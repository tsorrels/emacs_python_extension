from variable_parser.import_parser import ImportParser
import unittest
import sys

class TestImportParser(unittest.TestCase):
    def setup(self):
        pass

    def test_parse_package_socket(self):
        import_parser = ImportParser()
        import sys
        variable = import_parser.parse_package('socket')
        self.assertTrue('htons' in variable.methods)
        self.assertTrue('IPV6_RTHDR' in variable.fields)
