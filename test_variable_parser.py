from variable_parser.variable_parser import VariableParser
import unittest


class TestVariableParser(unittest.TestCase):
    def setup(self):
        self.parser = VariableParser()

    def test_parse_line_assignment(self):
        variable_parser = VariableParser()
        symbol = 'variable'
        line = "variable = 4"
        variable = variable_parser.parse_line(line)
        self.assertTrue(symbol == variable.symbol)

    def test_parse_line_assignment_indented(self):
        variable_parser = VariableParser()
        symbol = 'variable'
        line = "\t variable = 4"
        variable = variable_parser.parse_line(line)
        self.assertTrue(symbol == variable.symbol)

    def test_parse_line_only_whitespace(self):
        variable_parser = VariableParser()
        line = "\t   \t  "
        variable = variable_parser.parse_line(line)        
        self.assertTrue(not variable)

    def test_parse_line_void_function(self):
        variable_parser = VariableParser()
        line = "exit(1)"
        variable = variable_parser.parse_line(line)        
        self.assertTrue(not variable)
        
        
