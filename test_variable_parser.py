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

    def test_parse_line_invalid_line(self):
        variable_parser = VariableParser()
        line = "\t variable1.field + Max(3, 4) "
        variable = variable_parser.parse_line(line)        
        self.assertTrue(not variable)
        
    def test_parse_line_void_function(self):
        variable_parser = VariableParser()
        line = "exit(1)"
        variable = variable_parser.parse_line(line)        
        self.assertTrue(not variable)

    def test_parse_line_function_defintion(self):
        variable_parser = VariableParser()
        line = "def parse_line(self, variable):"
        variable = variable_parser.parse_line(line)        
        self.assertTrue(not variable)

    def test_parse_line_class_definition(self):
        variable_parser = VariableParser()
        line = "class VariableParser(object):"
        variable = variable_parser.parse_line(line)        
        #print variable.symbol
        self.assertTrue(not variable)

    def test_parse_line_comment(self):
        variable_parser = VariableParser()
        line = "#var = 4"
        variable = variable_parser.parse_line(line)        
        #print variable.symbol
        self.assertTrue(not variable)

    def test_parse_line_comment_mid_line(self):
        variable_parser = VariableParser()
        line = "var #="
        variable = variable_parser.parse_line(line)        
        #print variable.symbol
        self.assertTrue(not variable)
        
    def test_parse_file(self):
        variable_parser = VariableParser()
        fd = open("./test/test_input_script.py")
        input = fd.read()
        fd.close()
        variables = variable_parser.parse_input(input)
        variable_symbols = []
        for variable in variables:
            variable_symbols.append(variable.symbol)
        self.assertTrue('Threads' in variable_symbols)
        self.assertTrue('lock' in variable_symbols)
        self.assertTrue('connection' in variable_symbols)
        self.assertTrue(not 'run_time' in variable_symbols)
