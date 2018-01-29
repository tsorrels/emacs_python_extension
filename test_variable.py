from variable_parser.variable import Variable
import unittest


class TestVariableParser(unittest.TestCase):
    def setup(self):
        pass

    def test_variable_to_string_no_fields_no_methods(self):
        expected_output = 'symbol1;;'
        variable = Variable('symbol1')
        output_string = variable.to_string()        
        self.assertTrue(expected_output == output_string)


    def test_variable_to_string_with_fields(self):
        expected_output = 'symbol1;field1,field2;'
        fields = ['field1', 'field2']
        variable = Variable('symbol1', fields)
        output_string = variable.to_string()
        self.assertTrue(expected_output == output_string)

    def test_variable_to_string_with_fields_and_methods(self):
        expected_output = 'symbol1;field1,field2;method1,method2,method3'
        fields = ['field1', 'field2']
        methods = ['method1', 'method2', 'method3']        
        variable = Variable('symbol1', fields, methods)
        output_string = variable.to_string()
        self.assertTrue(expected_output == output_string)
        
