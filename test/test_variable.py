from variable_parser.variable import Variable
import unittest


class TestVariableParser(unittest.TestCase):
    def setup(self):
        pass

    def test_variable_to_string_no_fields_no_methods(self):
        expected_output = '(symbol1;;;)'
        variable = Variable('symbol1')
        
        output_string = variable.to_string()
        
        self.assertTrue(expected_output == output_string)


    def test_variable_to_string_with_fields(self):
        expected_output = '(symbol1;field1,field2;;)'
        fields = ['field1', 'field2']
        variable = Variable('symbol1', fields)
        
        output_string = variable.to_string()
        
        self.assertTrue(expected_output == output_string)

        
    def test_variable_to_string_with_fields_and_methods(self):
        expected_output = '(symbol1;field1,field2;method1,method2,method3;)'
        fields = ['field1', 'field2']
        methods = ['method1', 'method2', 'method3']        
        variable = Variable('symbol1', fields, methods)
        
        output_string = variable.to_string()
        
        self.assertTrue(expected_output == output_string)

        
    def test_variable_to_string_with_one_nested_variable(self):
        expected_output = '(s1;f11,f12;m11,m12;(s2;f21,f22;m21,m22;))'
        fields = ['f11', 'f12']
        methods = ['m11', 'm12']        
        nested_fields = ['f21', 'f22']
        nested_methods = ['m21', 'm22']        
        nested_variable = Variable('s2', nested_fields, nested_methods, [])
        variable = Variable('s1', fields, methods, [nested_variable,])
        
        output_string = variable.to_string()
        
        self.assertTrue(expected_output == output_string)

        
    def test_variable_to_string_with_one_single_nested_variable(self):
        expected_output = '(s1;f11,f12;m11,m12;(s2;f21,f22;m21,m22;))'
        fields = ['f11', 'f12']
        methods = ['m11', 'm12']        
        nested_fields = ['f21', 'f22']
        nested_methods = ['m21', 'm22']        
        nested_variable = Variable('s2', nested_fields, nested_methods, [])
        variable = Variable('s1', fields, methods, [nested_variable,])
        
        output_string = variable.to_string()
        
        self.assertTrue(expected_output == output_string)
        

    def test_variable_to_string_with_two_single_nested_variable(self):
        expected_output = '(s1;f11,f12;m11,m12;(s2;f21,f22;m21,m22;),(s3;f31,f32;m31,m32;))'
        fields = ['f11', 'f12']
        methods = ['m11', 'm12']        
        first_nested_fields = ['f21', 'f22']
        first_nested_methods = ['m21', 'm22']        
        first_nested_variable = Variable('s2',
                                         first_nested_fields,
                                         first_nested_methods,
                                         [])

        second_nested_fields = ['f31', 'f32']
        second_nested_methods = ['m31', 'm32']        
        second_nested_variable = Variable('s3',
                                          second_nested_fields,
                                          second_nested_methods,
                                          [])


        variable = Variable('s1', fields, methods, [first_nested_variable,
                                                    second_nested_variable])
        
        output_string = variable.to_string()
        
        self.assertTrue(expected_output == output_string)



    def test_variable_to_string_with_double_nested_variable(self):
        expected_output = '(s1;f11,f12;m11,m12;(s2;f21,f22;m21,m22;(s3;f31,f32;m31,m32;)))'
        fields = ['f11', 'f12']
        methods = ['m11', 'm12']        
        first_nested_fields = ['f21', 'f22']
        first_nested_methods = ['m21', 'm22']        

        second_nested_fields = ['f31', 'f32']
        second_nested_methods = ['m31', 'm32']        
        second_nested_variable = Variable('s3',
                                          second_nested_fields,
                                          second_nested_methods,
                                          [])

        first_nested_variable = Variable('s2',
                                         first_nested_fields,
                                         first_nested_methods,
                                         [second_nested_variable,])
        
        variable = Variable('s1', fields, methods, [first_nested_variable,])
        
        output_string = variable.to_string()
        self.assertTrue(expected_output == output_string)


    def test_variable_to_string_with_single_and_double_nested_variable(self):
        expected_output = '(s1;f11,f12;m11,m12;(s2;f21,f22;m21,m22;(s3;f31,f32;m31,m32;)),(s4;f41,f42;m41,m42;))'
        fields = ['f11', 'f12']
        methods = ['m11', 'm12']        
        first_nested_fields = ['f21', 'f22']
        first_nested_methods = ['m21', 'm22']        

        second_nested_fields = ['f31', 'f32']
        second_nested_methods = ['m31', 'm32']        

        last_nested_fields = ['f41', 'f42']
        last_nested_methods = ['m41', 'm42']        
        last_variable = Variable('s4',
                                 last_nested_fields,
                                 last_nested_methods,
                                 [])
        
        second_nested_variable = Variable('s3',
                                          second_nested_fields,
                                          second_nested_methods,
                                          [])

        first_nested_variable = Variable('s2',
                                         first_nested_fields,
                                         first_nested_methods,
                                         [second_nested_variable,])        

        variable = Variable('s1', fields, methods, [first_nested_variable,
                                                    last_variable])
        
        output_string = variable.to_string()
        self.assertTrue(expected_output == output_string)
        
