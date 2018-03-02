import sys
import types
import importlib
from variable import Variable

class ImportParser(object):

    def __init__(self):
        self.packages = []

    def parse_package(self, package_name):
        
        try:
            importlib.import_module(package_name)
            module = sys.modules[package_name]
            variable = parse_complex_type(module)            
            return variable
                    
        except Exception as E:
            # print E
            # no retry logic or special error handling
            # TODO: log something
            pass

    def parse_complex_type(object):
        methods = []
        members = []
        variables = []

        for key, value in module.__dict__.iteritems():
            if key[0] == '_': # if private member, like __copywrite__
                continue

            if (isinstance(value, types.FunctionType) or
                isinstance(value, types.BuiltinFunctionType) or
                isinstance(value, types.BuiltinMethodType)):
                methods.append(key)

            elif hasattr(value, '__dict__'):
                # this is not a function, so must be complex variable type
                complex_object = parse_complex_type(value)
                if complex_object:
                    variables.append(complex_object)
                                        
            else:
                members.append(key)

        variable = Variable(package_name, members, methods, variables)
        
        return variable
    
