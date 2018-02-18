import sys
import types
import importlib
from variable import Variable

class ImportParser(object):

    def __init__(self):
        self.packages = []

    def parse_package(self, package_name):
        methods = []
        members = []
        
        try:
            importlib.import_module(package_name)

            module = sys.modules[package_name]

            for key, value in module.__dict__.iteritems():
                if key[0] == '_': # if private member, like __copywrite__
                    pass
                
                # TODO: parse classes

                if (isinstance(value, types.FunctionType) or
                    isinstance(value, types.BuiltinFunctionType) or
                    isinstance(value, types.BuiltinMethodType)):
                    methods.append(key)

                else:
                    members.append(key)

            variable = Variable(package_name, members, methods)

            return variable
                    
        except:
            # no retry logic or special error handling
            # TODO: log something
            pass
