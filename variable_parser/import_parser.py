import sys
import types
import importlib
from variable import Variable

class ImportParser(object):

    def __init__(self):
        self.packages = []


    def parse_attribute(self, module_name, member_name):
        try:
            #full_path = module_name + '.' + member_name
            importlib.import_module(module_name)
            module = sys.modules[module_name]
            attribute = module.__dict__[member_name]

            #symbol_table = globals()
            #attribute = symbol_table[member_name]
            #module = sys.modules[module_name]
            variable = self.parse_complex_type(member_name,
                                               attribute,
                                               [])
            return variable
                    
        except Exception as E:
            # print E
            # no retry logic or special error handling
            # TODO: log something
            pass
        
    def parse_module(self, module_name):        
        try:
            importlib.import_module(module_name)
            module = sys.modules[module_name]
            variable = self.parse_complex_type(module_name, module, [])
            return variable
                    
        except Exception as E:
            # print E
            # no retry logic or special error handling
            # TODO: log something
            pass

    def parse_complex_type(self, symbol, obj, being_parsed):
        methods = []
        members = []
        variables = []

        dictionary = None
        
        try:
            dictionary = getattr(obj, '__dict__')

        except AttributeError:            
            return Variable(symbol, members, methods, variables)
        
        for key, value in dictionary.iteritems():
            if key[0] == '_': # if private member, like __copywrite__
                continue
            
            if (isinstance(value, types.FunctionType) or
                isinstance(value, types.BuiltinFunctionType) or
                isinstance(value, types.BuiltinMethodType)):
                methods.append(key)

            elif hasattr(value, '__dict__'):
                # this is not a function, so must be complex variable type
                #print key, value
                complex_object = None
                if value not in being_parsed:
                    being_parsed.append(value)
                    complex_object = self.parse_complex_type(key,
                                                             value,
                                                             being_parsed)
		    being_parsed = being_parsed[:-1]
                else:
                    pass
                    #print "Found circular reference for " + str(value)

                if complex_object:
                    variables.append(complex_object)
                                        
            else:
                members.append(key)

        variable = Variable(symbol, members, methods, variables)
        
        return variable
    
