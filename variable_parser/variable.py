
def format_list(list):
    string = ''
    for item in list:
        string += '{0},'.format(item)

    string = string[0:len(string)-1] #remove last ','
    return string

class Variable(object):
    def __init__(self, symbol, fields = [], methods = [], variables = []):
        self.symbol = symbol
        self.fields = fields
        self.methods = methods
        self.variables = variables

    def to_string(self):        
        fields_string = format_list(self.fields)
        methods_string = format_list(self.methods)
        variable_string = ''
        
        for variable in self.variables:
            variable_string += variable.to_string()
            variable_string += ','

        if len(variable_string) > 0 and variable_string[-1] == ',':
            variable_string = variable_string[0:-1] #remove last ','
        
        string = '({0};{1};{2};{3})'.format(self.symbol,
                                            fields_string,
                                            methods_string,
                                            variable_string)
            
        return string
