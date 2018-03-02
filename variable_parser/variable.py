
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

    def to_string(self, write_variables = False):        
        fields_string = format_list(self.fields)
        methods_string = format_list(self.methods)

        if write_variables:
            string = '{0};{1};{2};{3}'.format(self.symbol,
                                              fields_string,
                                              methods_string)
        else:
            string = '{0};{1};{2}'.format(self.symbol,
                                              fields_string)
            
        return string
