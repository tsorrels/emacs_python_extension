delimeters = [
    '.',
    '=',
    '+',
    '/',
    '%',
    '>',
    '<',
    '>',
    '&'

    ]


class Variable(object):
    def __init__(self, symbol, fields = [], methods = []):
        self.symbol = ''
        self.fields = fields
        self.methods = methods

    def to_string(self):

        # use string.format()
        pass 


def is_keyword(word):
    return False

def parse_line(line):
    symbol = ''
    index = 0
    while index < len(line):        
        if line[index] in delimeters:
            break
        else:
            symbol += line[index]
            index ++

    variable = Variable(symbol)
    return variable
    

def parse_input(input):
    variables = []
    index = 0
    line = ''
    while index < len(input):
        if input[index] != '\n':
            line += input[index]
            
        else:
            variable = parse_line(line)
            if variable:
                variables.append(variable)
            line = ''  #reset line variable
    
        index ++

    return variables
            
def get_stdin():
    returned_text = ''
    while(True):
        try:
            input = raw_input()
            returned_text += input + '\n'
        except EOFError:
            break
    return returned_text



def write_output(variables):
    pass


def main():
    input = get_stdin()
    variables = parse_input(input)
    write_output(variables)


if __name__ == '__main__':
    main()
