from variable import Variable

delimeters = [
    '.',
    '=',
    '+',
    '/',
    '%',
    '>',
    '<',
    '>',
    '&',
    ' ',
    '\t'
    ]
import string

def is_keyword(word):
    return False


class VariableParser(object):

    def __init__(self):
        pass

    def parse_line(self, line):
        index = 0
        #strip whitespace
        while index < len(line):
            if line[index] in string.whitespace:
                index += 1
            else:
                break
        

        symbol = ''
        while index < len(line):        
            if line[index] in delimeters:
                break
            else:
                symbol += line[index]
            index+=1

        #symbol = symbol.strip() #remove white space
        if symbol != '':
            variable = Variable(symbol)
            return variable

        return None
    

    def parse_input(self, input):
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
    
        index+=1

        return variables
            
