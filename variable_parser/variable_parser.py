import keyword
import string
from variable import Variable
from import_parser import ImportParser

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
    '#',
    '\t'
    ]

class VariableParser(object):

    def __init__(self):
        self.import_parser = ImportParser()

    def parse_line(self, line):
        index = 0
        last_char_index = len(line) #- 1

        # check for a comment
        try:
            last_char_index = line.index('#')

        except ValueError:
            pass
        # splice off comment
        line = line[index:last_char_index]

        #strip whitespace
        while index < len(line):
            if line[index] in string.whitespace:
                index += 1
            else:
                break

        #check for import statement
        words = line.split()
        if len(words) == 2 and words[0] == 'import':
            variable = self.import_parser.parse_module(words[1])
            return variable

        elif len(words) == 4 and words[0] == 'from' and words[2] == 'import':
            variable = self.import_parser.parse_attribute(words[1], words[3])
            return variable

        
        #build symbol
        symbol = ''
        while index < len(line):        
            if line[index] in delimeters:
                break
            else:
                symbol += line[index]
            index+=1
            
        #symbol = symbol.strip() #remove white space
        if symbol != '' and not keyword.iskeyword(symbol):
            variable = Variable(symbol)
            
        #check for variable assignment
        if '=' in line[index:last_char_index]:
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
                variable = self.parse_line(line)
                if variable:
                    variables.append(variable)
                line = ''  #reset line variable
    
            index+=1

        return variables
