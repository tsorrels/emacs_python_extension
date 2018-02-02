import sys
from variable_parser import VariableParser



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
    for variable in variables:
        sys.stdout.write(variable.to_string() + '\n')


def main():
    variable_parser = VariableParser()
    input = get_stdin()
    variables = variable_parser.parse_input(input)
    write_output(variables)


if __name__ == '__main__':
    main()
