

def get_stdin():
    returned_text = ''
    while(True):
        try:
            input = raw_input()
            returned_text += input + '\n'
        except EOFError:
            break
    return returned_text


def main():
    input = get_stdin()
    pass


if __name__ == '__main__':
    main()
