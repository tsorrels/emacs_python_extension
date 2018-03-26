import test_module_b.test_module_b

class TestModuleA(object):

    def __init__(self):
        self.field_a = 'a'
        self.field_b = 'b'
        self.test_mobule_b = test_module_b.TestModuleB()


    def function_a(self):
	pass

def test_module_a_function():
    return True


test_module_a_variable = 1



test_module_b_object = test_module_b.test_module_b.TestModuleB()
