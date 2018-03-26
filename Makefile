
clean:
	-rm *.pyc
	-rm ./variable_parser/*.pyc
	-rm ./test/*.pyc
	-rm ./test/test_package/*.pyc
	-rm ./test/test_package/test_module_b/*.pyc
	-rm ./*~

test:
