echo "-----------------------COMPILING PS1 & PS1_TEST---------------------------"
cs3110 compile ps1.ml
cs3110 compile ps1_test.ml
echo "-------------------------RUNNING UNIT TESTS NOW---------------------------"
cs3110 test ps1_test.ml
