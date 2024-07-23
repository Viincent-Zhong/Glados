#!/bin/bash

EXIT_FAILURE=1
EXIT_SUCESS=0

TEST_NBR=0
TEST_SUCESS=0
TEST_ERROR=0

DIR="test/Test_Files/"

check_executable() {
    if [ -e "./glados" ]; then
        echo "Glados executable found, functional tests starting..."
    else
        echo "Glados executable not found"
        exit $EXIT_FAILURE
    fi
}

test() {
    result=$(./glados "$DIR$1");

    let TEST_NBR+=1
    if [[ $result == $2 ]]; then
        echo -e "$3: [\033[32m✔\033[0m]"
        let TEST_SUCESS+=1
    else
        echo -e "$3: [\033[31m✘\033[0m]"
        let TEST_ERROR+=1
    fi
}

check_executable

test "test_example.scm" "10" "Sample test"


echo -e "\nSimple Test"
# *
test "Simple/mult_p.scm" "42" "21 * 2"
test "Simple/mult_n.scm" "-42" "-21 * 2"
test "Simple/mult_z.scm" "0" "0 * 2"


# div
test "Simple/div_p.scm" "42" "84 / 2"
test "Simple/div_n.scm" "-42" "-84 / 2"
test "Simple/div_z.scm" "0" "0 / 2"


# mod
test "Simple/mod_p.scm" "4" "84 % 5"
test "Simple/mod_n.scm" "1" "-84 % 5"
test "Simple/mod_z.scm" "0" "0 % 2"


# +
test "Simple/add_p.scm" "42" "40 + 2"
test "Simple/add_n.scm" "-42" "-44 + 2"


# -
test "Simple/sous_p.scm" "42" "44 - 2"
test "Simple/sous_n.scm" "-42" "-40 - 2"





echo -e "\nSubject Test"
test "Subject/factorial.scm" "3628800" "Factorial 10"
test "Subject/foo.scm" "42" "21 * 2"
test "Subject/error.scm" "\"foo is undefined\"" "Error no variable"
test "Subject/call.scm" "5" "Just a call (div 10 2)"
test "Subject/lambda2.scm" "3" "((lambda (a b) (+ a b)) 1 2)"
test "Subject/lambda3.scm" "7" "Lambda 3: create lambda + add"
test "Subject/function1.scm" "7" "Create add function"
test "Subject/if1.scm" "1" "(if #t 1 2)"
test "Subject/if2.scm" "2" "(if #f 1 2)"
test "Subject/if3.scm" "21" "Define if >"
test "Subject/builtins1.scm" "11" "(+ (* 2 3) (div 10 2))"
test "Subject/fibonacci.scm" "6765" "Fib 20"

test "Sugar/exec_add.clj" "2" "((print x) = x) (x = 1) (print (x + 1))"
test "Sugar/exec_factorial.clj" "3628800" "((fact x) = (if (x eq? 1) 1 (x * (fact (x - 1))))) (fact 10)"
test "Sugar/exec_fibonacci.clj" "6765" "((fib-it a b n) = (if (n < 1) a (fib-it b (a + b) (n - 1)))) ((fib n) = (fib-it 0 1 n)) (fib 20)"
test "Sugar/exec_sub.clj" "0" "((print x) = x) (x = 1) (print (x - 1))"
test "Sugar/lambda.clj" "21" "(import test/Test_Files/Sugar/print.clj) (print_sum = (lambda (a b) (print (a + b)))) (print_sum 10 11)"
test "Sugar/main.clj" "false" "(import test/Test_Files/Sugar/print.clj) (x = 3000) (if ((x % 4) eq? 3) (print true) (print false))"
test "Sugar/print_int.clj" "10" "(import test/Test_Files/Sugar/print.clj) (print 10)"
# test "Subject/builtins2.scm" "#t" "(+ (* 2 3) (div 10 2))"
# test "Subject/builtins3.scm" "#f" "(+ (* 2 3) (div 10 2))"




echo -e "\n\n$TEST_NBR: [\033[32m$TEST_SUCESS\033[0m]/[\033[31m$TEST_ERROR\033[0m] ($((100 * $TEST_SUCESS / $TEST_NBR))%)"


if [ $TEST_SUCESS -eq $TEST_NBR ]; then
    exit $EXIT_SUCESS
else
    exit $EXIT_FAILURE
fi