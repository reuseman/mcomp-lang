import os
import subprocess

# variable with path of test
TEST = "test/samples/"

# array that contains all the files with extensions .mc in the test folder
FILES = [f for f in os.listdir(TEST) if f.endswith(".mc")]

# counter of all the successfull tests
success = 0
# length of files
total = len(FILES)

# counter of tests
current_test = 0

RED = "\033[0;31m"
GREEN = "\033[0;32m"
NC = "\033[0m"

menhir_errors = 0


def is_error(line):
    return (
        "*** Error at line" in line
        or "Error during linking" in line
        or "Fatal error: exception Mcomp_lang.Parser.MenhirBasics.Error" in line
    )

def is_success(line):
    return (
        "Parsing succeded!" in line
        or "Type-check succeded!" in line
        or "Linking succeded!" in line
    )


# loop through all the files in the test folder
for f in FILES:
    # if file begins with "fail-link" then skip the test
    current_test += 1
    out = subprocess.run(
        ["dune", "exec", "bin/mcompc.exe", "--", "-l", TEST + f],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    ).stdout.decode("utf-8")

    # if file starts with "test-" then it must succeed, if it starts with "fail-" then it must fail
    if f.startswith("test-"):
        # check if the output of the test contains "Parsing succeded!"
        if is_success(out):
            print(f"{current_test} - TEST {f}: {GREEN}OK{NC}")
            success += 1
        else:
            print("-----------------------------")
            print(f"{current_test} - TEST {f}: {RED}FAIL!{NC}")
            print(out)
            print("-----------------------------")

    elif f.startswith("fail-"):
        # check if the output out contains "*** Error at line"
        if is_error(out):
            print(f"{current_test} - TEST {f}: {GREEN}OK{NC}")
            success += 1
        else:
            print("-----------------------------")
            print(f"{current_test} - TEST {f}: {RED}FAIL!{NC}")
            print(out)
            print("-----------------------------")

# Print the total number of tests passed
print(GREEN + "Test " + str(success) + "/" + str(total) + " passed" + NC)
