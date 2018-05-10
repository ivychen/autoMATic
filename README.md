# The autoMATic compiler [![CircleCI](https://circleci.com/gh/ivychen/autoMATic.svg?style=svg&circle-token=54dcd36f8be7646ac05391c2c8762d34d29847e1)](https://circleci.com/gh/ivychen/autoMATic) [![Coverage Status](https://coveralls.io/repos/github/ivychen/autoMATic/badge.svg?t=PU57Uu)](https://coveralls.io/github/ivychen/autoMATic)

COMS4115 PLT | Spring 2018

Final Report Link: [https://www.overleaf.com/15714794nxkrsxbpvwrn](https://www.overleaf.com/15714794nxkrsxbpvwrn)

## Team.
| Name            | Email                |
| --------------- |:--------------------:|
| Jimmy O'Donnell | jo2474@columbia.edu  |
| Ivy Chen        | ic2389@columbia.edu  |
| Nguyen Chi Dung | ncd2118@columbia.edu |
| Nelson Gomez    | ng2573@columbia.edu  |

## Introduction

Coded in OCaml, autoMATic takes in data types such as: ints, floats,
bools, void, matrix, and string types. Additionally, it allows for
arithmetic, if-else, for, while statements, and user-defined functions.

Programs in our language end in `.ic`.

## To compile and execute:
```
$ make clean
$ make
rm -f *.o
ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 automatic.native
Finished, 16 targets (0 cached) in 00:00:01.
```

## To run test script:

Run `./test.sh`

**Do not** run './testgen.sh'

### Additional tests submitted for Deliverable #4
"Positive" test programs have names that begin with `test_` and "negative" test programs have names that begin with `fail_`; the positive test programs are listed first. Whether or not the feature it tests is new (did not originate in MicroC) is indicated after the test program name. Tests can be found in `/tests`. To run all tests, execute the following command: `sh test.sh`.
1. `test_auto_fun1.ic` (new) tests the `auto` keyword in function declarations. `auto` forces the compiler to determine the return type of the function dynamically.
2. `test_break1.ic` (new) tests the `break` keyword in a while-loop. `break` immediately terminates the innermost loop.
3. `test_scope1.ic` (new) tests the reassignment of a variable in a nested block. This is part of our implementation of lexical scoping.
4. `test_elseif1.ic` tests `else if` control flow, which allows if-else statements to be chained.
5. `test_continue1.ic` (new) tests the `continue` keyword in a for-loop. `continue` skips the remaining statements in the innermost loop and executes the loop again (i.e. re-evaluates the loop condition).
6. `test_scope_redecl.ic` (new) tests whether variables declared in separate blocks (not nested) overwrite or conflict with each other. In lexical scoping, they should not conflict.
7. `test_auto_var.ic` (new) tests the `auto` keyword in variable assignments. `auto` forces the compiler to determine the type of the variable dynamically.
8. `fail_continue_no_loop.ic` (new) ensures that `continue` causes an error when used outside the context of a loop.
9. `fail_dangling_elseif.ic` ensures that `else if` is not used without a preceding `if` block.
10. `fail_redeclared_formal.ic` ensures that the names of formals in function definitions cannot be assigned or reused. 

## Additional Syntax to be Added:

1. `elif` statements need to be parsed
2. allow for variable declaration within statements
3. matrix slicing
4. decide on whether to keep array type

## Build environment setup
autoMATic needs the OCaml llvm library, specifically version **5.0**, which
is most easily installed through opam.

Install LLVM-5.0 and its development libraries, the m4 macro preprocessor,
and opam, then use opam to install llvm.

The version of the OCaml llvm library must match the version of the LLVM
system installed on your system.

### Ubuntu 16.04, 17.10, 18.04 and Debian 9

Install the matching version of the OCaml LLVM bindings:

```
sudo apt install ocaml llvm-5.0 llvm-5.0-runtime m4 opam
opam init
opam install llvm.5.0
eval `opam config env`

make
./test.sh
```

### Installation under OS X

1. Install Homebrew:

   `ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

2. Verify Homebrew is installed correctly:

   `brew doctor`

3. Install opam:

   `brew install opam`

4. Set up opam:

   `opam init`

5. Install llvm:

   `brew install llvm-5.0`

   Take note of where brew places the llvm executables. It will show
   you the path to them under the CAVEATS section of the post-install
   terminal output. For me, they were in /usr/local/opt/llvm/bin. Also
   take note of the llvm version installed.

6. Have opam set up your enviroment:

   `eval `opam config env``

7. Install the OCaml llvm library:

   `opam install llvm.5.0`

   Ensure that the version of llvm you install here matches the
   version you installed via brew. Brew should install llvm version 5.0,
   so install llvm.5.0 with opam.

   IF YOU HAVE PROBLEMS ON THIS STEP, it's probably because you are
   missing some external dependencies. Ensure that libffi is installed
   on your machine. It can be installed with

   `brew install libffi`

   If, after this, `opam install llvm.5.0` is still not working, try
   running

   `opam list --external --required-by=llvm.5.0`

   This will list all of the external dependencies required by
   llvm.5.0. Install all the dependencies listed by this command.

   IF THE PREVIOUS STEPS DO NOT SOLVE THE ISSUE, it may be a problem
   with using your system's default version of llvm. Install a
   different version of llvm and opam install llvm with that version
   by running:

   ```
   brew install homebrew/versions/llvm37
   opam install llvm.5.0
   ```

   Where the number at the end of both commands is a version different 
   from the one your system currently has.

8. Create a symbolic link to the lli command:

   `sudo ln -s /usr/local/opt/llvm/bin/lli-5.0 /usr/bin/lli`

   Create the symlink from wherever brew installs the llvm executables
   and place it in your bin. From step 5, I know that brew installed
   the lli executable in the folder, /usr/local/opt/llvm/bin/, so this
   is where I symlink to. Brew might install the lli executables in a
   different location for you, so make sure you symlink to the right
   directory.

   IF YOU GET OPERATION NOT PERMITTED ERROR, then this is probably a
   result of OSX's System Integrity Protection. 

   One way to get around this is to reboot your machine into recovery
   mode (by holding cmd-r when restarting). Open a terminal from
   recovery mode by going to Utilities -> Terminal, and enter the
   following commands:

   ```
   csrutil disable
   reboot
   ```
   
   After your machine has restarted, try the `ln....` command again,
   and it should succeed.

   IMPORTANT: the prevous step disables System Integrity Protection,
   which can leave your machine vulnerable. It's highly advisable to
   reenable System Integrity Protection when you are done by 
   rebooting your machine into recovery mode and entering the following
   command in the terminal:

   ```
   csrutil enable
   reboot
   ```

   Another solution is to update your path, e.g.,

   `export PATH=$PATH:/usr/local/opt/llvm/bin`

   A third solution is to modify the definition of LLI in test.sh to
   point to the absolute path, e.g., `LLI="/usr/local/opt/llvm/bin/lli-5.0"`

9. To run and test, navigate to the autoMATic folder. Once there, run

   `make ; ./test.sh`

   autoMATic should build without any complaints and all tests should
   pass.

   IF RUNNING ./test.sh FAILS ON SOME TESTS, check to make sure you
   have symlinked the correct executable from your llvm installation.
   For example, if the executable is named lli-[version], then the 
   previous step should have looked something like:

   `sudo ln -s /usr/local/opt/llvm/bin/lli-5.0 /usr/bin/lli`

   As before, you may also modify the path to lli in test.sh
