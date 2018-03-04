# The autoMATic compiler [![CircleCI](https://circleci.com/gh/ivychen/autoMATic.svg?style=svg&circle-token=54dcd36f8be7646ac05391c2c8762d34d29847e1)](https://circleci.com/gh/ivychen/autoMATic)

COMS4115 PLT | Spring 2018

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

## Additional Syntax to be Added:

1. `elif` statements need to be parsed
2. allow for variable declaration within statements
3. matrix slicing
4. decide on whether to keep array type

## Build environment setup
autoMATic needs the OCaml llvm library, which is most easily installed through opam.

Install LLVM and its development libraries, the m4 macro preprocessor,
and opam, then use opam to install llvm.

The version of the OCaml llvm library must match the version of the LLVM
system installed on your system.

### Ubuntu 16.04

LLVM 3.8 is the default under 16.04. Install the matching version of
the OCaml LLVM bindings:

```
sudo apt install ocaml llvm llvm-runtime m4 opam
opam init
opam install llvm.3.8
eval `opam config env`

make
./test.sh
```

### Installation under Ubuntu 15.10

LLVM 3.6 is the default under 15.10, so we ask for a matching version of the
OCaml library.

```
sudo apt-get install -y ocaml m4 llvm opam
opam init
opam install llvm.3.6 ocamlfind
eval `opam config env`

make
./test.sh
```

### Installation under Ubuntu 14.04

The default LLVM package is 3.4, so we install the matching OCaml
library using opam.  The default version of opam under 14.04 is too
old; we need to use a newer package.

```
sudo apt-get install m4 llvm software-properties-common

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -y opam
opam init

eval `opam config env`

opam install llvm.3.4 ocamlfind
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

   `brew install llvm`

   Take note of where brew places the llvm executables. It will show
   you the path to them under the CAVEATS section of the post-install
   terminal output. For me, they were in /usr/local/opt/llvm/bin. Also
   take note of the llvm version installed. For me, it was 3.6.2.

6. Have opam set up your enviroment:

   `eval `opam config env``

7. Install the OCaml llvm library:

   `opam install llvm.3.6 `

   Ensure that the version of llvm you install here matches the
   version you installed via brew. Brew installed llvm version 3.6.2,
   so I install llvm.3.6 with opam.

   IF YOU HAVE PROBLEMS ON THIS STEP, it's probably because you are
   missing some external dependencies. Ensure that libffi is installed
   on your machine. It can be installed with

   `brew install libffi`

   If, after this, `opam install llvm.3.6` is still not working, try
   running

   `opam list --external --required-by=llvm.3.6`

   This will list all of the external dependencies required by
   llvm.3.6. Install all the dependencies listed by this command.

   IF THE PREVIOUS STEPS DO NOT SOLVE THE ISSUE, it may be a problem
   with using your system's default version of llvm. Install a
   different version of llvm and opam install llvm with that version
   by running:

   ```
   brew install homebrew/versions/llvm37
   opam install llvm.3.7
   ```

   Where the number at the end of both commands is a version different 
   from the one your system currently has.

8. Create a symbolic link to the lli command:

   `sudo ln -s /usr/local/opt/llvm/bin/lli /usr/bin/lli`

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
   point to the absolute path, e.g., `LLI="/usr/local/opt/llvm/bin/lli"`

9. To run and test, navigate to the autoMATic folder. Once there, run

   `make ; ./test.sh`

   autoMATic should build without any complaints and all tests should
   pass.

   IF RUNNING ./test.sh FAILS ON SOME TESTS, check to make sure you
   have symlinked the correct executable from your llvm installation.
   For example, if the executable is named lli-[version], then the 
   previous step should have looked something like:

   `sudo ln -s /usr/local/opt/llvm/bin/lli-3.7 /usr/bin/lli`

   As before, you may also modify the path to lli in test.sh
