Quick Start
===========

Why the *Yale Rotating Evolution Code*? YREC is an efficient, high-performance stellar evolution code designed for precise modeling of stars ranging in mass from the brown dwarf limit to high-mass stars. Its modern treatment of microphysics, magnetic and rotational phenomena, internal composition, and transport processes give it flexibility in handling a wide range of astrophysically relevant phenomena.

To run YREC you need to first build it, link it with the appropriate input physics tables, and provide a starting model. It is important to note that you must choose a starting model from the library of solutions, as YREC is a relaxation code. Changes to starting models are processed with a “rescaling” option.

## Installation

To begin with the installation process, you need to obtain a copy of the source files. The easiest way is to clone the repository:

```
git clone https://github.com/yreclab/yrec.git
```

However, the repository can also be downloaded by clicking on the `Code` button on the Github page: [https://github.com/yreclab/yrec](https://github.com/yreclab/yrec).

```{admonition} Downloading a stable release
:class: note
:name: downloading-stable
Downloading the most recent version of YREC to start out with may not always be the best idea. Although YREC developers do try to keep the repository running smoothly, breaking changes may occur to the main repository at times; changes may also occur to the code to make the documentation obsolete. In order to prevent this, consider downloading a stable release of YREC from the following url: [https://github.com/yreclab/yrec/releases](https://github.com/yreclab/yrec/releases).
```

Once you have obtained a copy of the source files, enter the directory by doing `cd yrec`.

### Setup

YREC is implemented in the Fortran 77 programming language. To proceed with
compilation, you will need either the GNU Fortran (`gfortran`) or Intel Fortran
(`ifort`) compiler. The Intel compiler suite is a commercial product and may be
available at your institution. `gfortran` is a freely available Fortran
compiler that is available for many systems and will remain the focus of this
installation procedure.


#### Install gfortran using your operating system's package manager.

##### Linux

For instance, with a Debian-based operating system, try

```
sudo apt-get install gfortran
```

##### Mac OS

```
xcode-select --install
```

##### Windows

YREC can be built with gfortran on Windows by intstalling your Linux distribution of choice through the Windows Subsystem for Linux (WSL), and then following the Linux or mamba procedure to get the compiler.
Information on setting up WSL may be found here: [https://learn.microsoft.com/en-us/windows/wsl/install](https://learn.microsoft.com/en-us/windows/wsl/install)

#### Linux, Mac OS, or Windows - Install gfortran using conda/mamba

If you are unable to or do not want to use the above installation method for your platform, you can use an alternative package manager called 'mamba', which works on all supported operating systems.
Install miniforge in order to obtain the mamba package/environment manager.
Download from here, [https://conda-forge.org/download](https://conda-forge.org/download) and follow the installation instructions provided.

Then,
```
mamba create -n YREC gfortran
```
which will create a new environment called 'YREC' and install the gfortran compiler within it.
Activate this environment to make the compiler available for use and verify that the compiler is usable by having it display its version.

```
mamba activate YREC
gfortran --version
```

### Building YREC

```{admonition} Code Location
:class: seealso
:name: code-location
The code can be found in `src/` (all files ending in `.f`, e.g. `*.f`.) They are assembled into an executable by using GNU Make which is controlled by the supplied `Makefile`. This will require a Fortran compiler on your machine. The name of the executable can be adjusted, or the location of it moved to your working directory.
```

Enter the `yrec/src` directory by doing `cd src`, then run the `make` command. It will create a `yrec` binary in the current directory.

This binary can then be moved to another directory, if you like, and called using `./yrec file.nml1 file.nml2`.

To install yrec to ~/bin, run the following command:

```
make && make install PREFIX=~/bin
```

If `~/bin` has been added to your $PATH, you should now be able to call yrec from within any working directory by typing `yrec`.

You can also run it without a PREFIX specified, using:

```
make; sudo make install
```

Doing this will install to `/usr/local/bin/yrec`, so on a multi-user system anyone can call `yrec` from any directory without modifying the $PATH variable.

## Setting up a run

Each YREC run requires two namelists to be defined: the control namelist (`.nml1`), and the physics namelist (`.nml2`). The control namelist includes input/output data and global run information. The physics namelist includes microphysics and numerical parameters. To perform a run with `filename_A.nml1` and `filename_B.nml2`, simply run:

```
yrec filename_A.nml1 filename_B.nml2
```

This is assuming that `yrec` is on your $PATH. If it is instead in your current directory, try:

```
./yrec filename_A.nml1 filename_B.nml2
```

Replace `./yrec` with the relative or absolute directory of the YREC executable that you compiled in the prior step.

### Paths for input and output

In order to read in the relevant microphysics tables, YREC needs to know where the tables are stored.

Currently, the paths defined in the control namelist (`.nml1`) are hardcoded as relative paths. This assumes a directory filestructure and a location for the `input/` folder and the location of where the output files are to go (usually in `output/`).

When namelist files are arbitrarily moved, often the input folder will no longer be present at the location that the namelist expects, and it will error as such:

```
Fortran runtime error: Cannot open file '../../input/eos/opal2006/EOSOPAL06Z0.016492': No such file or directory
```

An easy fix to this solution is to place the `input/` folder at a known location on your computer, then replace the relative paths to the input folder with absolute paths, for instance `/home/user/yrec/input/`.

If you get an error like this:
```
Fortran runtime error: Cannot open file 'output/Test_*.store': No such file or directory
```

Then please check and make sure the path exists. If the folder `output` does not exist, the file will not run, so `mkdir output` in this case.

Before starting a run, please make sure your namelists point to the correct input and output directories that you expect!

### Starting model

The other prerequisite for starting a YREC run is to define the starting model, which is pointed to by the `FFIRST` variable in the control namelist (`.nml1`). Suitable models can be found in the `yrec/input/models` directory, under subfolders. `seed` models are original legacy models. `start` models are initialized high up on the Hayashi track, making them a useful starting point for large changes in mass or composition. `dbl` models are models where p+d fusion has started on the Deuterium burning birthline. This is the recommended starting point for most calculations. Scripts for generating these models are found in the `yrec/examples` folders.

Some runs (particularly up the giant branch) may also benefit from starting on the terminal age main sequence as well. Some selected models for the test suites have been saved in the `tams` folder as well.



```{admonition} Rescaling models
:class: seealso
:name: rescaling
Changes to starting models are processed with a "rescaling" option in the control namelist, `KINDRN(i) = 3`. For more details see the [control namelist](namelist_control.md).
```

## Running the test suite

There are existing runs packaged with YREC in the `testsuite/` and `examples/` directories. These include a variety of different test cases which span a wide range in mass and age. To run, simply open a terminal at the folder and follow the instructions in the README to feed in the appropriate `nml1` and `nml2` files. These files are also documented in the [test suite](testsuite.md) part of the documentation.

```{admonition} Namelist file locations
:class: caution
:name: namelist-paths
The example templates have all their paths set relative to the directory structure, rather than as an absolute path. If you are designing your own namelists in a custom directory structure, make sure to double check that your paths in your `.nml1` file are set correctly. It may be helpful to place the `input/` folder in a known location on the filesystem, and use an absolute path to point to it.
```

### Understanding the output

YREC produces multiple output files, which can be parsed and plotted. The nature of these output files are as follows:

| Filetype     | Description    |
| ------------ | -------------- |
| `.track`      | A stellar model track, recording general model information at each individual timestep in the YREC run. It does not record the full interior structure of the star at each step; rather, it presents observables such as the central temperature and pressure, or the stellar luminosity, at each timestep. |
| `.last`      | The last converged stellar structure model computed, readable by YREC. The file format is the same as that of the starting model. |
| `.short`      | Detailed numerical information about the model as it evolves. Can be set to output frequently, for instance when `LPULSE=.TRUE.` |
| `.store`      | Stored model snapshots over the course of a run. |
| `.pmod`      | Pulsation output for the interior model. |
| `.penv`      | Pulsation output for the envelope model. |
| `.atm`      | Pulsation output for the atmosphere model. |
| `.full`      | Deprecated detailed model structure output, formatted. |
| `.excomp`      | Deprecated composition output over time, moved to .track. |

### Scripting

While the YREC repository itself doesn't have an "official" set of scripts, many scripts and analysis notebooks have been contributed by the community. Some examples include:

- [YREC User Tools](https://github.com/sbyrom2/yrec_user_tools): Functions and guidelines for using the Yale Rotating Evolution Code 
