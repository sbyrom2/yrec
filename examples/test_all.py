#!/usr/bin/env python
#
# YREC test runner
#   Requires:
#     python 3.x
#     pytest
#     pytest-xdist

import os, sys
from glob import glob
import pytest
import subprocess as sp
import configparser as cfp


config = cfp.ConfigParser()
config.read("tests.conf")
yrec_exe = config['paths']['yrec']
test_dirs = [x for x in config['paths']['test_dirs'].split('\n') if len(x) > 0 ]


def collect_tests(tdirs):
    '''Traverse testing directories and define a test for each applicable pair
    of input/output files.'''
    tcases = []
    for tdir in tdirs:
        curdir = os.getcwd()
        os.chdir(tdir)
        for nml1 in sorted(glob(f'*.nml1')):
            tbase = nml1.replace(".nml1", "")
            test_nml2 = f"{tbase}.nml2"
            if os.path.exists(test_nml2):
                tcases.append([tdir, nml1, test_nml2])
            else:
                dir_nml2 = f"{tdir}.nml2"
                tcases.append([tdir, nml1, dir_nml2])
        os.chdir(curdir)
    return tcases


test_cases = collect_tests(test_dirs)


@pytest.mark.parametrize("tdir,nml1,nml2", test_cases)
def test_yrec(tdir, nml1, nml2):
    '''Process a test case definition consisting of
       (test directory, NML1 file, NML2 file)
    by running YREC executable within the given test directory
    with those two inputs. By default STDOUT and STDERR are displayed
    for any test case failures. '''
    startdir = os.getcwd()

    # Run the executable with the inputs for a given test case.
    os.chdir(tdir)
    proc = sp.run([yrec_exe, nml1, nml2],
            stdout=sp.PIPE,
            stderr=sp.PIPE)
    os.chdir(startdir)
    print(proc.stdout.decode())
    print(proc.stderr.decode())

    # Fail on abnormal termination
    assert proc.returncode == 0, "Program terminated abnormally"

    # If process completed without error code,
    # check for the presence of a reference standard.
    # If no reference standard, copy outputs to
    # test reference standard locadtion and return.
    tbase = nml1.replace(".nml1", "")
    outputs = glob(f"{tdir}/output/{tbase}.*")

    # Fail on missing outputs
    assert len(outputs) > 0, "Missing output(s)"

    # Otherwise, call function here to validate
    # outputs against a known reference.
    #assert identical_to_standard == True

