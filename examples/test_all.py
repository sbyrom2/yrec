#!/usr/bin/env python
#
# YREC test runner
#   Requires:
#     python 3.x
#     pytest
#     pytest-xdist

import os, sys
import shutil
from glob import glob
import re
import numbers
import pytest
import subprocess as sp
import configparser as cfp

config = cfp.ConfigParser()
config.read("tests.conf")
yrec_exe = config['paths']['yrec']
tests = [x for x in config['paths']['tests'].split('\n') if len(x) > 0 ]
float_abs_tol = float(config['tolerances']['float_abs_tol'])
int_abs_tol = int(config['tolerances']['int_abs_tol'])

# The directory within each test subdirectory in which the test reference
# standard outputs will be stored.
ref_dir = "standard"

class colors:
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    ENDC = '\033[0m'

def collect_tests(testreqs):
    '''Traverse testing directories and define a test for each applicable pair
    of input/output files.'''
    tcases = []
    for testreq in testreqs:
        curdir = os.getcwd()
        os.chdir(testreq)
        for nml1 in sorted(glob(f'*.nml1')):
            tbase = nml1.replace(".nml1", "")
            test_nml2 = f"{tbase}.nml2"
            if os.path.exists(test_nml2):
                tcases.append([testreq, nml1, test_nml2])
            else:
                dir_nml2 = f"{testreq}.nml2"
                tcases.append([testreq, nml1, dir_nml2])
        os.chdir(curdir)
    return tcases


def vals_from_line(line):
    vals = []
    # Split on whitespace or '-' not found in exponent notation.
    for tok in re.split("\\s+|(?<![Ee])-(?=\\d)", line.strip()):
        try:
            val = float(tok)
            vals.append(val)
        except ValueError as e:
            try:
                val = int(tok)
                vals.append(val)
            except ValueError as e:
                val = tok
                vals.append(val)
    return vals


def compare_filevals(ref_file, out_file, float_tol, int_tol):
    '''Iterate over lines in two files to compare, split lines into tokens and
    compare each token with an integer or floating-point tolerance for numeric
    values or literally, otherwise.'''
    ref = open(ref_file, 'r')
    out = open(out_file, 'r')
    lineno = 1
    files_identical = True

    # Iterate over lines of files
    while True:
        diff_found = False
        locs = []
        try:
            refline = next(ref)
            outline = next(out)
            ref_vals = vals_from_line(refline)
            out_vals = vals_from_line(outline)
        except StopIteration as ex:
            break

        # Compare all values on line
        for i, val in enumerate(ref_vals):
            if val == 'T' or val == 'F':
                if val != out_vals[i]:
                    locs.append(i)
                    diff_found = True
            elif isinstance(val, numbers.Number):
                diff = abs(out_vals[i] - val)
                if type(val) == float and diff > float_tol:
                    locs.append(i)
                    diff_found = True
                if type(val) == int and diff > int_tol:
                    locs.append(i)
                    diff_found = True
                #if diff > tol:
                #    locs.append(i)
                #    diff_found = True
            else:
                if val != out_vals[i]:
                    diff_found = True
                    locs.append(i)

        if diff_found:
            files_identical = False
            print(f"line: {lineno}")
            print(refline, end='')
            print(color_indicator(outline, locs))
        lineno += 1

    ref.close()
    out.close()
    return files_identical


def chunk_indicator(line, indices):
    indicator = list(" " * len(line))
    for i, chunk in enumerate(re.finditer("\\s+|(?<![Ee])-(?=\\d)", line)):
        if i in indices:
            indicator[chunk.start()] = "^"
    indicator = ''.join(indicator)
    return indicator


def color_indicator(line, indices):
    '''For each non-whitespace segment of 'line', highlight the segments
    indicated in the list 'indices' in red, leaving the rest of the line
    unchanged.
    Return the modified line.
    '''
    segs = []
    out = ""
    last = 0
    matches = list(re.finditer("\\s+|(?<![Ee])-(?=\\d)", line))
    breaks = [ x.span() for x in matches ]
    flat = [item for sublist in breaks for item in sublist]
    if flat[0] == 0:
        del flat[0]
    else:
        flat.insert(0, 0)
    flat.append(len(line))
    flat = iter(flat)
    segs = zip(flat, flat)
    for i, seg in enumerate(segs):
        out = f"{out}{line[last:seg[0]]}"
        last = seg[1]
        if i in indices:
            out = f"{out}{colors.RED}"
        out = f"{out}{line[seg[0]:seg[1]]}"
        if i in indices:
            out = f"{out}{colors.ENDC}"
    return out


test_cases = collect_tests(tests)




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

    # For each output, check if a refrence exists.
    # If not, copy output into reference.
    print("--------------------------------------------------------------")
    print("Comparing outputs with reference standards...\n")
    for out in outputs:
        refname = os.path.basename(out)
        ref = f"{tdir}/{ref_dir}/{refname}"
        if not os.path.isfile(ref):
            shutil.copyfile(out, ref)
            print(f"{out} copied to reference")
        # Compare
        print(f"--- Comparing standard {out}\n")
        assert compare_filevals(ref, out, float_abs_tol, int_abs_tol), "Output differs from reference standard"

