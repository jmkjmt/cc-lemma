import os
import scripts.config as config
import scripts.util as util
import time
import subprocess
import csv
import glob
import sys

def _get_dataset(dataset_name):
    dataset_path = os.path.join(config.dataset_root, "cvc4", dataset_name)
    return [(name, file, "") for (name, file) in util.collect_benchmarks(dataset_path, ".smt2")]

def _run_cvc(task_name, inp_file, output_path, extra_flag):
    oup_file = os.path.join(output_path, task_name + ".out")
    err_file = os.path.join(output_path, task_name + ".err")
    util.create_path(oup_file)
        
    command = [ 'ulimit -v ' + str(config.memory_limit * 1024 * 1024) + ';', "timeout " + str(config.timeout * 2), config.cvc4_bin_name, config.cvc4_args, inp_file, "--tlimit=" + str(config.timeout * 1000), ">", oup_file, "2>", err_file]
    command = " ".join(command)
    start_time = time.time()

    try:
        result = subprocess.run(command, shell=True, check=True)
    except subprocess.CalledProcessError as e:
        # output is noisy and unhelpful since it's just the error code -
        # should check the .err files instead
        # print(f"Command '{command}' failed with return code {e.returncode}")
        pass
    except KeyboardInterrupt:
        print("\nExecution halted by user")
        sys.exit(0)
    end_time = time.time()

    with open(oup_file, "r") as res:
        line = "\n".join(res.readlines())
    with open(err_file, "r") as err: 
        err_lines = "\n".join(err.readlines())
    line = "".join(filter(lambda char: not char.isspace(), line))

    if "Parse Error" in err_lines:
        print("CVC4 failed in parsing ", os.path.basename(task_name))
    if line == "sat" or line == "unsat":
        return True, end_time - start_time
    return False, None

def _collect_results(output_dir, summary_path, dataset):
    results_file = os.path.join(summary_path, dataset + ".csv")
    util.create_path(results_file)
    with open(results_file, 'w') as f:
        writer = csv.DictWriter(f, ['name', 'result', 'time'])
        writer.writeheader()
        for err_file in glob.glob(output_dir + '/*.err'):
            writer.writerow(create_cvc4_results(err_file))

def create_cvc4_results(err_file):
    with open(err_file) as f:
        lines = [line.rstrip() for line in f.readlines()]
        prop_name = err_file.removesuffix('.err')
        if 'unsat' in lines[-2]:
            sat_unsat_msg, result = lines[-2].split(', ')
            total_time_msg, time = lines[-1].split(', ')
            assert(sat_unsat_msg == 'driver::sat/unsat')
            assert(total_time_msg == 'driver::totalTime')
            return {
                'name': prop_name,
                'result': result == 'unsat',
                'time': float(time),
            }
        else:
            return {
                'name': prop_name,
                'result': False,
                'time': 0.0,
            }

def get_runner():
    return {"dataset": _get_dataset, "runner": _run_cvc, "name": "cvc4", "post": _collect_results}
