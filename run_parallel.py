import subprocess
import os
import argparse
from concurrent.futures import ProcessPoolExecutor, as_completed
import sys
import re
import psutil
import time
import threading

ROOT = "./"
BENCHMARK = ["dilemma-bench"]

# ---------------------------
# Memory Control Utilities
# ---------------------------

def kill_tree(pid):
    try:
        parent = psutil.Process(pid)
        children = parent.children(recursive=True)

        for p in children:
            try:
                p.kill()
            except psutil.NoSuchProcess:
                pass

        parent.kill()
        psutil.wait_procs([parent] + children, timeout=1)

    except psutil.NoSuchProcess:
        pass


def monitor_memory(pid, limit_bytes, stop_event, result):
    try:
        proc = psutil.Process(pid)
    except psutil.NoSuchProcess:
        return

    while not stop_event.is_set():
        try:
            rss = proc.memory_info().rss
            for c in proc.children(recursive=True):
                try:
                    rss += c.memory_info().rss
                except psutil.NoSuchProcess:
                    pass

            if rss > limit_bytes:
                result["memout"] = True
                kill_tree(pid)
                stop_event.set()
                return

        except psutil.NoSuchProcess:
            return

        time.sleep(0.2)


# ---------------------------
# Execution
# ---------------------------

def run_single_file(cmd, time_budget, mem_limit_gb):
    proc = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        stdin=subprocess.DEVNULL,
        text=True,
        bufsize=1,
        start_new_session=True,
    )

    result = {
        "memout": False,
        "timeout": False,
    }

    stop_event = threading.Event()

    # memory watchdog
    if mem_limit_gb > 0:
        t = threading.Thread(
            target=monitor_memory,
            args=(proc.pid, mem_limit_gb * 1024**3, stop_event, result),
            daemon=True,
        )
        t.start()
    else:
        t = None

    try:
        stdout, stderr = proc.communicate(
            timeout=time_budget if time_budget > 0 else None
        )

    except subprocess.TimeoutExpired:
        result["timeout"] = True
        kill_tree(proc.pid)
        stdout, stderr = proc.communicate()

    finally:
        stop_event.set()
        if t:
            t.join(timeout=1)

    # status priority
    if result["memout"]:
        return "Memout"
    if result["timeout"]:
        return "Timeout"

    value = None
    contents = stdout.strip()

    if proc.returncode != 0:
        print("stderr:", stderr.strip())

    for line in contents.split("\n"):
        if "uncyclic:" not in line:
            continue
        line = line.split(":")[1].strip()
        value = line.split("(")[0].strip()

    return value if value is not None else "NoResult"


# ---------------------------
# Benchmark Runner
# ---------------------------

def run_benchmark(extra_flags, selected_benchmarks, time_budget, threads, mem_limit):
    results = {}

    for benchmark in selected_benchmarks:
        results[benchmark] = {}

    jobs = []

    for benchmark in selected_benchmarks:
        benchmark_path = os.path.join(ROOT, benchmark)

        if benchmark in ("dilemma-bench", "optimization"):
            for group in os.listdir(benchmark_path):
                group_path = os.path.join(benchmark_path, group)
                if not os.path.isdir(group_path):
                    continue

                if group not in results[benchmark]:
                    results[benchmark][group] = {}

                for file in os.listdir(group_path):
                    if file.endswith(".dil"):
                        file_path = os.path.join(group_path, file)
                        jobs.append((benchmark, group, file, file_path))
        else:
            for file in os.listdir(benchmark_path):
                if file.endswith(".dil"):
                    file_path = os.path.join(benchmark_path, file)
                    jobs.append((benchmark, None, file, file_path))

    # Process-based parallelism
    with ProcessPoolExecutor(max_workers=threads) as executor:
        future_map = {}

        for benchmark, group, file, file_path in jobs:
            print(f"▶ Running: {benchmark}/{group + '/' if group else ''}{file}")

            cmd = [
                "./target/release/cc-lemma",
                file_path,
            ] + extra_flags

            future = executor.submit(
                run_single_file,
                cmd,
                time_budget,
                mem_limit,
            )

            future_map[future] = (benchmark, group, file)

        for future in as_completed(future_map):
            benchmark, group, file = future_map[future]
            result_value = future.result()

            print(f"✔ Done: {benchmark}/{group + '/' if group else ''}{file} → {result_value}")

            if group is None:
                results[benchmark][file] = result_value
            else:
                results[benchmark][group][file] = result_value

    return results


# ---------------------------
# Sorting / Printing
# ---------------------------

priority = {"ta": 0, "sol": 1}

def parse_token(token):
    m = re.match(r"([a-zA-Z]+)(\d+)", token)
    prefix, num = m.group(1), int(m.group(2))
    return (priority[prefix], num)

def sort_key(name):
    core = name.rsplit(".", 1)[0]
    tokens = core.split("-")
    return [parse_token(t) for t in tokens]


def print_benchmark_result(all_results, benchmark_name, out):
    print(f"\n--- {benchmark_name} ---", file=out)

    if benchmark_name in ("dilemma-bench", "optimization"):
        all_results[benchmark_name] = dict(
            sorted(all_results[benchmark_name].items(), key=lambda x: x[0])
        )

        for group in all_results[benchmark_name]:
            print(f"\n  >> Group: {group}", file=out)

            sorted_files = sorted(
                all_results[benchmark_name][group].keys(), key=sort_key
            )

            for file in sorted_files:
                r = all_results[benchmark_name][group][file]
                print(f"{file} \t {r}", file=out)

    else:
        sorted_files = sorted(all_results[benchmark_name].keys())
        for file in sorted_files:
            r = all_results[benchmark_name][file]
            print(f"{benchmark_name}/{file} \t {r}", file=out)


def print_summary(all_results, file_name):
    for benchmark in all_results:
        if file_name:
            with open(f"results/{file_name}/{benchmark}.res", "w") as out:
                print_benchmark_result(all_results, benchmark, out)
        else:
            print_benchmark_result(all_results, benchmark, sys.stdout)


# ---------------------------
# Main
# ---------------------------

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run dilemma benchmarks")

    parser.add_argument(
        "--benchmarks",
        type=str,
        nargs="+",
        choices=BENCHMARK,
        default=BENCHMARK,
    )

    parser.add_argument("--time", type=int, default=0)

    parser.add_argument("--threads", type=int, default=1)

    parser.add_argument(
        "--mem-limit",
        type=int,
        default=0,
        help="Memory limit per process in GB (0 means no limit)",
    )

    args = parser.parse_args()

    extra_flags = [
        "--no-generalization",
        "--exclude-bid-reachable",
        "--saturate-only-parent",
        "--no-destructive-rewrites",
    ]

    time_budget = args.time if args.time else 0
    threads = max(1, args.threads)
    mem_limit = args.mem_limit

    all_results = run_benchmark(
        extra_flags,
        args.benchmarks,
        time_budget,
        threads,
        mem_limit,
    )

    print_summary(all_results, None)