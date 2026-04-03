import subprocess
import os
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import sys
from threading import Lock
import re

ROOT = "./"
BENCHMARK = ["dilemma-bench"]


def run_single_file(cmd, time_budget):
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=time_budget if time_budget > 0 else None,
        )
        value = None
        contents = result.stdout.strip()
        if result.returncode != 0:
            print("stderr: " + result.stderr.strip())
        for line in contents.split("\n"):
            if "uncyclic:" not in line:
                continue
            line = line.split(":")[1].strip()
            value = line.split("(")[0].strip()

        return value if value is not None else "NoResult"

    except subprocess.TimeoutExpired:
        return "Timeout"


def run_benchmark(extra_flags, selected_benchmarks, time_budget, threads):
    results = {}
    results_lock = Lock()
    print(extra_flags)

    for benchmark in selected_benchmarks:
        results[benchmark] = {}

    jobs = []  # (benchmark, group, file, file_path)

    for benchmark in selected_benchmarks:
        benchmark_path = os.path.join(ROOT, benchmark)

        # dilemma-bench / optimization 은 그룹 존재
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

    # 이제 스레드 풀로 병렬 실행
    with ThreadPoolExecutor(max_workers=threads) as executor:
        future_map = {}
        for benchmark, group, file, file_path in jobs:
            print(
                f"▶ Running benchmark: {benchmark}/{group + '/' if group else ''}{file}"
            )

            cmd = [
                "./target/release/cc-lemma",
                file_path,
            ] + extra_flags

            future = executor.submit(run_single_file, cmd, time_budget)
            future_map[future] = (benchmark, group, file)

        for future in as_completed(future_map):
            benchmark, group, file = future_map[future]
            result_value = future.result()

            with results_lock:
                if group is None:
                    # no group
                    results[benchmark][file] = result_value
                else:
                    results[benchmark][group][file] = result_value

    return results

priority = {
    "ta": 0,
    "sol": 1,
}

def parse_token(token):
    m = re.match(r'([a-zA-Z]+)(\d+)', token)
    prefix, num = m.group(1), int(m.group(2))
    return (priority[prefix], num)

def sort_key(name):
    core = name.rsplit('.', 1)[0]      # 확장자 제거
    tokens = core.split('-')           # '-' 기준 분리
    return [parse_token(t) for t in tokens]

def config_name(allow_imply_lemma, no_pattern):
    if allow_imply_lemma and not no_pattern:
        return "full"
    elif allow_imply_lemma:
        return "only_imply"
    elif not no_pattern:
        return "only_pattern"
    else:
        return "baseline"
    
def print_benchmark_result(all_results, benchmark_name, out):
    print(f"\n--- {benchmark_name} ---", file=out)
    if benchmark_name in ("dilemma-bench", "optimization"):
        all_results[benchmark_name] = dict(sorted(all_results[benchmark_name].items(), key=lambda x: x[0]))
        for group in all_results[benchmark_name]:
            print(f"\n  >> Group: {group}", file=out)
            if benchmark_name == "optimization":
                # optimization 은 파일 이름 정렬 방식이 다름
                sorted_files = sorted(all_results[benchmark_name][group].keys())
            else:
                sorted_files = sorted(all_results[benchmark_name][group].keys(), key=sort_key)
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
                
                
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run dilemma benchmarks")
   
    parser.add_argument(
        "--benchmarks",
        type=str,
        nargs="+",
        choices=BENCHMARK,
        default=BENCHMARK,
        help="Select benchmarks to run",
    )
    parser.add_argument(
        "--time",
        type=int,
        default=0,
        help="Time budget for each theorem in seconds (0 means no limit)",
    )
    parser.add_argument(
        "--threads",
        type=int,
        default=1,
        help="Number of threads to run benchmarks in parallel",
    )
   
    args = parser.parse_args()

    extra_flags = ["--no-generalization", "--exclude-bid-reachable", "--saturate-only-parent", "--no-destructive-rewrites"]
    time_budget = args.time if args.time else 0
    threads = max(1, args.threads)              
    all_results = run_benchmark(extra_flags, args.benchmarks, time_budget, threads)
     
    print_summary(all_results, None)

        
