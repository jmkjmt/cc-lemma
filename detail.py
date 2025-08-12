import os

path = "results/cclemma/optimization"

if os.path.exists(path):
    dir_list = os.listdir(path)
    result = { }
    for file in dir_list:
        if file.endswith(".csv"):
            file_path = os.path.join(path, file)
            with open(file_path, 'r') as f:
                content = f.read()
                if "Valid" in content:
                    result[file] = "Success"
                else:
                    result[file] = "Fail"
    for key in sorted(result.keys()):
        print(f"{key}: \t {result[key]}")