import subprocess
import matplotlib
matplotlib.use('TkAgg')  # Use the backend that supports interactive display
import matplotlib.pyplot as plt

def run_perf(perf_command):
    # Run perf command and capture output
    # result = subprocess.run(perf_command.split(), stdout=subprocess.PIPE)
    # output = result.stdout.decode('utf-8')
    perf_output = subprocess.check_output(perf_command, shell=True).decode("utf-8")
    # print(output)
    # return output
    return perf_output

def parse_perf_output(output):
    # Parse perf output to extract relevant data
    print("start parsing")
    data = {}
    lines = output.split('\n')
    for line in lines:
        print(line)
        if line.startswith('#') or line.startswith("["):
            continue
        parts = line.split()
        if len(parts) == 2:
            data[parts[0]] = float(parts[1])
    return data

def plot_perf_data(data):
    # Plot perf data
    labels = data.keys()
    values = data.values()

    plt.bar(labels, values)
    plt.xlabel('Events')
    plt.ylabel('Counts')
    plt.title('Perf Output Visualization')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

if __name__ == "__main__":
    # perf_command = "perf stat -e instructions,cycles ../build/test/test_gc"
    perf_command = "perf stat -e instructions,cycles ls"
    output = run_perf(perf_command)
    data = parse_perf_output(output)
    plot_perf_data(data)
