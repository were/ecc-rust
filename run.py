import os
import subprocess

def parse_time(raw):
    for line in raw.split('\n'):
        if line.startswith('Exec time: '):
            res = line[11:]
            coef = 1 if res.endswith('ms') else 1000
            return float(res.rstrip('ms')) * coef

def run_cmd(cmd):
    return subprocess.check_output(cmd, shell=True).decode('utf-8')

def average(lst):
    return sum(lst) / len(lst)

n = 10
prefix = '../tests/performance/'
for f in sorted(os.listdir('../tests/performance/')):
    myown = [parse_time(run_cmd(f'./test.sh {prefix}/{f} --opt 2 2>&1')) for i in range(n)]
    myown = average(myown)
    myown = '%.2f' % myown
    emcc = [parse_time(run_cmd(f'./test.sh {prefix}/{f} --backend emcc --opt 0 2>&1')) for i in range(n)]
    emcc = average(emcc)
    emcc = '%.2f' % emcc
    emcc2 = [parse_time(run_cmd(f'./test.sh {prefix}/{f} --backend emcc --opt 2 2>&1')) for i in range(n)]
    emcc2 = average(emcc2)
    emcc2 = '%.2f' % emcc2
    print(f, '%s %s %s' % (myown, emcc, emcc2))
    # print(f, '%.2f %.2f' % (myown, emcc))
