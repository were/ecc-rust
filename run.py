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


n = 10
prefix = '../tests/performance/'
for f in sorted(os.listdir('../tests/performance/')):
    myown = sum(parse_time(run_cmd(f'./test.sh {prefix}/{f} 2>&1')) for i in range(n)) / n
    emcc = sum(parse_time(run_cmd(f'./test.sh {prefix}/{f} emcc 2>&1')) for i in range(n)) / n
    emcc2 = sum(parse_time(run_cmd(f'./test.sh {prefix}/{f} emcc 2 2>&1')) for i in range(n)) / n
    print(f, '%.2f %.2f %.2f' % (myown, emcc, emcc2))
