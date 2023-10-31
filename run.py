import os
import subprocess

def parse_time(raw):
    for line in raw.split('\n'):
        if line.startswith('Exec time: '):
            res = line[11:]
            coef = 1 if res.endswith('ms') else 1000
            return float(res.rstrip('ms')) * coef

n = 10
prefix = '../tests/performance/'
for f in sorted(os.listdir('../tests/performance/')):
    myown = sum(parse_time(subprocess.check_output(f'./test.sh {prefix}/{f} 2>&1', shell=True).decode('utf-8')) for i in range(n)) / n
    emcc = sum(parse_time(subprocess.check_output(f'./test.sh {prefix}/{f} emcc 2>&1', shell=True).decode('utf-8')) for i in range(n)) / n
    print(f, myown, emcc)
