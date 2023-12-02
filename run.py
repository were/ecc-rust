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
    try:
        myown = sum(parse_time(run_cmd(f'./test.sh {prefix}/{f} --opt 2 2>&1')) for i in range(n)) / n
        myown = '%.2f' % myown
    except:
        myown = 'x'
    try:
        emcc = sum(parse_time(run_cmd(f'./test.sh {prefix}/{f} --backend emcc --opt 0 2>&1')) for i in range(n)) / n
        emcc = '%.2f' % emcc
    except:
        emcc = 'x'
    try:
        emcc2 = sum(parse_time(run_cmd(f'./test.sh {prefix}/{f} --backend emcc --opt 2 2>&1')) for i in range(n)) / n
        emcc2 = '%.2f' % emcc2
    except:
        emcc2 = 'x'
    print(f, '%s %s %s' % (myown, emcc, emcc2))
    # print(f, '%.2f %.2f' % (myown, emcc))
