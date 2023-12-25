import os
import sys
import subprocess
import argparse

target = sys.argv[1]

tasks = {
    'wasm': [('emcc', [1, 3]), ('myown', [3])],
    'apple-arm': [('clang', [1, 2, 3])],
}

fmt = '%d' if target == 'apple-arm' else '%.2f'

todo = tasks[target]

def parse_time(raw):
    for line in raw.split('\n'):
        if line.startswith('Average exec time: '):
            raw = line.split()
            unit = raw[-1]
            num = raw[-2]
            coef = 1 if unit == 'ms' else 1000
            return float(num) * coef
    for line in raw.split('\n'):
        if line.startswith('Instructions retired: '):
            return int(line.split()[-1])

def run_cmd(cmd):
    return subprocess.check_output(cmd, shell=True).decode('utf-8')

prefix = '../tests/performance/'

head = []
for backend, opts in todo:
    for lv in opts:
        head.append(f'{backend}-{lv}')
print(',', ','.join(head), sep='')

for f in sorted(os.listdir('../tests/performance/')):
    data = []
    for backend, opts in todo:
        for lv in opts:
            cmd = f'./test.sh {prefix}/{f} {target} --timeit --backend {backend} --opt {lv} 2>&1'
            res = parse_time(run_cmd(cmd))
            data.append(res)
    print(f, ','.join(fmt % i for i in data), sep=',')

