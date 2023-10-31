import os
import subprocess

prefix = '../tests/performance/'
for f in sorted(os.listdir('../tests/performance/')):
    raw = subprocess.check_output(f'./test.sh {prefix}/{f}', shell=True).decode('utf-8')
    for line in raw.split('\n'):
        if line.startswith('Exec time: '):
            print(line[11:])
