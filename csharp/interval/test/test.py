import subprocess
import itertools

perfect_degrees   = [1, 4, 5, 8, 11, 12, 15]
imperfect_degrees = list(set(range(1, 15)) - set(perfect_degrees))

perfect_qualities   = ['d', 'P', 'a']
imperfect_qualities = ['d', 'm', 'M', 'a']

perfect_intervals   = itertools.product(perfect_degrees, perfect_qualities)
imperfect_intervals = itertools.product(imperfect_degrees, imperfect_qualities)

intervals = [f"{q}{d}" for d, q 
             in sorted(list(perfect_intervals) + list(imperfect_intervals),
                       key=lambda pair: pair[0])]

pitches = ['c', 'd', 'e', 'f', 'g', 'a', 'b']
operators = ['+', '-']

def expr(pitch, operator, interval):
    return f"Pitch({pitch}) {operator} Interval({interval})"

program = './bin/Debug/net8.0/interval'

for perm in itertools.product(pitches, operators, intervals):
    cmd = expr(*perm)
    try:
        subprocess.run([program, cmd], check=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        print(f"{cmd}:\n{e.stdout}\n{e.stderr}\n\n")



