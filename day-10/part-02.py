import re
import numpy as np
from scipy.optimize import milp, Bounds, LinearConstraint

def solve_machine(machine):
    button_groups = re.findall(r"\((\d+(?:,\d+)*)\)", machine)
    target_match = re.search(r"\{(\d+(?:,\d+)*)\}", machine)
    
    if not target_match or not button_groups:
        return 0

    buttons = [list(map(int, b.split(","))) for b in button_groups]
    joltages = list(map(int, target_match.group(1).split(",")))

    size = len(joltages)
    num_buttons = len(buttons)
    
    A = np.zeros((size, num_buttons))
    for col, indices in enumerate(buttons):
        for row in indices:
            if row < size:
                A[row, col] = 1
    
    b = np.array(joltages)
    c = np.ones(num_buttons)
    
    constraints = LinearConstraint(A, b, b)
    integrality = np.ones(num_buttons)
    bounds = Bounds(0, np.inf)

    res = milp(
        c=c, 
        constraints=constraints, 
        integrality=integrality, 
        bounds=bounds,
        options={"presolve": True}
    )

    return int(np.round(res.fun))

with open("input.txt", "r") as file:
    nums = [solve_machine(line) for line in file if line.strip()]

print(f"Total Sum: {sum(nums)}")
