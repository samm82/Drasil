# Test Plan for Projectile

## Tests for InputParameters Module

### T:get_input_default_float

**Input:**

- $v_\text{launch} = 20.0 \frac{\text{m}}{\text{s}}$
- $\theta = 0.785398 \text{ rad}$
- $p_\text{target} = 41.0\text{m}$

**Initial State:** A file named default_float.txt exists with all the above inputs properly entered.  
**Expected Output:**

- $\text{self}.v_\text{launch} = 20.0 \frac{\text{m}}{\text{s}}$
- $\text{self}.\theta = 0.785398 \text{ rad}$
- $\text{self}.p_\text{target} = 41.0\text{m}$

**Source of Expected Output:** The state of the `InputParameters` object after reading the data should match the provided inputs.

**Rationale:** The InputParameters module should be able to read data correctly when it satisfies all constraints if it is entered using floating point numbers.

### T:get_input_default_int

**Input:**

- $v_\text{launch} = 20 \frac{\text{m}}{\text{s}}$
- $\theta = 1 \text{ rad}$
- $p_\text{target} = 41\text{m}$

**Initial State:** A file named default_int.txt exists with all the above inputs properly entered.  
**Expected Output:**

- $\text{self}.v_\text{launch} = 20 \frac{\text{m}}{\text{s}}$
- $\text{self}.\theta = 1 \text{ rad}$
- $\text{self}.p_\text{target} = 41\text{m}$

**Source of Expected Output:** The state of the `InputParameters` object after reading the data should match the provided inputs.

**Rationale:** The InputParameters module should be able to read data correctly when it satisfies all constraints if it is entered using integers.

### T:get_input_projectile_went_long

**Input:**

- $v_\text{launch} = 20.0 \frac{\text{m}}{\text{s}}$
- $\theta = 0.785398 \text{ rad}$
- $p_\text{target} = 40.0\text{m}$

**Initial State:** A file named projectile_went_long.txt exists with all the above inputs properly entered.  
**Expected Output:**

- $\text{self}.v_\text{launch} = 20.0 \frac{\text{m}}{\text{s}}$
- $\text{self}.\theta = 0.785398 \text{ rad}$
- $\text{self}.p_\text{target} = 40.0\text{m}$

**Source of Expected Output:** The state of the `InputParameters` object after reading the data should match the provided inputs.

**Rationale:** The InputParameters module should be able to read data correctly when it satisfies all constraints if it is entered using floating point numbers.
