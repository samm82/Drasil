# TODO: should this be here?
invalid_input_files = [
    "zero_v_launch",   # violates lower bound of v_launch
    "zero_theta",      # violates lower bound of theta
    "too_large_theta", # violates upper bound of theta
    "zero_p_target",   # violates lower bound of p_target
]

expected_outputs = [
    {
        "filename": "default_float",
        "v_launch": 20.0,
        "theta":    0.785398,
        "p_target": 41.0,
        "t_flight": 2.8861496557024258,
        "p_land":   40.81632653061006266,
        "d_offset": -0.18367346938993734,
        "s":        "The target was hit.",
    },
    {
        "filename": "default_int",
        "v_launch": 20,
        "theta":    1,
        "p_target": 41,
        "t_flight": 3.434575448195496,
        "p_land":   37.1141806867625164,
        "d_offset": -3.8858193132374836,
        "s":        "The projectile fell short."
    },
    {
        "filename": "projectile_went_long",
        "v_launch": 20.0,
        "theta":    0.785398,
        "p_target": 40.0,
        "t_flight": 2.8861496557024258,
        "p_land":   40.8163265306100627,
        "d_offset": 0.8163265306100627,
        "s":        "The projectile went long."
    },
]