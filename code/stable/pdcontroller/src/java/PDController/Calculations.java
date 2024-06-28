package PDController;

/** \file Calculations.java
    \author Naveen Ganesh Muralidharan
    \brief Provides functions for calculating the outputs
    \note Generated by Drasil v0.1-alpha
*/

import java.util.ArrayList;
import org.apache.commons.math3.ode.FirstOrderIntegrator;
import org.apache.commons.math3.ode.nonstiff.DormandPrince54Integrator;

public class Calculations {
    
    /** \brief Calculates Process Variable: The output value from the power plant
        \param K_d Derivative Gain: Gain constant of the derivative controller
        \param K_p Proportional Gain: Gain constant of the proportional controller
        \param r_t Set-Point: The desired value that the control system must reach. This also knows as the reference variable
        \param t_sim Simulation Time: Total execution time of the PD simulation (s)
        \param t_step Step Time: Simulation step time (s)
        \return Process Variable: The output value from the power plant
    */
    public static ArrayList<Double> func_y_t(double K_d, double K_p, double r_t, double t_sim, double t_step) {
        ArrayList<Double> y_t;
        ODEStepHandler stepHandler = new ODEStepHandler();
        ODE ode = new ODE(K_p, K_d, r_t);
        double[] curr_vals = {0.0, 0.0};
        
        FirstOrderIntegrator it = new DormandPrince54Integrator(t_step, t_step, Constants.AbsTol, Constants.RelTol);
        it.addStepHandler(stepHandler);
        it.integrate(ode, 0.0, curr_vals, t_sim, curr_vals);
        y_t = stepHandler.y_t;
        
        return y_t;
    }
}
