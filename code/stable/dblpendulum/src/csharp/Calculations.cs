/** \file Calculations.cs
    \author Dong Chen
    \brief Provides functions for calculating the outputs
*/
using System;
using System.Collections.Generic;
using Microsoft.Research.Oslo;

public class Calculations {
    
    /** \brief Calculates dependent variables (rad)
        \param m_1 mass of the first object (kg)
        \param m_2 mass of the second object (kg)
        \param L_2 length of the second rod (m)
        \param L_1 length of the first rod (m)
        \return dependent variables (rad)
    */
    public static List<double> func_theta(double m_1, double m_2, double L_2, double L_1) {
        List<double> theta;
        Func<double, Vector, Vector> f = (t, theta_vec) => {
            return new Vector(theta_vec[1], (-9.8 * (2.0 * m_1 + m_2) * Math.Sin(theta_vec[0]) - m_2 * 9.8 * Math.Sin(theta_vec[0] - 2.0 * theta_vec[2]) - 2.0 * Math.Sin(theta_vec[0] - theta_vec[2]) * m_2 * (Math.Pow(theta_vec[3], 2.0) * L_2 + Math.Pow(theta_vec[1], 2.0) * L_1 * Math.Cos(theta_vec[0] - theta_vec[2]))) / (L_1 * (2.0 * m_1 + m_2 - m_2 * Math.Cos(2.0 * theta_vec[0] - 2.0 * theta_vec[2]))), theta_vec[3], 2.0 * Math.Sin(theta_vec[0] - theta_vec[2]) * (Math.Pow(theta_vec[1], 2.0) * L_1 * (m_1 + m_2) + 9.8 * (m_1 + m_2) * Math.Cos(theta_vec[0]) + Math.Pow(theta_vec[3], 2.0) * L_2 * m_2 * Math.Cos(theta_vec[0] - theta_vec[2])) / (L_2 * (2.0 * m_1 + m_2 - m_2 * Math.Cos(2.0 * theta_vec[0] - 2.0 * theta_vec[2]))));
        };
        Options opts = new Options();
        opts.AbsoluteTolerance = 1.0e-6;
        opts.RelativeTolerance = 1.0e-6;
        
        Vector initv = new Vector(new double[] {1.3463968515384828, 0.0, 2.356194490192345, 0.0});
        IEnumerable<SolPoint> sol = Ode.RK547M(0.0, initv, f, opts);
        IEnumerable<SolPoint> points = sol.SolveFromToStep(0.0, 20.0, 1.0e-3);
        theta = new List<double> {};
        foreach (SolPoint sp in points) {
            theta.Add(sp.X[0]);
        }
        
        return theta;
    }
}
