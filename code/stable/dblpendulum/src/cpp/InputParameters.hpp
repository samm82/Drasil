/** \file InputParameters.hpp
    \author Dong Chen
    \brief Provides the function for reading inputs and the function for checking the physical constraints on the input
*/
#ifndef InputParameters_h
#define InputParameters_h

#include <string>

using std::ifstream;
using std::string;

/** \brief Reads input from a file with the given file name
    \param filename name of the input file
    \param L_1 length of the first rod (m)
    \param L_2 length of the second rod (m)
    \param m_1 mass of the first object (kg)
    \param m_2 mass of the second object (kg)
*/
void get_input(string filename, double &L_1, double &L_2, double &m_1, double &m_2);

/** \brief Verifies that input values satisfy the physical constraints
    \param L_1 length of the first rod (m)
    \param L_2 length of the second rod (m)
    \param m_1 mass of the first object (kg)
    \param m_2 mass of the second object (kg)
*/
void input_constraints(double L_1, double L_2, double m_1, double m_2);

#endif
