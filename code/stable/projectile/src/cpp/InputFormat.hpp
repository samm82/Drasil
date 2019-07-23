/** \file InputFormat.hpp
*/
#ifndef InputFormat_h
#define InputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "InputParameters.hpp"

/** \brief Reads input from a file with the given file name
    \param filename No description given
*/
void get_input(string filename, InputParameters &inParams);

#endif