#pragma once

#include <unordered_map>
#include <vector>
#include <memory>
#include <exception>
#include <typeinfo>
#include <iostream>
#include <fstream>
#include <utility>
#include <sstream>

#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/trim.hpp>


namespace options {

/****
 forward declaration
*****/
class option;
class option_setfunc;
template <typename T> class option_ref;
class option_help;
class option_params;

/****
 typedefs and definitions
*****/
extern int PARAM_OPTION_DESCRIPTION_DISTANCE;   // Distance between option + usage and its description
extern int PARAM_OPTION_INDENT;                 // Characters used for indent
extern const int ERROR_ANY;                     // throws, if a non-option parameter appears
extern const int ERROR_OPTION;                  // throws, if a non-option parameter beginning with "-", "[" or "]"
extern const int ERROR_IGNORE;                  // no throw, but stop at guessed first non-option parameter

/**
* @param params - vector containing the option parameter
* @param start - start index to read the parameter
* @param names - long name of the option, short name, if there is no long name
*/
typedef std::function<size_t (std::string &names, const std::vector<std::string> &params, size_t start)> set_func;

// return value of pre_option_parse-function
struct pre_opt_parse_ret {
    size_t index;   // next index to parse
    size_t consumed;  // how many arguments have been consumed
    // note: consumed > index iff short options (-R123) have been split into
    // two arguments (-R 123) for a canonical representation and simpler parsing logic

    std::shared_ptr<options::option> option; // the option that was used
} ;

// return value which contains the long and short option
struct split_option_names {
    std::string short_name;
    std::string long_name;
};

// return value of parse_section
struct parse_section_ret {
    size_t next_index; // next index of arguments to parse
    size_t consumed; // number of arguments consume
    // note: consumed > next_index iff short options (-R123) have been split into
    // two arguments (-R 123) for a canonical representation and simpler parsing logic
};

/**
 * @brief splits the string into short and long option
 * @param names - input string
 * @return short option and long option
 */
split_option_names split(std::string &names);

// parse a single argument and return its value
template <typename T>
inline T param_parse(const std::string &s) {
    try {
        return boost::lexical_cast<T>(s);
    } catch (const boost::bad_lexical_cast &e) {
        throw std::runtime_error("Invalid argument " + s );
    }
}

// function template specializations for streams
template <>
inline std::shared_ptr<std::istream> param_parse<std::shared_ptr<std::istream>>(const std::string &s) {
    if (s.compare("-") == 0)
        return std::shared_ptr<std::istream>(&std::cin, [](std::istream*) {});
    return std::make_shared<std::ifstream>(s);
}

template <>
inline std::shared_ptr<std::ostream> param_parse<std::shared_ptr<std::ostream>>(const std::string &s) {
    if (s.compare("-") == 0)
        return std::shared_ptr<std::ostream>(&std::cout, [](std::ostream*) {});
    return std::make_shared<std::ofstream>(s);
}

// function template specialization for better performance for std::string
template <>
inline std::string param_parse<std::string>(const std::string &s) {
    return s;
}

/**
 * Represents a parameter of an option, but
 * this class is abstract, only use param_ref.
 * @class param
 * @author
 * @date 04/20/16
 * @file parameter.hpp
 */
class param {
public:
    std::string name;           // name of this value, enclosed in <> in help text
    bool show_current_value;    // wether the current value is shown or not

    // constructor
    param(const std::string &name, bool show_current_value): name(name), show_current_value(show_current_value) {}

    // parse the value and set the reference
    virtual void parse(const std::string&) {}
    virtual std::string get_current_ref_value() {
        return "";
    }
};

/**
 * this class sets arguments of parameters of the program to a referenced variable and before set,
 * the argument can be checked, if it fits to a specified check-function
 * e.g. -e <first> <second>: first and second can be represented by param_ref, if there is a variable
 * to reference to
 *
 * @class param_ref
 * @author
 * @date 04/29/16
 * @file parameter.hpp
 */
template <typename T>
class param_ref : public param {
public:
    typedef std::function<bool (std::string, T)> check_func_t;

private:
    T &ref;
    check_func_t check_func; // function that checks if the parameter is valid


public:
    param_ref(const std::string &name, T &ref, check_func_t check_func = nullptr, bool show_current_value = false) :
        param(name, show_current_value), ref(ref), check_func(check_func) {
    }

    /**
     * @brief try to check thr string (s) and sets the reference to s
     * @param s - argument string for the parameter
     */
    void parse(const std::string &s) {
        auto value = param_parse<T>(s);
        if (!check_func || check_func(name, value))
            ref = value;
        else
            throw std::runtime_error(s + " is not suitable as " + name);
    }

    /**
     * @brief reference value for help table
     * @return current reference value
     */
    std::string get_current_ref_value() {
        return boost::lexical_cast<std::string>(ref);
    }
};

/**
 * basic class and should be used once in a program to add and parse like this:
 *      option o;
 *      o.add(...);
 *      o.add(...).parameter(...);
 *      o.parse(argc,argv,errorflag);
 *
 * @class option
 * @author
 * @date 04/20/16
 * @file parameter.hpp
 */
class option {
protected:
    std::string short_name;
    std::string long_name;
    std::string description;

    std::unordered_map<std::string, std::shared_ptr<option>> options;   // nested options

    std::vector<std::shared_ptr<option>> order;                         // order for the help_page,
    // where you can add special string
    // between options (option_help)

    /**
     * @brief throws an exception if elements of remaining_parameters start either with "-", "[" or "]"
     * @param argv - argv from program arguments
     * @param start - index to start
     */
    void check_remaining_parameters(std::vector<std::string> argv, size_t start);

    /**
     * @brief parses an option with all parameters either by a set-function or by reference
     * @return number of parsed parameters from vector
     */
    virtual size_t parse_option(std::vector<std::string>&, size_t) {
        return 0;
    }

    /**
     * @brief parses a section of options with a specified errorflag
     * @param argv - string vector of arguments to parse
     * @param opt_index - start parsing at index opt_index in argv
     * @param error_flag
     * @param cancel - reference to a variable that is set to "true" if canceled, need for recursion
     * @param depth - recursion depth
     * @return number of "argv"-entries parsed and number of complex short options
     */
    parse_section_ret parse_section(std::vector<std::string> &argv, size_t opt_index, const int error_flag, bool &cancel, int depth = 0);

    /**
     * @brief splitting up e.g. -i5 to -i 5 and look for the option when it exists
     * @param argv - string vector containing the program options
     * @param start - index to start
     * @return next index to parse and number of consumed arguments from argv
     */
    pre_opt_parse_ret pre_parse_option(std::vector<std::string> &argv, size_t start);

    // constructor overload for inherited classes
    option(std::string short_name, std:: string long_name, std::string description) :
        short_name(short_name), long_name(long_name), description(description) {}

public:

    /**
     * @brief default c'tor
     */
    option() {}

    /**
     * @brief adds another option object and all of its content
     * @param o - option object
     */
    void add(const option &o);

    /**
     * @brief add a sub-option, where a user-defined function parses the values
     * @param names - string containing the names seperated by ",": e.g. "e,exit" or "find,f"
     * @param usage - usage string like "<file" or "<name> <count>"
     * @param description - description showed in help table
     * @param set_fnc - callback function, return the number of arguments used
     */
    void add(std::string names, std::string usage, std::string description, set_func set_fnc);

    /**
     * adds a sub-option in order to add reference-parameters right behind like this:
     * add("f,file", "description").parameter("<filename>", filename);
     *
     * @param names - string containing the names seperated by ",": e.g. "e,exit" or "find,f"
     * @param description - description showed in help table
     * @param option_used - pointer to a variable that is set to true if the option is used
     * @return reference to the new option_param object added
     */
    option_params& add(std::string names, std::string description, bool *option_used = nullptr);

    /**
     * operator overload for easy use in order to do this (nested options):
     * option o;
     * o["e"].add(...);
     *
     * @param key - long or short name of the option
     * @return option reference
     */
    option& operator[](std::string key) {
        try {
            return *options.at(key);
        } catch (std::out_of_range &e) {
            throw std::logic_error("option[" + key + "]: Option does not exist!");
        }
    }

    /**
     * @brief parses the argc and argv from the main function
     * @param argc - number of arguments
     * @param args - pointer to cstring (char*) array
     * @param error_flag - flag describing what to do when a name begins with "-" for example, see flag definition
     * @return number of elements parsed
     */
    size_t parse(int argc, char** args, const int error_flag = ERROR_ANY);

    /**
     * @brief prints the help desc and exit
     * @param depth - used for nested options, only change when need
     */
    virtual void print_help(int depth = 0);

    /**
     * @brief adds a string to the help output
     * @param add - string to be added
     */
    void add_help(std::string add) {
        auto ptr = std::make_shared<option_help>(add);
        order.emplace_back(ptr);
    }

    // return the name of this option (prefer long name, otherwise short name)
    std::string getReadableName() {
        if (!long_name.empty())
            return "--" + long_name;
        return "-" + short_name;
    }

    /**
     * @brief returns usage string, e.g. "<filename>"
     * @return the usage string
     */
    virtual std::string usage() {
        return "";
    }

    /**
     * @brief returns the description showed in help table
     * @return the description
     */
    virtual std::string descr() {
        return description;
    }

};


/**
 * an option with parameters that are all set by a single function, but can contain multiple arguments.
 * This class should not be used directly, only by using 1 option object and using the function add like this:
 *      option o;
 *      o.add(...);
 *      o.add(...).parameter(...);
 *      o.parse(argc,argv,errorflag);
 *
 * @class option_setfunc
 * @author
 * @date 04/21/16
 * @file parameter.hpp
 */
class option_setfunc : public option {
    set_func set_fnc;
    std::string usage_string;
public:
    option_setfunc(std::string short_name, std::string long_name, std::string usage, std::string description, set_func set_fnc) :
        options::option(short_name, long_name, description), set_fnc(set_fnc), usage_string(usage) {}

    /**
     * @brief parses an option with all parameters either by a set-function or by reference
     * @return number of parsed parameters from vector
     */
    size_t parse_option(std::vector<std::string>& in, size_t start);

    /**
     * @brief returns usage string, e.g. "<filename>"
     * @return the usage string
     */
    inline std::string usage() {
        return usage_string;
    }
};


/**
 * An option with arbitrarily many parameters.
 * This class should not be used directly, only by using 1 option object and using the function 'add' like this:
 *      option o;
 *      o.add(...);
 *      o.add(...).parameter(...);
 *      o.parse(argc,argv,errorflag);
 *
 * @class option_params
 * @author
 * @date 04/21/16
 * @file parameter.hpp
 */
class option_params : public option {
    std::vector<std::shared_ptr<param>> parameters;
    bool *used;

public:
    option_params(std::string short_name, std::string long_name, std::string description, bool *used) :
        options::option(short_name, long_name, description), used(used) {}

    /**
     * @brief parses an option with all parameters either by a set-function or by reference
     * @return number of parsed parameters from vector
     */
    size_t parse_option(std::vector<std::string>& in, size_t start);

    /**
     * @brief creates the usage string, e.g. "<filename>"
     * @return the usage string
     */
    std::string usage() {
        std::string ret;
        for (size_t i = 0; i < parameters.size(); i++)
            ret += "<" + parameters[i]->name + "> ";
        boost::trim(ret);
        return ret;
    }

    /**
     * @brief creates the description string showed in the help table with showing current value functionality
     * @return the usage string
     */
    std::string descr() {
        bool show_cur = false;
        std::string ret = description;
        std::string cur = " (currently: ";
        for (size_t i = 0; i < parameters.size(); i++) {
            if (parameters[i]->show_current_value) {
                show_cur = true;
                cur += parameters[i]->get_current_ref_value() + " ";
            }
        }
        cur += ")";
        return (show_cur)? ret + cur : ret;
    }

    /**
     * same as parameter(name, ref, nullptr, show_current_value)
     */
    template <typename T, typename F = std::function<bool (std::string, T)> >
    option_params& parameter(const std::string &name, T &ref, bool show_current_value = false) {
        return parameter(name, ref, nullptr, show_current_value);
    }

    /**
     * add a parameter to the option like this:
     * You have an option "-o" and can add a filename (string) and count (int) like this:
     * ...add("o",...).parameter("filename",...).parameter("count",...);
     * @param name - name of the parameter, that is the usage string
     * @param ref - reference to the variable that is set for the parameter
     * @param check_func - function to check if the parameter fits or not
     * @param show_current_value - if current value should be shown in the description in the help table
     * @return option_param reference to itself after adding the parameter
     */
    template <typename T, typename F = std::function<bool (std::string, T)> >
    option_params& parameter(const std::string &name, T &ref, F check_func, bool show_current_value = false) {
        static_assert(!(std::is_same<T, char*>::value || std::is_same<T, const char*>::value), "options: char* and const char* is not supported as parameter type, use std::string instead");
        std::function<bool (std::string, T)> check_function(check_func);
        parameters.emplace_back(std::make_shared<param_ref<T>>(name, ref, check_func, show_current_value));
        return *this;
    }

};


/**
 * modifyes the help page. Do not use this class directly,
 * only by using add_help() function in option class
 *
 * @class option_help
 * @author
 * @date 04/21/16
 * @file parameter.hpp
 */
class option_help : public option {
    std::string addings;
public:
    option_help(std::string addings) : addings(addings) {}

    /**
     * @brief overwrites the print_help function to print the addings string
     * @param depth
     */
    void print_help(int depth = 1);
};
} // namespace option
