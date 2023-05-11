
#include "parameter.hpp"

namespace options {

int PARAM_OPTION_DESCRIPTION_DISTANCE = 30; // Distance between option + usage and its description
int PARAM_OPTION_INDENT = 2;                // Characters used for indent
const int ERROR_ANY = 1;                        // throws, if a non-option parameter appears
const int ERROR_OPTION = 2;                     // throws, if a non-option parameter beginning with "-", "[" or "]"
const int ERROR_IGNORE = 3;                     // no throw, but stop at guessed first non-option parameter or not found parameter


split_option_names split(std::string &names) {
    if (names.empty())
        throw std::logic_error("split(): No option names.");

    split_option_names r;
    size_t pos = names.find(",");
    if (pos == std::string::npos) {
        if (names.length() == 1) {
            r.short_name = names;
            r.long_name = "";
        } else {
            r.short_name = "";
            r.long_name = names;
        }
        return r;
    }
    if (pos == 1) {
        if (names.substr(2, std::string::npos).find(",") != std::string::npos)
            throw std::logic_error("split(): Too many option names.");
        r.short_name = names[0];
        r.long_name = names.substr(2, std::string::npos);
    } else {
        r.short_name = names.substr(pos+1, 1);
        r.long_name = names.substr(0, pos);
    }
    boost::trim(r.short_name);
    boost::trim(r.long_name);
    return r;
}

void option::add(const option &o) {
    for (auto &i : o.options)
        options.insert(i);

    for (auto &i : o.order)
        order.push_back(i);
}

void option::add(std::string names, std::string usage, std::string description, set_func set_fnc) {
    split_option_names r = split(names);
    auto ptr = std::make_shared<option_setfunc>(r.short_name, r.long_name, usage, description, set_fnc);

    order.emplace_back(ptr);

    if (!r.short_name.empty()) {
        auto ret = options.emplace(r.short_name, ptr);
        if (!ret.second)
            throw std::logic_error("option.add(): Element " + r.short_name + " was already added.");
    }
    if (!r.long_name.empty()) {
        auto ret = options.emplace(r.long_name, ptr);
        if (!ret.second)
            throw std::logic_error("option.add(): Element " + r.long_name + " was already added.");
    }
}

option_params& option::add(std::string names, std::string description, bool *option_used) {
    split_option_names r = split(names);
    auto ptr = std::make_shared<option_params>(r.short_name, r.long_name, description, option_used);

    order.emplace_back(ptr);

    if (!r.long_name.empty()) {
        auto ret = options.emplace(r.long_name, ptr);
        if (!ret.second)
            throw std::logic_error("option.add(): Element " + r.long_name + " was already added.");
    }
    if (!r.short_name.empty()) {
        auto ret = options.emplace(r.short_name, ptr);
        if (!ret.second)
            throw std::logic_error("option.add(): Element " + r.short_name + " was already added.");

    }

    return *ptr;
}

void option::print_help(int depth) {
    if (depth == 0) {
        std::cerr << "Options:\n";
        for (auto &i : order)
            i->print_help(1);
        return;
    }

    // print a help string in the format <name> <arguments> <description>,
    // while alligning the <description> to a common column PARAM_OPTION_DESCRIPTION_DISTANCE

    std::string indent_fill(PARAM_OPTION_INDENT*depth, ' ');
    std::string name_args = indent_fill; // <name> and <arguments>

    // print both short (-?) and long (--help) name, if available
    if (!short_name.empty()) {
        name_args += "-" + short_name;
        if (!long_name.empty())
            name_args += ", --" + long_name;
    } else {
        name_args += "--" + long_name;
    }

    // add arguments
    name_args += " " + usage();

    // try to align the descriptions to column PARAM_OPTION_DESCRIPTION_DISTANCE
    // if name_args exceeds the column, ensure there is at least one space
    // (thus, shift description to the right)
    size_t align_fill_count = std::max<size_t>(PARAM_OPTION_DESCRIPTION_DISTANCE - name_args.size(), 1);

    // print the actual help line
    std::cerr   << name_args // name and arguments
                << std::string(align_fill_count, ' ') // fill with space so that description gets aligned
                << descr() // description
                << "\n"
                ;

    // print help of sub-options, if any
    if (!options.empty()) {
        std::cerr << indent_fill << "[ sub-options:\n";
        for (auto &i : options)
            i.second->print_help(depth + 1);
        std::cerr << indent_fill << "]\n";
    }
}

// print the additional help line
void option_help::print_help(int depth) {
    std::cerr << std::string(PARAM_OPTION_INDENT*depth, ' ') << addings;
}

// parses a sequence of (possibly nested) options
// calls pre_parse_option() for every encountered option
parse_section_ret option::parse_section(std::vector<std::string> &argv, size_t opt_index, const int error_flag, bool& cancel, int depth) {
    std::shared_ptr<option> last_option; // last option, required for parsing sub-options
    parse_section_ret ret { opt_index, 0 };

    while (opt_index < argv.size()) {
        if (cancel)
            break;

        // test type of argument
        if (argv[opt_index][0] == '-') {
            // this is a long (--help) or short (-?, -R123) option

            // test if it is the end of option marker "--"
            if (argv[opt_index][1] == '-' && argv[opt_index].length() == 2 ) {
                if (depth > 0)
                    throw new std::runtime_error("end-of-options reached by \"--\", but a sub-option was not ended by \"]\"");

                cancel = true; // not really needed, as this is the top-level, but just for completeness
                ret.next_index = opt_index + 1;
                ret.consumed++;
                return ret;
            }

            // a normal option - try to parse it
            try {
                pre_opt_parse_ret r = pre_parse_option(argv, opt_index);
                last_option = r.option;
                opt_index = r.index;
                ret.consumed += r.consumed;
            } catch (std::exception &e) {
                if (error_flag != ERROR_IGNORE)
                    throw;
                else {
                    cancel = true;
                    // opt_index is probably not an option and should be left to the application
                    break;
                }
            }
        } else if (argv[opt_index][0] == '[') {
            // start of sub-options
            try {
                auto t = last_option->parse_section(argv, opt_index+1, error_flag, cancel, depth+1);
                opt_index = t.next_index;
                ret.consumed += t.consumed + 1; // + 1, because we also consumed "]"
            } catch (std::exception &e) {
                if (error_flag != ERROR_IGNORE)
                    throw std::runtime_error("Error in parsing sub-options of " + last_option->getReadableName() + ": " + e.what());
                else {
                    cancel = true;
                    // an error occured inside a sub-option
                    // currently, simply break and return the index of the "[" (i.e. it turns out it was not a sub-option)
                    // we may also let parse_section return the last succesful index, and add +1 to that
                    break;
                }
            }
        } else if (argv[opt_index][0] == ']') {
            // end of sub-options
            if (depth == 0) {
#if DEBUG_OPTIONS
                std::clog << "Invalid end of sub-options at depth 0\n";
#endif
                if (error_flag != ERROR_IGNORE)
                    throw std::runtime_error("Unknown option: " + argv[opt_index]);
                else {
                    // at depth 0 we should return the number of parameters processed,
                    // and "]" obviously does not belong to an option
                    ret.next_index = opt_index;
                    // ret.consumed stays unchanged
                    return ret;
                }

            } else {
                // this was a recursive call for these sub-options
                // thus, end the recursive vall here
                ret.next_index = opt_index+1; // caller must continue at next position
                ret.consumed++; // consumed the final "]"
                return ret;
            }
        } else {
            if (error_flag == ERROR_ANY)
                throw std::runtime_error("Unknown option: " + argv[opt_index]);
            else {
                cancel = true;
                // the current index is not an option, thus leave it to the application
                ret.next_index = opt_index;
                return ret;
            }
        }
    }

    ret.next_index = opt_index;
    return ret;
}

pre_opt_parse_ret option::pre_parse_option(std::vector<std::string> &argv, size_t start) {
    std::string option_name;
    bool split = false; // if a short option ("-R123") was split

    // check which form the current option has
    if (argv[start][1] != '-') {
        // short option ("-r" or "-R" "123")
        option_name = argv[start].substr(1, 1);

        if (argv[start].length() > 2) {
            // complex short option ("-R123")
            split = true;

            // split the complex option into two arguments ("-R" and "123")
            // extract argument
            argv[start] = argv[start].substr(2, std::string::npos);
            // insert option name before
            argv.insert(argv.begin() + start, option_name);
        }
    } else {
        // normal long option ("--random")
        option_name = argv[start].substr(2, std::string::npos);
    }

    try {
        auto &option = options.at(option_name);
        // let the option parse its arguments and execute
        size_t read = option->parse_option(argv, start + 1);

        // return value:
        // * the next index to parse for the following option
        // * the number of consumed arguments
        //   we consumed at least the argument at [start] (+ arguments parsed by the option itself)
        // * the actual option
        pre_opt_parse_ret r { start + 1 + read, read + 1, option };
        if (split)
            // when split an argument, we actually consumed one less
            r.consumed--;
#if DEBUG_OPTIONS > 1
        std::clog   << "Parsed option " << option_name << " at index " << start << " (next to process: " << r.index << ")"
                    << ", consumed " << r.consumed << " arguments from input\n";
#endif
        return r;
    } catch (std::out_of_range &o) {
        throw std::runtime_error("Option not found: " + option_name);
    }
}

size_t option_params::parse_option(std::vector<std::string>& in, size_t start) {
    std::string *name = &this->long_name;
    if (name->empty())
        name = &this->short_name;
    if (used != nullptr)
        *used = true;
    if (start + parameters.size() > in.size()) // not enough arguments
        throw std::runtime_error("Not enough arguments for option " + *name);
    try {
        for (size_t i = 0; i < parameters.size(); i++)
            parameters[i]->parse(in[start + i]);
    } catch (std::exception &e) {
        throw std::runtime_error("Option " + *name + ": " + e.what());
    }

    return parameters.size();
}

size_t option_setfunc::parse_option(std::vector<std::string>& in, size_t start) {
    std::string *name = &long_name;
    if (name->empty())
        name = &short_name;
    try {
        return set_fnc(*name, in, start);

    } catch (std::exception &e) {
        throw std::runtime_error("Option " + *name + ": " + e.what());
    }
}

// entry point for parsing a command line of options
size_t option::parse(int argc, char** args, const int error_flag) {
    if (argc < 0)
        throw std::logic_error("argc < 0");
    // checking error_flag
    if (error_flag != ERROR_ANY && error_flag != ERROR_OPTION && error_flag != ERROR_IGNORE) {
        throw std::logic_error("Invalid error_flag.");
    }
    std::vector<std::string> argv(argc-1); // converting to a string vector
    for (int i = 1; i < argc; i++ ) {
        argv[i-1] = args[i];
    }

    bool cancel = false; // needed for nested sections
    auto ret = parse_section(argv, 0, error_flag, cancel);
#if DEBUG_OPTIONS
    std::clog << "finished parsing options, consumed " << ret.consumed << " arguments\n";
#endif
    if (error_flag == ERROR_OPTION) // only at ERROR_OPTION
        check_remaining_parameters(argv, ret.next_index);

    // + 1 because we started with the argv vector at element 1, see above
    return ret.consumed + 1;
}

void option::check_remaining_parameters(std::vector<std::string> argv, size_t start) {
    for (size_t i = start; i < argv.size(); i++) {
        if (argv[i][0] == '-' || argv[i][0] == '[' || argv[i][0] == ']')
            throw std::runtime_error("Unknown identifier: " + argv[i]);
    }
}

} // namespace options
