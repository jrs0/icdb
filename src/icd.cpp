// Notes:
//
// The error "Object was created without names"
// is some kind of out-of-bounds issue -- look for
// places where a name doesn't exist or maybe something
// is NULL (google the string above, with quotes, to
// see the source code)
//
// When constructing things, use the = version, not
// {} or () which do not work with Rcpp.
//
// The code below converts to a native C++ type as
// quickly as possible, despite a possible dip in
// performance due to copying, because of the much
// larger usability of C++ types. Large structures
// like Rcpp::List (nested) are not copied into maps
// or vectors.
//
// Use Rcpp::Rcout instead of std::cout. However, still
// overload output to std::ostream -- that just works.


#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <regex>
#include <map>
#include <set>

std::ostream & operator << (std::ostream & os,
			    const std::vector<std::string> & v)
{
    os << "(";
    for (const auto & str : v) {
	os << str << ",";
    }
    os << ")";
    return os;
}


// Results structure for ICD parse
class ParseResult
{
public:
    ParseResult(int type, const std::vector<std::size_t> & indices,
		const std::set<std::string> & groups,
		const std::string & name,
		const std::string & trailing)
	: type_{type}, indices_{indices},
	  trailing_{trailing}, groups_(groups.begin(), groups.end()),
	  name_{name}
    {}

    ParseResult(int type, const std::vector<std::size_t> & indices,
		const std::set<std::string> & groups,
		const std::string & name)
	: type_{type}, indices_{indices},
	  groups_(groups.begin(), groups.end()), name_{name}
    {}

    ParseResult(int type) : type_{type} {}

    Rcpp::NumericVector indices() const {
	return Rcpp::NumericVector(std::rbegin(indices_), std::rend(indices_));
    }
    
    // The type -- whether the parse succeeded or not
    int type() const { return type_; }

    std::string basic_name() const {
    	if (indices_.size() == 0) {
    	    return "NA";
    	} else {
	    return name_;
    	}
    }
   
    
    std::string name() const {
    	if (indices_.size() == 0) {
    	    return "(" + trailing_ + ")";
    	} else {
    	    if (trailing_.size() > 0) {
    		return name_ + "(" + trailing_ + ")";
    	    } else {
    		return name_;
    	    }
    	}
    }
    
    // Any unparsed trailing matter
    std::string trailing() const { return trailing_; }

    // The set of groups associated with this code
    Rcpp::CharacterVector groups() const {
	return Rcpp::CharacterVector(std::begin(groups_),
				     std::end(groups_));
    }

    // Prepend an index to the indices list
    void add_index(std::size_t position_val)
    {
	indices_.push_back(position_val);
    }
    
private:
    int type_{0};

    // PERF: Profiling shows a large chunk of time is spent in
    // the destructor or ParseResult
    std::vector<std::size_t> indices_{};
    std::string trailing_{""};
    std::vector<std::string> groups_{};
    std::string name_{""};
};

class Cat
{
public:
    Cat(const Rcpp::List & cat)
    {
	// This is only here because I need to use =
	cat_ = cat;
    }

    // This function returns the string for the category
    // key and the code key (both of which will probably
    // be called category in a future version)
    std::string category() const
    {
	try {
	    return Rcpp::as<std::string>(cat_["category"]);
	} catch(Rcpp::index_out_of_bounds &) {    
	    return Rcpp::as<std::string>(cat_["code"]);
	}
    } 
    std::string docs() const
    {
	return Rcpp::as<std::string>(cat_["docs"]);
    }

    // Get excluded groups (will throw std::out_of_range
    // on no exclude key)
    std::set<std::string> exclude() const
    {
	auto val{Rcpp::as<std::vector<std::string>>(cat_["exclude"])};
	return std::set<std::string>(val.begin(), val.end());
    }
    
    // True if this category has subcategories (false
    // for leaf nodes with single codes)
    bool has_subcats() const
    {
	// This is simpler than checking the names,
	// can check performance later
	try {
	    Rcpp::List val = cat_["child"];
	    return true;
	} catch (const Rcpp::index_out_of_bounds &) {
	    return false;
	}
    }

    Rcpp::List get_subcats()
    {
	return Rcpp::as<Rcpp::List>(cat_["child"]);
    }
    
    // Return true if code is (lexicographically) contained
    // in the range specified by the index of this Cat
    bool contains(const std::string & str) const
    {
	auto idx{index()};
	// Truncates to the length of the first part of
	// the range, assuming the two parts are equal length
	std::string trunc{str.substr(0, idx[0].size())};
	if (idx.size() == 2) {
	    return (str >= idx[0]) && (trunc <= idx[1]);
	} else {
	    // Truncate the string to the length of the index
	    return trunc == idx[0];
	}	
    }
    
    // Return the index (either one string or two for a range)
    std::vector<std::string> index() const
    {
	return Rcpp::as<std::vector<std::string>>(cat_["index"]);
    }

    
private:
    Rcpp::List cat_; ///< Pointing to the category
};

// \brief Comparison of category with raw code
//
// This function is the main comparison operator that is
// used in the binary search to find a raw code in the ICD-10
// codes file.
// 
// Each category has an index, which is the range of codes
// that this category contains. The index is of the form
// (M,N), where M and N are code roots -- e.g. (B15,B19).
// The top of this range is inclusive, meaning that
// the code B199 would be inside this category, even
// though B199 > B19 lexicographically. The trick is to
// truncate the code B199 to the length of the upper end
// of the range before doing the check. There are also
// categories that contain a degenerate range, of the
// form (M,) -- e.g. (A01,). Here, any code that starts
// with A01 is in the category (again, truncate and compare).
//
// The program below uses a binary search to find which
// category a code belongs to, based on the std::upper_bound
// function. For the purpose of this search, the categories
// are ordered by the first element of their index, M, because
// this guarantees that the upper element N is also ordered
// (categories do not overlap, although there are gaps between
// categories). For the particular string str of interest,
// the upper bound category is the first cat such that
// cat > str. This means that the previous cat was the last
// cat where cat <= str. If this is applied to the start
// of the range M, then this is the right condition to
// find the best candidate category (where M <= str, and
// M is maximal)
//
// The upper bound is enough to guarantee that "if str is
// in any category, it is in this one". It is still necessary
// to check that the str is not larger than the upper end
// of the range, N. This check is performed after the search
// is complete.
//
bool operator < (const std::string & str, const Cat & cat)
{
    auto idx{cat.index()};
    // Only need the first element M for < operator
    // (see the comments above)
    return str < idx[0];
    
}

// Not sure what the 'true' means
std::ostream & operator << (std::ostream & os,
			    const Cat & cat)
{
    // This shouldn't be needed (the cstring), but couldn't
    // figure out why it wasn't working without it -- moved on.
    os << cat.category()
       << " -- "
       << cat.docs()
       << " "
       << cat.index();
    
    return os;
}

// Parse a single ICD-10 string into a vector of indices that locates
// it in the code definition structure, and handle parsing errors. This
// function is the counterpart to the R function icd10_str_to_indices.
//
// The returned object is a list containing the following fields:
// - type: a integer describing the result of the parse. 0 for
//   success; 1 for empty string; 2 for invalid code.
// - indices: if success, a vector of indices locating
// - trailing: an content at the end of the string that was not
//   parsed (sometimes ICD-10 codes contain trailing matter -- the
//   goal is to eventually interpret all of this)
// - groups: a character vector of groups that contain this code
//
// This function is the faster implementation of the ICD-10 parser.
// The input is a vector of strings to be parsed, and the output is
// a list of three vectors (the same length as the input) containing
// information about the parsed codes.
//
// Note: passing groups by value is deliberate, because it is
// modified and should not modify the caller's version. Todo:
// try to remove this, it might be possible to keep only one
// copy and modify it globally.
// 
// @param str The input character vector of strings that should be
// parsed. The strings can have leading and trailing whitespace,
// which will be removed before parsing by this function.
// @param code_def The code definition structure, which is a nested
// list of lists following the structure of the icd10.yaml file.
// @return A named list containing indices, type and groups
// 
// 
ParseResult icd10_str_to_indices_impl(const std::string & str,
				      const Rcpp::List & codes,
				      std::set<std::string> groups)
{
    // Check for empty string. Return type = 1 if empty
    // and set all other fields to empty
    const std::string std_str{str};
    if(std::all_of(std_str.begin(), std_str.end(), isspace)) {
	return ParseResult{1};
    }   
    
    // Look through the index keys at the current level
    // and find the position of the code. Inside the codes
    // structure, the index keys provide an array to search
    // (using binary search) for the ICD code in str.
    
    // Get a vector of category objects to search
    std::vector<Cat> cats;
    for (auto i{std::begin(codes)}; i < std::end(codes); ++i) {
	const Rcpp::List cat = *i;
	cats.emplace_back(cat);
    }

    // Perform the binary search
    auto position = std::upper_bound(std::begin(cats), std::end(cats), str);
    const bool found = (position != std::begin(cats)) &&
 	((position-1)->contains(str));
    // Decrement the position to point to the largest category
    // cat such that cat <= str    
    position--;

    // If founcd == false, then a match was not found. This
    // means that the str is not a valid member of any member
    // of this level, so it is not a valid code. Return a
    // type of 2 (for invalid code), and set other fields to
    // empty
    if (!found)
    {
	throw std::logic_error("Invalid code");
    }

    // Convert the iterator to an integer position (indexed
    // from 1 for R) for use later
    const std::size_t position_val {
	std::distance(std::begin(cats), position) + 1ULL
    };
    
    // If you get here, the code was valid at the current
    // level. The remainder of the function is concerned with
    // whether the current category is the best match, or
    // whether the next category down is better.
    
    // Check for any group exclusions at this level and remove
    // them from the current group list (note that if exclude
    // is not present, NULL is returned, which works fine).
    try {
	std::set<std::string> exclude = position->exclude();
	for (const auto & e : exclude) {
	    groups.erase(e);
	}
    } catch (const Rcpp::index_out_of_bounds &) {
	// No exclude tag present, no need to remove anything,
	// groups is still valid
    }
   
    // If there is a subcategory, make a call to this function
    // to process the next category down. Otherwise you are
    // at a leaf node, so start returning up the call graph.
    // TODO: since this function is linearly recursive,
    // there should be a tail-call optimisation available here
    // somewhere.

    if (position->has_subcats()) {

	// Query that category for the code indices
	ParseResult res {
	    icd10_str_to_indices_impl(str,
				      position->get_subcats(),
				      groups)
	};
	
        // Code from here onwards is in the reverse pass of the
        // call tree (i.e. we are moving up the tree now, towards
        // more general categories).
	res.add_index(position_val);	
	return res;
    }
    else // the category is a leaf node (a single-code category)
    {
        // This section handles two cases
        // 1) Codes that exactly match a code leaf node
        // 2) Codes that exactly match a code leaf node,
        //    but also contain un-parsed trailing matter
        // The case where a code does not match any of the
        // code leaf nodes is handled in the detect_index

        // Use the start of the index of the code as a pattern
        // to search for at the start of the string (the
	// leaf node only has a single index value, hence [0])
	const std::string index{position->index()[0]};
	const std::string name{position->category()};
	// const std::regex pattern{"^" + index};
	// int type{0};
        // if (std::regex_search(index, pattern))
        // {
	// Then the code index agrees with the string
	// at the start. Check for trailing matter
	if (index.size() < str.size()) {
	    std::string trailing{
		str.substr(index.size(), str.size())
	    };
	    return ParseResult(3, {position_val},
			       groups, name, trailing);
	} else {
	    
	    return ParseResult(0, {position_val},
			       groups, name);
	}
    }
}

//' Implementation of the ICD-10 parser for vectors of ICD-10 strings
//'
//' This function is the faster implementation of the ICD-10 parser.
//' The input is a vector of strings to be parsed, and the output is
//' a list of three vectors (the same length as the input) containing
//' information about the parsed codes.
//' 
//' @param str The input character vector of strings that should be
//' parsed. The strings can have leading and trailing whitespace,
//' which will be removed before parsing by this function.
//' @param code_def The code definition structure, which is a nested
//' list of lists following the structure of the icd10.yaml file.
//' @return A named list containing indices, type and groups
//' 
//' 
// [[Rcpp::export]]
Rcpp::List new_icd10_impl(const std::vector<std::string> & str,
			  const Rcpp::List & code_def)
{    
    // TODO: fix this -- there should be a proper way to handle
    // an empty list of strings
    std::set<std::string> groups;
    if (Rcpp::as<Rcpp::List>(code_def["groups"]).size() > 0) {
	auto val{Rcpp::as<std::vector<std::string>>(code_def["groups"])};
	groups = std::set<std::string>(val.begin(), val.end());
    }

    // Create separate lists for each output (to avoid doing it in R)
    Rcpp::List lst_indices(str.size());
    Rcpp::NumericVector lst_type(str.size());
    Rcpp::List lst_groups(str.size());
    Rcpp::CharacterVector lst_name(str.size());
    Rcpp::CharacterVector lst_basic_name(str.size());
    
    std::map<std::string, ParseResult> cache;       

    // Unfortunately you cannot parallelise any function which has
    // calls to Rcpp (this is everything I would want to write). Potentially,
    // you can't even parallelise functions that call these kind of functions
    // either (i.e. with furrr), but there must surely be a workaround for that.
    //#pragma omp parallel for 
    for (std::size_t n = 0; n < str.size(); ++n) {

	// Try the cache first, then parse the string
	// Checked that the cache makes almost no difference
	// to the runtime of the function.
	try {
	    ParseResult res = cache.at(str[n]);
	    lst_indices[n] = res.indices();
	    lst_type[n] = res.type();
	    lst_groups[n] = res.groups();
	    lst_name[n] = res.name();
	    lst_basic_name[n] = res.basic_name();
	} catch (const std::out_of_range &) {	
	    try {
		ParseResult res = icd10_str_to_indices_impl(str[n],
							    code_def["categories"],
							    groups);	
		lst_indices[n] = res.indices();
		lst_type[n] = res.type();
		lst_groups[n] = res.groups();
		lst_name[n] = res.name();
		lst_basic_name[n] = res.basic_name();

		cache.insert({str[n], res});
	    } catch (const std::logic_error &) {
		// Catch the invalid code error
		ParseResult res = ParseResult(2, {}, {}, "", str[n]);
		lst_indices[n] = res.indices();
		lst_type[n] = res.type();
		lst_groups[n] = res.groups();
		lst_name[n] = res.name();
		lst_basic_name[n] = res.basic_name();

		cache.insert({str[n], res});
	    }
	}
    }

    // Pre-allocating seems faster than push_back
    Rcpp::List results = Rcpp::List::create(
	Rcpp::_["indices"] = lst_indices,
	Rcpp::_["types"] = lst_type,
	Rcpp::_["groups"] = lst_groups,
	Rcpp::_["name"] = lst_name,
	Rcpp::_["basic_name"] = lst_basic_name);

    return results;
}

