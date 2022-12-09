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
#include <list>
#include <vector>
#include <algorithm>
#include <iostream>
#include <regex>

// Results structure for ICD parse
class ParseResult
{
public:
    ParseResult(int type, const std::list<std::size_t> & indices,
		const std::vector<std::string> & groups,
		const std::string & trailing)
	: type_{type}, indices_{indices},
	  trailing_{trailing}, groups_{groups}
    {}

    ParseResult(int type, const std::list<std::size_t> & indices,
		const std::vector<std::string> & groups)
	: type_{type}, indices_{indices}, groups_{groups}
    {}

    ParseResult(int type) : type_{type} {}

    Rcpp::List to_R_list() const {
	return Rcpp::List::create(Rcpp::_["indices"] = indices_,
				  Rcpp::_["type"] = type_,
				  Rcpp::_["trailing"] = trailing_,
				  Rcpp::_["groups"] = groups_);
    }
    
    // The type -- whether the parse succeeded or not
    int type() const { return type_; }
    
    // The index list which shows where the code is in
    // the codes definition structure. This is a list
    // because of the need to push_front (this can be
    // removed with some refactoring -- however, might
    // not be a performance issue due to the need to copy
    // to an Rcpp::List at the end anyway.
    std::list<std::size_t> indices() const { return indices_; }

    // Any unparsed trailing matter
    std::string trailing() const { return trailing_; }

    // The set of groups associated with this code
    std::vector<std::string> groups() const { return groups_; }

    // Prepend an index to the indices list
    void add_index(std::size_t position_val)
    {
	indices_.push_front(position_val);
    }
    
private:
    int type_{0};
    std::list<std::size_t> indices_{};
    std::string trailing_{""};
    std::vector<std::string> groups_{};
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
    std::vector<std::string> exclude() const
    {
	return Rcpp::as<std::vector<std::string>>(cat_["exclude"]);
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
	if (idx.size() == 2) {
	    return (str >= idx[0]) && (str <= idx[1]);
	} else {
	    // Truncate the string to the length of the index
	    std::string trunc{str.substr(0, idx[0].size())};
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

//' \brief Comparison of category with raw code
//'
//' This function is the main comparison operator that is
//' used in the binary search to find a raw code in the ICD-10
//' codes file.
//' 
//' Each category has an index, which is the range of codes
//' that this category contains. The index is of the form
//' (M,N), where M and N are code roots -- e.g. (B15,B19).
//' The top of this range is inclusive, meaning that
//' the code B199 would be inside this category, even
//' though B199 > B19 lexicographically. The trick is to
//' truncate the code B199 to the length of the upper end
//' of the range before doing the check. There are also
//' categories that contain a degenerate range, of the
//' form (M,) -- e.g. (A01,). Here, any code that starts
//' with A01 is in the category (again, truncate and compare).
//'
//' The program below uses a binary search to find which
//' category a code belongs to, based on the std::upper_bound
//' function. For the purpose of this search, the categories
//' are ordered by the first element of their index, M, because
//' this guarantees that the upper element N is also ordered
//' (categories do not overlap, although there are gaps between
//' categories). For the particular string str of interest,
//' the upper bound category is the first cat such that
//' cat > str. This means that the previous cat was the last
//' cat where cat <= str. If this is applied to the start
//' of the range M, then this is the right condition to
//' find the best candidate category (where M <= str, and
//' M is maximal)
//'
//' The upper bound is enough to guarantee that "if str is
//' in any category, it is in this one". It is still necessary
//' to check that the str is not larger than the upper end
//' of the range, N. This check is performed after the search
//' is complete.
//'
bool operator < (const std::string & str, const Cat & cat)
{
    auto idx{cat.index()};
    // Only need the first element M for < operator
    // (see the comments above)
    return str < idx[0];
    
}

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

//' Parse a single ICD-10 string into a vector of indices that locates
//' it in the code definition structure, and handle parsing errors. This
//' function is the counterpart to the R function icd10_str_to_indices.
//'
//' The returned object is a list containing the following fields:
//' - type: a integer describing the result of the parse. 0 for
//'   success; 1 for empty string; 2 for invalid code.
//' - indices: if success, a vector of indices locating
//' - trailing: an content at the end of the string that was not
//'   parsed (sometimes ICD-10 codes contain trailing matter -- the
//'   goal is to eventually interpret all of this)
//' - groups: a character vector of groups that contain this code
//'
//' This function is the faster implementation of the ICD-10 parser.
//' The input is a vector of strings to be parsed, and the output is
//' a list of three vectors (the same length as the input) containing
//' information about the parsed codes.
//'
//' Note: passing groups by value is deliberate, because it is
//' modified and should not modify the caller's version. Todo:
//' try to remove this, it might be possible to keep only one
//' copy and modify it globally.
//' 
//' @param str The input character vector of strings that should be
//' parsed. The strings can have leading and trailing whitespace,
//' which will be removed before parsing by this function.
//' @param code_def The code definition structure, which is a nested
//' list of lists following the structure of the icd10.yaml file.
//' @return A named list containing indices, type and groups
//' 
//' 
ParseResult icd10_str_to_indices_impl(const std::string & str,
				      const Rcpp::List & codes,
				      std::vector<std::string> groups)
{
    // Check for empty string. Return type = -1 if empty
    // and set all other fields to empty
    const std::string std_str{str};
    if(std::all_of(std_str.begin(), std_str.end(), isspace)) {
	return ParseResult{-1};
    }   
    
    // Look through the index keys at the current level
    // and find the position of the code. Inside the codes
    // structure, the index keys provide an array to search
    // (using binary search) for the ICD code in str.

    //
    
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
	return ParseResult{2};                     
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
	std::vector<std::string> diff;
	std::vector<std::string> exclude = position->exclude();

	// Move the elements to diff, and then swap diff with
	// groups afterwords.
	std::set_difference(std::make_move_iterator(std::begin(groups)),
			    std::make_move_iterator(std::end(groups)),
			    std::begin(exclude), std::end(exclude),
			    std::inserter(diff, std::begin(diff)));
	groups.swap(diff);
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
	const std::regex pattern{"^" + index};
        if (std::regex_search(index, pattern))
        {
            // Then the code index agrees with the string
            // at the start. Check for trailing matter
            if (index.size() < str.size())
            {
		std::string trailing{
		    str.substr(index.size() + 1, str.size())
		};
		
		return ParseResult(3, {position_val},
				   groups, trailing);
		    }
            else
		{
		    // Exact match (no need for trailing)
		return ParseResult(0, {position_val},
				   groups);
            }
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

    Rcpp::List results(str.size());
    for (std::size_t n{0}; n < str.size(); ++n) {
	auto res{icd10_str_to_indices_impl(str[n],
					   code_def["child"],
					   code_def["groups"])};	
	results[n] = res.to_R_list();
    }

    return results;
        
}

