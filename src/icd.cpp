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

#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>

// Results structure for ICD parse
class ParseResult
{
public:
    ParseResult(int type) : type_{type} {}

    int type() const { return type_; }
    std::vector<std::size_t> indices() const { return indices_; }
    std::string trailing() const { return trailing_; }
    std::vector<std::string> groups() const { return groups_; }
    
private:
    int type_{0};
    std::vector<std::size_t> indices_{};
    std::string trailing_{""};
    std::vector<std::string> groups_{};
};

// Remove
struct Index
{
    std::string start_;
    std::string end_;
    Index(const std::vector<std::string> & index)
    {
	if (index.size() == 0) {
	    throw std::logic_error("Index cannot be length zero");
	}
	if (index.size() == 2) {
	    start_ = index[0];
	    end_ = index[1];
	} else {
	    start_ = index[0];
	    end_ = index[0];
	}
    }
};

class Cat
{
public:
    Cat(const Rcpp::List & cat)
    {
	// This is only here because I need to use =
	cat_ = cat;
    }
    Rcpp::String category() const
    {
	return Rcpp::as<Rcpp::String>(cat_["category"]);
    }

    // Return true if code is (lexicographically) contained
    // in the range specified by the index of this Cat
    bool contains(const std::string & str)
    {
	auto idx{index()};
	if (idx.size() == 2) {
	    (str >= idx[0]) && (str <= idx[1]);
	} else {
	    // Truncate the string to the length of the index
	    std::string trunc{str.substr(0, idx[0].size())}   
	    trunc == idx[0];
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

bool operator < (const Rcpp::String & str, const Cat & cat)
{
    Rcpp::List idx = cat.index();
    // Only need the first 
    return str < Rcpp::as<Rcpp::String>(idx[0]); 
}

// Not sure what the 'true' means
Rcpp::Rostream<true> & operator << (Rcpp::Rostream<true> & os, const Cat & cat)
{
    // This shouldn't be needed (the cstring), but couldn't
    // figure out why it wasn't working without it -- moved on.
    Rcpp::Rcout << cat.category().get_cstring();
    
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
//' @param str The input character vector of strings that should be
//' parsed. The strings can have leading and trailing whitespace,
//' which will be removed before parsing by this function.
//' @param code_def The code definition structure, which is a nested
//' list of lists following the structure of the icd10.yaml file.
//' @return A named list containing indices, type and groups
//' 
//' 
ParseResult icd10_str_to_indices_impl(const Rcpp::String & str,
				      const Rcpp::List & codes,
				      const Rcpp::List & groups)
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
	Rcpp::Rcout << cats.back() << std::endl;
    }

    // Perform the binary search
    auto lower = std::lower_bound(std::begin(cats), std::end(cats), str);
    const bool found = (lower != std::end(cats)) && (*lower == str);
    
    
    return ParseResult{0};


    //return indices;
    
    // === R impl to find the position ========================
    // position <- codes %>%
    //      purrr::map("index") %>%
    //     // to obtain the first TRUE, which is
    //     // the category that str is contained in
    //     purrr::detect_index(function(x) {

    //         // Truncate the str to the same length
    //         // as the start and end codes, because
    //         // the end of the range is interpreted
    //         // as anything beginning with this
    //         // string
    //         trunc <- substr(str,1,nchar(x[[1]]))
            
    //         if (length(x) == 2) {
    //             // If the index is a range, check that
    //             // str lies in the range.
    //             (trunc >= x[[1]]) && (trunc <= x[[2]])
    //         }
    //         else
    //         {
    //             // If the index is a single item,
    //             // truncate str to the length of
    //             // x and compare for equality
    //             trunc == x[[1]]
    //         }
    //     })
    // ==============================================
    
    // If position is 0, then a match was not found. This
    // means that the str is not a valid member of any member
    // of this level, so it is not a valid code. Return a
    // type of 2 (for invalid code), and set other fields to
    // empty

    // === Old R impl ================================
    // if (position == 0)
    // {
    //     rlang::abort("error_invalid",
    //                  message = "ICD-10 code was not valid",
    //                  result = list(
    //                      indices = list(),
    //                      type = c(2),
    //                      trailing = str,
    //                      groups = list()
    //                  ))
                     
    // }
    //==============================================

    // If you get here, the code was valid at the current
    // level. The remainder of the function is concerned with
    // whether the current category is the best match, or
    // whether the next category down is better.

    // Check for any group exclusions at this level and remove
    // them from the current group list (note that if exclude
    // is not present, NULL is returned, which works fine).

    // ==== OLD R impl =======
    //groups <- setdiff(groups, codes[[position]]$exclude)
    // ===============

    // If there is a subcategory, make a call to this function
    // top process the next category down. Otherwise you are
    // at a leaf node, so start returning up the call graph

    // ==== Old R impl =================================
    // if (!is.null(codes[[position]]$category))
    // {   
    //     // Query that category for the code indices
    //     res <- icd10_str_to_indices(str, codes[[position]]$child, groups)
    //     x <- res$indices
    //     t <- res$type
    //     s <- res$trailing
    //     g <- res$groups
        
    //     // Code from here onwards is in the reverse pass of the
    //     // call tree (i.e. we are moving up the tree now, towards
    //     // more general categories). The x returned above
    //     // contains a -1 if the next level down was not a better
    //     // match for the code. In which case, we use the current
    //     // level as the best match and return the indices to
    //     // this level.
    //     // TODO: this whole thing is drop where == -1, replace
    //     // with one liner
    //     val <- tail(x, n=1)
    //     if (val > 0)
    //     {
    //         // Return the entire list
    //         list(
    //             indices = c(position, x),
    //             type = t,
    //             trailing = s,
    //             groups = g
    //         )
    //     }
    //     else if (val == -1)
    //     {
    //         // The code is not better matched by the next level
    //         // down. In this case, drop 
	//         list(
	//             indices = c(position, head(x, n=-1)),
	//             type = t,
	//             trailing = s,
	//             groups = g
	//         )
	//     }
	// }
	// else if (!is.null(codes[[position]]$code))
	// {
	//     // This section handles two cases
	//     // 1) Codes that exactly match a code leaf node
	//     // 2) Codes that exactly match a code leaf node,
	//     //    but also contain un-parsed trailing matter
	//     // The case where a code does not match any of the
	//     // code leaf nodes is handled in the detect_index

	//     // Use the start of the index of the code as a pattern
	//     // to search for at the start of the string
	//     index <- codes[[position]]$index
	//     pattern <- paste0("^", index)
	//     if (grepl(pattern, str))
	//     {
	//         // Then the code index agrees with the string
	//         // at the start. Check for trailing matter
	//         if (nchar(index) < nchar(str))
	//         {
	//             list(
	//                 indices = c(position),
	//                 type = 3,
	//                 trailing = substr(str,
	//                                   nchar(index)+1,
	//                                   nchar(str)),
	//                 groups = groups
	//             )
	//         }
	//         else
	//         {
	//             // Exact match
	//             list(
	//                 indices = c(position),
	//                 type = 0,
	//                 groups = groups
	//             )
	//         }
	//     }
	// }
	// ===============================
	
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
Rcpp::List new_icd10_impl(const Rcpp::CharacterVector & str,
			  const Rcpp::List & code_def)
{
    ParseResult res{
	icd10_str_to_indices_impl(str[0],
				  code_def["child"],
				  code_def["groups"])
    };
    
    return Rcpp::List::create(Rcpp::_["indices"] = res.type(),
			      Rcpp::_["type"] = "type",
			      Rcpp::_["groups"] = "groups");
}
