#include "expr.hpp"
#include "grammar.hpp"
#include "parser.hpp"

#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/parse_tree.hpp>
#include <tao/pegtl/contrib/parse_tree_to_dot.hpp>
#include <tao/pegtl/contrib/tracer.hpp>

#include <iostream>
#include <string>

namespace pegtl = tao::pegtl;

void print_dot_ast( std::ostream& os, const dhall::Expr& expr )
{
    os << "digraph ast\n{\n";
    expr.PrintDot( os );
    os << "}\n";
}

int main(int argc, char** argv) {
    std::string name;

    std::string filename( argv[1] );
    pegtl::file_input in( filename );

    auto root = tao::pegtl::parse_tree::parse< dhall::complete_expression, dhall::my_node, dhall::my_selector >(in);
    // auto root = tao::pegtl::parse_tree::parse< dhall::complete_expression, dhall::my_node, dhall::my_selector, tao::pegtl::nothing, tao::pegtl::tracer >(in);

    // pegtl::parse_tree::print_dot(std::cout, *root);
    // std::unique_ptr< dhall::Expr > ast = std::move( root->children.at(0)->ast_node );
    // print_dot_ast( std::cout, *ast );

    if ( root != nullptr ) {
        exit(0);
    } else {
        exit(1);
    }
}
