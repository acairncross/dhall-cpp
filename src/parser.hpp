#pragma once
#include "expr.hpp"
#include "grammar.hpp"

#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/parse_tree.hpp>

#include <algorithm>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

namespace dhall {

    struct my_node
        : tao::pegtl::parse_tree::basic_node< my_node >
    {
        std::unique_ptr<Expr> ast_node;
    };

    // Is this even needed at all?
    struct adopt_ast
        : tao::pegtl::parse_tree::apply< adopt_ast >
    {
        template< typename Node, typename... States >
        static void transform( std::unique_ptr< Node >&n, States&&... st )
        {
            assert( n->children.size() == 1 );
            assert( n->ast_node == nullptr );
            assert( n->children.at(0)->ast_node != nullptr );
            std::swap( n->ast_node, n->children.at(0)->ast_node );
        }
    };

    template< typename Rule >
    struct my_selector
        : tao::pegtl::parse_tree::selector< Rule,
            adopt_ast::on<
                interpolation,
                type_selector> >
    {};

    template<> struct my_selector< simple_label > : std::true_type {};

    template<> struct my_selector< quoted_label > : std::true_type {};

    template<> struct my_selector< label > : std::true_type {};

    template<> struct my_selector< nonreserved_label > : std::true_type {};

    template<> struct my_selector< double_quote_char > : std::true_type {};

    template<> struct my_selector< double_quote_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            // The children of this should be the children of *double-quote-chunk
            // Go through each child, checking whether it's an interpolation,
            // a double-quote-escpaped (note: will be missing its backslash), or
            // a double-quote char

            std::vector<Chunks> chunks;

            std::transform(
                n->children.begin(), n->children.end(),
                std::back_inserter(chunks),
                []( std::unique_ptr<my_node>& child ) {
                    if ( child->is_type< interpolation >() ) {
                        // TODO Should steal the unique_ptr, not just use the raw pointer
                        return Chunks({std::make_pair("", child->ast_node.get())}, "");
                    } else if ( child->is_type< double_quote_escaped >() ) {
                        return Chunks({}, child->string() );
                    } else if ( child->is_type< double_quote_char >() ) {
                        return Chunks({}, child->string() );
                    } else {
                        assert( false );
                    }
                }
            );

            // std::cout << "chunks.size(): " << chunks.size() << std::endl;

            Chunks chunk = std::accumulate( chunks.begin(), chunks.end(), Chunks({}, "") );
            n->ast_node = std::make_unique< TextLitExpr >( chunk );

            // assert( n->children.size() == 3); // Actually it will only have 1 child?
            // n->ast_node = std::move( n->children.at(1)->ast_node );
        }
    };

    template<> struct my_selector< single_quote_continue > : std::true_type {};

    template<> struct my_selector< escaped_quote_pair > : std::true_type {};

    template<> struct my_selector< escaped_interpolation > : std::true_type {};

    template<> struct my_selector< single_quote_char > : std::true_type {};

    template<> struct my_selector< single_quote_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            assert( n->children.size() == 1 );  // Just a single_quote_continue
            Chunks chunks;
            my_node* sqc = n->children.at(0).get();
            while ( sqc->children.size() == 2 ) {
                if ( sqc->children.at(0)->is_type< interpolation >() ) {
                    // Is using the raw pointer the correct thing to do? So it's valid as long as the AST
                    // exists and is unmodified
                    chunks += Chunks( { { "", sqc->children.at(0)->ast_node.get() } } , "" );
                } else if ( sqc->children.at(0)->is_type< escaped_quote_pair >() ) {
                    chunks += Chunks( {}, "''" );
                } else if ( sqc->children.at(0)->is_type< escaped_interpolation >() ) {
                    chunks += Chunks( {}, "${" );
                } else if ( sqc->children.at(0)->is_type< single_quote_char >() ) {
                    // TODO consume multiple characters at once?
                    chunks += Chunks( {}, sqc->children.at(0)->string() );
                } else {
                    assert( false );
                }
                sqc = sqc->children.at(1).get();
            }

            assert( sqc->children.empty() );  // End of text literal

            n->ast_node = std::make_unique< TextLitExpr >( chunks );
        }
    };

#define DEFINE_CONST(rule) \
    template<> struct my_selector< rule > : std::true_type \
    { \
        static void transform( std::unique_ptr< my_node >& n) { \
            n->ast_node = std::make_unique< ConstExpr >( Const::rule ); \
        } \
    }

    DEFINE_CONST(Type);
    DEFINE_CONST(Kind);
    DEFINE_CONST(Sort);

#undef DEFINE_CONST

#define DEFINE_BUILTIN(rule, builtin) \
    template<> struct my_selector< rule > : std::true_type \
    { \
        static void transform( std::unique_ptr< my_node >& n) { \
            n->ast_node = std::make_unique< BuiltinExpr >( Builtin::builtin ); \
        } \
    }

    DEFINE_BUILTIN(Bool, Bool);
    DEFINE_BUILTIN(Natural, Natural);
    DEFINE_BUILTIN(Natural_fold, NaturalFold);
    DEFINE_BUILTIN(Natural_build, NaturalBuild);
    DEFINE_BUILTIN(Natural_isZero, NaturalIsZero);
    DEFINE_BUILTIN(Natural_even, NaturalEven);
    DEFINE_BUILTIN(Natural_odd, NaturalOdd);
    DEFINE_BUILTIN(Natural_toInteger, NaturalToInteger);
    DEFINE_BUILTIN(Natural_show, NaturalShow);
    DEFINE_BUILTIN(Integer, Integer);
    DEFINE_BUILTIN(Integer_clamp, IntegerClamp);
    DEFINE_BUILTIN(Integer_negate, IntegerNegate);
    DEFINE_BUILTIN(Integer_show, IntegerShow);
    DEFINE_BUILTIN(Integer_toDouble, IntegerToDouble);
    DEFINE_BUILTIN(Double, Double);
    DEFINE_BUILTIN(Double_show, DoubleShow);
    DEFINE_BUILTIN(Text, Text);
    DEFINE_BUILTIN(Text_show, TextShow);
    DEFINE_BUILTIN(List, List);
    DEFINE_BUILTIN(List_build, ListBuild);
    DEFINE_BUILTIN(List_fold, ListFold);
    DEFINE_BUILTIN(List_length, ListLength);
    DEFINE_BUILTIN(List_head, ListHead);
    DEFINE_BUILTIN(List_last, ListLast);
    DEFINE_BUILTIN(List_indexed, ListIndexed);
    DEFINE_BUILTIN(List_reverse, ListReverse);
    DEFINE_BUILTIN(Optional, Optional);
    DEFINE_BUILTIN(None, None);
    DEFINE_BUILTIN(Optional_fold, OptionalFold);
    DEFINE_BUILTIN(Optional_build, OptionalBuild);

#undef DEFINE_BUILTIN

#define DEFINE_BOOLLIT(rule, value) \
    template<> struct my_selector< rule > : std::true_type \
    { \
        static void transform( std::unique_ptr< my_node >& n) { \
            n->ast_node = std::make_unique< BoolLitExpr >( value ); \
        } \
    }

    DEFINE_BOOLLIT(True, true);
    DEFINE_BOOLLIT(False, false);

#undef DEFINE_BOOLLIT
    

    template<> struct my_selector< double_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<DoubleLitExpr>(std::stod(n->string()));
        }
    };

    template<> struct my_selector< natural_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<NaturalLitExpr>(std::stoi(n->string()));
        }
    };

    template<> struct my_selector< integer_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<IntegerLitExpr>(std::stoi(n->string()));
        }
    };

    template<> struct my_selector< variable > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            int index = n->children.size() > 1 ? std::stoi( n->children.at(1)->string() ) : 0;
            n->ast_node = std::make_unique<VarExpr>( Var(n->children.at(0)->string(), index) );
        }
    };

    template<> struct my_selector< missing > : std::true_type {};

    template<> struct my_selector< local > : std::true_type {};
    template<> struct my_selector< parent_path > : std::true_type {};
    template<> struct my_selector< here_path > : std::true_type {};
    template<> struct my_selector< home_path > : std::true_type {};
    template<> struct my_selector< absolute_path > : std::true_type {};

    template<> struct my_selector< http > : std::true_type {};
    template<> struct my_selector< http_raw > : std::true_type {};
    template<> struct my_selector< scheme > : std::true_type {};
    template<> struct my_selector< authority > : std::true_type {};
    template<> struct my_selector< url_path > : std::true_type {};
    template<> struct my_selector< query > : std::true_type {};

    template<> struct my_selector< env > : std::true_type {};
    template<> struct my_selector< bash_environment_variable > : std::true_type {};
    template<> struct my_selector< posix_environment_variable > : std::true_type {};

    template<> struct my_selector< hash > : std::true_type {};

    template<> struct my_selector< import_hashed > : std::true_type {};

    template<> struct my_selector< import > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            auto& import_hashed_n = n->children.at(0);

            auto& import_type_n = import_hashed_n->children.at(0);

            std::unique_ptr< ImportType > import_type;
            if ( import_type_n->is_type< missing >() ) {
                import_type = std::make_unique< MissingImportType >();
            } else if ( import_type_n->is_type< local >() ) {
                auto& local_n = import_type_n->children.at(0);
                if ( local_n->is_type< parent_path >() ) {
                    import_type = std::make_unique< LocalImportType >(
                        std::string( local_n->string(), 0, 2 ), // ..
                        std::string( local_n->string(), 2, local_n->string().size() - 2 ));
                } else if ( local_n->is_type< here_path >() ) {
                    import_type = std::make_unique< LocalImportType >(
                        std::string( local_n->string(), 0, 1 ), // .
                        std::string( local_n->string(), 1, local_n->string().size() - 1 ));
                } else if ( local_n->is_type< home_path >() ) {
                    import_type = std::make_unique< LocalImportType >(
                        std::string( local_n->string(), 0, 2 ), // ~/
                        std::string( local_n->string(), 2, local_n->string().size() - 2));
                } else if ( local_n->is_type< absolute_path >() ) {
                    import_type = std::make_unique< LocalImportType >("", local_n->string());
                } else {
                    assert( false );
                }
            } else if ( import_type_n->is_type< http >() ) {
                // 0: http-raw
                // (1): (using) import-expression
                URL url;
                auto& http_raw_n = import_type_n->children.at(0);
                auto scheme = http_raw_n->children.at(0)->string();
                auto authority = http_raw_n->children.at(1)->string();
                auto path = http_raw_n->children.at(2)->string();
                if ( http_raw_n->children.size() > 3 ) {
                    auto query = http_raw_n->children.at(3)->string();
                    url = URL( scheme, authority, path, query, nullptr );
                } else {
                    url = URL( scheme, authority, path, "", nullptr );
                }
                if ( import_type_n->children.size() > 1 ) {
                    url.headers = std::move(import_type_n->children.at(1)->ast_node);
                }
                import_type = std::make_unique< RemoteImportType >(
                    std::move(url));
            } else if ( import_type_n->is_type< env >() ) {
                import_type = std::make_unique< EnvImportType >(
                    // children.at(0) regardless of bash/posix-environment-variable
                    import_type_n->children.at(0)->string() );
            } else {
                assert( false );
            }

            std::string hash = import_hashed_n->children.size() > 1 ? import_hashed_n->children.at(1)->string() : ""; // FIXME This will include the sha256:
            auto import_hashed = std::make_unique<ImportHashed>(hash, std::move(import_type));

            ImportMode import_mode =
              n->children.size() > 1
              ? (n->children.at(1)->string() == "Text" ? ImportMode::RawText : ImportMode::Location)
              : ImportMode::Code;

            auto import = std::make_unique<Import>(std::move(import_hashed), import_mode);

            n->ast_node = std::make_unique<EmbedExpr>(std::move(import));
            
            // 0: import-hashed
            // (1): text / location

            // import-hashed
            //   0: import-type
            //   (1): hash

            // import-type
            // 0: missing / local / http / env

            // Need: (Hash), ImportType, ImportMode
        }
    };

    template<> struct my_selector< lambda_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            // Non reserved label?
            n->ast_node = std::make_unique<LamExpr>(
                // FIXME Should use read_label
                n->children.at(0)->string(),
                std::move(n->children.at(1)->ast_node),
                std::move(n->children.at(2)->ast_node));
        }
    };

    template<> struct my_selector< if_then_else_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique< BoolIfExpr >(
                std::move(n->children.at(0)->ast_node),
                std::move(n->children.at(1)->ast_node),
                std::move(n->children.at(2)->ast_node));
        }
    };

    template<> struct my_selector< let_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            // first n nodes are let-bindings which have a label, optionally a
            // type annotation, and a body
            // final node is the let-expression body

            std::vector< Binding > bindings;
            // it ranges through all the let_binding nodes
            for ( auto it = n->children.begin(); it != n->children.end() - 1; ++it ) {
                auto& let_binding_n = *it;
                assert( let_binding_n->is_type< let_binding >() );
                assert( let_binding_n->children.at(0)->is_type< nonreserved_label >() );
                // FIXME Should use read_label
                std::string label = let_binding_n->children.at(0)->string();

                std::unique_ptr<Expr> type =
                    let_binding_n->children.size() == 2 ? nullptr : std::move(let_binding_n->children.at(1)->ast_node);
                std::unique_ptr<Expr> value =
                    std::move( let_binding_n->children.at( let_binding_n->children.size() - 1 )->ast_node );

                bindings.emplace_back( label, std::move(type), std::move(value) );
            }

            std::unique_ptr< Expr > body = std::move( n->children.back()->ast_node );

            for ( auto it = bindings.rbegin(); it != bindings.rend(); ++it ) {
                body = std::make_unique< LetExpr >(std::move(*it), std::move(body));
            }

            n->ast_node = std::move( body );
        }
    };

    template<> struct my_selector< forall_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<LamExpr>(
                n->children.at(0)->string(), // FIXME Use read_label
                std::move(n->children.at(1)->ast_node),
                std::move(n->children.at(2)->ast_node));
        }
    };

    template<> struct my_selector< arrow_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<PiExpr>(
                "_",
                std::move(n->children.at(0)->ast_node),
                std::move(n->children.at(1)->ast_node));
        }
    };

    // TODO merge_expression

    template<> struct my_selector< merge_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<MergeExpr>(
                std::move(n->children.at(0)->ast_node),
                std::move(n->children.at(1)->ast_node),
                std::move(n->children.at(2)->ast_node));
        }
    };

    // Don't need TODO empty_list_literal_expression
    // TODO toMap_expression

    template<> struct my_selector< assert_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<AssertExpr> (
                std::move(n->children.at(0)->ast_node));
        }
    };

    template<> struct my_selector< annotated_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            if ( n->children.size() > 1 ) {
                n->ast_node = std::make_unique<AnnotExpr>(
                    std::move(n->children.at(0)->ast_node),
                    std::move(n->children.at(1)->ast_node));
            } else {
                n->ast_node = std::move( n->children.at(0)->ast_node );
            }
        }
    };

    template<> struct my_selector< let_binding > : std::true_type {};

    template<> struct my_selector< empty_list_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique< ListLitExpr >( std::move(n->children.at(0)->ast_node), std::vector< std::unique_ptr< Expr > >{} );
        }
    };

#define DEFINE_BINOP(rule, op) \
    template<> struct my_selector< rule > : std::true_type \
    { \
        static void transform( std::unique_ptr< my_node >& n) { \
            assert( n->children.size() > 0); \
 \
            n->ast_node = std::move( n->children.front()->ast_node ); \
 \
            for ( auto it = n->children.begin() + 1; it != n->children.end(); ++it ) { \
                n->ast_node = std::make_unique< BinopExpr >( Binop::op, std::move((*it)->ast_node), std::move(n->ast_node) ); \
            } \
        } \
    }

    DEFINE_BINOP( import_alt_expression, ImportAlt );
    DEFINE_BINOP( or_expression, BoolOr );
    DEFINE_BINOP( plus_expression, NaturalPlus );
    DEFINE_BINOP( text_append_expression, TextAppend );
    DEFINE_BINOP( list_append_expression, ListAppend );
    DEFINE_BINOP( and_expression, BoolAnd );
    DEFINE_BINOP( combine_expression, Combine );
    DEFINE_BINOP( prefer_expression, Prefer );
    DEFINE_BINOP( combine_types_expression, TypeCombine );
    DEFINE_BINOP( times_expression, NaturalTimes );
    DEFINE_BINOP( equal_expression, BoolEq );
    DEFINE_BINOP( not_equal_expression, BoolNeq );
    DEFINE_BINOP( equivalent_expression, Equivalent );

#undef DEFINE_BINOP

    template<> struct my_selector< application_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            assert( n->children.size() > 0);

            n->ast_node = std::move( n->children.front()->ast_node );

            for ( auto it = n->children.begin() + 1; it != n->children.end(); ++it ) {
                n->ast_node = std::make_unique< AppExpr >( std::move((*it)->ast_node), std::move(n->ast_node) );
            }
        }
    };

    template<> struct my_selector< merge_first_application_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<MergeExpr>(
                std::move(n->children.at(0)->ast_node),
                std::move(n->children.at(1)->ast_node),
                nullptr);
        }
    };

    template<> struct my_selector< some_first_application_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<SomeExpr>( std::move(n->children.at(0)->ast_node ));
        }
    };

    template<> struct my_selector< tomap_first_application_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<ToMapExpr>( std::move(n->children.at(0)->ast_node), nullptr );
        }
    };

    template<> struct my_selector< completion_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            if ( n->children.size() == 1 ) {
                n->ast_node = std::move( n->children.at(0)->ast_node );
            } else if ( n->children.size() == 2 ) {
                n->ast_node = std::make_unique< RecordCompletionExpr >(
                    std::move( n->children.at(0)->ast_node ),
                    std::move( n->children.at(1)->ast_node ));
            } else {
                assert( false );
            }
        }
    };

    template<> struct my_selector< selector_expression > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::move( n->children.at(0)->ast_node );

            for ( auto it = n->children.begin() + 1; it != n->children.end(); ++it ) {
                if ( (*it)->is_type< any_label >() ) {
                    n->ast_node = std::make_unique<FieldExpr>( std::move(n->ast_node), (*it)->string() );
                } else if ( (*it)->is_type< labels >() ) {
                    std::vector<std::string> fields;
                    std::transform( (*it)->children.begin(), (*it)->children.end(), std::back_inserter( fields ), [](auto& child) { return child->string(); } ); // TODO use read_label in case quoted? And they could be Some?
                    n->ast_node = std::make_unique<ProjectSetExpr>( std::move(n->ast_node), fields );
                } else if ( (*it)->is_type< type_selector >() ) {
                    n->ast_node = std::make_unique<ProjectExprExpr>( std::move(n->ast_node), std::move((*it)->ast_node) );
                }
            }
        }
    };

    template<> struct my_selector< any_label > : std::true_type {};
    template<> struct my_selector< labels > : std::true_type {};

    template<> struct my_selector< non_empty_list_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            std::vector<std::unique_ptr<Expr>> elements;
            for ( auto& child : n->children ) {
                elements.push_back( std::move(child->ast_node) );
            }

            n->ast_node = std::make_unique<ListLitExpr>( nullptr, std::move(elements) );
        }
    };

    template<> struct my_selector< empty_record_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique< RecordLitExpr >(
                std::map< std::string, std::unique_ptr<Expr> >() );
        }
    };

    template<> struct my_selector< empty_record_type > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique< RecordExpr >(
                std::map< std::string, std::unique_ptr<Expr> >() );
        }
    };

    template<> struct my_selector< non_empty_record_type > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            // TODO Handle "Some" labels?
            std::map< std::string, std::unique_ptr< Expr > > rec;

            for ( auto& child : n->children ) {
                rec.emplace( child->children.at(0)->string(), std::move(child->children.at(1)->ast_node) );
            }

            n->ast_node = std::make_unique<RecordExpr>( std::move(rec) );
        }
    };

    template<> struct my_selector< record_type_entry > : std::true_type {};

    template<> struct my_selector< non_empty_record_literal > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            // TODO Handle "Some" labels?
            std::map< std::string, std::unique_ptr< Expr > > rec;

            for ( auto& child : n->children ) {
                rec.emplace( child->children.at(0)->string(), std::move(child->children.at(1)->ast_node) );
            }

            n->ast_node = std::make_unique<RecordLitExpr>( std::move(rec) );
        }
    };

    template<> struct my_selector< record_literal_entry > : std::true_type {};

    template<> struct my_selector< empty_union_type > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            n->ast_node = std::make_unique<UnionExpr>(
                std::map<std::string, std::unique_ptr<Expr>>());
        }
    };

    template<> struct my_selector< non_empty_union_type > : std::true_type
    {
        static void transform( std::unique_ptr< my_node >& n ) {
            // TODO Handle "Some" labels?
            std::map< std::string, std::unique_ptr< Expr > > rec;

            for ( auto& child : n->children ) {
                rec.emplace( child->children.at(0)->string(), child->children.size() > 1 ? std::move(child->children.at(1)->ast_node) : nullptr );
            }

            n->ast_node = std::make_unique<UnionExpr>( std::move(rec) );
        }
    };

    template<> struct my_selector< union_type_entry > : std::true_type {};

}
