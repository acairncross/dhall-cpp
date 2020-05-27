#pragma once
#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/icu/utf8.hpp>

namespace dhall {

struct end_of_line
    : tao::pegtl::sor<tao::pegtl::one<0x0A>, tao::pegtl::string<0x0D, 0x0A>> {};
struct valid_non_ascii
    : tao::pegtl::sor<tao::pegtl::utf8::range<0x80, 0xD7FF>,
                 tao::pegtl::utf8::range<0xE000, 0xFFFD>,
                 tao::pegtl::utf8::range<0x10000, 0x1FFFD>,
                 tao::pegtl::utf8::range<0x20000, 0x2FFFD>,
                 tao::pegtl::utf8::range<0x30000, 0x3FFFD>,
                 tao::pegtl::utf8::range<0x40000, 0x4FFFD>,
                 tao::pegtl::utf8::range<0x50000, 0x5FFFD>,
                 tao::pegtl::utf8::range<0x60000, 0x6FFFD>,
                 tao::pegtl::utf8::range<0x70000, 0x7FFFD>,
                 tao::pegtl::utf8::range<0x80000, 0x8FFFD>,
                 tao::pegtl::utf8::range<0x90000, 0x9FFFD>,
                 tao::pegtl::utf8::range<0xA0000, 0xAFFFD>,
                 tao::pegtl::utf8::range<0xB0000, 0xBFFFD>,
                 tao::pegtl::utf8::range<0xC0000, 0xCFFFD>,
                 tao::pegtl::utf8::range<0xD0000, 0xDFFFD>,
                 tao::pegtl::utf8::range<0xE0000, 0xEFFFD>,
                 tao::pegtl::utf8::range<0xF0000, 0xFFFFD>,
                 tao::pegtl::utf8::range<0x100000, 0x10FFFD>> {};
struct tab : tao::pegtl::one<0x09> {};
struct block_comment_continue;
struct block_comment
    : tao::pegtl::seq<tao::pegtl::string<'{', '-'>, block_comment_continue> {};
struct block_comment_char : tao::pegtl::sor<tao::pegtl::range<0x20, 0x7F>,
                                            valid_non_ascii, tab, end_of_line> {};
struct block_comment_continue
    : tao::pegtl::sor<
          tao::pegtl::string<'-', '}'>,
          tao::pegtl::seq<block_comment, block_comment_continue>,
          tao::pegtl::seq<block_comment_char, block_comment_continue>> {};
struct not_end_of_line
    : tao::pegtl::sor<tao::pegtl::range<0x20, 0x7F>, valid_non_ascii, tab> {};
struct line_comment
    : tao::pegtl::seq<tao::pegtl::string<'-', '-'>,
                      tao::pegtl::star<not_end_of_line>, end_of_line> {};
struct whitespace_chunk
    : tao::pegtl::sor<tao::pegtl::one<' '>, tab, end_of_line, line_comment,
                      block_comment> {};
struct whsp : tao::pegtl::star<whitespace_chunk> {};
struct whsp1 : tao::pegtl::plus<whitespace_chunk> {};
struct ALPHA : tao::pegtl::sor<tao::pegtl::range<0x41, 0x5A>,
                               tao::pegtl::range<0x61, 0x7A>> {};
struct DIGIT : tao::pegtl::range<0x30, 0x39> {};
struct ALPHANUM : tao::pegtl::sor<ALPHA, DIGIT> {};
struct HEXDIG
    : tao::pegtl::sor<DIGIT, tao::pegtl::istring<'A'>, tao::pegtl::istring<'B'>,
                      tao::pegtl::istring<'C'>, tao::pegtl::istring<'D'>,
                      tao::pegtl::istring<'E'>, tao::pegtl::istring<'F'>> {};
struct simple_label_first_char : tao::pegtl::sor<ALPHA, tao::pegtl::one<'_'>> {
};
struct simple_label_next_char
    : tao::pegtl::sor<ALPHANUM, tao::pegtl::one<'-'>, tao::pegtl::one<'/'>,
                      tao::pegtl::one<'_'>> {};
struct simple_label
    : tao::pegtl::seq<simple_label_first_char,
                      tao::pegtl::star<simple_label_next_char>> {};
struct quoted_label_char : tao::pegtl::sor<tao::pegtl::range<0x20, 0x5F>,
                                           tao::pegtl::range<0x61, 0x7E>> {};
struct quoted_label : tao::pegtl::plus<quoted_label_char> {};
struct label
    : tao::pegtl::sor<tao::pegtl::seq<tao::pegtl::one<'`'>, quoted_label,
                                      tao::pegtl::one<'`'>>,
                      simple_label> {};
struct builtin;
struct keyword;
struct nonreserved_label
    : tao::pegtl::sor<
        tao::pegtl::seq< tao::pegtl::sor< keyword, builtin >, tao::pegtl::plus< simple_label_next_char > >,
        tao::pegtl::seq< tao::pegtl::not_at< tao::pegtl::sor< keyword, builtin > >, label > > {};
struct any_label : label {};
struct Some;
struct any_label_or_some : tao::pegtl::sor<any_label, Some> {};
struct interpolation;
struct double_quote_escaped;
struct double_quote_char;
struct double_quote_chunk
    : tao::pegtl::sor<
          interpolation,
          tao::pegtl::seq<tao::pegtl::one<0x5C>, double_quote_escaped>,
          double_quote_char> {};
struct unicode_escape;
struct double_quote_escaped
    : tao::pegtl::sor<
          tao::pegtl::one<0x22>, tao::pegtl::one<0x24>, tao::pegtl::one<0x5C>,
          tao::pegtl::one<0x2F>, tao::pegtl::one<0x62>, tao::pegtl::one<0x66>,
          tao::pegtl::one<0x6E>, tao::pegtl::one<0x72>, tao::pegtl::one<0x74>,
          tao::pegtl::seq<tao::pegtl::one<0x75>, unicode_escape>> {};
struct unbraced_escape;
struct braced_escape;
struct unicode_escape
    : tao::pegtl::sor<unbraced_escape,
                      tao::pegtl::seq<tao::pegtl::one<'{'>, braced_escape,
                                      tao::pegtl::one<'}'>>> {};
struct unicode_suffix
    : tao::pegtl::sor<
          tao::pegtl::seq<
              tao::pegtl::sor<
                  DIGIT, tao::pegtl::istring<'A'>, tao::pegtl::istring<'B'>,
                  tao::pegtl::istring<'C'>, tao::pegtl::istring<'D'>,
                  tao::pegtl::istring<'E'>>,
              tao::pegtl::rep<3, HEXDIG>>,
          tao::pegtl::seq<
              tao::pegtl::istring<'F'>, tao::pegtl::rep<2, HEXDIG>,
              tao::pegtl::sor<
                  DIGIT, tao::pegtl::istring<'A'>, tao::pegtl::istring<'B'>,
                  tao::pegtl::istring<'C'>, tao::pegtl::istring<'D'>>>> {};
struct unbraced_escape
    : tao::pegtl::sor<
          tao::pegtl::seq<tao::pegtl::sor<DIGIT, tao::pegtl::istring<'A'>,
                                          tao::pegtl::istring<'B'>,
                                          tao::pegtl::istring<'C'>>,
                          tao::pegtl::rep<3, HEXDIG>>,
          tao::pegtl::seq<
              tao::pegtl::istring<'D'>,
              tao::pegtl::sor<tao::pegtl::range<'0', '7'>>,
              HEXDIG, HEXDIG>,
          tao::pegtl::seq<tao::pegtl::istring<'E'>, tao::pegtl::rep<3, HEXDIG>>,
          tao::pegtl::seq<
              tao::pegtl::istring<'F'>, tao::pegtl::rep<2, HEXDIG>,
              tao::pegtl::sor<
                  DIGIT, tao::pegtl::istring<'A'>, tao::pegtl::istring<'B'>,
                  tao::pegtl::istring<'C'>, tao::pegtl::istring<'D'>>>> {};
struct braced_codepoint
    : tao::pegtl::sor<
          tao::pegtl::seq<
              tao::pegtl::sor<
                  tao::pegtl::range<'1', '9'>, tao::pegtl::istring<'A'>,
                  tao::pegtl::istring<'B'>, tao::pegtl::istring<'C'>,
                  tao::pegtl::istring<'D'>, tao::pegtl::istring<'E'>,
                  tao::pegtl::istring<'F'>, tao::pegtl::string<'1', '0'>>,
              unicode_suffix>,
          unbraced_escape,
          tao::pegtl::seq<HEXDIG, tao::pegtl::rep_opt<2, HEXDIG>>> {};
struct braced_escape : tao::pegtl::seq<tao::pegtl::star<tao::pegtl::one<'0'>>,
                                       braced_codepoint> {};
struct double_quote_char
    : tao::pegtl::sor<tao::pegtl::range<0x20, 0x21>,
                      tao::pegtl::range<0x23, 0x5B>,
                      tao::pegtl::range<0x5D, 0x7F>, valid_non_ascii> {};
struct double_quote_literal
    : tao::pegtl::seq<tao::pegtl::one<0x22>,
                      tao::pegtl::star<double_quote_chunk>,
                      tao::pegtl::one<0x22>> {};
struct single_quote_continue;
struct escaped_quote_pair;
struct escaped_interpolation;
struct single_quote_char;
struct single_quote_continue
    : tao::pegtl::sor<
          tao::pegtl::seq<interpolation, single_quote_continue>,
          tao::pegtl::seq<escaped_quote_pair, single_quote_continue>,
          tao::pegtl::seq<escaped_interpolation, single_quote_continue>,
          tao::pegtl::string<'\'', '\''>,
          tao::pegtl::seq<single_quote_char, single_quote_continue>> {};
struct escaped_quote_pair : tao::pegtl::string<'\'', '\'', '\''> {};
struct escaped_interpolation : tao::pegtl::string<'\'', '\'', '$', '{'> {};
struct single_quote_char : tao::pegtl::sor<tao::pegtl::range<0x20, 0x7F>,
                                           valid_non_ascii, tab, end_of_line> {
};
struct single_quote_literal
    : tao::pegtl::seq<tao::pegtl::string<'\'', '\''>, end_of_line,
                      single_quote_continue> {};
struct complete_expression;
struct interpolation
    : tao::pegtl::seq<tao::pegtl::string<'$', '{'>, complete_expression,
                      tao::pegtl::one<'}'>> {};
struct text_literal
    : tao::pegtl::sor<double_quote_literal, single_quote_literal> {};
struct if_ : TAO_PEGTL_STRING("if") {};
struct then : TAO_PEGTL_STRING("then") {};
struct else_ : TAO_PEGTL_STRING("else") {};
struct let : TAO_PEGTL_STRING("let") {};
struct in : TAO_PEGTL_STRING("in") {};
struct as : TAO_PEGTL_STRING("as") {};
struct using_ : TAO_PEGTL_STRING("using") {};
struct merge : TAO_PEGTL_STRING("merge") {};
// From the Haskell parser. Otherwise the "missing" in "missingFoo" will get parsed as in import
struct missing
    : tao::pegtl::seq<
        TAO_PEGTL_STRING("missing"),
        tao::pegtl::not_at<
            tao::pegtl::sor< ALPHANUM, tao::pegtl::one<'_'>, tao::pegtl::one<'-'>, tao::pegtl::one<'/'> > > > {};
struct Infinity : TAO_PEGTL_STRING("Infinity") {};
struct NaN : TAO_PEGTL_STRING("NaN") {};
struct Some : TAO_PEGTL_STRING("Some") {};
struct toMap : TAO_PEGTL_STRING("toMap") {};
struct assert : TAO_PEGTL_STRING("assert") {};
struct forall
    : tao::pegtl::sor<
        tao::pegtl::utf8::one<0x2200>,
        TAO_PEGTL_STRING("forall")> {};
struct keyword
    : tao::pegtl::sor<if_, then, else_, let, in, using_, missing, assert, as,
                      Infinity, NaN, merge, Some, toMap, forall> {};
struct Optional : TAO_PEGTL_STRING("Optional") {};
struct Text : TAO_PEGTL_STRING("Text") {};
struct List : TAO_PEGTL_STRING("List") {};
struct Location : TAO_PEGTL_STRING("Location") {};
struct Bool : TAO_PEGTL_STRING("Bool") {};
struct True : TAO_PEGTL_STRING("True") {};
struct False : TAO_PEGTL_STRING("False") {};
struct None : TAO_PEGTL_STRING("None") {};
struct Natural : TAO_PEGTL_STRING("Natural") {};
struct Integer : TAO_PEGTL_STRING("Integer") {};
struct Double : TAO_PEGTL_STRING("Double") {};
struct Type : TAO_PEGTL_STRING("Type") {};
struct Kind : TAO_PEGTL_STRING("Kind") {};
struct Sort : TAO_PEGTL_STRING("Sort") {};
struct Natural_fold : TAO_PEGTL_STRING("Natural/fold") {};
struct Natural_build : TAO_PEGTL_STRING("Natural/build") {};
struct Natural_isZero : TAO_PEGTL_STRING("Natural/isZero") {};
struct Natural_even : TAO_PEGTL_STRING("Natural/even") {};
struct Natural_odd : TAO_PEGTL_STRING("Natural/odd") {};
struct Natural_toInteger : TAO_PEGTL_STRING("Natural/toInteger") {};
struct Natural_show : TAO_PEGTL_STRING("Natural/show") {};
struct Natural_subtract : TAO_PEGTL_STRING("Natural/subtract") {};
struct Integer_toDouble : TAO_PEGTL_STRING("Integer/toDouble") {};
struct Integer_show : TAO_PEGTL_STRING("Integer/show") {};
struct Integer_negate : TAO_PEGTL_STRING("Integer/negate") {};
struct Integer_clamp : TAO_PEGTL_STRING("Integer/clamp") {};
struct Double_show : TAO_PEGTL_STRING("Double/show") {};
struct List_build : TAO_PEGTL_STRING("List/build") {};
struct List_fold : TAO_PEGTL_STRING("List/fold") {};
struct List_length : TAO_PEGTL_STRING("List/length") {};
struct List_head : TAO_PEGTL_STRING("List/head") {};
struct List_last : TAO_PEGTL_STRING("List/last") {};
struct List_indexed : TAO_PEGTL_STRING("List/indexed") {};
struct List_reverse : TAO_PEGTL_STRING("List/reverse") {};
struct Optional_fold : TAO_PEGTL_STRING("Optional/fold") {};
struct Optional_build : TAO_PEGTL_STRING("Optional/build") {};
struct Text_show : TAO_PEGTL_STRING("Text/show") {};
struct builtin
    : tao::pegtl::sor<Natural_fold, Natural_build, Natural_isZero, Natural_even,
                      Natural_odd, Natural_toInteger, Natural_show,
                      Integer_toDouble, Integer_show, Integer_negate,
                      Integer_clamp, Natural_subtract, Double_show, List_build,
                      List_fold, List_length, List_head, List_last,
                      List_indexed, List_reverse, Optional_fold, Optional_build,
                      Text_show, Bool, True, False, Optional, None, Natural,
                      Integer, Double, Text, List, Type, Kind, Sort> {};
struct combine
    : tao::pegtl::sor<tao::pegtl::utf8::one<0x2227>, tao::pegtl::string<'/', '\\'>> {
};
struct combine_types
    : tao::pegtl::sor<tao::pegtl::utf8::one<0x2A53>,
                      tao::pegtl::string<'/', '/', '\\', '\\'>> {};
struct equivalent : tao::pegtl::sor<tao::pegtl::utf8::one<0x2261>,
                                    tao::pegtl::string<'=', '=', '='>> {};
struct prefer
    : tao::pegtl::sor<tao::pegtl::utf8::one<0x2AFD>, tao::pegtl::string<'/', '/'>> {};
struct lambda : tao::pegtl::sor<tao::pegtl::utf8::one<0x3BB>, tao::pegtl::one<'\\'>> {
};
struct arrow
    : tao::pegtl::sor<tao::pegtl::utf8::one<0x2192>, tao::pegtl::string<'-', '>'>> {};
struct complete : tao::pegtl::string<':', ':'> {};
struct exponent
    : tao::pegtl::seq<tao::pegtl::istring<'e'>,
                      tao::pegtl::opt<tao::pegtl::sor<tao::pegtl::one<'+'>,
                                                      tao::pegtl::one<'-'>>>,
                      tao::pegtl::plus<DIGIT>> {};
struct numeric_double_literal
    : tao::pegtl::seq<
          tao::pegtl::opt<
              tao::pegtl::sor<tao::pegtl::one<'+'>, tao::pegtl::one<'-'>>>,
          tao::pegtl::plus<DIGIT>,
          tao::pegtl::sor<
              tao::pegtl::seq<tao::pegtl::one<'.'>, tao::pegtl::plus<DIGIT>,
                              tao::pegtl::opt<exponent>>,
              exponent>> {};
struct minus_infinity_literal
    : tao::pegtl::seq<tao::pegtl::one<'-'>, Infinity> {};
struct plus_infinity_literal : Infinity {};
struct double_literal
    : tao::pegtl::sor<numeric_double_literal, minus_infinity_literal,
                      plus_infinity_literal, NaN> {};
struct natural_literal
    : tao::pegtl::sor<
          tao::pegtl::seq<tao::pegtl::one<'0'>, tao::pegtl::one<0x78>,
                          tao::pegtl::plus<HEXDIG>>,
          tao::pegtl::seq<
              tao::pegtl::range<'1', '9'>,
              tao::pegtl::star<DIGIT>>,
          tao::pegtl::one<'0'>> {};
struct integer_literal
    : tao::pegtl::seq<
          tao::pegtl::sor<tao::pegtl::one<'+'>, tao::pegtl::one<'-'>>,
          natural_literal> {};
struct variable;
struct identifier : tao::pegtl::sor<variable, builtin> {};
struct variable
    : tao::pegtl::seq<nonreserved_label,
                      tao::pegtl::opt<tao::pegtl::seq<
                          whsp, tao::pegtl::one<'@'>, whsp, natural_literal>>> {
};
struct path_character
    : tao::pegtl::sor<
          tao::pegtl::one<0x21>, tao::pegtl::range<0x24, 0x27>,
          tao::pegtl::range<0x2A, 0x2B>, tao::pegtl::range<0x2D, 0x2E>,
          tao::pegtl::range<0x30, 0x3B>, tao::pegtl::one<0x3D>,
          tao::pegtl::range<0x40, 0x5A>, tao::pegtl::range<0x5E, 0x7A>,
          tao::pegtl::one<0x7C>, tao::pegtl::one<0x7E>> {};
struct quoted_path_character
    : tao::pegtl::sor<tao::pegtl::range<0x20, 0x21>,
                      tao::pegtl::range<0x23, 0x2E>,
                      tao::pegtl::range<0x30, 0x7F>, valid_non_ascii> {};
struct unquoted_path_component : tao::pegtl::plus<path_character> {};
struct quoted_path_component : tao::pegtl::plus<quoted_path_character> {};
struct path_component
    : tao::pegtl::seq<tao::pegtl::one<'/'>,
                      tao::pegtl::sor<unquoted_path_component,
                                      tao::pegtl::seq<tao::pegtl::one<0x22>,
                                                      quoted_path_component,
                                                      tao::pegtl::one<0x22>>>> {
};
struct path : tao::pegtl::plus<path_component> {};
struct parent_path;
struct here_path;
struct home_path;
struct absolute_path;
struct local
    : tao::pegtl::sor<parent_path, here_path, home_path, absolute_path> {};
struct parent_path : tao::pegtl::seq<tao::pegtl::string<'.', '.'>, path> {};
struct here_path : tao::pegtl::seq<tao::pegtl::one<'.'>, path> {};
struct home_path : tao::pegtl::seq<tao::pegtl::one<'~'>, path> {};
struct absolute_path : path {};
struct scheme : tao::pegtl::seq<tao::pegtl::string<0x68, 0x74, 0x74, 0x70>,
                                tao::pegtl::opt<tao::pegtl::one<0x73>>> {};
struct authority;
struct url_path;
struct query;
struct http_raw
    : tao::pegtl::seq<
          scheme, tao::pegtl::string<':', '/', '/'>, authority, url_path,
          tao::pegtl::opt<tao::pegtl::seq<tao::pegtl::one<'?'>, query>>> {};
struct segment;
struct url_path
    : tao::pegtl::star<tao::pegtl::sor<
          path_component, tao::pegtl::seq<tao::pegtl::one<'/'>, segment>>> {};
struct userinfo;
struct host;
struct port;
struct authority
    : tao::pegtl::seq<
          tao::pegtl::opt<tao::pegtl::seq<userinfo, tao::pegtl::one<'@'>>>,
          host, tao::pegtl::opt<tao::pegtl::seq<tao::pegtl::one<':'>, port>>> {
};
struct unreserved;
struct pct_encoded;
struct sub_delims;
struct userinfo
    : tao::pegtl::star<tao::pegtl::sor<unreserved, pct_encoded, sub_delims,
                                       tao::pegtl::one<':'>>> {};
struct IP_literal;
struct IPv4address;
struct domain;
struct host : tao::pegtl::sor<IP_literal, IPv4address, domain> {};
struct port : tao::pegtl::star<DIGIT> {};
struct IPv6address;
struct IPvFuture;
struct IP_literal : tao::pegtl::seq<tao::pegtl::one<'['>,
                                    tao::pegtl::sor<IPv6address, IPvFuture>,
                                    tao::pegtl::one<']'>> {};
struct IPvFuture
    : tao::pegtl::seq<tao::pegtl::istring<'v'>, tao::pegtl::plus<HEXDIG>,
                      tao::pegtl::one<'.'>,
                      tao::pegtl::plus<tao::pegtl::sor<unreserved, sub_delims,
                                                       tao::pegtl::one<':'>>>> {
};
struct h16;
struct ls32;
struct IPv6address
    : tao::pegtl::sor<
          tao::pegtl::seq<tao::pegtl::rep<6, h16, tao::pegtl::one<':'>>, ls32>,
          tao::pegtl::seq<tao::pegtl::string<':', ':'>,
                          tao::pegtl::rep<5, h16, tao::pegtl::one<':'>>, ls32>,
          tao::pegtl::seq<tao::pegtl::opt<h16>, tao::pegtl::string<':', ':'>,
                          tao::pegtl::rep<4, h16, tao::pegtl::one<':'>>, ls32>,
          tao::pegtl::seq<tao::pegtl::opt<tao::pegtl::seq<
                              h16, tao::pegtl::opt<tao::pegtl::one<':'>, h16>>>,
                          tao::pegtl::string<':', ':'>,
                          tao::pegtl::rep<3, h16, tao::pegtl::one<':'>>, ls32>,
          tao::pegtl::seq<
              tao::pegtl::opt<tao::pegtl::seq<
                  h16, tao::pegtl::rep_max<2, tao::pegtl::one<':'>, h16>>>,
              tao::pegtl::string<':', ':'>,
              tao::pegtl::rep<2, h16, tao::pegtl::one<':'>>, ls32>,
          tao::pegtl::seq<
              tao::pegtl::opt<tao::pegtl::seq<
                  h16, tao::pegtl::rep_max<3, tao::pegtl::one<':'>, h16>>>,
              tao::pegtl::string<':', ':'>, h16, tao::pegtl::one<':'>, ls32>,
          tao::pegtl::seq<
              tao::pegtl::opt<tao::pegtl::seq<
                  h16, tao::pegtl::rep_max<4, tao::pegtl::one<':'>, h16>>>,
              tao::pegtl::string<':', ':'>, ls32>,
          tao::pegtl::seq<
              tao::pegtl::opt<tao::pegtl::seq<
                  h16, tao::pegtl::rep_max<5, tao::pegtl::one<':'>, h16>>>,
              tao::pegtl::string<':', ':'>, h16>,
          tao::pegtl::seq<
              tao::pegtl::opt<tao::pegtl::seq<
                  h16, tao::pegtl::rep_max<6, tao::pegtl::one<':'>, h16>>>,
              tao::pegtl::string<':', ':'>>> {};
struct h16 : tao::pegtl::seq<HEXDIG, tao::pegtl::rep_opt<3, HEXDIG>> {};
struct ls32 : tao::pegtl::sor<tao::pegtl::seq<h16, tao::pegtl::one<':'>, h16>,
                              IPv4address> {};
struct dec_octet;
struct IPv4address : tao::pegtl::seq<dec_octet, tao::pegtl::one<'.'>, dec_octet,
                                     tao::pegtl::one<'.'>, dec_octet,
                                     tao::pegtl::one<'.'>, dec_octet> {};
struct dec_octet
    : tao::pegtl::sor<
          tao::pegtl::seq<tao::pegtl::string<'2', '5'>,
                          tao::pegtl::range<'0', '5'>>,
          tao::pegtl::seq<tao::pegtl::one<'2'>, tao::pegtl::range<'0', '4'>,
                          DIGIT>,
          tao::pegtl::seq<tao::pegtl::one<'1'>, tao::pegtl::rep<2, DIGIT>>,
          tao::pegtl::seq<tao::pegtl::range<'1', '9'>, DIGIT>, DIGIT> {};
struct domainlabel;
struct domain
    : tao::pegtl::seq<domainlabel,
                      tao::pegtl::star<tao::pegtl::one<'.'>, domainlabel>,
                      tao::pegtl::opt<tao::pegtl::one<'.'>>> {};
struct domainlabel
    : tao::pegtl::seq<tao::pegtl::plus<ALPHANUM>,
                      tao::pegtl::star<tao::pegtl::plus<tao::pegtl::one<'-'>>,
                                       tao::pegtl::plus<ALPHANUM>>> {};
struct pchar;
struct segment : tao::pegtl::star<pchar> {};
struct pchar : tao::pegtl::sor<unreserved, pct_encoded, sub_delims,
                               tao::pegtl::one<':'>, tao::pegtl::one<'@'>> {};
struct query
    : tao::pegtl::star<
          tao::pegtl::sor<pchar, tao::pegtl::one<'/'>, tao::pegtl::one<'?'>>> {
};
struct pct_encoded : tao::pegtl::seq<tao::pegtl::one<'%'>, HEXDIG, HEXDIG> {};
struct unreserved
    : tao::pegtl::sor<ALPHANUM, tao::pegtl::one<'-'>, tao::pegtl::one<'.'>,
                      tao::pegtl::one<'_'>, tao::pegtl::one<'~'>> {};
struct sub_delims
    : tao::pegtl::sor<tao::pegtl::one<'!'>, tao::pegtl::one<'$'>,
                      tao::pegtl::one<'&'>, tao::pegtl::one<'\''>,
                      tao::pegtl::one<'*'>, tao::pegtl::one<'+'>,
                      tao::pegtl::one<';'>, tao::pegtl::one<'='>> {};
struct import_expression;
struct http
    : tao::pegtl::seq<http_raw, tao::pegtl::opt<tao::pegtl::seq<
                                    whsp, using_, whsp1, import_expression>>> {
};
struct bash_environment_variable;
struct posix_environment_variable;
struct env
    : tao::pegtl::seq<
          TAO_PEGTL_ISTRING("env:"),
          tao::pegtl::sor<
              bash_environment_variable,
              tao::pegtl::seq<tao::pegtl::one<'"'>, posix_environment_variable,
                              tao::pegtl::one<'"'>>>> {};
struct bash_environment_variable
    : tao::pegtl::seq<
          tao::pegtl::sor<ALPHA, tao::pegtl::one<'_'>>,
          tao::pegtl::star<tao::pegtl::sor<ALPHANUM, tao::pegtl::one<'_'>>>> {};
struct posix_environment_variable_character;
struct posix_environment_variable
    : tao::pegtl::plus<posix_environment_variable_character> {};
struct posix_environment_variable_character
    : tao::pegtl::sor<
          tao::pegtl::seq<
              tao::pegtl::one<0x5C>,
              tao::pegtl::sor<tao::pegtl::one<0x22>, tao::pegtl::one<0x5C>,
                              tao::pegtl::one<0x61>, tao::pegtl::one<0x62>,
                              tao::pegtl::one<0x66>, tao::pegtl::one<0x6E>,
                              tao::pegtl::one<0x72>, tao::pegtl::one<0x74>,
                              tao::pegtl::one<0x76>>>,
          tao::pegtl::range<0x20, 0x21>, tao::pegtl::range<0x23, 0x3C>,
          tao::pegtl::range<0x3E, 0x5B>, tao::pegtl::range<0x5D, 0x7E>> {};
struct import_type : tao::pegtl::sor<missing, local, http, env> {};
struct hash
    : tao::pegtl::seq< TAO_PEGTL_STRING("sha256:"), tao::pegtl::rep<64, HEXDIG>> {};
struct import_hashed
    : tao::pegtl::seq<import_type,
                      tao::pegtl::opt<tao::pegtl::seq<whsp1, hash>>> {};
struct import
    : tao::pegtl::seq<import_hashed,
                      tao::pegtl::opt<tao::pegtl::seq<
                          whsp, as, whsp1, tao::pegtl::sor<Text, Location>>>> {
};
struct expression;
struct let_binding;
struct operator_expression;
struct application_expression;
struct empty_list_literal;
struct annotated_expression;
struct lambda_expression :
    tao::pegtl::seq<lambda, whsp, tao::pegtl::one<'('>, whsp,
                    nonreserved_label, whsp, tao::pegtl::one<':'>, whsp1,
                    expression, whsp, tao::pegtl::one<')'>, whsp, arrow,
                    whsp, expression> {};
struct if_then_else_expression :
    tao::pegtl::seq<if_, whsp1, expression, whsp, then, whsp1, expression,
                    whsp, else_, whsp1, expression> {};
struct let_expression :
    tao::pegtl::seq<tao::pegtl::plus<let_binding>, in, whsp1, expression> {};
struct forall_expression :
    tao::pegtl::seq<forall, whsp, tao::pegtl::one<'('>, whsp,
                    nonreserved_label, whsp, tao::pegtl::one<':'>, whsp1,
                    expression, whsp, tao::pegtl::one<')'>, whsp, arrow,
                    whsp, expression> {};
struct arrow_expression :
    tao::pegtl::seq<operator_expression, whsp, arrow, whsp, expression> {};
struct merge_expression :
    tao::pegtl::seq<
        merge, whsp1, import_expression, whsp1,
        import_expression, whsp, tao::pegtl::one<':'>, whsp1,
        application_expression> {};
struct assert_expression :
    tao::pegtl::seq<assert, whsp, tao::pegtl::one<':'>, whsp1, expression> {};
struct expression
    : tao::pegtl::sor<
          lambda_expression,
          if_then_else_expression,
          let_expression,
          forall_expression,
          arrow_expression,
          merge_expression,
          empty_list_literal,
          // tao::pegtl::seq<toMap, whsp1, import_expression, whsp,
          //                 tao::pegtl::one<':'>, whsp1, application_expression>,
          assert_expression,
          annotated_expression> {};
struct annotated_expression
    : tao::pegtl::seq<operator_expression,
                      tao::pegtl::opt<tao::pegtl::seq<
                          whsp, tao::pegtl::one<':'>, whsp1, expression>>> {};
struct let_binding
    : tao::pegtl::seq<let, whsp1, nonreserved_label, whsp,
                      tao::pegtl::opt<tao::pegtl::seq<tao::pegtl::one<':'>,
                                                      whsp1, expression, whsp>>,
                      tao::pegtl::one<'='>, whsp, expression, whsp> {};
struct empty_list_literal
    : tao::pegtl::seq<
          tao::pegtl::one<'['>, whsp,
          tao::pegtl::opt<tao::pegtl::seq<tao::pegtl::one<','>, whsp>>,
          tao::pegtl::one<']'>, whsp, tao::pegtl::one<':'>, whsp1,
          application_expression> {};
struct or_expression;
struct import_alt_expression
    : tao::pegtl::seq<
          or_expression,
          tao::pegtl::star<whsp, tao::pegtl::one<'?'>, whsp1, or_expression>> {
};
struct operator_expression : import_alt_expression {};
struct plus_expression;
struct or_expression
    : tao::pegtl::seq<plus_expression,
                      tao::pegtl::star<whsp, tao::pegtl::string<'|', '|'>, whsp,
                                       plus_expression>> {};
struct text_append_expression;
struct plus_expression
    : tao::pegtl::seq<text_append_expression,
                      tao::pegtl::star<whsp, tao::pegtl::one<'+'>, whsp1,
                                       text_append_expression>> {};
struct list_append_expression;
struct text_append_expression
    : tao::pegtl::seq<list_append_expression,
                      tao::pegtl::star<whsp, tao::pegtl::string<'+', '+'>, whsp,
                                       list_append_expression>> {};
struct and_expression;
struct list_append_expression
    : tao::pegtl::seq<
          and_expression,
          tao::pegtl::star<whsp, tao::pegtl::one<'#'>, whsp, and_expression>> {
};
struct combine_expression;
struct and_expression
    : tao::pegtl::seq<combine_expression,
                      tao::pegtl::star<whsp, tao::pegtl::string<'&', '&'>, whsp,
                                       combine_expression>> {};
struct prefer_expression;
struct combine_expression
    : tao::pegtl::seq<prefer_expression, tao::pegtl::star<whsp, combine, whsp,
                                                          prefer_expression>> {
};
struct combine_types_expression;
struct prefer_expression
    : tao::pegtl::seq<
          combine_types_expression,
          tao::pegtl::star<whsp, prefer, whsp, combine_types_expression>> {};
struct times_expression;
struct combine_types_expression
    : tao::pegtl::seq<
          times_expression,
          tao::pegtl::star<whsp, combine_types, whsp, times_expression>> {};
struct equal_expression;
struct times_expression
    : tao::pegtl::seq<equal_expression,
                      tao::pegtl::star<whsp, tao::pegtl::one<'*'>, whsp,
                                       equal_expression>> {};
struct not_equal_expression;
struct equal_expression
    : tao::pegtl::seq<not_equal_expression,
                      tao::pegtl::star<whsp, tao::pegtl::string<'=', '='>, whsp,
                                       not_equal_expression>> {};
struct equivalent_expression;
struct not_equal_expression
    : tao::pegtl::seq<equivalent_expression,
                      tao::pegtl::star<whsp, tao::pegtl::string<'!', '='>, whsp,
                                       equivalent_expression>> {};
struct equivalent_expression
    : tao::pegtl::seq<
          application_expression,
          tao::pegtl::star<whsp, equivalent, whsp, application_expression>> {};
struct first_application_expression;
struct application_expression
    : tao::pegtl::seq<first_application_expression,
                      tao::pegtl::star<whsp1, import_expression>> {};
struct merge_first_application_expression
    : tao::pegtl::seq<merge, whsp1, import_expression, whsp1, import_expression> {};
struct some_first_application_expression
    : tao::pegtl::seq<Some, whsp1, import_expression> {};
struct tomap_first_application_expression
    : tao::pegtl::seq<toMap, whsp1, import_expression> {};
struct first_application_expression
    : tao::pegtl::sor<merge_first_application_expression,
                      some_first_application_expression,
                      tomap_first_application_expression,
                      import_expression> {};
struct completion_expression;
struct import_expression : tao::pegtl::sor<import, completion_expression> {};
struct selector_expression;
struct completion_expression
    : tao::pegtl::seq<selector_expression,
                      tao::pegtl::opt<tao::pegtl::seq<whsp, complete, whsp,
                                                      selector_expression>>> {};
struct primitive_expression;
struct selector;
struct selector_expression
    : tao::pegtl::seq<
          primitive_expression,
          tao::pegtl::star<whsp, tao::pegtl::one<'.'>, whsp, selector>> {};
struct labels;
struct type_selector;
struct selector : tao::pegtl::sor<any_label, labels, type_selector> {};
struct labels : tao::pegtl::seq<tao::pegtl::one<'{'>, whsp,
                                tao::pegtl::opt<tao::pegtl::seq<
                                    any_label_or_some, whsp,
                                    tao::pegtl::star<tao::pegtl::one<','>, whsp,
                                                     any_label_or_some, whsp>>>,
                                tao::pegtl::one<'}'>> {};
struct type_selector : tao::pegtl::seq<tao::pegtl::one<'('>, whsp, expression,
                                       whsp, tao::pegtl::one<')'>> {};
struct record_type_or_literal;
struct union_type;
struct non_empty_list_literal;
struct primitive_expression
    : tao::pegtl::sor<
          double_literal, natural_literal, integer_literal, text_literal,
          tao::pegtl::seq<
              tao::pegtl::one<'{'>, whsp,
              tao::pegtl::opt<tao::pegtl::seq<tao::pegtl::one<','>, whsp>>,
              record_type_or_literal, whsp, tao::pegtl::one<'}'>>,
          tao::pegtl::seq<
              tao::pegtl::one<'<'>, whsp,
              tao::pegtl::opt<tao::pegtl::seq<tao::pegtl::one<'|'>, whsp>>,
              union_type, whsp, tao::pegtl::one<'>'>>,
          non_empty_list_literal,
          identifier,
          tao::pegtl::seq<tao::pegtl::one<'('>, complete_expression,
                          tao::pegtl::one<')'>>> {};
struct empty_record_literal;
struct non_empty_record_type_or_literal;
struct empty_record_type;
struct record_type_or_literal
    : tao::pegtl::sor<empty_record_literal, non_empty_record_type_or_literal,
                      empty_record_type> {};
struct empty_record_literal : tao::pegtl::one<'='> {};
struct empty_record_type : tao::pegtl::string<> {};
struct non_empty_record_literal;
struct non_empty_record_type;
struct non_empty_record_type_or_literal
    : tao::pegtl::sor<non_empty_record_literal, non_empty_record_type> {};
struct record_type_entry;
struct non_empty_record_type
    : tao::pegtl::seq<record_type_entry,
                      tao::pegtl::star<whsp, tao::pegtl::one<','>, whsp,
                                       record_type_entry>> {};
struct record_type_entry
    : tao::pegtl::seq<any_label_or_some, whsp, tao::pegtl::one<':'>, whsp1,
                      expression> {};
struct record_literal_entry;
struct non_empty_record_literal
    : tao::pegtl::seq<record_literal_entry,
                      tao::pegtl::star<whsp, tao::pegtl::one<','>, whsp,
                                       record_literal_entry>> {};
struct record_literal_entry
    : tao::pegtl::seq<
          any_label_or_some,
          tao::pegtl::star<whsp, tao::pegtl::one<'.'>, whsp, any_label_or_some>,
          whsp, tao::pegtl::one<'='>, whsp, expression> {};
struct non_empty_union_type;
struct empty_union_type;
struct union_type : tao::pegtl::sor<non_empty_union_type, empty_union_type> {};
struct empty_union_type : tao::pegtl::string<> {};
struct union_type_entry;
struct non_empty_union_type
    : tao::pegtl::seq<union_type_entry,
                      tao::pegtl::star<whsp, tao::pegtl::one<'|'>, whsp,
                                       union_type_entry>> {};
struct union_type_entry
    : tao::pegtl::seq<any_label_or_some,
                      tao::pegtl::opt<tao::pegtl::seq<
                          whsp, tao::pegtl::one<':'>, whsp1, expression>>> {};
struct non_empty_list_literal
    : tao::pegtl::seq<
          tao::pegtl::one<'['>, whsp,
          tao::pegtl::opt<tao::pegtl::seq<tao::pegtl::one<','>, whsp>>,
          expression, whsp,
          tao::pegtl::star<tao::pegtl::one<','>, whsp, expression, whsp>,
          tao::pegtl::one<']'>> {};
struct complete_expression : tao::pegtl::seq<whsp, expression, whsp> {};

} // namespace dhall
