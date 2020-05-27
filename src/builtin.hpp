#pragma once

namespace dhall {

enum struct Builtin {
    Bool,
    Natural,
    NaturalFold,
    NaturalBuild,
    NaturalIsZero,
    NaturalEven,
    NaturalOdd,
    NaturalToInteger,
    NaturalShow,
    Integer,
    IntegerClamp,
    IntegerNegate,
    IntegerShow,
    IntegerToDouble,
    Double,
    DoubleShow,
    Text,
    TextShow,
    List,
    ListBuild,
    ListFold,
    ListLength,
    ListHead,
    ListLast,
    ListIndexed,
    ListReverse,
    Optional,
    None,
    OptionalFold,
    OptionalBuild
};

inline std::ostream& PrintBuiltin( std::ostream& os, const Builtin& fun ) {
  switch (fun) {
    case Builtin::Bool:
      return os << "Builtin::Bool";
    case Builtin::Natural:
      return os << "Builtin::Natural";
    case Builtin::NaturalFold:
      return os << "Builtin::NaturalFold";
    case Builtin::NaturalBuild:
      return os << "Builtin::NaturalBuild";
    case Builtin::NaturalIsZero:
      return os << "Builtin::NaturalIsZero";
    case Builtin::NaturalEven:
      return os << "Builtin::NaturalEven";
    case Builtin::NaturalOdd:
      return os << "Builtin::NaturalOdd";
    case Builtin::NaturalToInteger:
      return os << "Builtin::NaturalToInteger";
    case Builtin::NaturalShow:
      return os << "Builtin::NaturalShow";
    case Builtin::Integer:
      return os << "Builtin::Integer";
    case Builtin::IntegerClamp:
      return os << "Builtin::IntegerClamp";
    case Builtin::IntegerNegate:
      return os << "Builtin::IntegerShow";
    case Builtin::IntegerToDouble:
      return os << "Builtin::IntegerToDouble";
    case Builtin::IntegerShow:
      return os << "Builtin::IntegerShow";
    case Builtin::Double:
      return os << "Builtin::Double";
    case Builtin::DoubleShow:
      return os << "Builtin::DoubleShow";
    case Builtin::Text:
      return os << "Builtin::Text";
    case Builtin::TextShow:
      return os << "Builtin::TextShow";
    case Builtin::List:
      return os << "Builtin::List";
    case Builtin::ListBuild:
      return os << "Builtin::ListBuild";
    case Builtin::ListFold:
      return os << "Builtin::ListFold";
    case Builtin::ListLength:
      return os << "Builtin::ListLength";
    case Builtin::ListHead:
      return os << "Builtin::ListHead";
    case Builtin::ListLast:
      return os << "Builtin::ListLast";
    case Builtin::ListIndexed:
      return os << "Builtin::ListIndexed";
    case Builtin::ListReverse:
      return os << "Builtin::ListReverse";
    case Builtin::Optional:
      return os << "Builtin::Optional";
    case Builtin::None:
      return os << "Builtin::None";
    case Builtin::OptionalFold:
      return os << "Builtin::OptionalFold";
    case Builtin::OptionalBuild:
      return os << "Builtin::OptionalBuild";
  }
}

}
