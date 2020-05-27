#pragma once

namespace dhall {

enum struct Binop {
    ImportAlt,
    BoolOr,
    NaturalPlus,
    TextAppend,
    ListAppend,
    BoolAnd,
    Combine,
    Prefer,
    TypeCombine,
    NaturalTimes,
    BoolEq,
    BoolNeq,
    Equivalent
};

inline std::ostream& PrintBinop( std::ostream& os, const Binop& op ) {
  switch (op) {
    case Binop::ImportAlt:
      return os << "Binop::ImportAlt";
    case Binop::BoolOr:
      return os << "Binop::BoolOr";
    case Binop::NaturalPlus:
      return os << "Binop::NaturalPlus";
    case Binop::TextAppend:
      return os << "Binop::TextAppend";
    case Binop::ListAppend:
      return os << "Binop::ListAppend";
    case Binop::BoolAnd:
      return os << "Binop::BoolAnd";
    case Binop::Combine:
      return os << "Binop::Combine";
    case Binop::Prefer:
      return os << "Binop::Prefer";
    case Binop::TypeCombine:
      return os << "Binop::TypeCombine";
    case Binop::NaturalTimes:
      return os << "Binop::NaturalTimes";
    case Binop::BoolEq:
      return os << "Binop::BoolEq";
    case Binop::BoolNeq:
      return os << "Binop::BoolNeq";
  }
}

}
