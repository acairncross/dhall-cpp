#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "binop.hpp"
#include "builtin.hpp"

namespace dhall {

enum struct Const { Type, Kind, Sort };

struct Var {
  Var(std::string name, int index) : name(name), index(index) {}
  std::string name;
  int index;
};

inline std::ostream& PrintConst( std::ostream& os, Const c ) {
  switch (c) {
    case Const::Type:
      return os << "Const::Type";
    case Const::Kind:
      return os << "Const::Kind";
    case Const::Sort:
      return os << "Const::Sort";
  }
}

inline std::ostream& PrintVar( std::ostream& os, Var v ) {
  return os << "Var(" << v.name << "," << v.index << ")";
}

struct Expr {
  virtual std::ostream& PrintDot( std::ostream& os ) const = 0;
};

struct ConstExpr : public Expr {
  ConstExpr(Const value) : value(value) {}
  Const value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"Const\" ]\n";
    os << "  x" << this << "_value [ label=\"";
    PrintConst( os, value );
    os << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_value\n";
    return os;
  }
};

struct VarExpr : public Expr {
  VarExpr(Var value) : value(value) {}
  Var value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"VarExpr\" ]\n";
    os << "  x" << this << "_value [ label=\"" << value.name << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_value\n";
    return os;
  }
};

struct LamExpr : public Expr {
  LamExpr(std::string name, std::unique_ptr<Expr> type, std::unique_ptr<Expr> body)
      : name(name), type(std::move(type)), body(std::move(body)) {}
  std::string name;
  std::unique_ptr<Expr> type;
  std::unique_ptr<Expr> body;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"LamExpr\" ]\n";
    os << "  x" << this << "_name [ label=\"" << name << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_name\n";
    type->PrintDot( os );
    os << "  x" << this << " -> x" << type.get() << "\n";
    body->PrintDot( os );
    os << "  x" << this << " -> x" << body.get() << "\n";
    return os;
  }
};

struct PiExpr : public Expr {
  PiExpr(std::string name, std::unique_ptr<Expr> type, std::unique_ptr<Expr> body)
      : name(name), type(std::move(type)), body(std::move(body)) {}
  std::string name;
  std::unique_ptr<Expr> type;
  std::unique_ptr<Expr> body;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"PiExpr\" ]\n";
    os << "  x" << this << "_name [ label=\"" << name << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_value\n";
    type->PrintDot( os );
    os << "  x" << this << " -> x" << type.get() << "\n";
    body->PrintDot( os );
    os << "  x" << this << " -> x" << body.get() << "\n";
    return os;
  }
};

struct AppExpr : public Expr {
  AppExpr(std::unique_ptr<Expr> fun, std::unique_ptr<Expr> arg)
      : fun(std::move(fun)), arg(std::move(arg)) {}
  std::unique_ptr<Expr> fun;
  std::unique_ptr<Expr> arg;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"AppExpr\" ]\n";
    fun->PrintDot( os );
    os << "  x" << this << " -> x" << fun.get() << "\n";
    arg->PrintDot( os );
    os << "  x" << this << " -> x" << arg.get() << "\n";
    return os;
  }
};

struct Binding {
  Binding(std::string variable, std::unique_ptr<Expr> type, std::unique_ptr<Expr> value) : variable(variable), type(std::move(type)), value(std::move(value)) {}
  std::string variable;
  std::unique_ptr<Expr> type;  // nullable
  std::unique_ptr<Expr> value;

  std::ostream& PrintDot( std::ostream& os ) const {
    os << "  x" << this << " [ label=\"Binding\" ]\n";
    os << "  x" << this << "_variable [ label=\"" << variable << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_variable\n";
    if ( type != nullptr ) {
      type->PrintDot( os );
      os << "  x" << this << " -> x" << type.get() << "\n";
    }
    value->PrintDot( os );
    os << "  x" << this << " -> x" << value.get() << "\n";
    return os;
  }

};

struct LetExpr : public Expr {
  LetExpr(Binding binding, std::unique_ptr<Expr> body)
    : binding(std::move(binding)), body(std::move(body)) {}
  Binding binding;
  std::unique_ptr<Expr> body;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"LetExpr\" ]\n";
    binding.PrintDot( os );
    os << "  x" << this << " -> x" << &binding << "\n";
    body->PrintDot( os );
    os << "  x" << this << " -> x" << body.get() << "\n";
    return os;
  }
};

struct BoolLitExpr : public Expr {
  BoolLitExpr(bool value) : value(value) {}
  bool value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    return os << "  x" << this << " [ label=\"" <<  (value ? "True" : "False") << "\" ]\n";
  }
};

struct BoolIfExpr : public Expr {
  BoolIfExpr(std::unique_ptr<Expr> cond, std::unique_ptr<Expr> left, std::unique_ptr<Expr> right)
    : cond(std::move(cond)), left(std::move(left)), right(std::move(right)) {};
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"BoolIfExpr\" ]\n";
    cond->PrintDot( os );
    os << "  x" << this << " -> x" << cond.get() << "\n";
    left->PrintDot( os );
    os << "  x" << this << " -> x" << left.get() << "\n";
    right->PrintDot( os );
    os << "  x" << this << " -> x" << right.get() << "\n";
    return os;
  }
};

struct NaturalLitExpr : public Expr {
  NaturalLitExpr(int value) : value(value) {}
  int value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"NaturalLitExpr\" ]\n";
    os << "  x" << this << "_value [ label=\"" << value << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_value\n";
    return os;
  }
};

struct IntegerLitExpr : public Expr {
  IntegerLitExpr(int value) : value(value) {}
  int value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"IntegerLitExpr\" ]\n";
    os << "  x" << this << "_value [ label=\"" << value << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_value\n";
    return os;
  }
};

struct DoubleLitExpr : public Expr {
  DoubleLitExpr(double value) : value(value) {}
  double value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"DoubleLitExpr\" ]\n";
    os << "  x" << this << "_value [ label=\"" << value << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_value\n";
    return os;
  }
};

struct Chunks {
  Chunks() = default;
  // Can we use unique_ptr?
  Chunks(std::vector<std::pair<std::string, Expr *>> interpolations,
         std::string text)
      : interpolations(interpolations), text(text) {
  }
  std::vector<std::pair<std::string, Expr *>> interpolations;
  std::string text;
};

inline Chunks& operator+=(Chunks& lhs, const Chunks& rhs) {
  if (rhs.interpolations.empty()) {
    lhs.text += rhs.text;
  } else {
    lhs.interpolations.push_back(
        std::make_pair(lhs.text + rhs.interpolations[0].first,
                       rhs.interpolations[0].second));
    std::copy(rhs.interpolations.begin() + 1, rhs.interpolations.end(),
              std::back_inserter(lhs.interpolations));
    lhs.text = rhs.text;
  }
  return lhs;
}

inline Chunks operator+(Chunks lhs, const Chunks& rhs) {
  lhs += rhs;
  return lhs;
}

struct TextLitExpr : public Expr {
  TextLitExpr(Chunks value) : value(value) {}
  Chunks value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"TextLitExpr\" ]\n";

    for (int i = 0; i < value.interpolations.size(); ++i) {
      os << "  x" << this << "_value_interpolations_" << i << "_first [ label=\"" << value.interpolations[i].first << "\" ]\n";
      value.interpolations[i].second->PrintDot( os );
    }

    os << "  x" << this << "_value_text [ label=\"" << value.text << "\" ]\n";

    os << "  x" << this << " -> { ";
    for (int i = 0; i < value.interpolations.size(); ++i) {
      os << "x" << this << "_value_interpolations_" << i << "_first";
      os << ", ";
      os << "x" << value.interpolations[i].second;
      os << ", ";
    }
    os << "x" << this << "_value_text";
    os << " }\n";
    return os;
  }
};

struct ListLitExpr : public Expr {
  ListLitExpr(std::unique_ptr<Expr> type, std::vector<std::unique_ptr<Expr>> values)
    : type(std::move(type)), values(std::move(values)) {};
  std::unique_ptr<Expr> type; // nullable
  std::vector<std::unique_ptr<Expr>> values;
  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"ListLitExpr\" ]\n";
    if ( type != nullptr ) {
      type->PrintDot( os );
      os << "  x" << this << " -> { x" << type.get() << " }\n";
    }
    for (auto& value: values) {
      value->PrintDot( os );
    }
    os << "  x" << this << " -> { ";
    for (int i = 0; i < values.size(); ++i) {
      os << "x" << values.at(i).get();
      if (i < values.size() - 1) {
        os << ", ";
      }
    }
    os << " }\n";
    return os;
  }
};

struct SomeExpr : public Expr {
  SomeExpr(std::unique_ptr<Expr> value) : value(std::move(value)) {}
  std::unique_ptr<Expr> value;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"SomeExpr\" ]\n";
    value->PrintDot( os );
    os << "  x" << this << " -> x" << value.get() << "\n";
    return os;
  }
};

struct RecordExpr : public Expr {
  RecordExpr(std::map<std::string, std::unique_ptr<Expr>> rec)
      : rec(std::move(rec)) {}
  std::map<std::string, std::unique_ptr<Expr>> rec;

  std::ostream& PrintDot( std::ostream& os ) const override {
    return os << "  x" << this << " [ label=\"RecordExpr\" ]\n";
  }
};

struct RecordLitExpr : public Expr {
  RecordLitExpr(std::map<std::string, std::unique_ptr<Expr>> rec)
      : rec(std::move(rec)) {}
  std::map<std::string, std::unique_ptr<Expr>> rec;

  std::ostream& PrintDot( std::ostream& os ) const override {
    return os << "  x" << this << " [ label=\"RecordLitExpr\" ]\n";
  }
};

struct UnionExpr : public Expr {
  // The unique_ptr<Expr> are nullable
  UnionExpr(std::map<std::string, std::unique_ptr<Expr>> rec)
      : rec(std::move(rec)) {}
  std::map<std::string, std::unique_ptr<Expr>> rec;

  std::ostream& PrintDot( std::ostream& os ) const override {
    return os << "  x" << this << " [ label=\"UnionExpr\" ]\n";
  }
};

struct RecordCompletionExpr : public Expr {
  RecordCompletionExpr(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right)
    : left(std::move(left)), right(std::move(right)) {}
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"RecordCompletionExpr\" ]\n";
    left->PrintDot( os );
    os << "  x" << this << " -> x" << left.get() << "\n";
    right->PrintDot( os );
    os << "  x" << this << " -> x" << right.get() << "\n";
    return os;
  }
};

struct MergeExpr : public Expr {
  MergeExpr(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right, std::unique_ptr<Expr> type)
    : left(std::move(left)), right(std::move(right)), type(std::move(type)) {}
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  std::unique_ptr<Expr> type; // nullable

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"MergeExpr\" ]\n";
    left->PrintDot( os );
    os << "  x" << this << " -> x" << left.get() << "\n";
    right->PrintDot( os );
    os << "  x" << this << " -> x" << right.get() << "\n";
    if ( type != nullptr ) {
      type->PrintDot( os );
      os << "  x" << this << " -> x" << type.get() << "\n";
    }
    return os;
  }
};

struct ToMapExpr : public Expr {
  ToMapExpr(std::unique_ptr<Expr> value, std::unique_ptr<Expr> type)
    : value(std::move(value)), type(std::move(type)) {}
  std::unique_ptr<Expr> value;
  std::unique_ptr<Expr> type; // nullable

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"MergeExpr\" ]\n";
    value->PrintDot( os );
    os << "  x" << this << " -> x" << value.get() << "\n";
    if ( type != nullptr ) {
      type->PrintDot( os );
      os << "  x" << this << " -> x" << type.get() << "\n";
    }
    return os;
  }
};

struct FieldExpr : public Expr {
  FieldExpr(std::unique_ptr<Expr> record, std::string field)
    : record(std::move(record)), field(field) {}
  std::unique_ptr<Expr> record;
  std::string field;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"FieldExpr\" ]\n";
    record->PrintDot( os );
    os << "  x" << this << " -> x" << record.get() << "\n";
    os << "  x" << this << "_field [ label=\"" << field << "\" ]\n";
    os << "  x" << this << " -> x" << this << "_field\n";
    return os;
  }
};

struct ProjectSetExpr : public Expr {
  ProjectSetExpr(std::unique_ptr<Expr> record, std::vector<std::string> fields)
    : record(std::move(record)), fields(fields) {}
  std::unique_ptr<Expr> record;
  std::vector<std::string> fields;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"ProjectSetExpr\" ]\n";
    record->PrintDot( os );
    os << "  x" << this << " -> x" << record.get() << "\n";
    for (int i = 0; i < fields.size(); ++i) {
      os << "  x" << this << "_fields_" << i << " [ label=\"" << fields[i] << "\" ]\n";
    }
    os << "  x" << this << " -> { ";
    for (int i = 0; i < fields.size(); ++i) {
      os << "x_fields_" << i;
      if (i < fields.size() - 1) {
        os << ", ";
      }
    }
    os << " }\n";
    return os;
  }
};

// Or should this be ProjectTypeExpr?
struct ProjectExprExpr : public Expr {
  ProjectExprExpr(std::unique_ptr<Expr> record, std::unique_ptr<Expr> field)
    : record(std::move(record)), field(std::move(field)) {}
  std::unique_ptr<Expr> record;
  std::unique_ptr<Expr> field;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"ProjectExprExpr\" ]\n";
    record->PrintDot( os );
    os << "  x" << this << " -> x" << record.get() << "\n";
    field->PrintDot( os );
    os << "  x" << this << " -> x" << field.get() << "\n";
    return os;
  }
};


struct AnnotExpr : public Expr {
  AnnotExpr(std::unique_ptr<Expr> value, std::unique_ptr<Expr> type)
      : value(std::move(value)), type(std::move(type)) {}
  std::unique_ptr<Expr> value;
  std::unique_ptr<Expr> type;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"AnnotExpr\" ]\n";
    value->PrintDot( os );
    os << "  x" << this << " -> x" << value.get() << "\n";
    type->PrintDot( os );
    os << "  x" << this << " -> x" << type.get() << "\n";
    return os;
  }
};

struct AssertExpr : public Expr {
  AssertExpr(std::unique_ptr<Expr> assertion) : assertion(std::move(assertion)) {}
  std::unique_ptr<Expr> assertion;

  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"AssertExpr\" ]\n";
    assertion->PrintDot( os );
    os << "  x" << this << " -> x" << assertion.get() << "\n";
    return os;
  }
};

enum struct ImportMode { Code, RawText, Location };

// Maybe use std::variant?
struct ImportType {};

struct LocalImportType: public ImportType {
  LocalImportType(std::string file_prefix, std::string file) : file_prefix(file_prefix), file(file) {}
  std::string file_prefix;
  std::string file;
};

struct URL {
  URL() = default;
  URL(std::string scheme, std::string authority, std::string path, std::string query, std::unique_ptr<Expr> headers)
    : scheme(scheme), authority(authority), path(path), query(query), headers(std::move(headers)) {}
  std::string scheme;
  std::string authority;
  std::string path;
  std::string query;
  std::unique_ptr<Expr> headers;
};

struct RemoteImportType: public ImportType {
  RemoteImportType(URL url) : url(std::move(url)) {}
  URL url;
};

struct EnvImportType: public ImportType {
  EnvImportType(std::string text) : text(text) {}
  std::string text;
};

struct MissingImportType: public ImportType {
};

struct ImportHashed {
  ImportHashed(std::string hash, std::unique_ptr<ImportType> type)
    : hash(hash), type(std::move(type)) {}
  std::string hash;
  std::unique_ptr<ImportType> type;
};

struct Import {
  Import(std::unique_ptr<ImportHashed> hashed, ImportMode mode) : hashed(std::move(hashed)), mode(mode) {}
  std::unique_ptr<ImportHashed> hashed;
  ImportMode mode;
};

struct EmbedExpr : public Expr {
  EmbedExpr(std::unique_ptr<Import> import) : import(std::move(import)) {}
  std::unique_ptr<Import> import;
  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"ImportExpr\" ]\n";
    return os;
  }
};

struct BinopExpr : public Expr {
  BinopExpr(Binop op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
  Binop op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"";
    PrintBinop( os, op );
    os << "\" ]\n";
    lhs->PrintDot( os );
    os << "  x" << this << " -> x" << lhs.get() << "\n";
    rhs->PrintDot( os );
    os << "  x" << this << " -> x" << rhs.get() << "\n";
    return os;
  }
};

struct BuiltinExpr : public Expr {
  BuiltinExpr(Builtin fun) : fun(fun) {}
  Builtin fun;
  std::ostream& PrintDot( std::ostream& os ) const override {
    os << "  x" << this << " [ label=\"";
    PrintBuiltin( os, fun );
    os << "\" ]\n";
    return os;
  }
};

}
