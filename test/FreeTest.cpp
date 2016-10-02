#include "Free.h"
#include "Functor.h"
#include "List.h"
#include "Monad.h"

#include "catch.hpp"

#include <boost/variant.hpp>

#include <functional>
#include <string>

// A unit type, i.e. a type with exactly one value. Like Haskell's ().
// Could also use std::tuple<>.
struct unit {};

std::ostream &operator<<(std::ostream &os, unit) { return os << "unit{}"; }

// data Prog a = Read (Int -> a) | Write Int (() -> a)
template <typename Next>
struct Read {
  std::function<Next(int)> next;
};
template <typename Next>
struct Write {
  int x;
  std::function<Next(unit)> next;
};

template <typename Next>
struct Prog {
  boost::variant<Read<Next>, Write<Next>> v;
};

template <typename Next>
std::ostream &operator<<(std::ostream &os, const Read<Next> &) {
  return os << "Read{" /*<< read.next*/ << "}";
}
template <typename Next>
std::ostream &operator<<(std::ostream &os, const Write<Next> &write) {
  return os << "Write{" << write.x /*<< ", " << write.next*/ << "}";
}

template <typename F>
Prog<std::result_of_t<F(int)>> make_read(F next) {
  return {Read<std::result_of_t<F(int)>>{{next}}};
}

template <typename F>
Prog<std::result_of_t<F(unit)>> make_write(int x, F next) {
  return {Write<std::result_of_t<F(unit)>>{x, next}};
}

// instance Functor (Prog a) where
namespace Functor {
  template <>
  struct Functor<Prog> {
    template <typename Fun, typename Next>
    static Prog<std::result_of_t<Fun(Next)>> fmap(Fun &&fun,
                                                  const Prog<Next> &prog) {
      using Res = std::result_of_t<Fun(Next)>;
      struct Visitor {
        Fun &fun;
        // fmap f (Read n) = Read (f . n)
        Prog<Res> operator()(const Read<Next> &read) const {
          auto &f = this->fun;  // VS doesn't like fun=this->fun in the lambda
          return make_read([f, read](int x) { return f(read.next(x)); });
        }
        // fmap f (Write x n) = Write x (f . n)
        Prog<Res> operator()(const Write<Next> &write) const {
          auto &f = this->fun;
          return make_write(
              write.x, [f, write](unit u) { return f(write.next(u)); });
        }
      };
      return boost::apply_visitor(Visitor{fun}, prog.v);
    }
  };
}

// interpret :: Prog a -> List a
// interpret (Read n) = fmap n STATE
// interpret (Write x n) = STATE+=x; n ()
struct Interpret {
  List<int> l;
  template <typename A>
  List<A> operator()(Prog<A> prog) {
    struct Visitor {
      List<int> &l;
      List<A> operator()(const Read<A> &r) const {
        return Functor::fmap(r.next, l);
      }
      List<A> operator()(const Write<A> &w) {
        l.push_back(w.x);
        return {w.next(unit{})};
      }
    };
    Visitor v{l};
    return boost::apply_visitor(v, prog.v);
  }
};

namespace F = Free;
// newtype Program a = Free (Prog a)

template <typename A>
struct Program : F::Free<Prog, A> {
  using WrappedFree = F::Free<Prog, A>;
  Program(const WrappedFree& free) : F::Free<Prog, A>(free) {}
};

// run :: Program a -> List a
// run p = foldFree interpret p
template <typename A>
List<A> run(Program<A> program) {
  Interpret i;
  return Free::foldFree<List>(i, program);
}

// The identity function (template)
// id :: a -> a
template <typename A>
A id(A a) {
  return a;
}

// This is the boilerplate to lift the constructors of Prog into the
// corresponding free monad
// readF :: Free (Prog Int)
// readF = liftF (Read id)
Program<int> readF = Free::liftFree(make_read(id<int>));
// writeF :: Int -> Free (Prog ())
// writeF x = liftF (Write x id)
Program<unit> writeF(int x) { return Free::liftFree(make_write(x, id<unit>)); }

TEST_CASE("Free monad for Prog") {
  CHECK(run(readF) == List<int>());
  {
    Program<std::string> free = Monad::bind(Program<int>(readF), [](int x) {
      return Monad::pure<Program>(std::to_string(x));
    });
    CHECK(run(free) == List<std::string>());
  }
  {
    Program<std::string> free = readF >>=
        [](int x) { return Monad::pure<Program>(std::to_string(x)); };
    CHECK(run(free) == List<std::string>());
  }
  // do
  //   writeF 10
  //   x <- readF
  //   writeF 20
  //   y <- readF
  //   pure (x + y)
  auto free = writeF(10) >> readF >>= [](int x) {
    return writeF(20) >> readF >>=
           [x](int y) { return Monad::pure<Program>(x + y); };
  };
  CHECK(run(free) == List<int>({20, 30}));
}
