#include "Free.h"
#include "Functor.h"
#include "List.h"
#include "Monad.h"

#include "catch.hpp"

#include <boost/variant.hpp>

#include <functional>
#include <string>

/*!
  ### Using Free ###

  We will define a data structure and its corresponding free monad and show how
  to use the free monad to build and then interpret expressions in a simple
  domain-specific language.

  First, we will need a couple of basic definitions.

  #### Unit

  One of the basic types in Haskell is `()`, aka "unit". It has exactly one
  value, `()`. It is an empty product type. In C++ we have two options for
  product types: structs or tuples. An empty one of either will do the job but
  `std::tuple<>` has the advantage of giving us equality and ordering for free.
 */
using unit = std::tuple<>;

std::ostream& operator<<(std::ostream& os, unit) { return os << "unit{}"; }

/*!
  #### Identity

  A very useful generic function is the identity function. In Haskell it is:

  ```haskell
  id :: a -> a
  id x = x
  ```

  In C++ we can define a function template to do the same thing.
 */
template <typename A>
A id(A a) {
  return a;
}

/*!
  #### Function Composition

  Function composition is ubiquitous in Haskell and we will use it below. It
  doesn't come predefined in C++ but it's surprisingly easy these days.

  ```haskell
  compose :: (b -> c) -> (a -> b) -> a -> c
  compose f g = \x -> f (g x)
  ```
*/
template <typename F, typename G>
auto compose(F&& f, G&& g) {
  return [=](auto&& x) { return f(g(std::forward<decltype(x)>(x))); };
}

/*!
  #### Language Definition

  We define a data type to represent the operations in our language. In our
  example there are only two operations: `Read`, which takes the value or values
  so far written and does something, and `Write`, which "writes" a value and
  then does something.

  This is all very vague because the details are decoupled from this definition
  and are supplied separately.

  In Haskell our data type is just:

  ```haksell
  data Prog a =
      Read (Int -> a)
    | Write Int (() -> a)
  ```

  As usual, things get verbose in C++. We define a struct to correspond to each
  of the cases and wrap them in a variant.

  Note that I've cheated somewhat by using `std::function` instead of making the
  continuation type a template parameter in order to make this code simpler.
  Doing this probably stops the compiler from optimising this as much as it
  otherwise could.
 */
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

/*!
  As is typical when using class templates, we define corresponding "make_"
  function templates so that the template arguments can be deduced. These
  function templates have the additional benefit of wrapping the value up in our
  `Prog` struct.
 */
template <typename F>
Prog<std::result_of_t<F(int)>> make_read(F next) {
  return {Read<std::result_of_t<F(int)>>{{next}}};
}

template <typename F>
Prog<std::result_of_t<F(unit)>> make_write(int x, F next) {
  return {Write<std::result_of_t<F(unit)>>{x, next}};
}

/*!
  In order to use `Prog`'s Free Monad, `Prog` must be a Functor. In Haskell with
  the appropriate extension we could just add `deriving Functor` to the data
  type definition but we're stuck down here in C++ so we'll have to specialize
  the `Functor` class template manually.
 */

// instance Functor Prog where
namespace Functor {
  template <>
  struct Functor<Prog> {
    template <typename Fun, typename Next>
    static Prog<std::result_of_t<Fun(Next)>> fmap(Fun&& fun, const Prog<Next>& prog) {
      using Res = std::result_of_t<Fun(Next)>;
      struct Visitor {
        Fun& fun;
        // fmap f (Read n) = Read (f . n)
        Prog<Res> operator()(const Read<Next>& read) const {
          return make_read(compose(fun, read.next));
        }
        // fmap f (Write x n) = Write x (f . n)
        Prog<Res> operator()(const Write<Next>& write) const {
          return make_write(write.x, compose(fun, write.next));
        }
      };
      return boost::apply_visitor(Visitor{fun}, prog.v);
    }
  };
}

/*!
  #### The Free Monad

  Now, to use `Prog` as a Monad, all we need to do is define a wrapper around
  `Free<Prog, A>` so that we get a template with a single type parameter. The
  specialization of `Monad` defined in `Free.h` takes care of the rest.
*/
template <typename A>
struct Program : Free::Free<Prog, A> {
  using WrappedFree = Free::Free<Prog, A>;
  Program(const WrappedFree& free) : Free::Free<Prog, A>(free) {}
};

/*!
  There is some boilerplate required to lift the constructors of `Prog` into the
  `Program` Monad. This boilerplate is even required in Haskell!

  ```haskell
  readF :: Free (Prog Int)
  readF = liftFree (Read id)
  ```
*/
Program<int> readF = Free::liftFree(make_read(id<int>));
/*!
  ```haskell
  writeF :: Int -> Free (Prog ())
  writeF x = liftFree (Write x id)
  ```
*/
Program<unit> writeF(int x) { return Free::liftFree(make_write(x, id<unit>)); }

/*!
  #### An Interpreter

  In order to give some meaning to values of `Prog<A>` we need an interpreter,
  i.e. a function that takes an action, somehow performs the action associated
  with it and then invokes its continuation (zero or more times).

  For our example we will define an interpreter that keeps track of the values
  written and invokes `Read` continuations once for each such value. Because
  this is C++ and not Haskell, we can keep some mutable state; in Haskell we'd
  need to be in the State Monad or similar.
*/
// interpret :: Prog a -> List a
// interpret (Read n) = fmap n STATE
// interpret (Write x n) = STATE+=x; n ()
struct Interpreter {
  List<int> l;  // the values written so far
  template <typename A>
  List<A> operator()(Prog<A> prog) {
    struct Visitor {
      List<int>& l;
      List<A> operator()(const Read<A>& r) {
        // invoke the continuation with each of the values that have been written so far
        return Functor::fmap(r.next, l);
      }
      List<A> operator()(const Write<A>& w) {
        // remember the value and invoke the continuation
        l.push_back(w.x);
        return {w.next(unit{})};
      }
    };
    Visitor v{l};
    return boost::apply_visitor(v, prog.v);
  }
};

/*!
  Our interpreter works on values of `Prog<A>`, but when we use the Free Monad
  we will be constructing values of type `Program<A>`, aka `Free<Prog, A>`. So,
  we define a helper that runs our interpreter on such a value, using the
  `foldFree` function we defined in `Free.h`.
 */
// run :: Program a -> List a
// run p = foldFree interpret p
template <typename A>
List<A> run(Program<A> program) {
  Interpreter i;
  return Free::foldFree<List>(i, program);
}

/*!
  #### Tests

  OK, that's the tedious setup out of the way---now we can actually use our Free
  Monad!
*/
TEST_CASE("Free monad for Prog") {
  using std::string;
  using std::to_string;
  auto pure = [](auto&& x) { return Monad::pure<Program>(x); };

  /*!
    ```haskell
    do
      readF
    ```
  */
  {
    // clang-format off
    auto free =
      readF;
    // clang-format on
    CHECK(run(free) == List<int>());
  }
  /*!
    ```haskell
    do
      x <- readF
      pure (show x)
    ```
  */
  {
    // clang-format off
    Program<string> free =
      readF >>= [=](int x) { return
      pure(to_string(x));
    };
    // clang-format on
    CHECK(run(free) == List<string>());
  }
  /*!
    ```haskell
    do
      writeF 10
      x <- readF
      writeF 20
      y <- readF
      pure (x + y)
    ```
  */
  {
    // clang-format off
    auto free =
      writeF(10) >>
      readF >>= [=](int x) { return
      writeF(20) >>
      readF >>= [=](int y) { return
      pure(x + y);
    };};
    // clang-format on
    CHECK(run(free) == List<int>({20, 30}));
  }
}
