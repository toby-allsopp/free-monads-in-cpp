#pragma once

// Copyright (c) 2016 Toby Allsopp
// See LICENSE for license details.

#include "Functor.h"
#include "Monad.h"

#include <algorithm>
#include <iterator>
#include <ostream>
#include <vector>

/*!
  ## The List Monad ##

  We introduce a special kind of vector that we call List. We could use vector
  directly but this allows us to avoid making every vector a Monad.
*/
template <typename A>
struct List : std::vector<A> {
  using std::vector<A>::vector;
};

/*!
  It is useful to be able to print out Lists for testing and debugging.
*/
template <typename A>
std::ostream& operator<<(std::ostream& os, const List<A>& l) {
  os << "[";
  bool first = true;
  for (const auto& x : l) {
    if (!std::exchange(first, false)) os << ", ";
    os << x;
  }
  os << "]";
  return os;
}

/*!
  ### Making List a Functor ###

  First, we need to make `List` a Functor by partially specializing `Functor` and
  defining `fmap`.
*/
namespace Functor {
  // instance Functor List where
  template <>
  struct Functor<List> {
    // fmap :: (a -> b) -> List a -> List b
    template <typename Fun, typename A>
    static auto fmap(Fun&& f, const List<A>& l) {
      List<std::result_of_t<Fun(A)>> result;
      std::transform(
          l.begin(), l.end(), std::back_inserter(result), std::forward<Fun>(f));
      return result;
    }
  };
  static_assert(IsFunctor<List>, "List should be a Functor");
}

/*!
  ### Making List a Monad ###

  Then we can make `List` a Monad by partially specializing `Monad` and defining
  `pure` and `bind`.
*/
namespace Monad {
  // instance Monad List where
  template <>
  struct Monad<List> {
    // pure :: a -> List a
    template <typename A>
    static List<A> pure(const A& x) {
      return List<A>({x});
    }

    // bind :: List a -> (a -> List b) -> List b
    template <typename F, typename A, typename ListB = std::result_of_t<F(A)>>
    static ListB bind(const List<A>& l, F&& f) {
      ListB result;
      for (const A& x : l) {
        auto subresult = std::forward<F>(f)(x);
        std::copy(subresult.begin(), subresult.end(), std::back_inserter(result));
      }
      return result;
    }
  };
  static_assert(IsMonad<List>, "List should be a Monad");
}
