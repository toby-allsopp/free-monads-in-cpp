#pragma once

#include "Functor.h"
#include "Monad.h"

#include <algorithm>
#include <iterator>
#include <ostream>
#include <vector>

template <typename A>
struct List : std::vector<A> {
  using std::vector<A>::vector;
};

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

namespace Functor {
  template <>
  struct Functor<List> {
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

namespace Monad {
  template <>
  struct Monad<List> {
    template<typename A>
    static List<A> pure(const A& x) { return List<A>({x}); }

    template <typename F, typename A>
    static auto bind(const List<A>& l, F&& f) {
      std::result_of_t<F(A)> result;
      for (const A& x : l) {
        auto subresult = std::forward<F>(f)(x);
        std::copy(
            subresult.begin(), subresult.end(), std::back_inserter(result));
      }
      return result;
    }

    template<typename A>
    static auto join(const List<List<A>>& l) {
      List<A> result;
      for (const List<A>& x : l) {
        std::copy(x.begin(), x.end(), std::back_inserter(result));
      }
      return result;
    }
  };
  static_assert(IsMonad<List>, "List should be a Monad");
}
