#pragma once

#include "Functor.h"

namespace Monad {
  template <template <typename> class M, typename Enable = void>
  struct Monad;

  namespace detail {
    template <template <typename> class M, typename Enable = void>
    struct IsMonadT : std::false_type {};

    template <template <typename> class M>
    struct IsMonadT<M, std::enable_if_t<Functor::IsFunctor<M> && sizeof(Monad<M>) >= 1>>
        : std::true_type {};
  }

  template <template <typename> class M>
  constexpr bool IsMonad = detail::IsMonadT<M>::value;

  // pure :: (Monad m) => a -> m a
  template <template <typename> class M, typename A>
  M<A> pure(const A& x) {
    return Monad<M>::pure(x);
  }

  // join :: (Monad m) => m (m a) -> m a
  template <template <typename> class M, typename A>
  auto join(const M<M<A>>& x) {
    return Monad<M>::join(x);
  }

  // bind :: (Monad m) => m a -> (a -> m b) -> m b
  // bind = join . (flip fmap)
  template <typename F, template <typename> class M, typename A>
  std::result_of_t<F(A)> bind(const M<A>& m, F&& f) {
    return Monad<M>::bind(m, std::forward<F>(f));
    // return join(Functor::fmap(std::forward<F>(f), m));
  }
}

template <template <typename> class M,
          typename A,
          typename F,
          typename = std::enable_if_t<Monad::IsMonad<M>>>
auto operator>>=(const M<A>& m, F&& f) {
  return Monad::bind(m, std::forward<F>(f));
}

// (>>) :: (Monad m) => m a -> m () -> m ()
// x >> y = x >>= \_ -> y
template <template <typename> class M,
          typename A,
          typename B,
          typename = std::enable_if_t<Monad::IsMonad<M>>>
auto operator>>(const M<A>& m, const M<B>& v) {
  return Monad::bind(m, [=](auto&&) { return v; });
}
