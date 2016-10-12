#pragma once

#include "Functor.h"
#include "void_t.h"

/*!
  ### Monads ###

  The concept of a Monad comes from category theory, but the defintion I'm using
  is that of its practical application in Haskell.

  In Haskell, `Monad` is a typeclass, defined something like so:

  ```haskell
  class (Functor m) => Monad m where
    pure :: a -> m a
    bind :: m a -> (a -> m b) -> m b
  ```

  There are some additional laws that Monads are supposed to obey but for now I'm
  just going to focus on the type signatures.

  In C++, we can treat the type variable `m` as a class template with a class
  template template parameter and define a class template `Monad` that we will
  specialize for each class template `M` that we want to treat as a Monad.

*/
namespace Monad {
  template <template <typename> class M, typename Enable = void>
  struct Monad;

  /*!
    C++ doesn't provide a neat way to define the members that we expect
    specializations of the `Monad` template to have, but we can write a type-level
    predicate that uses SFINAE to check.
   */
  namespace detail {
    template <template <typename> class M, typename Enable = void>
    struct IsMonadT : std::false_type {};

    /*!
      At the moment we just have a very basic check---is there a specialization
      of Monad for the template and is the template a Functor.
     */
    template <template <typename> class M>
    struct IsMonadT<M, void_t<Monad<M>, std::enable_if_t<Functor::IsFunctor<M>>>>
        : std::true_type {};
  }

  template <template <typename> class M>
  constexpr bool IsMonad = detail::IsMonadT<M>::value;

  /*!
    Now that we can tell (approximately) whether a class template is a Monad, we
    can define the Monad operations as free functions so that the template
    arguments can be deduced in at least some cases.
  */
  // pure :: (Monad m) => a -> m a
  template <template <typename> class M,
            typename A,
            typename = std::enable_if_t<IsMonad<M>>>
  M<A> pure(const A& x) {
    return Monad<M>::pure(x);
  }

  // bind :: (Monad m) => m a -> (a -> m b) -> m b
  template <typename F,
            template <typename> class M,
            typename A,
            typename = std::enable_if_t<IsMonad<M>>>
  std::result_of_t<F(A)> bind(const M<A>& m, F&& f) {
    return Monad<M>::bind(m, std::forward<F>(f));
  }
}

/*!
  It is common in Haskell to use the infix (>>=) operator as a synonym for
  _bind_; this reduces the need for parentheses. We can do the same thing in
  C++, allowing us to write `m >>= [](){}` instead of `bind(m, [](){})`.
 */
template <template <typename> class M,
          typename A,
          typename F,
          typename = std::enable_if_t<Monad::IsMonad<M>>>
auto operator>>=(const M<A>& m, F&& f) {
  return Monad::bind(m, std::forward<F>(f));
}

/*!
  The (>>) operator is also commonly used in Haskell. It just throws away the
  result of evaluating the first argument and returns the second argument
  instead.

  ```haskell
  (>>) :: (Monad m) => m a -> m () -> m ()
  x >> y = x >>= \_ -> y
  ```
 */
template <template <typename> class M,
          typename A,
          typename B,
          typename = std::enable_if_t<Monad::IsMonad<M>>>
M<B> operator>>(const M<A>& m, const M<B>& v) {
  return Monad::bind(m, [=](auto&&) { return v; });
}
