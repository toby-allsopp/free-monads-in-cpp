#pragma once

#include <utility>

template <typename... Ts>
struct make_void {
  typedef void type;
};
template <typename... Ts>
using void_t = typename make_void<Ts...>::type;

namespace Functor {
  template <template <typename> class T, typename = void>
  struct Functor;

  namespace detail {
#ifdef _MSC_VER
    template <template <typename> class>
    using IsFunctorT = std::true_type;
#else
    template <template <typename> class, typename = void_t<>>
    struct IsFunctorT : std::false_type {};

    struct dummy1 {};
    struct dummy2 {};

    template <template <typename> class T>
    struct IsFunctorT<T,
                      void_t<std::enable_if_t<std::is_same<
                          T<dummy2>,
                          decltype(Functor<T>::fmap(std::declval<dummy2(dummy1)>(),
                                                    std::declval<T<dummy1>>()))>::value>>>
        : std::true_type {};
#endif
  }

  template <template <typename> class T>
  constexpr bool IsFunctor = detail::IsFunctorT<T>::value;

  template <template <typename> class F,
            typename A,
            typename Fun,
            typename = std::enable_if_t<IsFunctor<F>>>
  F<std::result_of_t<Fun(A)>> fmap(Fun&& fun, const F<A>& f) {
    return Functor<F>::fmap(std::forward<Fun>(fun), f);
  }

  // It is useful for testing purposes to have a very basic Functor - we call
  // this NullFunctor.
  namespace Test {
    template <typename A>
    struct NullFunctor {};
  }
  template <>
  struct Functor<Test::NullFunctor> {
    template <typename F, typename A>
    static Test::NullFunctor<std::result_of_t<F(A)>> fmap(F, Test::NullFunctor<A>) {
      return {};
    }
  };
  static_assert(IsFunctor<Test::NullFunctor>, "NullFunctor must be a Functor");
}
