// Copyright (c) 2016 Toby Allsopp
// See LICENSE for license details.

#include "Functor.h"
#include "void_t.h"

int id(int x) { return x; }

template <template <typename> class, typename = void>
struct fmap_compiles_t : std::false_type {};
template <template <typename> class T>
struct fmap_compiles_t<T, void_t<decltype(fmap(id, T<int>{}))>> : std::true_type {};
template <template <typename> class T>
constexpr bool fmap_compiles = fmap_compiles_t<T>::value;

namespace Functor {
#ifndef _MSC_VER
  template <typename A>
  struct NoSpec {};
  static_assert(!IsFunctor<NoSpec>, "no specialisation is not a Functor");

  template <typename A>
  struct Nothing {};
  template <>
  struct Functor<Nothing> {};
  static_assert(!IsFunctor<Nothing>, "specialisation with no members is not a Functor");
  static_assert(!fmap_compiles<Nothing>, "shouldn't compile");

  template <typename A>
  struct WrongFmapReturnType {};
  template <>
  struct Functor<WrongFmapReturnType> {
    template <typename F, typename A>
    static void fmap(F, WrongFmapReturnType<A>) {}
  };
  static_assert(!IsFunctor<WrongFmapReturnType>,
                "specialisation with wrong fmap return type is not a Functor");
  static_assert(!fmap_compiles<WrongFmapReturnType>,
                "fmap on incorrect specialisation doesn't compile");
#endif

  template <typename A>
  struct Correct {};
  template <>
  struct Functor<Correct> {
    template <typename F, typename A>
    static auto fmap(F, Correct<A>) {
      return Correct<std::result_of_t<F(A)>>{};
    }
  };
  static_assert(IsFunctor<Correct>, "correct specialisation is a Functor");
#ifndef _MSC_VER
  static_assert(fmap_compiles<Correct>, "fmap on correct specialisation compiles");
#endif
  void testCorrect() { fmap(id, Correct<int>{}); }
}
