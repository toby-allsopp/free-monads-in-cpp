#pragma once

#include "Functor.h"
#include "Monad.h"

/*!
  ### Free Monads ###

  Every type _f_ that is a Functor has a "Free" Monad. A Free Monad is some
  category theory gobbledygook but it's basically the simplest possible Monad
  that doesn't throw any information away.

  In Haskell, this is defined quite simply:

  ```
  data Free f a = Return a | Bind (f (Free f a))

  instance (Functor f) => Monad (Free f) where
  ...
  ```

  This is a bit complicated to express in C++ due to the lack of algebraic data
  types, in particular the lack of a sum type (`tuple` and `struct` provide
  product types). In C++17 we will get `variant` which plugs the gap acceptably
  and in the meantime we can use `boost::variant`.
*/
#include <boost/variant.hpp>

  /*!
    To define something like the Haskell `data` with two cases, we define a type
    for each case and then combine them into a `variant`. Because the `Bind`
    case is recursive we need to forward-declare them and use
    `recursive_wrapper` to define the `variant`.
   */
namespace Free {
  template <template <typename> class F, typename A>
  struct Return;
  template <template <typename> class F, typename A>
  struct Bind;

  /*!
    We wrap the `variant` in a `struct` so we have somewhere to hang a couple of
    type aliases that we will need later to be able to write down the type
    signatures for some functions.
   */
  template <template <typename> class F, typename A>
  struct Free {
    using ContainedType = A;
    using ReturnType    = Return<F, A>;
    boost::variant<boost::recursive_wrapper<Return<F, A>>,
                   boost::recursive_wrapper<Bind<F, A>>>
        v;
  };

  /*!
    Now we can define the individual cases, quite analogously to the Haskell
    definition. The only difference (apart from the syntax) is that we need to
    name the members something.
   */
  template <template <typename> class F, typename A>
  struct Return {
    A a;
  };
  template <template <typename> class F, typename A>
  struct Bind {
    F<Free<F, A>> x;
  };

  /*!
    For our convenience, we define some helper function templates that handle
    wrapping things up in the `Free` struct and allow some of the template
    arguments to be deduced.
  */

  // This one is for when you know the Functor template argument - it can deduce
  // the contained type.
  template <template <typename> class F, typename A>
  Free<F, A> make_return(const A& x) {
    return {Return<F, A>{x}};
  }

  // This one is for when you know the resulting Free type but not the template
  // arguments.
  template <typename FA>
  FA make_return(const typename FA::ContainedType& x) {
    return {typename FA::ReturnType{x}};
  }

  template <template <typename> class F, typename A>
  Free<F, A> make_bind(const F<Free<F, A>>& x) {
    return {Bind<F, A>{x}};
  }

  /*!
    It can be helpful while testing to be able to print out a value of `Free`,
    so we define an overload of the left shift^W^Wstream out operator. This also
    demonstrates how to get values out of the `variant` in a type-safe way---by
    using a visitor.
  */
  template <template <typename> class F, typename A>
  std::ostream& operator<<(std::ostream& os, const Free<F, A>& free) {
    struct Visitor {
      std::ostream& os;
      std::ostream& operator()(const Return<F, A>& r) {
        return os << "Return{" << r.a << "}";
      }
      std::ostream& operator()(const Bind<F, A>& b) {
        return os << "Bind{" << b.x << "}";
      }
    };
    Visitor v{os};
    return boost::apply_visitor(v, free.v);
  }

  /*!
    Now we implement the Functor functions. We don't actually create the
    specialization of the Functor template here because that has to be done in
    the Functor namespace but it is more convenient to write the functions in
    the Free namespace.

    The Functor instance for Free is hilariously short and to-the-point in
    Haskell:

    ```haskell
    instance Functor f => Functor (Free f) where
      fmap fun (Return x) = Return (fun x)
      fmap fun (Bind x)   = Bind (fmap (fmap fun) x)
    ```

    We assume that any user that wants to use the Functor instance of Free for a
    specific Functor F will create a wrapper type like so:

    ```
    template<typename A>
    struct Wrapper : Free<F, A> {};
    ```

    This is needed so that the template parameters match up correctly - all the
    Functor-related functions expect to deal with a template with a single
    parameter, whereas `Free` has two.
  */
  template <template <typename> class Wrapper>
  struct FunctorImpl {

    // The visitor struct can't be defined inside the fmap function because it
    // contains member function templates, which we use to get the compiler to
    // tell us what the template F is in the Free<F, A> inside the wrapper.
    template <typename A, typename Fn>
    struct Visitor {
      Fn& fun;
      template <template <typename> class F>
      auto operator()(const Return<F, A>& r) const {
        return make_return<F>(fun(r.a));
      }
      template <template <typename> class F>
      auto operator()(const Bind<F, A>& b) const {
        using Functor::fmap;
        return make_bind(fmap([&](const auto& f) { return fmap(fun, f); }, b.x));
      }
    };

    // fmap :: (a -> b) -> Free f a -> Free f b
    template <typename A, typename Fn>
    static Wrapper<std::result_of_t<Fn(A)>> fmap(Fn&& fun, const Wrapper<A>& f) {
      return boost::apply_visitor(Visitor<A, Fn>{fun}, f.v);
    }
  };

  /*!
    Now that we have a way to make `Wrapper<A>`, aka `Free<F, A>`, a Functor, we
    can also make it a Monad. Once again, the instance in Haskell is
    embarrassingly short:

    ```haskell
    instance (Functor f) => Monad (Free f) where
      return = Return
      (Bind x)   >>= f = Bind (fmap (>>= f) x)
      (Return r) >>= f = f r
    ```

    Again, we're not actually specializing the `Monad` class template here
    because that has to be done in the `Monad` namespace and it's more
    convenient to write this stuff in the `Free` namespace.

    Also again, we require the user to wrap their Functor to be Freed into
    something with a single template parameter, as `Free<F, A>` doesn't work
    directly. We make an additional assumption about the wrapper class
    template---it must have a type alias member named `WrappedFree` that is an
    alias for the `Free<F, A>` type that is being wrapped, e.g.

    ```
    template<typename A>
    struct Wrapper : Free<F, A> {
      using WrappedType = Free<F, A>;
    };
    ```
  */
  template <template <typename> class Wrapper>
  struct MonadImpl {
    template <typename A>
    using M = Wrapper<A>;

    // pure :: a -> m a
    // pure :: a -> Free<F>
    template <typename A>
    static M<A> pure(const A& x) {
      return make_return<typename Wrapper<A>::WrappedFree>(x);
    }

    // bind :: Free f a -> (a -> Free f b) -> Free f b
    template <typename A, typename Fn>
    struct BindVisitor {
      using result_type = std::result_of_t<Fn(A)>;
      using B           = typename result_type::ContainedType;
      static_assert(std::is_same<result_type, Wrapper<B>>::value, "");
      Fn&& fun;

      // bind (Bind x) f = Bind (fmap (\m -> bind m f) x)
      template <template <typename> class F>
      result_type operator()(const Bind<F, A>& b) {
        auto& f = this->fun;
        return make_bind(Functor::fmap(
            [f](const Free<F, A>& m) {
              return static_cast<Free<F, B>>(Monad::bind(Wrapper<A>(m), f));
            },
            b.x));
      }

      // bind (Return r) f = f r
      template <template <typename> class F>
      result_type operator()(const Return<F, A>& r) {
        return fun(r.a);
      }
    };

    template <typename A, typename Fn>
    static auto bind(const M<A>& x, Fn&& fun) {
      BindVisitor<A, Fn> v{std::forward<Fn>(fun)};
      return boost::apply_visitor(v, x.v);
    }
  };

  /*!
    It is traditional to provide some helpers to make working with free monads
    more convenient. The first of these is "liftFree", which is used to "lift" a
    functor F into a free monad.

    The implementation looks like so in Haskell:

    ```haskell
    liftFree :: (Functor f) => f a -> Free f a
    liftFree x = Bind (fmap Return x)
    ```

    In C++ this looks very similar, yay! A few extra type annotations are
    required to help the compiler but it's not too bad.
   */
  template <template <typename> class F, typename A>
  Free<F, A> liftFree(const F<A>& x) {
    return make_bind<F>(Functor::fmap(&make_return<F, A>, x));
  }

  /*!
    The other helper is really the whole point of the exercise; it allows you to
    take a value of `Free<F, A>` and "evaluate" it in some way to yield another
    monadic value.

    ```haskell
    foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
    foldFree _ (Return a)  = return a
    foldFree f (Bind as)   = f as >>= foldFree f
    ```

    Because we want to be totally generic with regards to the callable object
    that is passed in, it's not possible to deduce the resulting Monad template,
    so the caller has to specify it. The other template parameters can be
    deduced, however.
  */
  template <template <typename> class M,
            template <typename> class F,
            typename Fun,
            typename A>
  M<A> foldFree(Fun fun, const Free<F, A>& free) {
    struct Visitor {
      Fun fun;

      auto operator()(const Return<F, A>& r) { return Monad::pure<M>(r.a); }
      auto operator()(const Bind<F, A>& b) {
        return fun(b.x) >>= [&](const auto& x) { return foldFree<M>(fun, x); };
      }
    };
    Visitor v{fun};
    return boost::apply_visitor(v, free.v);
  }
}

/*!
  All that remains to be done is to actually make `Free<F>` a Functor and a
  Monad. Recall that `Free` actually has two template parameters but the
  `Functor` and `Monad` class templates expect a template with one parameter as
  their template argument. So, we require a wrapper class template that has only
  one template parameter.

  Given a class template `Wrapper` whose instantiations `Wrapper<A>` have
  `Free<F, A>` as a base and a type alias template member `WrappedFree` equal to
  `Free<F, A>`, we can create a partial speciailization of both `Functor` and
  `Monad`.

  The below speciailizations are not quite as constrained as I would like
  because they don't actually mention `Free` anywhere, so they apply to any
  class template that inherits from the class named by its `WrappedFree` member.
  The reason for this is that I haven't been able to figure out how to get the
  `F` out in order to write `Free<F, void>`.
 */
namespace Functor {
  template <template <typename> class Wrapper>
  struct Functor<Wrapper,
                 std::enable_if_t<std::is_base_of<typename Wrapper<void>::WrappedFree,
                                                  Wrapper<void>>::value>>
      : Free::FunctorImpl<Wrapper> {};
}

namespace Monad {
  template <template <typename> class Wrapper>
  struct Monad<Wrapper,
               std::enable_if_t<std::is_base_of<typename Wrapper<void>::WrappedFree,
                                                Wrapper<void>>::value>>
      : Free::MonadImpl<Wrapper> {};
}

/*!
  To finish things off, we do some compile-time verification that these partial
  specializations work as we expect by wrapping `NullFunctor`.
 */
namespace Free {
  namespace Test {
    template <typename A>
    struct NullFreeWrapper : Free<Functor::Test::NullFunctor, A> {
      using WrappedFree = Free<Functor::Test::NullFunctor, A>;
    };
    static_assert(Functor::IsFunctor<NullFreeWrapper>,
                  "Functor speciailization for Free wrappers works");
    static_assert(Monad::IsMonad<NullFreeWrapper>,
                  "Monad speciailization for Free wrappers works");
  }
}
