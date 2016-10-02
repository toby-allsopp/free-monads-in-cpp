#pragma once

#include "Functor.h"
#include "Monad.h"

#include <boost/variant.hpp>

/*
  data Free f a = Return a | Bind (f (Free f a))
*/
namespace Free {
  template <template <typename> class F, typename A>
  struct Return;
  template <template <typename> class F, typename A>
  struct Bind;

  template <template <typename> class F, typename A>
  struct Free {
    using ContainedType = A;
    using ReturnType    = Return<F, A>;
    boost::variant<boost::recursive_wrapper<Return<F, A>>,
                   boost::recursive_wrapper<Bind<F, A>>>
        v;
  };

  template <template <typename> class F, typename A>
  struct Return {
    A a;
  };
  template <template <typename> class F, typename A>
  struct Bind {
    F<Free<F, A>> x;
  };

  template <template <typename> class F, typename A>
  Free<F, A> make_return(const A& x) {
    return {Return<F, A>{x}};
  }

  template <typename FA>
  FA make_return(const typename FA::ContainedType& x) {
    return {typename FA::ReturnType{x}};
  }

  template <template <typename> class F, typename A>
  Free<F, A> make_bind(const F<Free<F, A>>& x) {
    return {Bind<F, A>{x}};
  }

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

  /*
    instance Functor f => Functor (Free f) where
    fmap fun (Return x) = Return (fun x)
    fmap fun (Bind x) = Bind (fmap (fmap fun) x)
  */
  template <template <typename> class Wrapper>
  struct FunctorImpl {
    template <typename A, typename Fn>
    struct Visitor {
      // using result_type = Free<F, std::result_of_t<Fn(A)>>;
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
    template <typename A, typename Fn>
    static Wrapper<std::result_of_t<Fn(A)>> fmap(Fn&& fun, const Wrapper<A>& f) {
      return boost::apply_visitor(Visitor<A, Fn>{fun}, f.v);
    }
  };

  /*
    instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r
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

    // join :: m (m a) -> m a
    // join :: Free<Free<F<A>>> -> Free<F<A>>
    // using JoinResType = Free::Free<typename
    // Functor::Functor<F>::ContainedType>;
    // static JoinResType join(const Free::Free<F>& x) {
    //     struct Visitor
    //     {
    //         using result_type = JoinResType;
    //         result_type operator()(const Free::Bind<F>& b)
    //         {
    //             return Free::Bind<G>{Functor::fmap(
    //                 [](const Free::Free<F>& x) { return x; },
    //                 b.x)};
    //         }
    //         result_type operator()(const Free::Return<F>& r) { return r.a;
    //         }
    //     };
    //     Visitor v{};
    //     return boost::apply_visitor(v, x);
    // }

    // bind :: m a -> (a -> m b) -> m b
    // bind :: Free<F<A>> -> (a -> Free<F<B>>) -> Free<F<B>>
    template <typename A, typename Fn>
    struct Visitor {
      using MB          = std::result_of_t<Fn(A)>;
      using result_type = MB;
      using B           = typename MB::ContainedType;
      static_assert(std::is_same<MB, Wrapper<B>>::value, "");
      Fn&& fun;

      // (Bind x) >>= f = Bind (fmap (>>= f) x)
      template <template <typename> class F>
      result_type operator()(const Bind<F, A>& b) {
        auto& f = this->fun;
        return make_bind(Functor::fmap(
            [f](const Free<F, A>& x) {
              return static_cast<Free<F, B>>(Monad::bind(Wrapper<A>(x), f));
            },
            b.x));
      }

      // (Return r) >>= f = f r
      template <template <typename> class F>
      result_type operator()(const Return<F, A>& r) {
        return fun(r.a);
      }
    };

    template <typename A, typename Fn>
    static auto bind(const M<A>& x, Fn&& fun) {
      Visitor<A, Fn> v{std::forward<Fn>(fun)};
      return boost::apply_visitor(v, x.v);
    }
  };

  /*
    liftFree :: (Functor f) => f a -> Free f a
    liftFree x = Bind (fmap Return x)
   */
  template <template <typename> class F, typename A>
  Free<F, A> liftFree(const F<A>& x) {
    return make_bind<F>(Functor::fmap([](const A& a) { return make_return<F>(a); }, x));
  }

  /*
    foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
    foldFree _ (Pure a)  = return a
    foldFree f (Free as) = f as >>= foldFree f
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
        return fun(b.x) >>= [&](const auto& x) { return foldFree<M, F>(fun, x); };
      }
    };
    Visitor v{fun};
    return boost::apply_visitor(v, free.v);
  }
}

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
