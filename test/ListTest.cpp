#include "catch.hpp"

#include "List.h"

#include <iostream>

TEST_CASE("List monad") {
  using Functor::fmap;
  using Monad::pure;
  const List<int> l({1, 2, 3});
  SECTION("fmap") {
    auto l0 = fmap([](int x) { return x + 1; }, l);
    REQUIRE(l0 == List<int>({2, 3, 4}));
    REQUIRE(fmap([](int x) { return -x; }, l) == List<int>({-1, -2, -3}));
  }
  SECTION("pure") {
    using namespace std::literals;
    REQUIRE(pure<List>("Hi"s) == List<std::string>({"Hi"}));
  }
  SECTION("bind") {
    // do
    //   x <- l
    //   y <- pure (x*x)
    //   [x, y]
    REQUIRE(
        Monad::bind(l, [](int x) {
          return Monad::bind(pure<List>(x * x), [x](int y) { return List<int>({x, y}); });
        }) == List<int>({1, 1, 2, 4, 3, 9}));
    REQUIRE(
        Monad::bind(l, [](int x) {
          return Monad::bind(pure<List>(x * x), [x](int y) { return List<int>({x, y}); });
        }) == List<int>({1, 1, 2, 4, 3, 9}));
    REQUIRE((l >>= [](int x) {
              return pure<List>(x * x) >>= [x](int y) { return List<int>({x, y}); };
            }) == (List<int>{1, 1, 2, 4, 3, 9}));
    List<int> l1 = Monad::bind(l, [](int x) -> List<int> {
      return Monad::bind(pure<List>(x * x), [x](int y) -> List<int> {
        return List<int>({x, y});
      });
    });
    REQUIRE(l1 == List<int>({1, 1, 2, 4, 3, 9}));
    auto l2 = l >>= [](int x) {
      return pure<List>(x * 2) >>= [&](int y) { return List<int>{x, y}; };
    };
    REQUIRE(l2 == List<int>({1, 2, 2, 4, 3, 6}));
    std::cout << l2 << "\n";
  }
}
