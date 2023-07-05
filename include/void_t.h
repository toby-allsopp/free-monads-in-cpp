#pragma once

// Copyright (c) 2016 Toby Allsopp
// See LICENSE for license details.

template <typename... Ts>
struct make_void {
  typedef void type;
};
template <typename... Ts>
using void_t = typename make_void<Ts...>::type;
