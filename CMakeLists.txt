cmake_minimum_required(VERSION 3.0)

project(FreeCpp)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# We use Boost for the header-only variant library
find_package(Boost REQUIRED COMPONENTS)

# Turn on lots of compiler warnings
if(MSVC)
  # Force to always compile with W4
  if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
    string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
  endif()
elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  # Update if necessary
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")
endif()

# Define our header-only Monad library
add_library(Monad INTERFACE)
target_include_directories(Monad INTERFACE include ${Boost_INCLUDE_DIRS})
# This turns on C++14 mode
target_compile_features(Monad INTERFACE
  cxx_alias_templates
  cxx_trailing_return_types
  cxx_return_type_deduction)

# Tests
add_subdirectory(test)

enable_testing()
add_test(NAME Test COMMAND TestRunner)
