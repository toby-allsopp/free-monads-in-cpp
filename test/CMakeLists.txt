cmake_minimum_required(VERSION 3.0)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Boost REQUIRED COMPONENTS)

add_executable(TestRunner TestRunner.cpp FunctorTest.cpp ListTest.cpp FreeTest.cpp)
target_include_directories(TestRunner PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(TestRunner Monad)
