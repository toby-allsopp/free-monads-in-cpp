cmake_minimum_required(VERSION 3.0)

add_executable(TestRunner TestRunner.cpp FunctorTest.cpp ListTest.cpp FreeTest.cpp)
target_link_libraries(TestRunner Monad)
