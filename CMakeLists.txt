cmake_minimum_required(VERSION 3.29.0)

project(Maestro)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(SOURCES src/bitboard.cpp
            src/position.cpp
            src/movegen.cpp
            src/perft.cpp
            src/search.cpp
            src/uci.cpp
            src/movepicker.cpp
            src/eval.cpp
            src/hashtable.cpp
            src/main.cpp)
set(HEADERS src/defs.hpp
            src/bitboard.hpp
            src/misc.hpp
            src/position.hpp
            src/movegen.hpp
            src/perft.hpp
            src/search.hpp
            src/movepicker.hpp
            src/psqt.hpp
            src/eval.hpp
            src/hashtable.hpp
            src/uci.hpp)

set(USER_FLAGS "-g -Ofast -march=native -lpthread")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${USER_FLAGS} ")

add_executable(Maestro ${SOURCES} ${HEADERS})