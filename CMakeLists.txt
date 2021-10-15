cmake_minimum_required(VERSION 3.10)  # bionic's cmake version

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Has to be set before `project()`, and ignored on non-macos:
set(CMAKE_OSX_DEPLOYMENT_TARGET 10.12 CACHE STRING "macOS deployment target (Apple clang only)")

set(LANGS CXX)

find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  foreach(lang ${LANGS})
    if(NOT DEFINED CMAKE_${lang}_COMPILER_LAUNCHER AND NOT CMAKE_${lang}_COMPILER MATCHES ".*/ccache")
      message(STATUS "Enabling ccache for ${lang}")
      set(CMAKE_${lang}_COMPILER_LAUNCHER ${CCACHE_PROGRAM} CACHE STRING "")
    endif()
  endforeach()
endif()

project(oxenenc
    VERSION 0.0.1
    DESCRIPTION "oxenenc - common bt and baseN encoding/decoding library"
    LANGUAGES ${LANGS})


find_package(PkgConfig REQUIRED)

include(CheckCXXSourceCompiles)
include(CheckLibraryExists)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

option(WITH_DOCS "build documentation" ON)
option(WITH_TESTS "build tests" ON)
option(WITH_PYBIND "build pybind module" OFF)
option(WITH_NODEJS "build nodejs native bindings" OFF)
option(WITH_JNI "build jni bindings" OFF)

add_library(oxenenc
  oxenenc/bt_serialize.cpp)


if(WITH_DOCS)
  add_subdirectory(docs)
endif()

if(WITH_PYBIND)
  add_subdirectory(pybind)
endif()

if(WITH_NODEJS)
  add_subdirectory(nodejs)
endif()

if(WITH_JNI)
  add_subdirectory(jni)
endif()

if(WITH_TESTS)
  add_subdirectory(tests)
endif()
