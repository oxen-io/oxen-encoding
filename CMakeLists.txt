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

project(oxenc
    VERSION 0.0.1
    DESCRIPTION "oxenc - Base 16/32/64 and Bittorrent Encoding/Decoding Header Only Library"
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


include(GNUInstallDirs)

message(STATUS "oxenc v${PROJECT_VERSION}")

set(OXENC_LIBVERSION 0)


if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    set(oxenc_IS_TOPLEVEL_PROJECT TRUE)
else()
    set(oxenc_IS_TOPLEVEL_PROJECT FALSE)
endif()

option(OXENC_BUILD_TESTS "Building and perform oxenc tests" ${oxenc_IS_TOPLEVEL_PROJECT})
option(OXENC_INSTALL "Add oxenc headers to install target" ${oxenc_IS_TOPLEVEL_PROJECT})


configure_file(oxenc/version.h.in oxenc/version.h @ONLY)
configure_file(liboxenc.pc.in liboxenc.pc @ONLY)


add_library(oxenc INTERFACE)

target_include_directories(oxenc
    INTERFACE
        $<INSTALL_INTERFACE:>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

export(
    TARGETS oxenc
    NAMESPACE oxenc::
    FILE oxencTargets.cmake
)

if(OXENC_INSTALL)
  install(
    TARGETS oxenc
    EXPORT oxencConfig
    DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )

  install(
    FILES
    oxenc/base32z.h
    oxenc/base64.h
    oxenc/bt_serialize.h
    oxenc/bt_value.h
    oxenc/byte_type.h
    oxenc/hex.h
    oxenc/variant.h
    ${CMAKE_CURRENT_BINARY_DIR}/oxenc/version.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/oxenc
    )


  install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/liboxenc.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
    )
endif()


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
