find_program(DOXYGEN doxygen)
if (NOT DOXYGEN)
    message(STATUS "Documentation generation disabled (doxygen not found)")
    return()
endif()
find_program(SPHINX_BUILD sphinx-build)
if (NOT SPHINX_BUILD)
    message(STATUS "Documentation generation disabled (sphinx-build not found)")
    return()
  endif()
  
set(oxenenc_doc_sources "${DOCS_SRC}")
string(REPLACE ";" " " oxenec_doc_sources_spaced "${oxenenc_doc_sources}")

add_custom_command(
    OUTPUT doxyxml/index.xml
    COMMAND ${DOXYGEN} Doxyfile
    DEPENDS
        ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        ${oxenenc_doc_sources}
)

add_custom_command(
    OUTPUT html/index.html
    COMMAND ${SPHINX_BUILD} -j auto
        -Dbreathe_projects.oxenenc=${CMAKE_CURRENT_BINARY_DIR}/doxyxml
        -Dversion=${oxenenc_VERSION} -Drelease=${oxenenc_VERSION}
        -Aversion=${oxenenc_VERSION} -Aversions=${oxenenc_VERSION_MAJOR},${oxenenc_VERSION_MINOR},${oxenenc_VERSION_PATCH}
        -b html
        ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/html
    DEPENDS
        ${CMAKE_CURRENT_BINARY_DIR}/index.rst
        ${CMAKE_CURRENT_BINARY_DIR}/conf.py
        ${CMAKE_CURRENT_BINARY_DIR}/doxyxml/index.xml
)
add_custom_target(doc DEPENDS html/index.html)
configure_file(conf.py.in conf.py @ONLY)
configure_file(Doxyfile.in Doxyfile @ONLY)
configure_file(index.rst index.rst COPYONLY)
