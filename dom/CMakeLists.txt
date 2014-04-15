#find_library(m_strings REQUIRED)
#include_directories(${m_strings_INCLUDE_DIRS})
include_directories(${xmlf90_BINARY_DIR}/strings)
include_directories(${xmlf90_BINARY_DIR}/sax)
include_directories(${xmlf90_BINARY_DIR}/wxml)
set(dom_src
    m_dom_types.f90
    m_dom_debug.f90
    m_dom_error.f90
    m_dom_namednodemap.f90
    m_dom_nodelist.f90
    m_dom_node.f90
    m_dom_attribute.f90
    m_dom_document.f90
    m_dom_element.f90
    m_dom_parse.f90
    m_dom_utils.f90
    flib_dom.f90)
add_library(dom ${dom_src})
target_link_libraries(dom m_strings sax wxml)
