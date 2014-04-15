set(sax_src
    flib_sax.f90
    m_buffer.f90
    m_charset.f90
    m_converters.f90
    m_debug.f90
    m_dictionary.f90
    m_elstack.f90
    m_entities.f90
    m_fsm.f90
    m_io.f90
    m_reader.f90
    m_xml_error.f90
    m_xml_parser.f90)
add_library(sax ${sax_src})
