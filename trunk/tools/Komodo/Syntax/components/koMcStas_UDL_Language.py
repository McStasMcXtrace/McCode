# Registers the McStas language in Komodo.

import logging
from koUDLLanguageBase import KoUDLLanguage

log = logging.getLogger("koMcStasLanguage")
#log.setLevel(logging.DEBUG)

def registerLanguage(registry):
    log.debug("Registering language McStas")
    registry.registerLanguage(KoMcStasLanguage())

class KoMcStasLanguage(KoUDLLanguage):

    # ------------ Komodo Registration Information ------------ #

    name = "McStas"
    lexresLangName = "McStas"
    _reg_desc_ = "%s Language" % name
    _reg_contractid_ = "@activestate.com/koLanguage?language=%s;1" % name
    _reg_categories_ = [("komodo-language", name)]
    _reg_clsid_ = "02a1a212-d5a1-4910-9112-dc17c7d43950"
    defaultExtension = '.instr'

    # ------------ Commenting Controls ------------ #

    commentDelimiterInfo = {
        "line": [
                '//',   # C-style one line comments
                #'#',    # Hash-style one line comments
                #'--',   # SQL-style one line comments
                #';',    # Lisp-style one line comments
                #'%',    # Erlang-style one line comments
                ],
        "block": [
                ('/*', '*/')   # C-style block comments
                #('(*', '*)')   # Pascal-style block comments
                ],
    }

    # ------------ Indentation Controls ------------ #

    # To support automatic indenting and dedenting after "{([" and "})]"
    supportsSmartIndent = "brace"
    # Other smart indenting types are:
    #   'text', 'python', 'XML' and 'keyword'

    # Indent/dedent after these words.
    #_indenting_statements = ['case']
    #_dedenting_statements = ['return', 'break', 'continue']

    # ------------ Sub-language Controls ------------ #

    #Check: Update 'lang_from_udl_family' as appropriate for your
    #      lexer definition. There are four UDL language families:
    #           M (markup), i.e. HTML or XML
    #           CSL (client-side language), e.g. JavaScript
    #           SSL (server-side language), e.g. Perl, PHP, Python
    #           TPL (template language), e.g. RHTML, Django, Smarty
    #      'lang_from_udl_family' maps each UDL family code (M,
    #      CSL, ...) to the sub-language name in your language.
    #      Some examples:
    #        lang_from_udl_family = {   # A PHP file can contain
    #           'M': 'HTML',            #   HTML
    #           'SSL': 'PHP',           #   PHP
    #           'CSL': 'JavaScript',    #   JavaScript
    #        }
    #        lang_from_udl_family = {   # An RHTML file can contain
    #           'M': 'HTML',            #   HTML
    #           'SSL': 'Ruby',          #   Ruby
    #           'CSL': 'JavaScript',    #   JavaScript
    #           'TPL': 'RHTML',         #   RHTML template code
    #        }
    #        lang_from_udl_family = {   # A plain XML can just contain
    #           'M': 'XML',             #   XML
    #        }
    lang_from_udl_family = {'SSL': 'McStas'}
