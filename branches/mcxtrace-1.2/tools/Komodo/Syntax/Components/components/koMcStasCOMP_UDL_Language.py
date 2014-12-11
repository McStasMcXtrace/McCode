# Registers the McStasCOMP language in Komodo.

import logging
from koUDLLanguageBase import KoUDLLanguage

log = logging.getLogger("koMcStasCOMPLanguage")
#log.setLevel(logging.DEBUG)

def registerLanguage(registry):
    log.debug("Registering language McStasCOMP")
    registry.registerLanguage(KoMcStasCOMPLanguage())

class KoMcStasCOMPLanguage(KoUDLLanguage):

    # ------------ Komodo Registration Information ------------ #

    name = "McStasCOMP"
    lexresLangName = "McStasCOMP"
    _reg_desc_ = "%s Language" % name
    _reg_contractid_ = "@activestate.com/koLanguage?language=%s;1" % name
    _reg_categories_ = [("komodo-language", name)]
    _reg_clsid_ = "9bb2ea86-98eb-4146-a855-b1f2c0d980b0"
    defaultExtension = '.comp'

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
    lang_from_udl_family = {'SSL': 'McStasCOMP'}
