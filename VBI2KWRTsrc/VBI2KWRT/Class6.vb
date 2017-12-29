''*CID:''+v110R~:#72                          update#=  122;          ''+v110R~
'************************************************************************************''+v110I~
'v110 2017/12/22 StringConstant reset required when lang changed       ''+v110I~
'************************************************************************************''+v110I~
Imports System.Globalization                                           ''~7615I~
Imports System.IO                                                      ''~7615I~
Imports System.Text                                                    ''~7615I~
Public Class Rstr                                                      ''~7615I~
    'localization not required                                             ''~7618I~
    '********************************************************              ''~7615I~
    Public Const STR_LANG_EN = "en-GB"                                  ''~7613I~''~7615R~''~7618M~
    Public Const STR_LANG_JP = "ja-JP"                                  ''~7613I~''~7615R~''~7618M~
    Public Const ID_LANG_EN = 1                                        ''~7618I~
    Public Const ID_LANG_JP = 0                                        ''~7618I~
    Public Const STRID_FONT_BOLD = "STR_FONT_BOLD"                      ''~7618R~
    Public Const STRID_FONT_ITALIC = "STR_FONT_ITALIC"                  ''~7618I~
    Public Shared MENU_NEWTEXT, MENU_NEWTEXT_FILE, MENU_NEWFILE, FILTER_IMAGE, FILTER_KANJITEXT, FILTER_KANATEXT, MSG_NO_TEXT As String ''~7617I~''~7618R~
    Public Shared MSG_REP_EXISTING, MSG_DISCARD_UPDATE, FORM1_TITLE_KANA_FILE, FORM1_TITLE_KANJI2KANA_FILE As String ''~7618R~
    Public Shared MSG_ERR_NOT_FOUND, MSG_ERR_READ, MSG_ERR_WRITE, MSG_ERR_PRINT_PAGE, MSG_INFO_SHOW_TEXT, FORM1_INITIAL_TEXT As String ''~7618R~
    Public Shared MSG_ERR_EXTRACT, MSG_ERR_ZOOM As String                        ''~7617I~''~7618R~
    Public Shared FORM3_TITLE, FORM3_TITLE_RECEIVED, MSG_INFO_SAVED, MSG_ERR_KANA_CONV, MSG_INFO_EXTRACTED As String ''~7618R~
    Public Shared FONT_STD, FONT_BOLD, FONT_ITALIC, MSG_ERR_FUNCKEY, MSG_ERR_PRINT_FONT, MSG_ERR_SCR_FONT, FONT_BOLD_ITALIC As String ''~7618R~
    Public Shared MSG_ERR_SPECIAL_KEY, MSG_ERR_SPECIAL_KEY_NULL As String ''~7618R~
    Public Shared MSG_ERR_SELECT_LINE, MSG_ERR_ADD_SELECT_LINE, MSG_CONFIRM_RESTORE_DEFAULT As String ''~7618I~
    Public Shared MSG_ERR_FIND_NULL, MSG_INFO_FIND_UP, MSG_INFO_FIND_DOWN, MSG_INFO_FIND_CASE_SENSITIVE As String ''~7618R~
    Public Shared MSG_INFO_FIND_CASE_INSENSE, MSG_ERR_FIND_NOT_FOUND, MSG_ERR_NOT_FOUND_CPOS, MSG_ERR_NOT_FOUND_LINE_COL, MSG_ERR_NOT_FOUND_NO_CPOS, MSG_ERR_REPSTR_SAME, MSG_ERR_NOT_FOUND_ALL As String ''~7618R~
    Public Shared MSG_ERR_SMALL_LARGE, MSG_ERR_SMALL_LARGE_OTHER As String ''~7618I~
    Public Shared MSG_ERR_FAILED_KANJI2KANA_CONV As String             ''~7618I~
    Public Shared MSG_ERR_ALREADY_EXTRACTED As String                  ''~7618R~
    Public Shared FORM2_TITLE As String                                ''~7619I~
    ''~7617I~
    Private Shared resMgr As Resources.ResourceManager                 ''~7615R~
    Private Shared resMgrEN As Resources.ResXResourceSet               ''~7615R~
    Private Shared swInitialized As Boolean = False                      ''+v110I~
    Private Shared oldLangEN As Boolean = False                          ''+v110I~
    Sub New()                                                          ''~7615I~
        resMgr = My.Resources.ResourceManager                            ''~7615I~
        resMgrEN = setupResourceEN()                                     ''~7615I~
        setupStrings()                                                  ''~7617I~
    End Sub                                                            ''~7615I~
    Private Function setupResourceEN() As Resources.ResXResourceSet    ''~7615R~
        Dim rm As Resources.ResXResourceSet                               ''~7615I~
        Dim txt As String = My.Resources.Resources_en_GB                  ''~7615I~
        Dim enc As Encoding = Encoding.GetEncoding("utf-8") ''~7615R~
        Dim txtbyte() As Byte = enc.GetBytes(txt)
        Dim stream As MemoryStream = New MemoryStream(txtbyte) ''~7615R~
        rm = New System.Resources.ResXResourceSet(stream)               ''~7615I~
        Return rm                                                      ''~7615I~
    End Function                                                            ''~7615I~
    Public Shared Function getStr(Pstrid As String) As String             ''~7615R~''~7617R~
        Dim str As String                                              ''~7615I~
        If FormOptions.swLangEN Then                                    ''~7615R~
            str = resMgrEN.GetString(Pstrid)                             ''~7615I~
        Else                                                           ''~7615I~
            str = resMgr.GetString(Pstrid)                               ''~7615I~
        End If                                                         ''~7615I~
        Return str                                                     ''~7615I~
    End Function                                                       ''~7615I~
    Public Shared Function getStr(Pstrid As String, Plang As Integer) As String ''~7618I~
        Dim str As String                                              ''~7618I~
        If Plang = ID_LANG_EN Then                                            ''~7618I~
            str = resMgrEN.GetString(Pstrid)                           ''~7618I~
        Else                                                           ''~7618I~
            str = resMgr.GetString(Pstrid)                             ''~7618I~
        End If                                                         ''~7618I~
        Return str                                                     ''~7618I~
    End Function                                                       ''~7618I~
    Public Shared Sub setupStrings()                                   ''~7617I~
        If swInitialized Then                                                ''+v110I~
            If oldLangEN = FormOptions.swLangEN Then                    ''+v110I~
                Exit Sub                ' avoid duplication at Form1; New Rstr() and setLocaleConst()''+v110I~
            End If                                                     ''+v110I~
        End If                                                         ''+v110I~
        swInitialized = True                                             ''+v110I~
        oldLangEN = FormOptions.swLangEN                                 ''+v110I~
        '******* Form1                                                 ''~7617R~
        MENU_NEWTEXT = getStr("STR_NEWTEXT")                           ''~7617R~
        MENU_NEWTEXT_FILE = getStr("STR_NEWTEXT_FILE")                 ''~7617R~
        MENU_NEWFILE = getStr("STR_NEWFILE")                           ''~7617R~
        FILTER_IMAGE = getStr("STR_FILTER_IMAGE")                      ''~7617R~
        FILTER_KANJITEXT = getStr("STR_FILTER_KANJITEXT")              ''~7617R~
        FILTER_KANATEXT = getStr("STR_FILTER_KANATEXT")                ''~7617R~
        MSG_DISCARD_UPDATE = getStr("STR_MSG_DISCARD_UPDATE")          ''~7617R~
        FORM1_TITLE_KANA_FILE = getStr("STR_FORM1_TITLE_KANA_FILE")    ''~7617R~
        FORM1_TITLE_KANJI2KANA_FILE = getStr("STR_FORM1_TITLE_KANJI2KANA_FILE") ''~7617R~
        MSG_ERR_NOT_FOUND = getStr("STR_MSG_ERR_NOT_FOUND")            ''~7617R~
        MSG_ERR_READ = getStr("STR_MSG_ERR_READ")                      ''~7617R~
        MSG_ERR_WRITE = getStr("STR_MSG_ERR_WRITE")                    ''~7617R~
        MSG_ERR_PRINT_PAGE = getStr("STR_MSG_ERR_PRINT_PAGE")          ''~7617R~
        MSG_INFO_SHOW_TEXT = getStr("STR_MSG_INFO_SHOW_TEXT")          ''~7617R~
        FORM1_INITIAL_TEXT = getStr("STR_FORM1_INITIAL_TEXT")           ''~7617R~
        MSG_NO_TEXT = getStr("STR_MSG_NO_TEXT")                        ''~7617R~
        MSG_REP_EXISTING = getStr("STR_MSG_REP_EXISTING")              ''~7617R~
        '****** Form2                                                  ''~7617R~
        FORM2_TITLE = getStr("STR_FORM2_TITLE")                        ''~7619I~
        MSG_ERR_ZOOM = getStr("STR_MSG_ERR_ZOOM")                   ''~7617I~
        MSG_ERR_ALREADY_EXTRACTED = getStr("STR_MSG_ERR_ALREADY_EXTRACTED")''~7618R~
        '****** Form3                                                  ''~7617I~
        MSG_ERR_EXTRACT = getStr("STR_MSG_ERR_EXTRACT")                ''~7617M~
        FORM3_TITLE = getStr("STR_FORM3_TITLE")                          ''~7617I~
        FORM3_TITLE_RECEIVED = getStr("STR_FORM3_TITLE_RECEIVED")        ''~7617I~
        MSG_INFO_SAVED = getStr("STR_MSG_INFO_SAVED")                    ''~7617I~
        MSG_ERR_KANA_CONV = getStr("STR_MSG_ERR_KANA_CONV")              ''~7617I~
        MSG_INFO_EXTRACTED = getStr("STR_MSG_INFO_EXTRACTED")            ''~7617I~
        '****** Form5                                                  ''~7618I~
        FONT_STD = getStr("STR_FONT_STD")                                ''~7618I~
        FONT_BOLD = getStr("STR_FONT_BOLD")                              ''~7618I~
        FONT_ITALIC = getStr("STR_FONT_ITALIC")                          ''~7618I~
        FONT_BOLD_ITALIC = getStr("STR_FONT_BOLD_ITALIC")                ''~7618I~
        MSG_ERR_FUNCKEY = getStr("STR_MSG_ERR_FUNCKEY")                  ''~7618I~
        MSG_ERR_PRINT_FONT = getStr("STR_MSG_ERR_PRINT_FONT")            ''~7618I~
        MSG_ERR_SCR_FONT = getStr("STR_MSG_ERR_SCR_FONT")                ''~7618I~
        '****** Form6                                                  ''~7618I~
        MSG_ERR_SELECT_LINE = getStr("STR_MSG_ERR_SELECT_LINE")          ''~7618I~
        MSG_ERR_ADD_SELECT_LINE = getStr("STR_MSG_ERR_ADD_SELECT_LINE")  ''~7618I~
        MSG_CONFIRM_RESTORE_DEFAULT = getStr("STR_MSG_CONFIRM_RESTORE_DEFAULT") ''~7618I~
        '****** Form7                                                  ''~7618I~
        MSG_ERR_SPECIAL_KEY = getStr("STR_MSG_ERR_SPECIAL_KEY")          ''~7618I~
        MSG_ERR_SPECIAL_KEY_NULL = getStr("STR_MSG_ERR_SPECIAL_KEY_NULL") ''~7618I~
        '****** Form8                                                  ''~7618I~
        MSG_ERR_FIND_NULL = getStr("STR_MSG_ERR_FIND_NULL")              ''~7618I~
        MSG_INFO_FIND_UP = getStr("STR_MSG_INFO_FIND_UP")                ''~7618I~
        MSG_INFO_FIND_DOWN = getSTr("STR_MSG_INFO_FIND_DOWN")            ''~7618I~
        MSG_INFO_FIND_CASE_SENSITIVE = getStr("STR_MSG_INFO_FIND_CASE_SENSITIVE")''~7618R~
        MSG_INFO_FIND_CASE_INSENSE = getStr("STR_MSG_INFO_FIND_CASE_INSENSE")''~7618R~
        MSG_ERR_FIND_NOT_FOUND = getStr("STR_MSG_ERR_FIND_NOT_FOUND")  ''~7618R~
        MSG_ERR_NOT_FOUND_CPOS = getStr("STR_MSG_ERR_NOT_FOUND_CPOS")    ''~7618I~
        MSG_ERR_NOT_FOUND_LINE_COL = getStr("STR_MSG_ERR_NOT_FOUND_LINE_COL") ''~7618I~
        MSG_ERR_NOT_FOUND_NO_CPOS = getStr("STR_MSG_ERR_NOT_FOUND_NO_CPOS") ''~7618I~
        MSG_ERR_REPSTR_SAME = getStr("STR_MSG_ERR_REPSTR_SAME")          ''~7618I~
        MSG_ERR_NOT_FOUND_ALL = getStr("STR_MSG_ERR_NOT_FOUND_ALL")      ''~7618I~
        '****** class1                                                 ''~7618I~
  		MSG_ERR_SMALL_LARGE=getStr("STR_MSG_ERR_SMALL_LARGE")          ''~7618I~
  		MSG_ERR_SMALL_LARGE_OTHER=getStr("STR_MSG_ERR_SMALL_LARGE_OTHER")''~7618I~
        '****** class5                                                 ''~7618I~
  		MSG_ERR_FAILED_KANJI2KANA_CONV=getStr("STR_MSG_ERR_FAILED_KANJI2KANA_CONV")''~7618I~
    End Sub                                                            ''~7617I~
End Class
