'CID:''+v164R~:#72                          update#=     24;          ''~v164R~
'************************************************************************************''~v078I~
'v164 2018/03/04 refresh required to toolstrip in any case?            ''~v164I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v078 2017/10/09 dialog status bar                                     ''~v078I~
'************************************************************************************''~v078I~
'*Show Msg on StatusBar ********************                           ''+v164I~
Imports System.Windows.Forms                                           ''~v078I~
Public Class SBM                                                       ''~v078R~
    Public Enum MSGID                                                  ''~v078I~
        CLEAR                                                          ''~v078I~
        LOAD                                                           ''~v078I~
        SAVE                                                           ''~v078M~
        SAVEAS                                                         ''~v078M~
        CUT                                                            ''~v078I~
        COPY                                                           ''~v078I~
        CUT_PASTE                                                      ''~v078I~
        COPY_PASTE                                                     ''~v078I~
        SEND                                                           ''~v078R~
    End Enum                                                           ''~v078I~
    Private SBL As ToolStripStatusLabel                             ''~v078R~
    Private SB As StatusStrip = Nothing                                 ''~v164I~
    Private msgPending As String = Nothing                               ''~v078I~
    '**************************************************************        ''~v078I~
    Sub New(Psb As ToolStripStatusLabel)                               ''~v078R~
        SBL = Psb                                                      ''~v078R~
    End Sub                                                            ''~v078I~
    Sub New(Psb As ToolStripStatusLabel, Pparent As StatusStrip)        ''~v164I~
        SBL = Psb                                                      ''~v164I~
        SB = Pparent                                                  ''~v164I~
    End Sub                                                            ''~v164I~
    '   Public Sub show(Pmsg)                                              ''~v078R~''~v101R~
    Public Sub show(Pmsg As String)                                    ''~v101I~
        SBL.Text = Pmsg                                                ''~v078R~
'*      Trace.W("Class8:show=" & Pmsg)                                 ''~v101R~''~v164R~
        refresh()                                                      ''~v164M~
    End Sub                                                            ''~v078I~
    Public Sub show(Pmsg As String, Pswdelay As Boolean)                          ''~v078I~
        msgPending = Pmsg                                                ''~v078I~
    End Sub                                                            ''~v078I~
    Public Sub show(Pmsgid As MSGID, Pdata As String)                      ''~v078I~
        Dim msg, prefix As String                                       ''~v078I~
        Select Case Pmsgid                                                  ''~v078I~
            Case MSGID.CLEAR                                           ''~v078I~
                If msgPending IsNot Nothing Then                            ''~v078I~
                    show(msgPending)                                   ''~v078I~
                    msgPending = Nothing                                 ''~v078I~
                Else                                                   ''~v078I~
                    show("")                                           ''~v078R~
                End If                                                 ''~v078I~
                Exit Sub                                               ''~v078I~
            Case MSGID.LOAD                                            ''~v078I~
                prefix = Rstr.getStr("STR_LOAD")                         ''~v078I~
            Case MSGID.SAVE                                            ''~v078I~
                prefix = Rstr.getStr("STR_SAVE")                         ''~v078I~
            Case MSGID.SAVEAS                                          ''~v078I~
                prefix = Rstr.getStr("STR_SAVEAS")                       ''~v078I~
            Case MSGID.CUT                                             ''~v078R~
                prefix = Rstr.getStr("STR_CUT")                          ''~v078I~
            Case MSGID.COPY                                            ''~v078R~
                prefix = Rstr.getStr("STR_COPY")                         ''~v078I~
            Case MSGID.CUT_PASTE                                       ''~v078R~
                prefix = Rstr.getStr("STR_CUTPASTE")                   ''~v078R~
            Case MSGID.COPY_PASTE                                      ''~v078I~
                prefix = Rstr.getStr("STR_COPYPASTE")                  ''~v078I~
            Case MSGID.SEND                                            ''~v078R~
                prefix = Rstr.getStr("STR_SEND")                       ''~v078I~
            Case Else                                                 ''~v078I~
                Exit Sub                                               ''~v078I~
        End Select                                                     ''~v078I~
        msg = prefix & " : " & Pdata                                     ''~v078I~
        show(msg)                                                      ''~v078I~
    End Sub                                                            ''~v078I~
    Public Sub refresh()                                               ''~v164I~
        If SB IsNot Nothing Then                                             ''~v164R~
            SB.Refresh()                                               ''~v164R~
'*          Trace.W("class8:refresh()")                                ''~v164R~
        End If                                                         ''~v164I~
    End Sub                                                            ''~v164I~
End Class
