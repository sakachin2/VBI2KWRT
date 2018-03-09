'CID:''+v165R~:#72                             update#=  173;         ''~v165I~
'************************************************************************************''~v165I~
'v165 2018/03/04 show caret even if focus lost by selectionStart/Length''~v165I~
'************************************************************************************''~v165I~
Public Class TBFocus                                                   ''~v165R~
    '**************************************************************    ''~v165I~
    Private swLeave As Boolean = False                                 ''~v165I~
    Private selStart As Integer = -1                                   ''~v165I~
    Private TB As TextBox                                              ''~v165I~
    '**************************************************************    ''~v165I~
    Sub New(Ptb As TextBox)                                            ''~v165I~
        TB = Ptb                                                         ''~v165I~
        TB.HideSelection = False 'do not hide selection when focus lost5R~''~v165I~
        AddHandler TB.LostFocus, AddressOf lostFocusTB                 ''~v165I~
        AddHandler TB.GotFocus, AddressOf gotFocusTB                   ''~v165I~
    End Sub                                                            ''~v165I~
    Private Sub lostFocusTB()                                          ''~v165I~
        Trace.W("Form1 TB LostFocus")                                  ''~v165I~
        saveCaret()                                                    ''~v165I~
    End Sub                                                            ''~v165I~
    Private Sub gotFocusTB()                                           ''~v165I~
        Trace.W("Form1 TB GotFocus")                                   ''~v165I~
        restoreCaret()                                                 ''~v165I~
    End Sub                                                            ''~v165I~
    '****************************                                      ''~v165I~
    Private Sub saveCaret()                                            ''~v165I~
        Trace.W("Form1 saveCaret")                                     ''~v165I~
        If TB.SelectionLength = 0 AndAlso TB.SelectionStart >= 0 Then    ''~v165I~
            selStart = TB.SelectionStart                               ''~v165I~
            swLeave = True                                             ''~v165I~
            TB.SelectionLength = 1                                     ''~v165I~
        Else                                                           ''~v165I~
            swLeave = False                                            ''~v165I~
        End If                                                         ''~v165I~
    End Sub                                                            ''~v165I~
    Private Sub restoreCaret()                                         ''~v165I~
        Trace.W("Form1 TB Enter")                                      ''~v165I~
        If Not swLeave Then                                            ''~v165I~
            Exit Sub                                                   ''~v165I~
        End If                                                         ''~v165I~
        TB.SelectionStart = selStart                                   ''~v165I~
        TB.SelectionLength = 0                                         ''~v165I~
        swLeave = False                                                ''+v165I~
    End Sub                                                            ''~v165I~
    '**************************************************************    ''+v165I~
    '*restore selection length=0 when foced at lostfocau               ''+v165I~
    '*to avoid replace it by partial extraction                        ''+v165I~
    Public Sub restoreSelection()                                     ''+v165I~
        restoreCaret() 'restore selectionLength=0                      ''+v165I~
    End Sub                                                            ''+v165I~
End Class
