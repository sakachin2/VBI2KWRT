''*CID:''+v210R~:#72                          update#=   23;         ''~v210R~
'************************************************************************************''~7915I~
'v210 2021/08/06 support imagefile multi selection                     ''~v210I~
'v163 2018/03/03 add string customizability for kata/hira chikan       ''~v163I~
'v076 2017/10/08 Symbol Dialog by DataGridView                         ''~v076I~
'v065 2017/09/24 Word dialog by Ctrl+char(except "1"-"0")              ''~v065I~
'v012 2017/09/15 Load/Save/SaveAs from/to disctionary file             ''~7915I~
'************************************************************************************''~7915I~
Public Class ClassMRU                                                  ''~7522R~
    'localization not required                                             ''~7618R~
'*  Public Const MRULISTSZ = 10                                        ''~7522R~''+v210R~
    Public Const MRULISTSZ = 20                                        ''~v210I~
    Public Const ID_DICTIONARY = 4                                     ''~v012R~
    Public Const ID_WORDS=5                                            ''~v065I~
    Public Const ID_SYMBOL=6                                           ''~v076I~
    Public Const ID_TRANSCH=7                                          ''~v163I~
    Private MRUList As New List(Of String)                         ''~7421R~''~7522M~
    Private MRUListImage As New List(Of String)                            ''~7411I~''~7421R~''~7522M~
    Private MRUListText As New List(Of String)                             ''~7411I~''~7421R~''~7522M~
    Private MRUListKanaText As New List(Of String)                         ''~7411I~''~7421R~''~7522M~
    Private MRUListBESText As New List(Of String)                          ''~7412I~''~7421R~''~7522M~
    Private MRUListDictionary As New List(Of String)                   ''~v012R~
    Private MRUListWords As New List(Of String)                        ''~v065I~
    Private MRUListSymbol As New List(Of String)                       ''~v076I~
    Private MRUListTransChars As New List(Of String)                   ''~v163I~
    Public Sub New()                                                   ''~7522I~
        '        loadMRUList()                                                  ''~7522M~
    End Sub                                                            ''~7522I~
    Public Function selectMRUList(Pcase As Integer) As List(Of String) ''~7522I~
        Select Case Pcase                                              ''~7411I~''~7522M~
            Case 1 'Image                                              ''~7411I~''~7522M~
                MRUList = MRUListImage                                   ''~7411I~''~7522M~
            Case 2 'KanjiText                                          ''~7411I~''~7522M~
                MRUList = MRUListText                                    ''~7411I~''~7522M~
            Case ID_DICTIONARY                                         ''~v012R~
                MRUList = MRUListDictionary                            ''~v012R~
            Case ID_WORDS                                              ''~v065I~
                MRUList = MRUListWords                                 ''~v065I~
            Case ID_SYMBOL                                             ''~v076I~
                MRUList = MRUListSymbol                                ''~v076I~
            Case ID_TRANSCH                                            ''~v163I~
                MRUList = MRUListTransChars                            ''~v163I~
            Case Else '3 'KanaText                                           ''~7411I~''~7522I~
                MRUList = MRUListKanaText                                ''~7411I~''~7522M~
        End Select                                                     ''~7411I~''~7522M~
        Return MRUList                                                 ''~7522I~
    End Function                                                            ''~7522M~
    Private Function deleteMRUList(Pfnm As String) As Boolean          ''~7522M~
        Try                                                            ''~7522M~
            If MRUList.Count = 0 Then                                  ''~7522M~
                Return False                                           ''~7522M~
            End If                                                     ''~7522M~
            If Pfnm.CompareTo(MRUList(0)) = 0 Then                     ''~7522M~
                Return True 'top of list no need to delete             ''~7522M~
            End If                                                     ''~7522M~
            MRUList.Remove(Pfnm)                                       ''~7522M~
        Catch ex As Exception                                          ''~7522M~
            MessageBox.Show(String.Format("deleteMRUList Exception Pfnm={0},MRUList.count={1}", Pfnm, MRUList.Count)) ''~7522M~
            Return False                                               ''~7522M~
        End Try                                                        ''~7522M~
        Return False                                                   ''~7522M~
    End Function                                                       ''~7522M~
    Private Sub saveMRUListCfg(Pcase As Integer, Pstr As String)        ''~7411I~''~7522M~
        Select Case Pcase                                              ''~7411I~''~7522M~
            Case 1 'Image                                              ''~7411I~''~7522M~
                My.Settings.CfgMRUImage = Pstr                           ''~7411I~''~7522M~
            Case 2 'KanjiText                                          ''~7411I~''~7522M~
                My.Settings.CfgMRUtext = Pstr                            ''~7411I~''~7522M~
            Case ID_DICTIONARY                                         ''~v012R~
                My.Settings.CfgMRUDictionary = Pstr                    ''~v012R~
            Case ID_WORDS                                              ''~v065I~
                My.Settings.CfgMRUWords = Pstr                         ''~v065I~
            Case ID_SYMBOL                                             ''~v076I~
                My.Settings.CfgMRUSymbol = Pstr                        ''~v076I~
            Case ID_TRANSCH                                            ''~v163I~
                My.Settings.CfgMRUTransChars = Pstr                    ''~v163I~
            Case 3 'KanaText                                           ''~7411I~''~7522M~
                My.Settings.CfgMRUKanaText = Pstr                        ''~7411I~''~7522M~
        End Select                                                     ''~7411I~''~7522M~
    End Sub                                                            ''~7411I~''~7522M~
    Public Sub insertMRUList(Pcase As Integer, Pfnm As String)        ''~7411I~''~7429R~''~7522M~
        selectMRUList(Pcase)                                           ''~7411I~''~7522M~
        If deleteMRUList(Pfnm) Then  'already on top of list           ''~7411I~''~7522M~
            Exit Sub                                                   ''~7411I~''~7522M~
        End If                                                         ''~7411I~''~7522M~
        MRUList.Insert(0, Pfnm)                                        ''~7411I~''~7522M~
        '       If Debug Then                                                  ''~7411I~''~7522I~
        '           Console.WriteLine("add MRU={0}={1}", MRUList.Count, Pfnm)  ''~7411I~''~7522I~
        '       End If                                                         ''~7411I~''~7522I~
        '       setMRUListMenu(Pcase)                                          ''~7411I~''~7522I~
        '       Form1.MainForm.setMRUListMenu(Pcase)                           ''~7522I~
        '       saveMRUList(Pcase)                                             ''~7411I~''~7522I~
    End Sub                                                            ''~7411I~''~7522M~
    Public Sub loadMRUListSub(Pcase As Integer)                       ''~7522I~
        Dim str As String                                              ''~7522I~
        selectMRUList(Pcase)                                           ''~7522I~
        If Form1.TestOption = 1 Then                                          ''~7522I~
            My.Settings.CfgMRUImage = ""                                 ''~7522I~
            My.Settings.CfgMRUtext = ""                                  ''~7522I~
            My.Settings.CfgMRUKanaText = ""                              ''~7522I~
            My.Settings.CfgMRUDictionary = ""                          ''~v012R~
            My.Settings.CfgMRUWords = ""                               ''~v065I~
            My.Settings.CfgMRUSymbol = ""                              ''~v076I~
        End If                                                         ''~7522I~
        Select Case Pcase                                              ''~7522I~
            Case 1 'Image                                              ''~7522I~
                str = My.Settings.CfgMRUImage                            ''~7522I~
            Case 2 'KanjiText                                          ''~7522I~
                str = My.Settings.CfgMRUtext                             ''~7522I~
            Case ID_DICTIONARY                                         ''~v012R~
                str = My.Settings.CfgMRUDictionary                     ''~v012R~
            Case ID_WORDS                                              ''~v065I~
                str = My.Settings.CfgMRUWords                          ''~v065I~
            Case ID_SYMBOL                                             ''~v076I~
                str = My.Settings.CfgMRUSymbol                         ''~v076I~
            Case ID_TRANSCH                                            ''~v163I~
                str = My.Settings.CfgMRUTransChars                     ''~v163I~
            Case Else '3 'KanaText                                     ''~7522I~
                str = My.Settings.CfgMRUKanaText                         ''~7522I~
        End Select                                                     ''~7522I~
        Dim fnmlist() As String = str.Split("|"c)                      ''~7522I~
        Dim ctrlist = fnmlist.Length()                                 ''~7522I~
        For ii As Integer = 0 To ctrlist - 1                           ''~7522I~
            Dim fnm As String = fnmlist(ii)                            ''~7522I~
            If (fnm.Length() > 0) Then                                 ''~7522I~
                MRUList.Add(fnm)                                       ''~7522I~
            End If                                                     ''~7522I~
        Next                                                           ''~7522I~
    End Sub                                                            ''~7522I~
    Public Sub saveMRUList(Pcase As Integer)                          ''~7411R~''~7522M~
        selectMRUList(Pcase)                                           ''~7411I~''~7522M~
        Dim sb = New System.Text.StringBuilder()                       ''~7522M~
        Dim ctr As Integer = 0                                         ''~7522M~
        Dim ctrlist As Integer = MRUList.Count                         ''~7522M~
        For ii As Integer = 0 To ctrlist - 1                           ''~7522M~
            Dim fnm As String = MRUList.Item(ii)                       ''~7522M~
            sb.Append(fnm & "|")                                       ''~7522M~
            ctr += 1                                                   ''~7522M~
            If ctr > MRULISTSZ Then                                    ''~7522R~
                Exit For                                               ''~7522M~
            End If                                                     ''~7522M~
        Next                                                           ''~7522M~
        Dim str = sb.ToString()                                        ''~7411R~''~7522M~
        saveMRUListCfg(Pcase, str)                                      ''~7411I~''~7522M~
    End Sub                                                            ''~7522M~
    Public Function clearMRUList(Pcase As Integer) As Integer          ''~v012R~
    	Dim tmp As List(Of String)                                     ''~v012R~
        Select Case Pcase                                              ''~v012R~
            Case 1 'Image                                              ''~v012R~
                tmp = MRUListImage                                     ''~v012R~
            Case 2 'KanjiText                                          ''~v012R~
                tmp = MRUListText                                      ''~v012R~
            Case ID_DICTIONARY                                         ''~v012R~
                tmp = MRUListDictionary                                ''~v012R~
            Case ID_WORDS                                              ''~v065I~
                tmp = MRUListWords                                     ''~v065I~
            Case ID_SYMBOL                                             ''~v076I~
                tmp = MRUListSymbol                                    ''~v076I~
            Case ID_TRANSCH                                           ''~v163I~
                tmp = MRUListTransChars                                ''~v163I~
            Case Else '3 'KanaText                                     ''~v012R~
                tmp = MRUListKanaText                                  ''~v012R~
        End Select                                                     ''~v012R~
        Dim rc=tmp.Count                                               ''~v012R~
        tmp.clear()                                                    ''~v012R~
        Return rc                                                      ''~v012R~
    End Function                                                       ''~v012R~
End Class
