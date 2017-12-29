''*CID:''+dateR~:#72                          update#=    4;          ''~7517I~
Public Class StackList                                                 ''~7517R~
'localization not required                                             ''+7618I~
    Private maxCtr As Integer                                          ''~7517R~
    Private lifoList As List(Of String)                                ''~7517I~
    Sub New(Ptype As String, Pmaxctr As Integer)                       ''~7517I~
        maxCtr = Pmaxctr                                                 ''~7517I~
        lifoList = New List(Of String)(maxCtr)                              ''~7517I~
    End Sub                                                            ''~7517I~
    Sub Load(Pstr As String, Psplitter As Char)                         ''~7517I~
        Dim parse() As String = Pstr.Split(Psplitter)                 ''~7517I~
        Dim listctr As Integer = parse.length                            ''~7517I~
        lifoList.Clear()                                               ''~7517I~
        For ii As Integer = 0 To listctr - 1                           ''~7517I~
            lifoList.Add(parse(ii))                                    ''~7517I~
        Next                                                           ''~7517I~
    End Sub                                                            ''~7517I~
    Function toArray() As String()                                     ''~7517I~
        Return lifoList.toArray()                                      ''~7517I~
    End Function                                                       ''~7517I~
    Function list2String(Psplitter As Char) As String                  ''~7517I~
        Dim strarray = lifoList.ToArray()                                ''~7517I~
        Dim str = String.Join(Psplitter, strarray)                        ''~7517I~
        Return str                                                     ''~7517I~
    End Function                                                       ''~7517I~
    Sub insert(Pmemb As String)                                        ''~7517I~
        Dim pos = lifoList.IndexOf(Pmemb)                                ''~7517I~
        If pos = 0 Then  'already on top of list                         ''~7517I~
            Exit Sub                                                   ''~7517I~
        End If                                                         ''~7517I~
        If pos >= 0 Then   'found                                              ''~7517I~
            lifoList.RemoveAt(pos)                                     ''~7517I~                                  ''~7517I~
        End If                                                         ''~7517I~
        If lifoList.Count >= maxCtr Then                                      ''~7517I~
            lifoList.RemoveAt(maxCtr - 1)                                ''~7517I~
        End If                                                         ''~7517I~
        lifoList.Insert(0, Pmemb)                                       ''~7517I~
    End Sub                                                            ''~7517I~
End Class
