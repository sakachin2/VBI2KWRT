'CID:''+v101R~:#72                          update#= 14             ''~v101R~
'************************************************************************************''~v079I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v079 2017/10/10 class DataGridView                                    ''~v079I~
'************************************************************************************''~v079I~
Imports System.Windows.Forms                                           ''~v079I~
Public Class KDGV                                                      ''~v079I~
    Private DGV As DataGridView                                        ''~v079I~
    '**************************************************************    ''~v079I~
    Sub New(Pdgv As DataGridView)                                      ''~v079I~
        DGV = Pdgv                                                     ''~v079I~
    End Sub                                                            ''~v079I~
    Public Sub clearDGV()                                              ''~v079I~
        '       DGV.RowCount = 0         '>=1 if AllowUserToAddRows is true(default)''~v079I~
        DGV.Rows.Clear()                                               ''~v079I~
    End Sub                                                            ''~v079I~
'   Public Function getRowCount()                                      ''~v079I~''~v101R~
    Public Function getRowCount() as Integer                           ''~v101I~
        Return DGV.RowCount                                            ''~v079I~
    End Function                                                       ''~v079I~
'   Public Function getColumnCount()                                   ''~v079I~''~v101R~
    Public Function getColumnCount() as Integer                        ''~v101I~
        Return DGV.ColumnCount                                         ''~v079I~
    End Function                                                       ''~v079I~
    Public Function getSelectedPos() As Integer                        ''~v079I~
        Return DGV.CurrentCell.RowIndex                                ''~v079I~
    End Function                                                       ''~v079I~
'   Public Function setSelectedPos(Prow As Integer, Pcell As Integer)  ''~v079I~''~v101R~
    Public Function setSelectedPos(Prow As Integer, Pcell As Integer) as Boolean''~v101I~
        Dim maxrow = getRowCount()                                 ''~v079I~
        If Prow >= 0 AndAlso Prow < maxrow Then                        ''~v079I~
            DGV.ClearSelection()                                       ''~v079I~
            DGV.CurrentCell = DGV.Rows(Prow).Cells(Pcell)              ''~v079I~
            Return True                                                ''~v079I~
        End If                                                         ''~v079I~
        Return False                                                   ''~v079I~
    End Function                                                       ''~v079I~
    Public Function getValidCPos(Pallownewpos As Boolean) As Integer   ''~v079I~
        Dim cpos, maxctr As Integer                                    ''~v079I~
        cpos = getSelectedPos()                                        ''~v079I~
        maxctr = getRowCount()                                    ''~v079I~
        If cpos < 0 OrElse cpos >= maxctr Then                         ''~v079I~
            Return -1                                                  ''~v079I~
        End If                                                         ''~v079I~
        If Not Pallownewpos Then                                       ''~v079I~
            If cpos = DGV.NewRowIndex Then                             ''~v079I~
                Return -1                                              ''~v079I~
            End If                                                     ''~v079I~
        End If                                                         ''~v079I~
        Return cpos                                                    ''~v079I~
    End Function                                                       ''~v079I~
    Public Function getValidCPos(Pallownewpos As Boolean, Prow As Integer) As Integer ''~v079I~
        Dim cpos, maxctr As Integer                                    ''~v079I~
        cpos = Prow                                                    ''~v079I~
        maxctr = getRowCount()                                     ''~v079I~
        If cpos < 0 OrElse cpos >= maxctr Then                         ''~v079I~
            Return -1                                                  ''~v079I~
        End If                                                         ''~v079I~
        If Not Pallownewpos Then                                       ''~v079I~
            If cpos = DGV.NewRowIndex Then                             ''~v079I~
                Return -1                                              ''~v079I~
            End If                                                     ''~v079I~
        End If                                                         ''~v079I~
        Return cpos                                                    ''~v079I~
    End Function                                                       ''~v079I~
    Public Function getRowDataString(Prow As Integer, Pcell As Integer, ByRef Pperr As Boolean) As String ''~v079I~
        Dim str As String = Nothing                                      ''~v079I~
        Dim row As Integer = getValidCPos(False, Prow)                 ''~v079I~
        If row < 0 Then                                                ''~v079I~
            Pperr = True                                                 ''~v079I~
            Return str                                                 ''~v079I~
        End If                                                         ''~v079I~
        Dim maxcell = DGV.Rows(row).Cells.Count                       ''~v079I~
        If Pcell < 0 OrElse Pcell >= maxcell Then                               ''~v079I~
            Pperr = True                                                 ''~v079I~
            Return str                                                 ''~v079I~
        End If                                                         ''~v079I~
'       str = DGV.Rows(row).Cells(Pcell).Value                         ''~v079I~''+v101R~
        str = Ctype(DGV.Rows(row).Cells(Pcell).Value,String)           ''+v101I~
        Pperr = False                                                    ''~v079I~
        Return str                                                     ''~v079I~
    End Function                                                       ''~v079I~
    Public Function getRowDataString(Prow As Integer, Pcell As Integer) As String ''~v079I~
        Dim err As Boolean                                             ''~v079I~
        Return getRowDataString(Prow, Pcell, err)                       ''~v079I~
    End Function                                                       ''~v079I~
    Public Function getCurrentRowData(ByRef Pppos as Integer) As ArrayList''~v079R~
    	Pppos=-1                                                       ''~v079I~
        Dim pos As Integer = getValidCPos(False)                       ''~v079R~
        If pos < 0 Then                                                ''~v079R~
            Return Nothing                                             ''~v079R~
        End If                                                         ''~v079I~
        Dim lst as New ArrayList()                                     ''~v079I~
    	lst=getRowData(pos)                                            ''~v079I~
    	Pppos=pos                                                      ''~v079I~
        Return lst                                                     ''~v079R~
    End Function                                                       ''~v079I~
    Public Function getTheRowData(Ppos as Integer) As ArrayList        ''~v079I~
        Dim pos As Integer = getValidCPos(False,Ppos)                  ''~v079R~
        If pos < 0 Then                                                ''~v079I~
            Return Nothing                                             ''~v079I~
        End If                                                         ''~v079I~
        Dim lst as New ArrayList()                                     ''~v079I~
    	lst=getRowData(pos)                                            ''~v079I~
        Return lst                                                     ''~v079I~
    End Function                                                       ''~v079I~
    Private Function getRowData(Ppos as Integer) As ArrayList          ''~v079I~
        Dim rows As DataGridViewRowCollection = DGV.Rows               ''~v079I~
        Dim cells As DataGridViewCellCollection                        ''~v079I~
        cells = rows(Ppos).Cells                                       ''~v079I~
        Dim maxcell as Integer= getColumnCount()                       ''~v079I~
'       Dim cols As DataGridViewColumnCollection = DGV.Columns         ''~v079I~
        Dim lst as New ArrayList()                                     ''~v079I~
        For ii As Integer = 0 To maxcell - 1                           ''~v079I~
'           Dim col As dataGridViewColumn = cols(ii)                   ''~v079I~
'           Dim typ As Type = col.CellType                             ''~v079I~
'           If col.GetType() Is GetType( DataGridViewCheckBoxColumn) Then''~v079I~
'           End If                                                     ''~v079I~
            lst.Add(cells(ii).Value)                                   ''~v079I~
        Next                                                           ''~v079I~
        Return lst                                                     ''~v079I~
    End Function                                                       ''~v079I~
    Public Function newRow(Plist As ArrayList) as DataGridViewRow      ''~v079I~
    	Dim row as DataGridViewRow=New DataGridViewRow()               ''~v079I~
        Dim cellctr as Integer= getColumnCount()                       ''~v079I~
        Dim listctr as Integer=Plist.Count                             ''~v079I~
        row.CreateCells(DGV)                                           ''~v079I~
        For ii As Integer = 0 To cellctr - 1                           ''~v079I~
        	if ii>=listctr                                             ''~v079I~
            	row.Cells(ii).Value=Nothing                            ''~v079I~
            else                                                       ''~v079I~
            	row.Cells(ii).Value=Plist(ii)                          ''~v079I~
            end if                                                     ''~v079I~
        Next                                                           ''~v079I~
        return row                                                     ''~v079I~
    End Function                                                       ''~v079I~
    Public Function insertRow(Ppos as Integer, Plist As ArrayList) as Boolean''~v079R~
        Dim pos As Integer = getValidCPos(False, Ppos)                 ''~v079I~
        If pos < 0 Then                                                ''~v079I~
            Return false                                               ''~v079I~
        End If                                                         ''~v079I~
    	Dim row as DataGridViewRow=newRow(Plist)                       ''~v079R~
        DGV.Rows.Insert(pos, row)                                      ''~v079R~
        Return true                                                    ''~v079I~
    End Function                                                       ''~v079R~
    Public Sub addRow(Plist As ArrayList)                              ''~v079I~
    	Dim row as DataGridViewRow=newRow(Plist)                       ''~v079I~
        DGV.Rows.Add(row)                                              ''~v079I~
    End Sub                                                            ''~v079I~
    Public Function removeRow(Ppos as Integer) as Boolean              ''~v079R~
        Dim pos As Integer = getValidCPos(False, Ppos)                 ''~v079I~
        If pos < 0 Then                                                ''~v079I~
            Return false                                               ''~v079I~
        End If                                                         ''~v079I~
    	DGV.Rows.RemoveAt(pos)                                         ''~v079R~
        Return true                                                    ''~v079I~
    End Function                                                       ''~v079R~
    Public sub updateRow(Ppos as Integer,Plist as ArrayList)           ''~v079I~
        Dim rows As DataGridViewRowCollection = DGV.Rows               ''~v079I~
        Dim cells As DataGridViewCellCollection= rows(Ppos).Cells      ''~v079I~
        Dim cellctr as Integer= getColumnCount()                       ''~v079I~
        Dim listctr as Integer= Plist.Count                            ''~v079I~
        For ii As Integer = 0 To cellctr - 1                           ''~v079I~
        	if ii<listctr                                              ''~v079I~
            	cells(ii).Value=Plist(ii)                              ''~v079I~
            end if                                                     ''~v079I~
        Next                                                           ''~v079I~
    End Sub                                                            ''~v079I~
End Class
