'CID:''+v210R~:#72                             update#=  687;        ''+v210R~
'************************************************************************************''~v026I~''~v100I~
'v210 2021/08/06 support imagefile multi selection                     ''+v210I~
'v201 2021/06/04 drop v200. MS-API works for english 2 page it self. Bug reason may be charH/charW setting for multi char on a OcrWord(occurs for english text)''~v201I~
'                drop droplist down list but leave horizontal2 logic   ''~v201I~
'v200 2021/06/04 support layout hrizontal 2 page for English           ''~v200I~
'v199 2021/06/03 support layout hrizontal 2 page                       ''~v199I~
'v198 2021/06/02 split header/footer as setting option                 ''~v198I~
'v197 2021/05/28 split footer                                          ''~v197I~
'v196 2021/05/27 split header on v192(extract by split image)          ''~v196I~
'v192 2021/05/21 support layoyt vertical 2 page                        ''~v192I~
'v178 2018/09/20 insert space after if X or Y is too far from next     ''~v178I~
'v177 2018/09/19 (Bug)inter word space was dropped when English mode   ''~v177I~
'v176 2018/09/16 vertical line sometimes updown reversed by differece of top char boundary.X, chk same line by center line X''~v176I~
'v155 2018/01/10 markword should use not sorted lines but result.OcrLines''~v155I~
'v154 2018/01/10 have to chk also horizontal                           ''~v154R~
'v153 2018/01/08 point size change consider small char, drop from average''~v153I~
'v148 2018/01/07 mark line requirs ajustment by textangle?             ''~v148I~
'v147 2018/01/06 OcrWord.Boundingrect is readonly                      ''~v147I~
'v146 2018/01/06 point change chk by not max H/W but by average        ''~v146I~
'v145 2018/01/06 determin horizontal or vertical for 1 char line by distance to prev/next char''~v145I~
'v144 2018/01/06 chk short line but sameline,do not inser crlf but insert space''~v144I~
'v143 2018/01/05 (Bug) word may contains multiple char for english     ''~v143I~
'v141 2018/01/04 add selection of japanese writing direction for sortline''~v141I~
'v140 2018/01/03 sort option by x or y of line                         ''~v140R~
'v139 2018/01/03 markWord position invalid when cliprect               ''~v139I~
'v138 2018/01/02 keep EOL for short line(add space at last if last is not delm)''~v138I~
'va05 2017/12/26 ext name Jpeg-->jpg,icon-->ico,tiff->tif              ''~va05I~
'va04 2017/12/25 save cut image to file                                ''~va04I~
'v110 2017/12/22 StringConstant reset required when lang changed       ''~v110I~
'v106 2017/12/20 partially extract from image(box by mouse dragging)   ''~v106I~
'v104 2017/12/20 drop space between chars of extracted text            ''~v104I~
'v100 2017/12/15 porting from MOD to Microsoft Ocr Library for Windows ''~v100I~
'************************************************************************************''~v100I~
Imports System.Drawing.Imaging
Imports System.Drawing                                                 ''~v138I~
Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Threading.Tasks
Imports System.Text
Imports System.Math                                                    ''~v138I~
Imports Windows.Foundation
Imports Windows.Globalization
Imports Windows.Graphics.Imaging
Imports Windows.Media.Ocr
Imports Windows.Storage
Imports Windows.Storage.Pickers
Imports Windows.Storage.Streams
'************************************************************************************''~v138I~
Public Class Cocr

    Private Const CNM = "Cocr:"
    Private Const LS_HORIZONTAL = &H1                                  ''~v138I~
    Private Const LS_EOL_DELM = &H2                                  ''~v138I~
    Private Const LS_ADD_CRLF = &H4                                  ''~v138I~
    Private Const LS_ADD_SPACE = &H8                                 ''~v138R~
    Private Const LS_CHANGE_DIR = &H100                              ''~v138I~
    Private Const LS_CHANGE_POINT = &H200                              ''~v138I~
    Private Const LS_CHANGE_LINESPACE = &H400                          ''~v140I~
    Private Const LS_REP_CRLF = &H800                          ''~v144I~
    Private Const LS_SKIP_CRLF = &H1000                                ''~v176I~
    Private Const LS_VERTICAL = &H2000                                ''~v196I~
    Private Const LS_SINGLE = &H4000                                ''~v196I~
    ''~v192I~
    Private Const LP_UPPER = &H1      '* layout position             ''~v192I~
    Private Const LP_LOWER = &H2                                     ''~v192R~
    Private Const LP_LEFT = &H4      '* Right=1,2, Left=5,6         ''~v192R~
    ''~v192I~
    Private Const RATE_POINT_CHANGED = 0.1 '*add crlf if char point changed 20%''~v138R~
    Private Const RATE_LENGTH_CHANGED = 0.8 '*add crlf if line length shorter than max of group''~v138I~
    Private Const RATE_LINESPACING = 2.5 '*add crlf lines spacing of both side is over 1.0 of char width/height''~v140R~''~v147R~
    Private Const RATE_WORDSPACING = 2.5 '*add space between word if distance is multiple of char size''~v178I~
    Private Const SORT_SCALE = 10000.0    '* sortkey for X and Y     ''~v140I~
    Private Const SORT_SCALE_V2POS = SORT_SCALE * 10000.0              ''~v192I~
    Private Const RATE_SMALL_CHAR = 0.5     'drop from average char width/height if charsize is small''~v153I~
    Private Const RATE_SPACE_HEADER = 1.8    'heder line hight vs space under header''~v197I~
    Private Const RATE_CENTERX_GAP = 0.1       ' if centerX gap is 10% of text range, center gap is wfounf''~v199I~
    Private softBitmap As SoftwareBitmap
    Dim tbLang As New DataTable()
    Public Const LANG_TAG = "tag"
    Public Const LANG_NAME = "name"
    Public Const SPACE_HEADER_LINE = "  "                               ''~v196I~
    Private imageFilename As String
    Private xText As String
    Private fileBMP As Bitmap
    Private result As OcrResult
    Private swOK As Boolean
    Private pendingStatusMsg As String = Nothing                       ''~v100I~
    Public statusMsg As String                                         ''~v100R~
    ''~v106I~
    Private clipRect As Rectangle                                      ''~v106I~
    Private swRectBMP As Boolean                                       ''~v106I~
'*  Private bmpRect As Bitmap                                          ''~v106I~''+v210R~
    Private scaleNew As Double                                         ''~v106I~
    Private lineStyle As Integer()                                     ''~v138I~
    '*  Private jpWriting() As String = {"ja", "日本語", "ja", "右縦書き", "ja", "左縦書き", "ja", "横書き右", "ja", "横書き左"} ''~v141I~''~v192R~
    '*  Private jpWriting() As String = {"ja", "日本語", "ja", "右縦書き", "ja", "左縦書き", "ja", "横書き右", "ja", "横書き左", "ja", "縦２段右", "ja", "縦２段左"} ''~v192R~''~v199R~
'*  Private jpWriting() As String = {"ja", "日本語", "ja", "右縦書き", "ja", "左縦書き", "ja", "横書き右", "ja", "横書き左", "ja", "縦２段右", "ja", "縦２段左", "ja", "横２頁左", "ja", "横２頁右"} ''~v199I~''~v200R~
'*  Private jpWriting() As String = {"ja", "日本語", "ja", "右縦書き", "ja", "左縦書き", "ja", "横書き右", "ja", "横書き左", "ja", "縦２段右", "ja", "縦２段左", "ja", "横２頁左", "ja", "横２頁右","en","英語２頁"} ''~v200R~''~v201R~
    Private jpWriting() As String = {"ja", "日本語", "ja", "右縦書き", "ja", "左縦書き", "ja", "横書き右", "ja", "横書き左", "ja", "縦２段右", "ja", "縦２段左", "ja", "横２頁左", "ja", "横２頁右"}''~v201I~
    '*  Private jpWritingE() As String = {"ja", "Japanese", "ja", "Jp-VR2L", "ja", "Jp-VL2R", "ja", "Jp-HR2L", "ja", "Jp-HL2R"} ''~v141I~''~v192R~
    '*  Private jpWritingE() As String = {"ja", "Japanese", "ja", "Jp-VR2L", "ja", "Jp-VL2R", "ja", "Jp-HR2L", "ja", "Jp-HL2R", "ja", "Jp-V2R2L", "ja", "Jp-V2L2R"} ''~v192I~''~v199R~
    '*  Private jpWritingE() As String = {"ja", "Japanese", "ja", "Jp-VR2L", "ja", "Jp-VL2R", "ja", "Jp-HR2L", "ja", "Jp-HL2R", "ja", "Jp-V2R2L", "ja", "Jp-V2L2R", "ja", "Jp-H2L2R", "ja", "Jp-H2R2L"} ''~v199I~''~v200R~
'*  Private jpWritingE() As String = {"ja", "Japanese", "ja", "Jp-VR2L", "ja", "Jp-VL2R", "ja", "Jp-HR2L", "ja", "Jp-HL2R", "ja", "Jp-V2R2L", "ja", "Jp-V2L2R", "ja", "Jp-H2L2R", "ja", "Jp-H2R2L", "en", "EN-2Page"} ''~v200R~''~v201R~
    Private jpWritingE() As String = {"ja", "Japanese", "ja", "Jp-VR2L", "ja", "Jp-VL2R", "ja", "Jp-HR2L", "ja", "Jp-HL2R", "ja", "Jp-V2R2L", "ja", "Jp-V2L2R", "ja", "Jp-H2L2R", "ja", "Jp-H2R2L"}''~v201I~
    Private Const SORT_VLINE_RTL = 1       '*sort vertical line by X:largeX-->smallX''~v140I~
    Private Const SORT_VLINE_LTR = 2       '*sort horizontal line by Y:smallY-->largeY''~v140I~''~v141R~
    Private Const SORT_HLINE_RTL = 3       '*sort horizontal line by Y:smallY-->largeY''~v141R~
    Private Const SORT_HLINE_LTR = 4       '*sort horizontal line by Y:smallY-->largeY''~v141I~
    Private Const SORT_V2_R2L = 5       '*vertical 2 part in page      ''~v192R~
    Private Const SORT_V2_L2R = 6       '*vertical 2 part in page      ''~v192I~
    Private Const SORT_H2_L2R = 7       '*vertical 2 part in page      ''~v199I~
    Private Const SORT_H2_R2L = 8       '*vertical 2 part in page      ''~v199I~
    Private Const SORT_H2_ENGLISH = 9     '*vertical 2 part in page for English''~v200I~
    Private sortOption As Integer = SORT_VLINE_RTL                     ''~v140R~''~v138R~''~v140R~
    '*  Private sortOption As Integer = 0                                  ''~v138I~''~v140R~
    '*  Private listLines As List(Of OcrLine)                            ''~v140R~''~v192R~
    Private listLines As List(Of OcrLineV2)                         ''~v192I~
'*  Private swPointChangeByMax As Boolean = True                         ''~v153I~''~v200R~
    Private swPointChangeByMax As Boolean = False '* by average        ''~v200I~
    Private langTag As String                                          ''~v177I~
    Private charH As Double()                                          ''~v178I~
    Private charW As Double()                                          ''~v178I~
    Private swVertical2 As Boolean = False                             ''~v192R~
    Private swHorizontal2 As Boolean = False                           ''~v199I~
    Private swVertical1 As Boolean = False                             ''~v196I~
    Private swGetHeader As Boolean = False                             ''~v196R~
    Private phaseGetHeader As Integer = 0                                ''~v196I~
    Private Const PGH_GETHEADER = 1     'extract Header                    ''~v196I~
    Private Const PGH_GETTEXT = 2   'extract text except Header        ''~v196I~
    Private headerBottomY As Integer = 0                               ''~v196R~
    Private footerTopY As Integer = 0                                  ''~v197I~
    Private bmpWidth As Integer = 0                                      ''~v196I~
    Private bmpHeight As Integer = 0                                     ''~v196I~
    Private xTextHeader As String                                      ''~v196I~
    Private xTextFooter As String                                      ''~v197I~
    Public linesHeader As IReadOnlyList(Of OcrLine) = Nothing                              ''~v197I~
    Public linesFooter As IReadOnlyList(Of OcrLine) = Nothing          ''~v197I~
    Private resultTextAngle, resultTextAngleHeader, resultTextAngleFooter As Double ''~v197R~
    Private clipRectHeader, clipRectFooter As Rectangle                ''~v197R~
    '**************************************************************************************
    Public Function setupComboBoxLang(Pcb As ToolStripComboBox, Pidxcfg As Integer) As Integer ''~v100R~
        Dim cb As ToolStripComboBox = Pcb
        Dim defaulttag As String = Language.CurrentInputMethodLanguageTag
        setupDataTableLang() 'setup tbLang
        cb.ComboBox.DataSource = tbLang
        cb.ComboBox.DisplayMember = LANG_NAME
        cb.ComboBox.ValueMember = LANG_TAG
        '       cb.Text = "Language"
        Dim idx As Integer = 0                                   ''~v100I~
        For Each row As DataRow In tbLang.Rows
            Dim tag As String = CType(row(LANG_TAG), String)            ''~v100R~
            If (Pidxcfg < 0) Then ' Not selected Then initially                   ''~v100I~
                If tag.CompareTo(defaulttag) = 0 Then
                    cb.Text = CType(row(LANG_NAME), String)                 ''~v100R~
                    cb.SelectedIndex = idx

                End If
            Else                                                         ''~v100I~
                If idx = Pidxcfg Then    'previously selected                ''~v100I~
                    cb.Text = CType(row(LANG_NAME), String)                ''~v100I~
                    cb.SelectedIndex = idx
                End If                                                     ''~v100I~
            End If                                                       ''~v100I~
            idx += 1                                                     ''~v100I~
        Next
        Return cb.SelectedIndex
    End Function
    '**************************************************
    Private Sub setupDataTableLang()
        Dim tb As New DataTable()
        tb.Columns.Add(LANG_TAG, GetType(String))
        tb.Columns.Add(LANG_NAME, GetType(String))
        Dim langlist = OcrEngine.AvailableRecognizerLanguages
        Dim jpw() As String                                            ''~v141I~
        If FormOptions.swLangEN Then                                   ''~v141I~
            jpw = jpWritingE                                           ''~v141R~
        Else                                                           ''~v141I~
            jpw = jpWriting                                            ''~v141R~
        End If                                                         ''~v141I~
        For Each item As Language In langlist
            Dim row As DataRow = tb.NewRow()
            If item.LanguageTag.StartsWith("ja") Then                         ''~v141I~
                For ii As Integer = 0 To jpw.Count - 1 Step 2          ''~v141R~
                    If ii > 0 Then                                            ''~v141I~
                        row = tb.NewRow()                              ''~v141I~
                    End If                                             ''~v141I~
                    row(LANG_TAG) = jpw(ii)                            ''~v141R~
                    row(LANG_NAME) = jpw(ii + 1)                       ''~v141R~
                    tb.Rows.Add(row)                                       ''~v141I~
                Next                                                       ''~v141I~
            Else                                                         ''~v141I~
                row(LANG_TAG) = item.LanguageTag
                If FormOptions.swLangEN Then                                     ''~v110I~
                    row(LANG_NAME) = item.NativeName                       ''~v110I~
                Else                                                       ''~v110I~
                    row(LANG_NAME) = item.DisplayName
                End If                                                     ''~v110I~
                tb.Rows.Add(row)
            End If                                                       ''~v141I~
        Next
        tb.AcceptChanges()
        tbLang = tb
    End Sub
    '**************************************************
    Public Function getSelectedLangTag(Pcb As ToolStripComboBox, ByRef Ppidxlang As Integer) As String ''~v100R~
        Dim cb As ToolStripComboBox = Pcb
        Dim idx As Integer = cb.SelectedIndex
        Dim tag As String = CType(tbLang.Rows(idx)(LANG_TAG), String)   ''~v100R~
        Dim name As String = CType(tbLang.Rows(idx)(LANG_NAME), String) ''~v141I~
        setWritingOption(name)                                         ''~v141I~
        Ppidxlang = idx                                                  ''~v100I~
        Return tag
    End Function
    '**************************************************                ''~v141I~
'*  Public Sub setWritingOption(Pname As String)                      ''~v141I~''~v200R~
    Private Sub setWritingOption(Pname As String)                      ''~v200I~
        Dim sortopt As Integer = 0                                       ''~v141I~
        Dim jpw() As String                                            ''~v141I~
        If FormOptions.swLangEN Then                                   ''~v141I~
            jpw = jpWritingE                                           ''~v141R~
        Else                                                           ''~v141I~
            jpw = jpWriting                                            ''~v141R~
        End If                                                         ''~v141I~
        For ii As Integer = 0 To jpw.Count - 1 Step 2                  ''~v141R~
            If Pname.CompareTo(jpw(ii + 1)) = 0 Then                   ''~v141R~
                sortopt = CType(ii / 2, Integer)                       ''~v141R~
            End If                                                     ''~v141I~
        Next                                                           ''~v141I~
        sortOption = sortopt                                             ''~v141I~
        If sortOption = SORT_V2_R2L Or sortOption = SORT_V2_L2R Then            ''~v192R~
            swVertical2 = True                                         ''~v192R~
        Else                                                           ''~v196I~
            swVertical2 = False                                        ''~v196I~
        End If                                                         ''~v196I~
        If sortOption = SORT_VLINE_RTL Or sortOption = SORT_VLINE_LTR Then ''~v196I~
            swVertical1 = True                                         ''~v196I~
        Else                                                           ''~v196I~
            swVertical1 = False                                        ''~v196I~
        End If                                                         ''~v196I~
        If sortOption = SORT_H2_L2R Or sortOption = SORT_H2_R2L Then   ''~v199R~
            swHorizontal2 = True                                        ''~v199I~
        Else                                                           ''~v199I~
            If sortOption = SORT_H2_ENGLISH Then                                ''~v200I~
                swHorizontal2 = True                                       ''~v200I~
            Else                                                           ''~v200I~
                swHorizontal2 = False                                       ''~v199I~
            End If                                                         ''~v200I~
        End If                                                         ''~v199I~
        '*      swGetHeader = swVertical1 Or swVertical2                         ''~v196I~''~v198R~
        '*Trace.W("class10:setWritingOption sortOption=" & sortOption & ",swHorizontal2=" & swHorizontal2 & ",swVertical2=" & swVertical2 & ",swVertical2=" & swVertical1) ''~v199I~''~v201R~
    End Sub                                                            ''~v141I~
    '**************************************************                ''~v106I~
    '* set clip box info before extact                                 ''~v106I~
    Public Sub setRect(PswRectBMP As Boolean, PbmpRect As Bitmap, PscaleNew As Double, PclipRect As Rectangle) ''~v106I~
        swRectBMP = PswRectBMP                                         ''~v106I~
'*      bmpRect = PbmpRect                                             ''~v106I~''+v210R~
        scaleNew = PscaleNew                                           ''~v106I~
        clipRect = PclipRect                                           ''~v106I~
        '*Trace.W("iOCR:setrect cliprect X=" & clipRect.X & ",Y=" & clipRect.Y & ",W=" & clipRect.Width & ",H=" & clipRect.Height) ''~v155R~''~v192R~''~v201R~
        '*Trace.W("iOCR:setrect BMP ww=" & bmpRect.Width & ",hh=" & bmpRect.Height & ",scale=" & scaleNew) ''~v192I~''~v196R~''~v201R~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Public Function cutBMPRect(PorgBMP As Bitmap) As Bitmap            ''~v106I~
        Dim xx, yy, ww, hh As Integer                                  ''~v106I~
        xx = CType(clipRect.X / scaleNew, Integer) 'dest and src position''~v106I~
        yy = CType(clipRect.Y / scaleNew, Integer)                     ''~v106I~
        ww = CType(clipRect.Width / scaleNew, Integer)                 ''~v106I~
        hh = CType(clipRect.Height / scaleNew, Integer)                ''~v106I~
        '*Trace.W("class10:cutBMPRect scaleNew=" & scaleNew & ",xx=" & xx & ",yy=" & yy & ",ww=" & ww & ",hh=" & hh) ''~v155R~''~v196R~''~v201R~
        Dim tgtRect As Rectangle = New Rectangle(xx, yy, ww, hh)       ''~v106I~
#If False Then                                                         ''~va04I~
        Dim bmp As Bitmap = New Bitmap(PorgBMP.Width, PorgBMP.Height)  ''~v106I~
        Dim g = Graphics.FromImage(bmp)                                ''~v106I~
        Dim unit As GraphicsUnit = GraphicsUnit.Pixel                    ''~v106I~
        g.DrawImage(PorgBMP, tgtRect, xx, yy, ww, hh, unit)              ''~v106I~
        g.Dispose()                                                    ''~v106I~
#Else                                                                  ''~va04I~
        Dim bmp As Bitmap = cutImage(PorgBMP, tgtRect)                 ''~va04I~
#End If                                                                ''~va04I~
        '*Trace.W("cutBMPRect org W=" & PorgBMP.Width & ",H=" & PorgBMP.Height) ''~v106I~''~va05R~''~v196R~''~v201R~
        '*Trace.W("cutBMPRect cut W=" & bmp.Width & ",H=" & bmp.Height)  ''~v196I~''~v201R~
        '*Trace.W("cutBMPRect clipRect X=" & clipRect.X & ",Y=" & clipRect.Y & ",W=" & clipRect.Width & ",H=" & clipRect.Height) ''~v106I~''~va05R~''~v196R~''~v201R~
        '*Trace.W("cutBMPRect scale=" & scaleNew)                        ''~v106I~''~va05R~''~v196R~''~v201R~
        '*Trace.W("cutBMPRect xx=" & xx & ",yy=" & yy & ",ww=" & ww & ",hh=" & hh) ''~v106I~''~va05R~''~v196R~''~v201R~
        '*Trace.W("cutBMPRect clip W=" & bmp.Width & ",H=" & bmp.Height) ''~v106I~''~va05R~''~v196R~''~v201R~
        '       bmp.Save("W:\cutbmprect.bmp", ImageFormat.BMP) '@@@@test       ''~v106I~
        Return bmp                                                     ''~v106I~
    End Function                                                       ''~v106I~
    '**************************************************                ''~v196I~
    Public Function cutBMPRect(PorgBMP As Bitmap, Prect As Rectangle) As Bitmap ''~v196I~
        '*Trace.W("cutBMPRect Prect X=" & Prect.X & ",Y=" & Prect.Y & ",W=" & Prect.Width & ",H=" & Prect.Height) ''~v196M~''~v201R~
        Dim bmp As Bitmap = cutImage(PorgBMP, Prect)                   ''~v196I~
        '*Trace.W("cutBMPRect orgBMP W=" & PorgBMP.Width & ",H=" & PorgBMP.Height) ''~v196I~''~v201R~
        '*Trace.W("cutBMPRect cutBMP W=" & bmp.Width & ",H=" & bmp.Height) ''~v196I~''~v201R~
        Return bmp                                                     ''~v196I~
    End Function                                                       ''~v196I~
    '**************************************************                ''~va04I~
    Public Function saveImage(Pbasename As String, Pextname As String, PswRectBMP As Boolean, PorgBMP As Bitmap, Pscale As Double, Prect As Rectangle) As String ''~va04R~
        '* from Form2                                                      ''~va04I~
        Dim bmp As Bitmap                                              ''~va04I~
        '*      Trace.W("iOCR:saveImage fnm=" & Pbasename & ",swRectBMP=" & PswRectBMP & ",scale=" & Pscale)''~v155R~
        '*      Trace.W("iOCR:saveImage cliprect X=" & Prect.X & ",Y=" & Prect.Y & ",W=" & Prect.Width & ",H=" & Prect.Height)''~v155R~
        If PswRectBMP Then 'clipped                                         ''~va04I~
            Dim xx, yy, ww, hh As Integer                              ''~va04I~
            xx = CType(Prect.X / Pscale, Integer) 'dest and src position''~va04I~
            yy = CType(Prect.Y / Pscale, Integer)                      ''~va04I~
            ww = CType(Prect.Width / Pscale, Integer)                  ''~va04I~
            hh = CType(Prect.Height / Pscale, Integer)                 ''~va04I~
            Dim tgtRect As Rectangle = New Rectangle(xx, yy, ww, hh)   ''~va04I~
            '*          Trace.W("saveImage X=" & xx & ",Y=" & yy & ",ww=" & ww & ",hh=" & hh)''~v155R~
            '*          Trace.W("saveImage bmp W=" & PorgBMP.Width & ",H=" & PorgBMP.Height)''~v155R~
            bmp = cutImage(PorgBMP, tgtRect)                           ''~va04I~
        Else                                                           ''~va04I~
            bmp = PorgBMP                                                ''~va04I~
        End If                                                         ''~va04I~
        Dim fmt As ImageFormat = str2Fmt(Pextname)                     ''~va04I~
        Dim fnm As String = saveImage(bmp, Pbasename, fmt)               ''~va04R~
        If PswRectBMP Then                                                  ''~va04I~
            bmp.Dispose()                                              ''~va04I~
        End If                                                         ''~va04I~
        Return fnm                                                     ''~va04I~
    End Function                                                       ''~va04R~
#If False Then
    '*************************************************************
    Public Function extractText(Pfnm As String, PfileBMP As Bitmap, Ptag As String, ByRef Pptext As String) As Boolean
        imageFilename = Pfnm
        fileBMP = PfileBMP
        xText = ""
        result = Nothing
        swOK = await extractTextAsync(Pfnm, Ptag)
        Pptext = xText
        Return swOK
    End Function
#Else
    '*************************************************************
    Public Function extractText(Pfnm As String, PfileBMP As Bitmap, Ptag As String, ByRef Pptext As String) As Boolean
        '*Trace.W("class10:extraceText swRectBMP=" & swRectBMP & ",sortOption=" & sortOption & ",swGetHeader=" & swGetHeader) ''~v196I~''~v201R~
        swGetHeader = FormOptions.swSplitHeader And (swVertical1 Or swVertical2) ''~v198M~
        '*Trace.W("class10:extractText swGetHeader=" & swGetHeader & ",splitHeader=" & FormOptions.swSplitHeader & ",swVertical1=" & swVertical1 & ",swVertical2=" & swVertical2) ''~v198R~''~v201R~
        langTag = Ptag                                                   ''~v177I~
        imageFilename = Pfnm
        fileBMP = PfileBMP
        If swRectBMP Then                                              ''~v106M~
            swGetHeader = False                                          ''~v198I~
            fileBMP = cutBMPRect(fileBMP)                              ''~v106M~
        End If                                                         ''~v106M~
        bmpWidth = fileBMP.Width                                         ''~v196R~
        bmpHeight = fileBMP.Height                                       ''~v196R~
        xText = ""
        linesHeader = Nothing                                            ''~v197I~
        linesFooter = Nothing                                          ''~v197I~
        result = Nothing
        statusMsg = Nothing                                              ''~v100R~
        '*      Trace.W("extraceText swRectBMP=" & swRectBMP & ",resolution H=" & fileBMP.HorizontalResolution & ",W=" & fileBMP.VerticalResolution) ''~v176R~''~v178R~
        Dim t As Task = Task.Run(Async Function()
                                     swOK = Await extractTextAsync(Pfnm, Ptag)
                                 End Function)
        t.Wait()
        If Not swRectBMP Then                                               ''~v196R~
            If swOK And swGetHeader And (headerBottomY <> 0 Or footerTopY <> 0) Then '*detected header/footer line''~v197R~
                swOK = extractHeaderAndText(Pfnm, Ptag)                   ''~v196R~
                If Not swOK Then                                            ''~v196I~
                    swGetHeader = False                                  ''~v196I~
                    extractText(Pfnm, PfileBMP, Ptag, Pptext)           ''~v196R~
                End If                                                 ''~v196R~
            End If                                                     ''~v196R~
        End If                                                         ''~v196I~
        Pptext = xText
        Return swOK
    End Function
#End If
    '*************************************************************
    Private Async Function extractTextAsync(Pfnm As String, Ptag As String) As Task(Of Boolean)
        Try
            softBitmap = Await LoadImage(Pfnm)
            If softBitmap Is Nothing Then
                Return False
            End If
            result = Await callOCR(Pfnm, softBitmap, Ptag)
            If result Is Nothing Then
                xText = "Extract failed"
                Return False
            End If
            '*          resultTextAngle=CType(result.TextAngle, Double)            ''~v197R~
            resultTextAngle = CType(IIf(result.TextAngle.HasValue, result.TextAngle, 0), Double) ''~v197I~
            xText = result.Text                                        ''~v197I~
            '*Trace.W("class10:extractTextAsync result.TextAngle=" & result.TextAngle & ",resultTextAngle=" & resultTextAngle & ",text=" & result.Text) ''~v197R~''~v201R~
            listLines = toList(result.Lines)                           ''~v140R~
            If listLines IsNot Nothing Then                                   ''~v196R~
                xText = makeLines(xText.Length) 'insert crlf between lines
            End If                                                       ''~v196R~
            swOK = True
        Catch ex As Exception
            '*          showStatus(CNM & "extractText exception:" & ex.Message)    ''~v140R~
            Form1.exceptionMsg("OCR:extractTextAsync", ex)             ''~v140I~
            xText = ex.Message
        End Try
        Return True
    End Function
    '*************************************************************
    Private Async Function LoadImage(Pfnm As String) As Task(Of SoftwareBitmap)
        Dim buff As Byte() = getImageBuff()
        Dim softbmp As SoftwareBitmap = Nothing
        Try
            Dim mem As MemoryStream = New MemoryStream(buff)
            mem.Position = 0
            Dim stream = Await ConvertToRandomAccessStream(mem)
            softbmp = Await LoadImage(stream)
        Catch ex As Exception
            '*          showStatus(CNM & "LoadImage file exception:" & Pfnm & ":" & ex.Message)''~v140R~
            Form1.exceptionMsg("OCR:LoadImage file", ex)               ''~v140I~
        End Try
        Return softbmp
    End Function
    '*************************************************************
    Private Function getImageBuff() As Byte()
        Dim buff As Byte() = Nothing
        Try
            Dim ms As MemoryStream = New MemoryStream()                ''~v100R~
            Dim bmp As Bitmap = fileBMP                                ''~v100R~
            Dim fmt As ImageFormat
            fmt = ImageFormat.Bmp
            bmp.Save(ms, fmt)
            ms.Close()
            buff = ms.ToArray()
        Catch ex As Exception
            '*          showStatus(CNM & "getImageBuff exception:" & ex.Message)   ''~v140R~
            Form1.exceptionMsg("OCR:getImageBuff", ex)                 ''~v140I~
        End Try
        Return buff
    End Function
    '*************************************************************
    Private Async Function ConvertToRandomAccessStream(Pms As MemoryStream) As Task(Of IRandomAccessStream)

        Dim randomAccessStream As InMemoryRandomAccessStream = New InMemoryRandomAccessStream()
        Dim outputStream As IOutputStream = randomAccessStream.GetOutputStreamAt(0)
        Dim dw As DataWriter = New DataWriter(outputStream)
        Try
            dw.WriteBytes(Pms.ToArray())
            Dim memtask = New Task(Sub()
                                       dw.WriteBytes(Pms.ToArray())
                                   End Sub)
            memtask.Start()
            Await memtask
            Await dw.StoreAsync()
            Await outputStream.FlushAsync()
        Catch ex As Exception
            '           showStatus(CNM & "ConvertToRandomAccess:" & ex.Message)    ''~v140R~
            Form1.exceptionMsg("OCR:ConvertToRandomAccess", ex)        ''~v140I~
        End Try
        Return randomAccessStream
    End Function
    '*************************************************************
    Private Async Function LoadImage(Pstream As IRandomAccessStream) As Task(Of SoftwareBitmap)
        Try
            Dim decoder = Await BitmapDecoder.CreateAsync(Pstream)
            Dim softbmp = Await decoder.GetSoftwareBitmapAsync()
            Return softbmp
        Catch ex As Exception
            '*          showStatus(CNM & "LoadImage stream :" & ex.Message)        ''~v140R~
            Form1.exceptionMsg("OCR:LoadImage Strem", ex)              ''~v140I~
        End Try
        Return Nothing
    End Function
    '*************************************************************
    Private Async Function callOCR(Pfnm As String, Pbmp As SoftwareBitmap, PlangTag As String) As Task(Of OcrResult)
        ' to support en languagepack will be set by ControlPanel
        Dim lng As Language = New Language(PlangTag)
        Dim engine As OcrEngine = OcrEngine.TryCreateFromLanguage(lng)
        Dim result As OcrResult = Nothing
        Try
            result = Await engine.RecognizeAsync(Pbmp)
        Catch ex As Exception
            '*          showStatus(CNM & "Extract failed:" & imageFilename & ":" & ex.Message)''~v140R~
            Form1.exceptionMsg("OCR:callOCR", ex)                      ''~v140I~
        End Try
        Return result
    End Function
    '*************************************************************
    Public Function markWords(Pbmp As Bitmap) As Boolean
        '** avoid exceotion:Indexed Pixel at Graphics.FromImage for mono color image
        Dim xx0 = 0, yy0 = 0                                               ''~v139I~
        Trace.setOn()                                                  ''~va05R~
        Try
            '********************
            '*Trace.W("Class10:MarkWords swRectBMP=" & swRectBMP & ",swGetHeader=" & swGetHeader & ",headerBottomY=" & headerBottomY) ''~v196I~''~v201R~
            '*Trace.W("Class10:MarkWords Pbmp Height=" & Pbmp.Height & ",width=" & Pbmp.Width) ''~v197I~''~v201R~
            If swRectBMP Then                                          ''~v139I~
                xx0 = CType(clipRect.X / scaleNew, Integer) 'dest and src position''~v139I~
                yy0 = CType(clipRect.Y / scaleNew, Integer)            ''~v139I~
            End If                                                     ''~v139I~
            Dim bmpDraw As Bitmap = Pbmp
            Dim g = Graphics.FromImage(bmpDraw)
            Dim br As Brush = New SolidBrush(System.Drawing.Color.FromArgb(&H20, System.Drawing.Color.Blue))
            '           Dim text As String = ""
            '*          For Each line As OcrLine In result.Lines                   ''~v140R~
            '*          Dim angle As Double = CType(result.TextAngle, Double)                      ''~v148I~''~v197R~
            Dim angle As Double = CType(IIf(result.TextAngle.HasValue, result.TextAngle, 0), Double) ''~v197I~
            Dim msin As Double                                         ''~v148I~
            Dim mcos As Double                                         ''~v148I~
            If angle <> 0 Then                                                ''~v148I~
                adjustMarkingBoxInit(angle, msin, mcos) '*get sin cosin''~v148R~
            End If                                                     ''~v148I~
            '*          For Each line As OcrLine In listLines                      ''~v140I~''~v155R~
            '*          For Each line As OcrLine In result.Lines                   ''~v155I~''~v197R~
            Dim lines As IReadOnlyList(Of OcrLine) = result.Lines                ''~v197I~
            Dim swMarkHeader As Boolean = False                          ''~v197R~
            Dim swMarkFooter As Boolean = False                        ''~v197I~
            Do                                                           ''~v197I~
                '*Trace.W("Class10:MarkWords swMarkHeader=" & swMarkHeader & ",angle=" & angle & ",clipRect X=" & clipRect.X & ",Y=" & clipRect.Y & ",W=" & clipRect.Width & ",H=" & clipRect.Height) ''~v197R~''~v201R~
                For Each line As OcrLine In lines                          ''~v197I~
                    '               text += line.Text & " "
                    '*              Trace.W("Class10:MarkWords:  word count=" & line.Words.Count & ",Line Text=" & line.Text)             ''~va05R~''~v138R~''~v155R~
                    For Each word As OcrWord In line.Words
                        Dim brect As Windows.Foundation.Rect = word.BoundingRect
                        If Not swRectBMP And swGetHeader Then              ''~v196M~''~v197I~
                            If swMarkHeader Then                       ''~v197R~
                                '*                              brect.Y -= Pbmp.Height - footerTopY    ''~v197R~
                            Else                                       ''~v197R~
                                If swMarkFooter Then                           ''~v197I~
                                    brect.Y += footerTopY                  ''~v197I~
                                Else                                     ''~v197I~
                                    brect.Y += headerBottomY                       ''~v196M~
                                End If                                             ''~v196M~''~v197R~
                            End If                                     ''~v197I~
                        End If                                           ''~v197I~
                        Dim rect As Rectangle = New System.Drawing.Rectangle(CType(brect.X, Integer), CType(brect.Y, Integer), CType(brect.Width, Integer), CType(brect.Height, Integer)) ''~v100R~
                        If angle <> 0 Then                                        ''~v148I~
                            '*                      Trace.W("Ajust angle angle=" & angle & ",text=" & word.Text) ''~v148I~''~v155R~
                            adjustMarkingBox(bmpDraw, rect, msin, mcos, swRectBMP, clipRect) '*get sin cosin''~v148R~
                        End If                                             ''~v148I~
                        rect.X += xx0                                       ''~v139I~
                        rect.Y += yy0                                       ''~v139I~
                        '*                  Trace.W("Class10:markWords: X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",text=" & word.Text) ''~va05R~''~v155R~
                        '*                  Trace.W("Class10:markWords rect: X=" & rect.X & ",Y=" & rect.Y & ",W=" & rect.Width & ",H=" & rect.Height & ",text=" & word.Text) ''~v147I~''~v155R~
                        '*Trace.W("Class10:MarkWords swMarkHeader=" & swMarkHeader & ",xx0=" & xx0 & ",yy0=" & yy0 & ",rect X=" & rect.X & ",Y=" & rect.Y & ",H=" & rect.Height & ",W=" & rect.Width) ''~v197R~''~v201R~
                        g.FillRectangle(br, rect)
                        g.DrawRectangle(Pens.Red, rect)
                        '                   text &= word.Text & " "
                    Next
                Next
                If linesHeader IsNot Nothing And Not swMarkHeader Then      ''~v197R~
                    swMarkHeader = True                                ''~v197I~
                    lines = linesHeader                                ''~v197R~
                    angle = resultTextAngleHeader                        ''~v197R~
                    clipRect = clipRectHeader                            ''~v197R~
                    If angle <> 0 Then                                 ''~v197R~
                        adjustMarkingBoxInit(angle, msin, mcos) '*get sin cosin''~v197R~
                    End If                                             ''~v197R~
                    '*Trace.W("Class10:MarkWords linesHeader ctr=" & lines.Count() & ",angle=" & angle) ''~v197R~''~v201R~
                    '*printLinesWordsRect("markWords HeaderLin", linesHeader) ''~v197R~''~v201R~
                    Continue Do                                        ''~v197I~
                End If                                                 ''~v197M~
                If linesFooter IsNot Nothing And Not swMarkFooter Then      ''~v197I~
                    swMarkFooter = True                                ''~v197I~
                    lines = linesFooter                                ''~v197I~
                    angle = resultTextAngleFooter                        ''~v197I~
                    clipRect = clipRectFooter                            ''~v197I~
                    If angle <> 0 Then                                 ''~v197I~
                        adjustMarkingBoxInit(angle, msin, mcos) '*get sin cosin''~v197I~
                    End If                                             ''~v197I~
                    '*Trace.W("Class10:MarkWords linesFooter ctr=" & lines.Count() & ",angle=" & angle) ''~v197I~''~v201R~
                    '*printLinesWordsRect("markWords Footer", linesFooter) ''~v197I~''~v201R~
                    Continue Do                                        ''~v197I~
                End If                                                 ''~v197I~
                Exit Do                                                ''~v197I~
            Loop                                                        ''~v197I~
            g.Dispose()
        Catch ex As Exception
            '           showStatus(CNM & "markWords :" & ex.Message)               ''~v140R~
            Form1.exceptionMsg("OCR:markWords", ex)                    ''~v140I~
            Return False
        End Try
        '*        Trace.setOff()                                                 ''~va05R~
        Return True
    End Function
    '*************************************************************     ''~v148I~
    Private Sub adjustMarkingBoxInit(Pangle As Double, ByRef Ppsin As Double, ByRef Ppcos As Double) ''~v148R~
        Dim rad As Single = CSng((Pangle / 180) * Math.PI)          ''~v148I~
        Dim msin As Double = Math.Sin(rad)                             ''~v148R~
        Dim mcos As Double = Math.Cos(rad)                             ''~v148R~
        Ppsin = msin                                                     ''~v148I~
        Ppcos = mcos                                                     ''~v148I~
        '*      Trace.W("ajustangleInit  msin=" & msin & ",mcos=" & mcos)      ''~v148I~''~v155R~
    End Sub                                                            ''~v148I~
    '*************************************************************     ''~v148I~
    '*x2=x1cos-y1sin, y2=x1sin+y1cos;center is (x/2,y/2)=x0,y0         ''~v148I~
    '*==>x2=x0+(x1-x0)cos-(y1-y0)sin, y2=y0+(x1-x0)sin+(y1-y0)cos      ''~v148I~
    '*************************************************************     ''~v148I~
    Private Sub adjustMarkingBox(Pbmp As Bitmap, ByRef Pprect As Rectangle, Psin As Double, Pcos As Double, PswrectBMP As Boolean, PclipRect As Rectangle) ''~v148R~
        '*Trace.W("class10:adjustmarkingBox PswRectBMP=" & PswrectBMP & ",PclipRect X=" & PclipRect.X & ",Y=" & PclipRect.Y & ",W=" & PclipRect.Width & ",H=" & PclipRect.Height) ''~v197I~''~v201R~
        Dim x0 As Double                                               ''~v148R~
        Dim y0 As Double                                               ''~v148R~
        If PswrectBMP Then                                                  ''~v148I~
            x0 = PclipRect.Width / 2                                     ''~v148I~
            y0 = PclipRect.Height / 2                                    ''~v148I~
        Else                                                           ''~v148I~
            x0 = Pbmp.Width / 2                                        ''~v148M~
            y0 = Pbmp.Height / 2                                       ''~v148M~
        End If                                                         ''~v148I~
        Dim x1 As Double = Pprect.X                                    ''~v148R~
        Dim y1 As Double = Pprect.Y                                    ''~v148I~
        Dim dx As Double = x1 - x0                                       ''~v148I~
        Dim dy As Double = y1 - y0                                       ''~v148I~
        Dim x2 As Double = x0 + dx * Pcos - dy * Psin                          ''~v148R~
        Dim y2 As Double = y0 + dx * Psin + dy * Pcos                          ''~v148I~
        Pprect.X = CType(x2, Integer)                                  ''~v148R~
        Pprect.Y = CType(y2, Integer)                                  ''~v148R~
        '*Trace.W("ajustangle  xx=" & x1 & ",yy=" & y1 & ",xxnew=" & x2 & ",yynew=" & y2) ''~v148R~''~v155R~''~v197R~''~v201R~
    End Sub                                                            ''~v148I~
    '*************************************************************     ''~v145I~
    '*chk horizontal for the case 1word/line,1char/word                ''~v145I~
    '*************************************************************     ''~v145I~
    '*  Public Function getDestOfOneCharLine(Pline As OcrLine, Pword As OcrWord, Plineno As Integer) As Integer ''~v145R~''~v192R~
    Public Function getDestOfOneCharLine(Pline As OcrLineV2, Pword As OcrWord, Plineno As Integer) As Integer ''~v192I~
        Dim type As Integer = 0                                          ''~v145I~
        '*      Dim pl As OcrLine = Nothing                                      ''~v145I~''~v192R~
        Dim pl As OcrLineV2 = Nothing                                  ''~v192I~
        Dim prect As Rect                                              ''~v145I~
        Dim crect As Rect                                              ''~v145I~
        Dim dx As Double                                               ''~v145R~
        Dim dy As Double                                               ''~v145R~
        crect = Pword.BoundingRect                                       ''~v145I~
        Do                                                             ''~v145I~
            If Plineno > 0 Then                                        ''~v145R~
                pl = listLines(Plineno - 1)                            ''~v145R~
                prect = pl.Words.Item(0).BoundingRect                  ''~v145R~
                dy = Abs(crect.Y - prect.Y)                                ''~v145R~
                dx = Abs(crect.X - prect.X)                                ''~v145I~
                '*Trace.W("class10:getDestOfOneCharLine dx=" & dx & ",dy=" & dy & ",text=" & Pline.Text) ''~v145I~''~v155R~''~v199R~''~v201R~
                '*Trace.W("class10:getDestOfOneCharLine chH=" & crect.Height & ",chW=" & crect.Width & "prev text=" & pl.Text) ''~v145I~''~v155R~''~v199R~''~v201R~
                If dy < crect.Height Then                                     ''~v145I~
                    type = LS_HORIZONTAL                               ''~v145I~
                    Exit Do                                            ''~v145I~
                End If                                                 ''~v145I~
                If dx < crect.Width Then                                      ''~v145I~
                    Exit Do                                            ''~v145I~
                End If                                                 ''~v145I~
            End If                                                     ''~v145I~
            If sortOption = SORT_HLINE_RTL OrElse sortOption = SORT_HLINE_LTR Then ''~v145I~
                type = LS_HORIZONTAL                                   ''~v145I~
            ElseIf sortOption = SORT_H2_R2L OrElse sortOption = SORT_H2_L2R Then ''~v199I~
                type = LS_HORIZONTAL                                   ''~v199I~
            End If                                                     ''~v145I~
            Exit Do                                                    ''~v145I~
        Loop                                                           ''~v145I~
        '*Trace.W("class10:getDestOfOneCharLine type=" & type)                              ''~v145I~''~v155R~''~v199R~''~v201R~
        Return type                                                    ''~v145I~
    End Function                                                       ''~v145I~
    '*************************************************************     ''~v138I~
    Public Sub chkLineStyle()                                          ''~v138R~
        '       Dim ctrline As Integer = result.Lines.Count()                    ''~v138I~''~v140R~
        Dim ctrline As Integer = listLines.Count()                     ''~v140I~
        Dim ls As Integer() = New Integer(ctrline) {}                      ''~v138I~
        Dim linesz As Double() = New Double(ctrline) {}               ''~v138R~
        '*      Dim charH As Double() = New Double(ctrline) {}                 ''~v138R~''~v178R~
        '*      Dim charW As Double() = New Double(ctrline) {}                 ''~v138I~''~v178R~
        charH = New Double(ctrline) {}                                 ''~v178I~
        charW = New Double(ctrline) {}                                 ''~v178I~
        Dim lineSpacing As Double() = New Double(ctrline) {}           ''~v140I~
        Dim baseline As Double() = New Double(ctrline) {}            ''~v147I~
        Dim bound1 As Double() = New Double(ctrline) {}                ''~v176R~
        Dim bound2 As Double() = New Double(ctrline) {}                ''~v176R~
        Dim axis As Double() = New Double(ctrline) {}                ''~v176I~
        Dim brect As Rect                                              ''~v138I~
        Dim brect1 As Rect                                        ''~v138I~
        Dim brect2 As Rect                                        ''~v138I~
        Dim lineno As Integer                                          ''~v138I~
        Dim type As Integer                                            ''~v138I~
        Dim ww As Double                                               ''~v138R~
        Dim hh As Double                                               ''~v138I~
        Dim sz As Double                                               ''~v138R~
        '*************************************                         ''~v138I~
        Dim minLineHeight As Double = 1000.0                            ''~v140I~
        Dim minLineWidth As Double = 1000.0                              ''~v140I~
        lineno = 0                                                       ''~v138I~
        '*** determine line is horizontal or vertical ***              ''~v176I~
        '*      For Each line As OcrLine In result.Lines                       ''~v138I~''~v140R~
        '*      For Each line As OcrLine In listLines                          ''~v140I~''~v192R~
        For Each line As OcrLineV2 In listLines                        ''~v192I~
            Dim ctrword As Integer = line.Words.Count                    ''~v138I~
            Dim word1 As OcrWord = line.Words.Item(0)                    ''~v138I~
            Dim word2 As OcrWord = line.Words.Item(ctrword - 1)            ''~v138I~
            brect1 = word1.BoundingRect                                 ''~v138I~
            brect2 = word2.BoundingRect                                 ''~v138I~
            Dim lenx As Double = Abs(brect2.X - brect1.X) + CType(IIf(brect2.X > brect1.X, brect2.Width, brect1.Width), Double) ''~v138R~
            Dim leny As Double = Abs(brect2.Y - brect1.Y) + brect2.Height ''~v138R~
            If line.Words.Count = 1 AndAlso word1.Text.Length = 1 Then                    ''~v145I~
                type = getDestOfOneCharLine(line, word1, lineno)       ''~v145R~
            Else                                                         ''~v145I~
                If lenx > leny Then                                               ''~v138I~
                    type = LS_HORIZONTAL     '*horizontal line               ''~v138I~
                    linesz(lineno) = lenx                                    ''~v138I~
                Else                                                       ''~v138I~
                    type = 0                                                 ''~v138I~
                    linesz(lineno) = leny                                    ''~v138I~
                End If                                                     ''~v138I~
            End If                                                       ''~v145I~
            '*          Trace.W("Class10:chkLineStyle type=" & type & ",lenx=" & lenx & ",leny=" & leny) ''~v138I~''~v155R~
            '*Trace.W("Class10:chkLineStyle type=" & type & ",lineno=" & lineno & ",X=" & brect1.X & ",txt=" & line.Text) ''~v138I~''~v155R~''~v192R~''~v201R~
            Dim txt = word2.Text                                        ''~v138I~
            Dim txtlen As Integer = txt.Length                           ''~v138I~
            If txtlen > 0 Then                                                ''~v138I~
                Dim ch As Char = txt.Chars(txtlen - 1)                     ''~v138I~
                If ClassKanaText.isDelmCharEOL(ch) Then                     ''~v138I~
                    '*                  Trace.W("EOL_DELM text=" & line.Text)              ''~v140I~''~v155R~
                    type = type Or LS_EOL_DELM     '* 0x02 line end with delm''~v138I~
                End If                                                 ''~v138I~
            End If                                                     ''~v138I~
            '*** get average char size ***                                                    ''~v138I~''~v176R~
            Dim ptmaxw As Double = 0.0                                 ''~v138R~
            Dim ptmaxh As Double = 0.0                                 ''~v138I~
            Dim ptmaxwMax As Double = 0.0                              ''~v153I~
            Dim ptmaxhMax As Double = 0.0                              ''~v153I~
            Dim wordctr = 0                                              ''~v146I~
            Dim base As Double = 0.0                                     ''~v147I~
            For Each word As OcrWord In line.Words                     ''~v138I~
                brect = word.BoundingRect                                ''~v138I~
                '*              Trace.W("Class10:chkLineStyle Words: X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",btext=" & word.Text) ''~v138I~''~v155R~''~v178R~
                hh = brect.Height                                      ''~v138R~
                ww = brect.Width                                       ''~v138R~
                '*              brect.Y += hh             '*change to baseline for line spacing''~v140I~''~v146R~''~v147R~
                '*Trace.W("Class10:chkLineStyle Words: X=" & brect.X & ",Y=" & brect.Y & ",hh=" & hh & ",ww=" & ww & ",text=" & word.Text) ''~v140I~''~v146R~''~v147R~''~v155R~''~v178R~''~v192R~''~v201R~
                If word.Text.Length > 1 Then                                  ''~v143I~
                    If (type And LS_HORIZONTAL) <> 0 Then     '*horizontal line''~v154I~
                        ww /= word.Text.Length                               ''~v143I~
                    Else                                                 ''~v154I~
                        hh /= word.Text.Length                             ''~v154I~
                    End If                                                 ''~v143I~
                End If                                                 ''~v154I~
                ptmaxhMax = Max(hh, ptmaxhMax)                               ''~v138R~''~v146R~''~v153R~
                ptmaxwMax = Max(ww, ptmaxwMax)                               ''~v138I~''~v146R~''~v153R~
                Dim wordlen = word.Text.Length                           ''~v147I~
                ptmaxh += hh * wordlen                                    ''~v146I~''~v147R~
                ptmaxw += ww * wordlen                                    ''~v146I~''~v147R~
                base = Max(base, brect.Y + hh)                           ''~v147R~
                wordctr += wordlen                                       ''~v146I~''~v147R~
            Next                                                       ''~v138I~
            ''~v153I~
            charH(lineno) = ptmaxh / wordctr                             ''~v146I~
            charW(lineno) = ptmaxw / wordctr                             ''~v146I~
            baseline(lineno) = base '*for line spacing chk             ''~v147R~
            '*Trace.W("class10:chkLineStyle average baseline=" & baseline(lineno) & ",sum=" & base & ",ctr=" & wordctr) ''~v147I~''~v153M~''~v155R~''~v178R~''~v200R~''~v201R~
            ''~v153I~
            '*** modify average char size except small size char ***   ''~v176I~
            Dim avh2 As Double = charH(lineno) * RATE_SMALL_CHAR          ''~v153I~
            Dim avw2 As Double = charW(lineno) * RATE_SMALL_CHAR          ''~v153I~
            '*Trace.W("class10.chkLineStyle lineNo=" & lineno & ",charH=" & charH(lineno) & ",charW=" & charW(lineno) & ",avh2=" & avh2 & ",avw2=" & avw2 & ",text=" & line.Text) ''~v196R~''~v200R~''~v201R~
            ptmaxw = 0.0                                               ''~v153I~
            ptmaxh = 0.0                                               ''~v153I~
            Dim wordctrH As Integer = 0                                  ''~v153R~
            Dim wordctrW As Integer = 0                                  ''~v153I~
            Dim pix1 As Double = 0                                     ''~v176R~
            Dim pix2 As Double = 0                                     ''~v176R~
            For Each word As OcrWord In line.Words                     ''~v153I~
                brect = word.BoundingRect                              ''~v153I~
                hh = brect.Height                                      ''~v153I~
                ww = brect.Width                                       ''~v153I~
                If (type And LS_HORIZONTAL) <> 0 Then     '*horizontal line''~v176I~
                    If pix1 = 0 Then                                          ''~v176I~
                        pix1 = brect.Y       '*min top                 ''~v176I~
                    Else                                               ''~v176I~
                        pix1 = Min(pix1, brect.Y)       '*min top      ''~v176R~
                    End If                                             ''~v176I~
                    pix2 = Max(pix2, brect.Y + hh)    '*max bottom       ''~v176I~
                Else                                                   ''~v176I~
                    If pix1 = 0 Then                                          ''~v176I~
                        pix1 = brect.X                 '*min left      ''~v176I~
                    Else                                               ''~v176I~
                        pix1 = Min(pix1, brect.X)       '*min left     ''~v176R~
                    End If                                             ''~v176I~
                    pix2 = Max(pix2, brect.X + ww)    '*max right        ''~v176I~
                End If                                                 ''~v176I~
                If word.Text.Length > 1 Then                           ''~v153I~
                    If (type And LS_HORIZONTAL) <> 0 Then     '*horizontal line''~v176I~
                        ww /= word.Text.Length                             ''~v153I~
                    Else                                                 ''~v176I~
                        hh /= word.Text.Length                             ''~v176I~
                    End If                                               ''~v176I~
                End If                                                 ''~v153I~
                Dim wordlen = word.Text.Length                         ''~v153M~
                If hh > avh2 Then                                           ''~v153R~
                    '*                  wordctrH += 1                                        ''~v153I~''~v200R~
                    wordctrH += wordlen                                ''~v200I~
                    ptmaxh += hh * wordlen                             ''~v153I~
                End If                                                 ''~v153I~
                If ww > avw2 Then                                           ''~v153I~
                    '*                  wordctrW += 1                                        ''~v153I~''~v200R~
                    wordctrW += wordlen                                ''~v200I~
                    ptmaxw += ww * wordlen                             ''~v153I~
                End If                                                 ''~v153I~
                '*              Trace.W("Class10:chkLineStyle largeChar Words: X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",btext=" & word.Text) ''~v153I~''~v155R~
            Next                                                       ''~v153I~
            If wordctrH > 0 Then                                              ''~v153R~
                charH(lineno) = ptmaxh / wordctrH                      ''~v153R~
            End If                                                     ''~v153I~
            If wordctrW > 0 Then                                              ''~v153I~
                charW(lineno) = ptmaxw / wordctrW                      ''~v153R~
            End If                                                     ''~v153I~
            '*Trace.W("class10:chkLneStyle average by large char ctrH=" & wordctrH & ",ctrW=" & wordctrW & ",average H=" & charH(lineno) & ",W=" & charW(lineno) & ",text=" & line.Text) ''~v153I~''~v155R~''~v196R~''~v200R~''~v201R~
            If (type And LS_HORIZONTAL) <> 0 Then     '*horizontal line       ''~v140I~
                minLineHeight = Min(minLineHeight, charH(lineno))      ''~v140R~
            Else                                                       ''~v140I~
                minLineWidth = Min(minLineWidth, charW(lineno))        ''~v140R~
            End If                                                     ''~v140I~
            '*Trace.W("class10*chkLineStyle minLineHeigh=" & minLineHeight & ",minLineWidth=" & minLineWidth) ''~v154I~''~v155R~''~v200R~''~v201R~
            If swPointChangeByMax Then                                      ''~v153I~''~v154R~
                charW(lineno) = ptmaxwMax                              ''~v153I~''~v154R~
                charH(lineno) = ptmaxhMax                              ''~v153I~''~v154R~
            End If                                                     ''~v153I~''~v154R~
            bound1(lineno) = pix1       'pixel of line box, start        ''~v176I~
            bound2(lineno) = pix2       'pixel of line box, end          ''~v176I~
            '*Trace.W("bound1=" & pix1 & ",bound2=" & pix2 & ",chw=" & charW(lineno) & ",chH=" & charH(lineno) & ",type=" & type & ",text=" & line.Text) ''~v176R~''~v178R~''~v199R~''~v201R~
            ls(lineno) = type     '*vertical line                        ''~v138I~
            lineno += 1                                                  ''~v138I~
        Next                                                           ''~v138I~
        '*** chk same line for vertical line                           ''~v176I~
        '***************************************************           ''~v140I~
        '*      sortLines(listLines, ls, linesz, charH, charW, minLineHeight, minLineWidth) ''~v140R~''~v147R~
        '*        sortLines(listLines, ls, linesz, charH, charW, minLineHeight, minLineWidth, baseline) ''~v147I~''~v176R~
        sortLines(listLines, ls, linesz, charH, charW, minLineHeight, minLineWidth, baseline, bound1, bound2, Nothing) ''~v176R~
        '***************************************************           ''~v176I~
        If chkSameLineAfterSort(0, listLines, ls, bound1, bound2, axis) Then '*same line found''~v176R~
            sortLines(listLines, ls, linesz, charH, charW, minLineHeight, minLineWidth, baseline, bound1, bound2, axis) ''~v176R~
            chkSameLineAfterSort(1, listLines, ls, bound1, bound2, axis) 'reset eol flag''~v176R~
        End If                                                         ''~v176I~
        '***************************************************           ''~v140I~
        '*chk char size changed                                                ''~v138I~
#If True Then                                                          ''~v176R~
        Dim hho As Double = 0.0                                        ''~v138R~
        Dim wwo As Double = 0.0                                        ''~v138I~
#Else                                                                  ''~v176I~
        Dim diffline As Double = 0.0                                   ''~v176I~
        Dim difflineo As Double = 0.0                                  ''~v176I~
#End If                                                                ''~v176I~
        Dim dirHo = False                                              ''~v138R~
        Dim poso As Double = 0.0                                         ''~v140I~
        For ii As Integer = 0 To ctrline - 1                               ''~v138I~
            type = ls(ii)                                                ''~v138I~
            '*Trace.W("class10:chkLineStyle ii=" & ii & ",type=0x" & Hex(type) & ",AfterSort text=" & listLines.Item(ii).Text) ''~v200R~''~v201R~
            hh = charH(ii)                                              ''~v138R~
            ww = charW(ii)                                              ''~v138I~
            Dim dirH As Boolean = (type And LS_HORIZONTAL) <> 0       '*horizontal''~v138R~''~v140R~
            Dim word As OcrWord = listLines.Item(ii).Words.Item(0)            ''~v140I~
            Dim pos As Double                                          ''~v140I~
#If False Then                                                         ''~v176I~
            If dirH Then '*horizontal                                       ''~v140I~
                '*              pos = word.BoundingRect.Y                       ''~v140I~''~v147R~
                pos = baseline(ii) '*baseline                          ''~v147I~
            Else                                                       ''~v140I~
                pos = word.BoundingRect.X                       ''~v140I~
            End If                                                     ''~v140I~
#Else                                                                  ''~v176I~
            pos = axis(ii)                                             ''~v176I~
#End If                                                                ''~v176I~
            lineSpacing(ii) = Abs(pos - poso)                           ''~v140R~
            '*Trace.W("class10:chkLineStyle linespacing from prev=" & lineSpacing(ii) & ",pos=" & pos & ",poso=" & poso) ''~v140I~''~v146R~''~v155R~''~v176R~''~v178R~''~v200R~''~v201R~
            poso = pos                                                   ''~v140I~
            If ii > 0 Then                                                    ''~v138I~
                If dirHo <> dirH Then           '*direction change            ''~v138I~
                    ls(ii - 1) = ls(ii - 1) Or LS_ADD_CRLF             '*add crlf at eol''~v138R~
                    ls(ii) = ls(ii) Or (LS_CHANGE_DIR)                 '*top of dir change''~v138I~
                    '*Trace.W("chkLineStyle Direction changed ADD_CRLF ii=" & ii & ",dirH=" & dirH & ",dirHo=" & dirHo & ",txt=" + listLines.Item(ii).Text) ''~v138R~''~v140R~''~v155R~''~v176R~''~v178R~''~v200R~''~v201R~
                Else                                                   ''~v138I~
                    '*                  Trace.W("chkLineStyle Direction point linespacing=" & lineSpacing(ii) & ",bound=(" & bound1(ii) & "-" & bound2(ii) & ")") ''~v176M~''~v178R~
#If True Then                                                         ''~v176R~
                    If lineSpacing(ii) <> 0 Then  'same line             ''~v176I~
                        Dim diffh As Double = Abs(hh - hho)                ''~v138R~
                        Dim diffw As Double = Abs(ww - wwo)                ''~v138I~
                        Dim rateh As Double = diffh / hho                 ''~v138R~
                        Dim ratew As Double = diffw / wwo                 ''~v138I~
                                              '*Trace.W("chkLineStyle Direction point ww=" & ww & ",wwo=" & wwo & ",diffw=" & diffw & ",ratew=" & ratew) ''~v138R~''~v155R~''~v176R~''~v178R~''~v200R~''~v201R~
                                              '*Trace.W("chkLineStyle Direction point hh=" & hh & ",hho=" & hho & ",diffh=" & diffh & ",rateh=" & rateh) ''~v138I~''~v155R~''~v176R~''~v178R~''~v200R~''~v201R~
                        If Min(ratew, rateh) > RATE_POINT_CHANGED Then      ''~v138R~
                            '*Trace.W("chkLineStyle Direction pointchange ADD_CRLF ii=" & ii & ",type prev=" & ls(ii - 1) & ",curr=" & ls(ii))  ''~v138R~''~v155R~''~v176R~''~v178R~''~v200R~''~v201R~
                            ls(ii - 1) = ls(ii - 1) Or LS_ADD_CRLF  '* add crlf to prev line''~v138R~
                            ls(ii) = ls(ii) Or LS_CHANGE_POINT            ''~v138I~
                            '*                          Trace.W("chkLineStyle Direction pointchange  without EOL_DELM add CRLF to Prev line ii=" & ii - 1) ''~v138R~''~v145R~''~v155R~''~v176R~''~v178R~
                        End If                                             ''~v138I~
                    End If                                               ''~v176I~
#Else                                                                  ''~v176M~
                    Dim diff As Double = Abs(bound2(ii) - bound1(ii))       ''~v176M~''~v196R~
                    If lineSpacing(ii) = 0 Then  'same line            ''~v176M~
                        diffline = Max(diff, diffline)                 ''~v176M~
                    Else                                               ''~v176M~
                        If diffline = 0 Then                           ''~v176M~
                            diffline = diff                            ''~v176M~
                        End If                                         ''~v176M~
                        If difflineo = 0 Then                          ''~v176M~
                            difflineo = 1.0                            ''~v176M~
                        End If                                         ''~v176M~
                        If diffline = 0 Then                           ''~v176M~
                            diffline = 1.0                             ''~v176M~
                        End If                                         ''~v176M~
                        Dim rate1 As Double = diffline / difflineo     ''~v176M~
                        Dim rate2 As Double = difflineo / diffline     ''~v176M~
                        '*Trace.W("chkLineStyle Direction point diff=" & difflineo & "-->" & diffline & ",rate=" & Min(rate1, rate2))''~v176M~''~v178R~''~v200R~''~v201R~
                        If Min(rate1, rate2) < (1 - RATE_POINT_CHANGED) Then''~v176M~
                            '*Trace.W("chkLineStyle Direction pointchange ii=" & ii & ",ADD_CRLF type prev=" & ls(ii - 1) & ",curr=" & ls(ii))''~v176I~''~v178R~''~v200R~''~v201R~
                            ls(ii - 1) = ls(ii - 1) Or LS_ADD_CRLF  '* add crlf to prev line''~v176I~
                            ls(ii) = ls(ii) Or LS_CHANGE_POINT         ''~v176I~
                            '*Trace.W("chkLineStyle Direction pointchange  without EOL_DELM add CRLF to Prev line ii=" & ii - 1)''~v176I~''~v178R~''~v200R~''~v201R~
                        End If                                         ''~v176I~
                        difflineo = diffline                             ''~v176I~
                        diffline = 0.0                                   ''~v176I~
                    End If  '* not same line                           ''~v176I~
#End If                                                                ''~v176M~
                End If                                                 ''~v138I~
            End If                                                     ''~v138I~
            dirHo = dirH                                                 ''~v138I~
#If True Then                                                         ''~v176R~
            hho = hh
            wwo = ww                                                     ''~v138I~
#End If                                                                ''~v176I~
        Next                                                           ''~v138I~
        '*chk short line/line spacing                                                       ''~v138I~''~v140R~
        Dim grouptop As Integer = 0                                    ''~v138R~
        Dim groupmax As Double = 0.0                                    ''~v138R~
        Dim groupctr As Integer = 0                                      ''~v138I~
        For ii As Integer = 0 To ctrline - 1                               ''~v138I~
            '*          chkSameLine(ii, ls, minLineHeight, minLineWidth)            ''~v144I~''~v147R~
            chkSameLine(ii, ls, minLineHeight, minLineWidth, baseline)  ''~v147I~
            chkLineSpacing(ii, ls, lineSpacing, charH, charW)              ''~v140I~
            type = ls(ii)                                                ''~v138I~
            '*Trace.W("chkLineStyle lineno=" & ii & ",type=0x" & Hex(type) & ",txt=" & listLines.Item(ii).Text) ''~v138R~''~v140R~''~v155R~''~v200R~''~v201R~
            '*          Trace.W("chkLineStyle and=" & (type And (LS_CHANGE_DIR + LS_CHANGE_POINT))) ''~v138M~''~v155R~
            sz = linesz(ii)                                             ''~v138I~
            If (type And (LS_CHANGE_DIR + LS_CHANGE_POINT)) <> 0 Then    '*direction/charsize change''~v138R~
                '*              Trace.W("chkLineStyle grouptop=" & grouptop & ",ctr=" & groupctr) ''~v138R~''~v155R~
                If groupctr > 1 Then      '* >= 2 line group            ''~v138I~''~v140R~''~v138R~
                    chkShortLine(listLines, linesz, groupmax, grouptop, groupctr, ls) ''~v138I~
                End If                                             ''~v138I~
                grouptop = ii     '* this line's point changed with prev,but same width may follow''~v138I~
                groupmax = sz                                        ''~v138I~
                groupctr = 1                                             ''~v138R~
            Else                                                   ''~v138I~
                If groupctr = 0 Then                                   ''~v138R~
                    grouptop = ii                                        ''~v138I~
                    groupmax = sz                                      ''~v138I~
                Else                                                   ''~v138I~
                    groupmax = Max(groupmax, sz)                       ''~v138I~
                End If                                                 ''~v138I~
                groupctr += 1                                            ''~v138I~
            End If                                                 ''~v138I~
        Next                                                           ''~v138I~
        If groupctr > 1 Then      '* >= 2 line group                   ''~v138I~
            '*          Trace.W("last chkshortLine")                               ''~v138I~''~v155R~
            chkShortLine(listLines, linesz, groupmax, grouptop, groupctr, ls) ''~v138I~
        End If                                                         ''~v138I~
        lineStyle = ls                                                   ''~v138I~
    End Sub                                                            ''~v138R~
    '*************************************************************     ''~v140I~
    Private Sub chkLineSpacing(Plineno As Integer, Pls() As Integer, PlineSpacing() As Double, PcharH() As Double, PcharW() As Double) ''~v140I~
        If Plineno = 0 Then                                                     ''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        If Plineno - 1 >= Pls.Count Then                                          ''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        Dim ii As Integer = Plineno                                      ''~v140I~
        Dim type As Integer = Pls(ii)                                    ''~v140I~
        Dim typeo As Integer = Pls(ii - 1)                                 ''~v140I~
        Dim typen As Integer = Pls(ii + 1)                                 ''~v140I~
        Dim dirH As Boolean = (type And LS_HORIZONTAL) <> 0       '*horizontal''~v140I~
        Dim dirHo As Boolean = (typeo And LS_HORIZONTAL) <> 0       '*horizontal''~v140I~
        Dim dirHn As Boolean = (typen And LS_HORIZONTAL) <> 0       '*horizontal''~v140I~
        If dirH <> dirHo OrElse dirH <> dirHn Then   '3 line continued direction''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        Dim prevSpace = PlineSpacing(ii)                                 ''~v140I~
        Dim nextSpace = PlineSpacing(ii + 1)                               ''~v140I~
        Dim charsz As Double                                                     ''~v140I~
        If dirH Then                                                        ''~v140I~
            charsz = PcharH(ii)                                          ''~v140I~
        Else                                                           ''~v140I~
            charsz = PcharW(ii)                                          ''~v140I~
        End If                                                         ''~v140I~
        '*      Trace.W("chkLineSpacing dirH=" & dirH & ",charH=" & PcharH(ii) & ",charW=" & PcharW(ii) & ",charsz pixel=" & charsz) ''~v140R~''~v155R~
        charsz *= RATE_LINESPACING '*2.0 if over 1 line spacing (linespacing include the line itself''~v154I~
        '*Trace.W("chkLineSpacing dirHo=" & dirHo & ",dirH=" & dirH & ",charsz=" & charsz & ",prev=" & prevSpace & ",next=" & nextSpace & ",text=" & listLines.Item(ii).Text) ''~v140I~''~v155R~''~v200I~''~v201R~
        If prevSpace > charsz AndAlso nextSpace > charsz Then                  ''~v140I~
            '*Trace.W("chkLineSpacing changelinespace ADD_CRLF ii=" & ii)         ''~v140I~''~v155R~''~v176R~''~v178R~''~v200R~''~v201R~
            Pls(ii) = Pls(ii) Or (LS_ADD_CRLF + LS_CHANGE_LINESPACE)       ''~v140I~
        End If                                                         ''~v140I~
    End Sub                                                            ''~v140I~
    '*************************************************************     ''~v144I~
    '*   Private Sub chkSameLine(Plineno As Integer, Pls() As Integer, PminLineHeight As Double, PminLineWidth As Double) ''~v144I~''~v147R~
    Private Sub chkSameLine(Plineno As Integer, Pls() As Integer, PminLineHeight As Double, PminLineWidth As Double, Pbaseline() As Double) ''~v147I~
        If Plineno = 0 Then                                            ''~v144I~
            Exit Sub                                                   ''~v144I~
        End If                                                         ''~v144I~
        Dim ii As Integer = Plineno                                    ''~v144I~
        Dim type As Integer = Pls(ii)                                  ''~v144I~
        Dim typeo As Integer = Pls(ii - 1)                             ''~v144I~
        Dim dirH As Boolean = (type And LS_HORIZONTAL) <> 0       '*horizontal''~v144I~
        Dim dirHo As Boolean = (typeo And LS_HORIZONTAL) <> 0       '*horizontal''~v144I~
        '*      Trace.W("chkSameLine dirH=" & dirH & ",dirHo=" & dirHo & ",text=" & listLines(ii).Text) ''~v145I~''~v155R~
        If dirH <> dirHo Then    'dir changed                               ''~v144I~
            Exit Sub                                                   ''~v144I~
        End If                                                         ''~v144I~
        '*      Dim line As OcrLine = listLines(ii)                            ''~v144I~''~v192R~
        Dim line As OcrLineV2 = listLines(ii)                          ''~v192I~
        '*      Dim lineo As OcrLine = listLines(ii - 1)                         ''~v144I~''~v192R~
        Dim lineo As OcrLineV2 = listLines(ii - 1)                     ''~v192I~
        Dim word As OcrWord = line.Words.Item(0)                               ''~v144I~
        Dim wordo As OcrWord = lineo.Words.Item(0)                             ''~v144I~
        Dim base As Double                                             ''~v144I~
        Dim baseo As Double                                            ''~v144I~
        If dirH Then                                                   ''~v144I~
            '*          base = word.BoundingRect.Y                                  ''~v144I~''~v147R~
            base = Pbaseline(ii)                                       ''~v147R~
            base /= PminLineHeight                                        ''~v144I~
            '*          baseo = wordo.BoundingRect.Y                                ''~v144I~''~v147R~
            baseo = Pbaseline(ii - 1)                                  ''~v147R~
            baseo /= PminLineHeight                                       ''~v144I~
        Else   '*vline                                                 ''~v144I~
            base = word.BoundingRect.X                                  ''~v144I~
            base /= PminLineWidth                                         ''~v144I~
            baseo = wordo.BoundingRect.X                                ''~v144I~
            baseo /= PminLineWidth                                        ''~v144I~
        End If                                                         ''~v144I~
        Dim lineid As Integer = CType(Math.Floor(base), Integer)                       ''~v144R~
        Dim lineido As Integer = CType(Math.Floor(baseo), Integer)                     ''~v144R~
        '*Trace.W("chkSameLine dirH=" & dirH & ",lineid=" & lineid & ",lineido=" & lineido & ",base=" & base & ",baseo=" & baseo & ",text=" & line.Text & ",texto=" & lineo.Text) ''~v144I~''~v145R~''~v155R~''~v200R~''~v201R~
        If lineid = lineido Then                                              ''~v144I~
            '*Trace.W("class10:chkSamelineSame REP_CRLF prevtext=" & lineo.Text & ",cur text=" & line.Text)           ''~v144R~''~v155R~''~v200R~''~v201R~
            typeo = typeo Or LS_REP_CRLF                                 ''~v144I~
            Pls(ii - 1) = typeo                                          ''~v144I~
        End If                                                         ''~v144I~
    End Sub                                                            ''~v144I~
    '*************************************************************     ''~v176I~
    '* chk same line after sort using center line                      ''~v176I~
    '*************************************************************     ''~v176I~
    '*  Private Function chkSameLineAfterSort(Popt As Integer, PlistLines As List(Of OcrLine), Pls() As Integer, Pbound1() As Double, Pbound2() As Double, Paxis() As Double) As Boolean ''~v176R~''~v192R~
    Private Function chkSameLineAfterSort(Popt As Integer, PlistLines As List(Of OcrLineV2), Pls() As Integer, Pbound1() As Double, Pbound2() As Double, Paxis() As Double) As Boolean ''~v192I~
        If sortOption = 0 Then                                         ''~v177I~
            Return False                                               ''~v177I~
        End If                                                         ''~v177I~
        Dim ii As Integer = 0                                          ''~v176I~
        Dim type As Integer                                            ''~v176I~
        Dim dirH As Boolean                                            ''~v176I~
        Dim dirHo As Boolean                                           ''~v176I~
        Dim bound1 As Double                                           ''~v176R~
        Dim bound2 As Double                                           ''~v176R~
        Dim bound1o As Double                                          ''~v176R~
        Dim bound2o As Double                                          ''~v176R~
        Dim center As Double                                           ''~v176R~
        Dim centero As Double                                          ''~v176R~
        Dim swChange As Boolean = False                                  ''~v176I~
        '*      For Each line As OcrLine In PlistLines                         ''~v176I~''~v192R~
        '*printLinesWords("chkSameLineAfterSort entry", PlistLines)          ''~v200I~''~v201R~
        For Each line As OcrLineV2 In PlistLines                       ''~v192I~
            '*printLineRect("chkSameLineAfterSort", line)                 ''~v199I~''~v201R~
            type = Pls(ii)                                             ''~v176I~
            dirH = (type And LS_HORIZONTAL) <> 0       '*horizontal    ''~v176I~
            bound1 = Pbound1(ii)                                       ''~v176I~
            bound2 = Pbound2(ii)                                       ''~v176I~
            If Popt = 0 Then                                                  ''~v176I~
                center = (bound1 + bound2) / 2                         ''~v176R~
                Paxis(ii) = center                                       ''~v176I~
            Else                                                       ''~v176I~
                center = Paxis(ii)                                       ''~v176I~
            End If                                                     ''~v176I~
            '*Trace.W("Class10:chkSameLineAfterSort line=" & ii & ",type=" & type & ",lineText=" & line.Text) ''~v176R~''~v178R~''~v199R~''~v201R~
            If ii > 0 Then                                             ''~v176R~
                '*Trace.W("chkSameLineAfterSort dirH=" & dirH & ",dirHo=" & dirHo & ",bound1=" & bound1 & ",bound2=" & bound2) ''~v176R~''~v178R~''~v199R~''~v201R~
                If dirH = dirHo Then                                           ''~v176R~
                    '*Trace.W("chkSameLineAfterSort center=" & center & ",centero=" & centero & ",prev bound=" & bound1o & "," & bound2o & "curr bound=" & bound1 & "," & bound2) ''~v176R~''~v178R~''~v199R~''~v201R~
                    If Popt = 0 Then                                          ''~v176I~
                        If center > bound1o AndAlso center < bound2o AndAlso centero > bound1 AndAlso centero < bound2 Then ''~v176R~
                            '*                          Pbound1(ii) = Pbound1(ii - 1)              ''~v176R~
                            '*                          Pbound2(ii) = Pbound2(ii - 1)              ''~v176R~
                            Paxis(ii) = Paxis(ii - 1)                  ''~v176I~
                            swChange = True                            ''~v176R~
                            '*Trace.W("chkSameLineAfterSort set center same as prev axis(ii)=" & Paxis(ii)) ''~v176R~''~v178R~''~v199R~''~v201R~
                        End If                                         ''~v176R~
                    Else                                               ''~v176I~
                        If center = centero Then  '*same line                 ''~v176I~
                            '*                          Trace.W("chkSameLineAfterSort Before type prev=" & Pls(ii - 1) & ",curr=" & Pls(ii)) ''~v176I~''~v178R~
                            Pls(ii - 1) = Pls(ii - 1) And (Not LS_EOL_DELM)   'do not add CRLF''~v176I~
                            Pls(ii - 1) = Pls(ii - 1) Or LS_SKIP_CRLF '*''~v176I~
                            swChange = True                            ''~v176I~
                            '*Trace.W("chkSameLineAfterSort After type prev=" & Pls(ii - 1) & ",curr=" & Pls(ii)) ''~v176I~''~v178R~''~v199R~''~v201R~
                        End If                                         ''~v176I~
                    End If                                             ''~v176I~
                End If                                                 ''~v176I~
            End If                                                     ''~v176I~
            dirHo = dirH                                               ''~v176I~
            bound1o = bound1                                           ''~v176I~
            bound2o = bound2                                           ''~v176I~
            centero = center                                           ''~v176I~
            ii += 1                                                    ''~v176I~
        Next                                                           ''~v176I~
        '*Trace.W("chkSameLineAfterSort rc=" & swChange)                 ''~v176I~''~v178R~''~v199R~''~v201R~
        '*printLinesWords("chkSameLineAfterSort exit", PlistLines)           ''~v200I~''~v201R~
        Return swChange                                                ''~v176I~
    End Function                                                       ''~v176R~
    '*************************************************************     ''~v138I~
    '*  Private Sub chkShortLine(PlistLines As List(Of OcrLine), Plinesz() As Double, Pgroupmax As Double, Pgrouptop As Integer, Pgroupctr As Integer, Pls() As Integer) ''~v138I~''~v192R~
    Private Sub chkShortLine(PlistLines As List(Of OcrLineV2), Plinesz() As Double, Pgroupmax As Double, Pgrouptop As Integer, Pgroupctr As Integer, Pls() As Integer) ''~v192I~
#If False Then    '* same linechk insert space between OcrLine on the same line''~v145I~
'*      Trace.W("chkShortLine grouptop=" & Pgrouptop & "groupctr=" & Pgroupctr & ",groupmax=" & Pgroupmax) ''~v138I~''~v155R~
        For jj As Integer = Pgrouptop To Pgrouptop + Pgroupctr - 1         ''~v138I~
'*          Trace.W("chkShortLine grouptext=" & PlistLines.Item(jj).Text) ''~v138I~''~v155R~
            Dim ratesz As Double = Plinesz(jj) / Pgroupmax             ''~v138I~
            If ratesz < RATE_LENGTH_CHANGED Then '*short line          ''~v138I~
'*              Trace.W("chkShortLine lengthchanged len=" & Plinesz(jj) & ",groupmax=" & Pgroupmax & ",rate=" & ratesz) ''~v138I~''~v155R~
                If (Pls(jj) And LS_EOL_DELM) = 0 Then                  ''~v138I~
                    Pls(jj) = Pls(jj) Or LS_ADD_SPACE                  ''~v138I~
'*                  Trace.W("chkShortline lengthchanged addspace") ''~v138I~''~v140R~''~v155R~
                End If                                                 ''~v138I~
            End If                                                     ''~v138I~
        Next                                                           ''~v138I~
#End If                                                                ''~v145I~
    End Sub                                                            ''~v138I~
    '*************************************************************     ''~v178I~
    '*  Private Function chkWordDistance(Ptype As Integer, Pline As OcrLine, Plineno As Integer, Pword As OcrWord, Pwordo As OcrWord) As Boolean ''~v178R~''~v192R~
    Private Function chkWordDistance(Ptype As Integer, Pline As OcrLineV2, Plineno As Integer, Pword As OcrWord, Pwordo As OcrWord) As Boolean ''~v192I~
        Dim brect, brecto As Rect                                       ''~v178I~
        Dim chsz, rate, diff As Double                                   ''~v178I~
        Dim rc As Boolean = False                                        ''~v178R~
        '***********                                                       ''~v178I~
        '*printLineRect("chkWordDistance", Pline)                        ''~v196R~''~v201R~
        If Pwordo Is Nothing Then '*top word                                ''~v178I~
            Return False                                               ''~v178I~
        End If                                                         ''~v178I~
        brect = Pword.BoundingRect                                     ''~v178I~
        brecto = Pwordo.BoundingRect                                   ''~v178I~
        If (Ptype And LS_HORIZONTAL) <> 0 Then                         ''~v178I~
            diff = Abs(brect.X - brecto.X)                                 ''~v178I~
            chsz = charW(Plineno)                                        ''~v178I~
        Else                                                           ''~v178I~
            diff = brect.Y - brecto.Y                                      ''~v178I~
            chsz = charH(Plineno)                                        ''~v178I~
        End If                                                         ''~v178I~
        If chsz = 0 Then                                                      ''~v178I~
            Return False                                               ''~v178I~
        End If                                                         ''~v178I~
        rate = diff / chsz                                                 ''~v178I~
        '*Trace.W("chkWordDistance insertspace rate=" & rate & ",diff=" & diff & ",chsz=" & chsz & ",txt=" & Pword.Text & ",prev=" & Pwordo.Text) ''~v178R~''~v196R~''~v201R~
        If rate > RATE_WORDSPACING Then '*add space between word if distance is multiple of char size''~v178I~
            '*Trace.W("chkWordDistance return True")                     ''~v178R~''~v196R~''~v201R~
            rc = True                                                    ''~v178I~
        End If                                                         ''~v178I~
        Return rc                                                      ''~v178I~
    End Function                                                       ''~v178I~
    '*************************************************************     ''~v196I~
    Private Function chkWordDistanceHeader(Pline As OcrLineV2, Pword As OcrWord, Pwordo As OcrWord, PwwAverage As Double) As Boolean ''~v196R~
        Dim brect, brecto As Rect                                      ''~v196I~
        Dim chsz, diff As Double                                       ''~v196I~
        Dim rc As Boolean = False                                      ''~v196I~
        '***********                                                   ''~v196I~
        '*printLineRect("chkWordDistanceHeader", Pline)                        ''~v196I~''~v197R~''~v201R~
        If Pwordo Is Nothing Then '*top word                           ''~v196I~
            Return False                                               ''~v196I~
        End If                                                         ''~v196I~
        brect = Pword.BoundingRect                                     ''~v196I~
        brecto = Pwordo.BoundingRect                                   ''~v196I~
        diff = Abs(brect.X - brecto.X)                                 ''~v196I~''~v197R~
        '*      diff = brect.X - brecto.X-brecto.Width                         ''~v197R~
        '*      chsz = brecto.Width                                            ''~v196R~
        chsz = PwwAverage                                              ''~v196I~''~v197R~
        '*      chsz = brecto.Width/Pword.Text.length                          ''~v197R~
        '*Trace.W("class10:chkWordDistanceHeader insertspace diff=" & diff & ",chsz=" & chsz & ",txt=" & Pword.Text & ",prev=" & Pwordo.Text) ''~v196I~''~v201R~
        '*      If diff > chsz * 2 Then                                                 ''~v196I~''~v197R~
        If diff > chsz * RATE_WORDSPACING Then '* =2.5    'heder line hight vs space under header''~v197R~
            '*      If diff > chsz Then                                            ''~v197R~
            '*Trace.W("class10:chkWordDistance return True")                     ''~v196I~''~v201R~
            rc = True                                                  ''~v196I~
        End If                                                         ''~v196I~
        '*Trace.W("class10:chkWordDistanceHeader rc=" & rc & ",word=" & Pword.Text & ",PwwAverage=" & PwwAverage) ''~v197I~''~v201R~
        Return rc                                                      ''~v196I~
    End Function                                                       ''~v196I~
    '*************************************************************
    Public Function makeLines(Plen As Integer) As String
        Dim sb = New StringBuilder(Plen * 2)
        Try
            chkLineStyle()    '*set lineStyle()                        ''~v138R~
            Dim lineno As Integer = 0                                    ''~v138I~
            '*          For Each line As OcrLine In result.Lines                   ''~v140R~
            '*          For Each line As OcrLine In listLines                     ''~v140I~''~v192R~
            If swGetHeader And phaseGetHeader = PGH_GETHEADER Then           ''~v196I~
                Return makeLinesHeader(Plen)                           ''~v196I~
            End If                                                     ''~v196I~
        	'*printLinesWords("makeLines entry",listLines)               ''~v200I~''~v201R~
                                                                       ''~v200I~
            For Each line As OcrLineV2 In listLines                    ''~v192I~
                '               sb.Append(line.Text)                                   ''~v104R~
                Dim type = lineStyle(lineno)                           ''~v178I~
                '*              Trace.W("makeLines type=" & type & ",lineText=" & line.Text) ''~v138I~''~v155R~''~v176R~''~v178R~''~v201R~
                If langTag.StartsWith("ja") Then                         ''~v177I~
                    Dim wordprev As OcrWord = Nothing                    ''~v178I~
                    For Each word As OcrWord In line.Words                 ''~v104I~
                        If chkWordDistance(type, line, lineno, word, wordprev) Then ''~v178R~
                            sb.Append(" "c)                            ''~v178I~
                        End If                                         ''~v178I~
                        sb.Append(word.Text)                               ''~v104I~
                        wordprev = word                                  ''~v178I~
                    Next                                                   ''~v104I~
                Else                                                     ''~v177I~
                    sb.Append(line.Text)                                   ''~v177I~
                End If                                                   ''~v177I~
                '*              Dim type = lineStyle(lineno)                             ''~v138I~''~v178R~
                If (type And LS_REP_CRLF) <> 0 Then  '*sameline          ''~v144I~
                    sb.Append(" "c)  '* double CRLF to avoid eol concatination''~v144I~
                    '*Trace.W("makeLines type LS_REPCRLF type=0x" & Hex(type) & ",lineText=" & line.Text)''~v200I~''~v201R~
                Else                                                     ''~v144I~
                    If (type And LS_EOL_DELM) = 0 Then                     ''~v138R~
                        If (type And LS_ADD_CRLF) <> 0 Then                ''~v138R~
                            '*Trace.W("makeLines type=ADD_CRLF type=0x" & Hex(type) & ",lineText=" & line.Text)''~v200I~''~v201R~
                            sb.Append(vbCrLf)  '* double CRLF to avoid eol concatination''~v138I~
                            '*                          Trace.W("makeLines addcrlf")                  ''~v138I~''~v155R~
                        Else                                               ''~v138I~
                            If (type And LS_ADD_SPACE) <> 0 Then           ''~v138R~
                                sb.Append(" "c)  '* double CRLF to avoid eol concatination''~v138I~
                                                              '*Trace.W("makeLines addspace")             ''~v138I~''~v155R~''~v200R~''~v201R~
                            End If                                         ''~v138R~
                        End If                                             ''~v138I~
                    End If    ''~v138I~
                    If (type And LS_SKIP_CRLF) = 0 Then '*line concatinated    ''~v176I~
                        '*Trace.W("makeLines type!=SKIP_CRLF type=0x" & Hex(type) & ",lineText=" & line.Text)''~v200I~''~v201R~
                        sb.Append(vbCrLf)
                    End If                                               ''~v176I~
                End If                                                   ''~v144I~
                lineno += 1                                              ''~v138I~
            Next
        Catch ex As Exception
            '*          showStatus(CNM & "makeLines :" & ex.Message)               ''~v140R~
            Form1.exceptionMsg("OCR:makeLines", ex)                    ''~v140I~
            Return "Failed to Extract Lines"
        End Try
        Return sb.ToString()
    End Function
    '*************************************************************     ''~v196I~
    Public Function makeLinesHeader(Plen As Integer) As String         ''~v196I~
        '*printLinesWords("makeLinesHeader entry", listLines)            ''~v196I~''~v201R~
        Dim sb = New StringBuilder(Plen * 2)                           ''~v196I~
        Dim sw1st As Boolean = True                                      ''~v196I~
        For Each line As OcrLineV2 In listLines                        ''~v196I~
            If sw1st Then                                                   ''~v196I~
                sw1st = False                                            ''~v196I~
            Else                                                       ''~v196I~
                sb.Append(SPACE_HEADER_LINE)                           ''~v196I~
            End If                                                     ''~v196I~
            If langTag.StartsWith("ja") Then                           ''~v196I~
                Dim wordprev As OcrWord = Nothing                      ''~v196I~
                Dim hhAverage, wwAverage As Double                      ''~v196R~
                getAverageCharSize(line, hhAverage, wwAverage, True)           ''~v196I~''~v197R~
                For Each word As OcrWord In line.Words                 ''~v196I~
                    If chkWordDistanceHeader(line, word, wordprev, wwAverage) Then ''~v196R~
                        sb.Append(" "c)                                ''~v196I~
                    End If                                             ''~v196I~
                    sb.Append(word.Text)                               ''~v196I~
                    wordprev = word                                    ''~v196I~
                Next                                                   ''~v196I~
            Else                                                       ''~v196I~
                sb.Append(line.Text)                                   ''~v196I~
            End If                                                     ''~v196I~
        Next                                                           ''~v196I~
        If Not sw1st Then   '* line exist                                  ''~v197I~
            sb.Append(vbCrLf)                                              ''~v196I~''~v197R~
            sb.Append(vbCrLf)                                          ''~v197R~
        End If                                                         ''~v197I~
        Dim rc As String = sb.ToString()                                     ''~v196I~
        '*Trace.W("class10:makeLinesHeader text=" & rc)                  ''~v196I~''~v201R~
        Return rc                                                      ''~v196I~
    End Function                                                       ''~v196I~
    '*************************************************************
    Private Sub showStatus(Pmsg As String)                         ''~v100R~
        If statusMsg Is Nothing Then                                        ''~v100I~
            statusMsg = Pmsg                                             ''~v100R~
        End If                                                         ''~v100I~
    End Sub
#If False Then                                                              ''~v196I~
    '*************************************************************     ''~v110I~''~va04I~
    Public Sub saveCutImage(Pbmp As Bitmap, Prect As Rectangle, Pfnm As String, Pfmt As ImageFormat) ''~va04I~
        Dim cutbmp As Bitmap = cutImage(Pbmp, Prect)                   ''~va04I~
        saveImage(cutbmp, Pfnm, Pfmt)                                  ''~va04I~
    End Sub                                                            ''~va04I~
#End If                                                                ''~v196I~
    '*************************************************************     ''~va04I~
    '*  Public Function saveImage(Pbmp As Bitmap, Pfnm As String, Pfmt As ImageFormat) As String ''~va04R~''~v196R~
    Private Function saveImage(Pbmp As Bitmap, Pfnm As String, Pfmt As ImageFormat) As String ''~v196I~
        '* from form2                                                      ''~va04I~
        Dim fnm As String = ""                                           ''~va04I~
        Try                                                            ''~va04I~
            Dim ext As String = getImageFormat(Pfmt)                   ''~va04R~
            fnm = Pfnm & "." & ext                                       ''~va04R~
            Pbmp.Save(fnm, Pfmt)                                       ''~va04I~
        Catch ex As Exception                                          ''~va04I~
            Form1.exceptionMsg("OCR:SaveImage", ex)                  ''~va04I~''~v140R~
            Return ""                                                  ''~va04I~
        End Try                                                        ''~va04I~
        Return fnm                                                     ''~va04I~
    End Function                                                        ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function cutImage(Pbmp As Bitmap, Prect As Rectangle) As Bitmap ''~va04I~
        Dim bmp As Bitmap = Pbmp.Clone(Prect, Pbmp.PixelFormat)        ''~va04I~
        '*      Trace.W("cutimage Prect X=" & Prect.X & ",Y=" & Prect.Y & ",W=" & Prect.Width & ",H=" & Prect.Height)''~v155R~
        Return bmp                                                     ''~va04I~
    End Function                                                       ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function getImageFormat(Pfmt As ImageFormat) As String      ''~va04I~
        If Pfmt.Equals(ImageFormat.Jpeg) Then                          ''~va05I~
            Return "jpg"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Icon) Then                          ''~va05I~
            Return "ico"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Tiff) Then                          ''~va05I~
            Return "tif"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Png) Then                           ''~va05I~
            Return "png"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Bmp) Then                           ''~va05I~
            Return "bmp"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Gif) Then                           ''~va05I~
            Return "gif"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        Dim fmt As String = Pfmt.ToString()                            ''~va04I~
        Return fmt                                                     ''~va04I~
    End Function                                                       ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function str2Fmt(Pstrfmt As String) As ImageFormat          ''~va04I~
        If String.Compare(Pstrfmt, "bmp", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Bmp                                     ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "gif", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Gif                                     ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "jpg", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Jpeg                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "jpeg", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Jpeg                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "png", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Png                                     ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "tiff", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Tiff                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "tif", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Tiff                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "icon", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Icon                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "ico", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Icon                                    ''~va04I~
        End If                                                         ''~va04I~
        Return ImageFormat.Bmp                                         ''~va04I~
    End Function                                                       ''~va04I~
    '*************************************************************     ''~v140I~
    '*    Private Function toList(Plist As IReadOnlyList(Of OcrLine)) As List(Of OcrLine) ''~v140I~''~v192R~
    '*        Dim lst = New List(Of OcrLine)                                 ''~v140I~''~v192R~
    '*        For Each line As OcrLine In Plist                              ''~v140I~''~v192R~
    '*            lst.Add(line)                                              ''~v140I~''~v192R~
    '*        Next                                                           ''~v140I~''~v192R~
    '*        Return lst                                                     ''~v140I~''~v192R~
    '*    End Function                                                       ''~v140I~''~v192R~
    Private Function toList(Plist As IReadOnlyList(Of OcrLine)) As List(Of OcrLineV2) ''~v192I~
        Dim lst = New List(Of OcrLineV2)                               ''~v192I~
        If swGetHeader Then                                            ''~v196M~
            If phaseGetHeader = 0 Then                                 ''~v196M~
                headerBottomY = getHeaderBottom(Plist)                 ''~v196R~
                footerTopY = getFooterTopY(Plist, bmpHeight)            ''~v197R~
                If headerBottomY <> 0 Or footerTopY <> 0 Then          ''~v197R~
                    Return Nothing                                     ''~v196I~
                End If                                                 ''~v196M~
            End If                                                     ''~v196M~
        End If                                                         ''~v196M~
        If swVertical2 Then                                            ''~v192R~
            If swGetHeader And phaseGetHeader = PGH_GETHEADER Then       ''~v196I~''~v197R~
                setVertical2Copy(Plist, lst)    '* split listLines for vertical2''~v196I~
            Else                                                         ''~v196I~
                setVertical2(Plist, lst)    '* split listLines for vertical2''~v192R~
            End If                                                       ''~v196I~
        Else                                                           ''~v192I~
            If swHorizontal2 Then                                        ''~v199R~
                setHorizontal2(Plist, lst)    '* split listLines for Horizontal 2 page''~v199I~
            Else                                                         ''~v199I~
                setVertical2Copy(Plist, lst)    '* split listLines for vertical2''~v192R~
            End If                                                       ''~v199I~
        End If                                                         ''~v192I~
        Return lst                                                     ''~v192I~
    End Function                                                       ''~v192I~
    '*************************************************************         ''~v140I~
    '*  Private Sub sortLines(PlistLines As List(Of OcrLine), Pls() As Integer, Plinesz() As Double, PcharH() As Double, PcharW() As Double, PminLineHeight As Double, PminLineWidth As Double) ''~v140R~''~v147R~
    '*    Private Sub sortLines(PlistLines As List(Of OcrLine), Pls() As Integer, Plinesz() As Double, PcharH() As Double, PcharW() As Double, PminLineHeight As Double, PminLineWidth As Double, Pbaseline() As Double) ''~v147I~''~v176R~
    '*  Private Sub sortLines(PlistLines As List(Of OcrLine), Pls() As Integer, Plinesz() As Double, PcharH() As Double, PcharW() As Double, PminLineHeight As Double, PminLineWidth As Double, Pbaseline() As Double, Pbound1() As Double, Pbound2() As Double, Paxis() As Double) ''~v176R~''~v192R~
    Private Sub sortLines(PlistLines As List(Of OcrLineV2), Pls() As Integer, Plinesz() As Double, PcharH() As Double, PcharW() As Double, PminLineHeight As Double, PminLineWidth As Double, Pbaseline() As Double, Pbound1() As Double, Pbound2() As Double, Paxis() As Double) ''~v192I~
        '*Trace.W("Class10:sortLines sortOption=" & sortOption)          ''~v196I~''~v201R~
        If sortOption = 0 Then                                              ''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        Dim lineno As Integer = 0                                        ''~v140I~
        Dim sList As New SortedList(Of Integer, Integer)                ''~v140R~
        '*      For Each line As OcrLine In PlistLines                         ''~v140R~''~v192R~
        For Each line As OcrLineV2 In PlistLines                       ''~v192I~
            '*          Trace.W("sortVRTL Line text=" & line.Text)                 ''~v140I~''~v155R~
            Dim type As Integer = Pls(lineno)                   ''~v140R~
            '*          Dim wordctr As Integer = line.Words.Count                    ''~v140I~''~v155R~
            Dim word As OcrWord = line.Words.Item(0)                     ''~v140I~
            Dim brect As Rect = word.BoundingRect                        ''~v140I~
            '*          brect.Y = Pbaseline(lineno)                                  ''~v147I~''~v176R~
            Dim center As Double                                       ''~v176R~
            If Paxis Is Nothing Then                                        ''~v176I~
                center = (Pbound1(lineno) + Pbound2(lineno)) / 2       ''~v176I~
            Else                                                       ''~v176I~
                center = Paxis(lineno)                                 ''~v176R~
            End If                                                     ''~v176I~
            If (type And LS_HORIZONTAL) <> 0 Then                      ''~v176I~
                brect.Y = center                                       ''~v176R~
            Else                                                       ''~v176I~
                brect.X = center                                       ''~v176R~
            End If                                                     ''~v176I~
            '*          Trace.W("Class10:sortLines sortOption=" & sortOption & ",WordsTop: X=" & brect.X & ",base Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",btext=" & word.Text) ''~v140R~''~v155R~''~v178R~
            Dim key As Double                                          ''~v140I~
            Dim chW As Double = PminLineWidth                          ''~v140R~
            Dim chH As Double = PminLineHeight                         ''~v140R~
            Select Case sortOption                                     ''~v141I~
                Case SORT_VLINE_RTL  '*= 1       '*sort vertical line by X:largeX-->smallX''~v141I~
                    If (type And LS_HORIZONTAL) <> 0 Then                                ''~v140I~
                        '*                      key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v140R~''~v176R~
                        key = brect.Y * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v176I~
                    Else                                                       ''~v140I~
                        '*                      key = -(CType(brect.X / chW, Integer) * SORT_SCALE) + brect.Y  '*X:large minus(Right)-->small minus(Left)  and Y:up-->Down''~v140R~''~v176R~
                        key = -brect.X * SORT_SCALE + brect.Y  '*X:large minus(Right)-->small minus(Left)  and Y:up-->Down''~v176R~
                    End If                                                     ''~v140I~
                Case SORT_VLINE_LTR  '*= 2       '*sort horizontal line by Y:smallY-->largeY''~v141I~
                    If (type And LS_HORIZONTAL) <> 0 Then                      ''~v141I~
                        '*                      key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v140R~''~v176R~
                        key = brect.Y * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v176I~
                    Else                                                       ''~v141I~
                        '*                      key = -CType((SORT_SCALE - brect.X) / chW, Integer) * SORT_SCALE + brect.Y  '*X:large minus(Left)-->small minus(Right) and Y:small(Up)-->Down''~v140R~''~v176R~
                        key = -(SORT_SCALE - brect.X) * SORT_SCALE + brect.Y  '*X:large minus(Left)-->small minus(Right) and Y:small(Up)-->Down''~v176I~
                    End If                                                     ''~v141I~
                Case SORT_HLINE_RTL  '*= 3       '*sort horizontal line by Y:smallY-->largeY''~v141I~''~v140R~
                    If (type And LS_HORIZONTAL) <> 0 Then              ''~v141I~
                        '*                      key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:large minus(Right)->Left''~v140R~''~v176R~
                        key = brect.Y * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:large minus(Right)->Left''~v176I~
                    Else                                               ''~v141I~
                        '*                      key = CType((SORT_SCALE - brect.X) / chW, Integer) * SORT_SCALE + brect.Y   '*X:small(Right)->Left Y:small(Up)-->Down''~v140R~''~v176R~
                        key = (SORT_SCALE - brect.X) * SORT_SCALE + brect.Y   '*X:small(Right)->Left Y:small(Up)-->Down''~v176I~
                    End If                                             ''~v141I~
                Case SORT_HLINE_LTR  '*= 4       '*sort horizontal line by Y:smallY-->largeY''~v141I~''~v140R~
                    If (type And LS_HORIZONTAL) <> 0 Then                      ''~v141I~
                        '*                      key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:small(Left)->Right''~v140R~''~v176R~
                        key = brect.Y * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:small(Left)->Right''~v176I~
                    Else                                                       ''~v141I~
                        '*                      key = CType((SORT_SCALE + brect.X) / chW, Integer) * SORT_SCALE + brect.Y   '*Y:small(Up)->Down, X:large minus(Right)->Left      '*after vertical line''~v140R~''~v176R~
                        key = (SORT_SCALE + brect.X) * SORT_SCALE + brect.Y   '*Y:small(Up)->Down, X:large minus(Right)->Left      '*after vertical line''~v176I~
                    End If                                                     ''~v141I~
                Case SORT_V2_R2L         '*=5 vertical 2 part in page R2L''~v192R~
                    If (type And LS_HORIZONTAL) <> 0 Then              ''~v192I~
                        key = brect.Y * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v192I~
                    Else                                               ''~v192I~
                        key = -brect.X * SORT_SCALE + brect.Y  '*X:large minus(Right)-->small minus(Left)  and Y:up-->Down''~v192I~
                    End If                                             ''~v192I~
                    key += line.posLayout * SORT_SCALE_V2POS               ''~v192I~
                Case SORT_V2_L2R         '*=6 vertical 2 part in page L2R''~v192I~
                    If (type And LS_HORIZONTAL) <> 0 Then              ''~v192I~
                        key = brect.Y * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v192I~
                    Else                                               ''~v192I~
                        key = -(SORT_SCALE - brect.X) * SORT_SCALE + brect.Y  '*X:large minus(Left)-->small minus(Right) and Y:small(Up)-->Down''~v192I~
                    End If                                             ''~v192I~
                    Dim pos As Integer                                 ''~v192I~
                    pos = line.posLayout                                 ''~v192I~
                    If (pos And LP_LOWER) = LP_LOWER Then '*lower           ''~v192I~
                        pos -= LP_LOWER       'reverse Lower/Upper      ''~v192I~
                    End If                                             ''~v192I~
                    key += -pos * SORT_SCALE_V2POS                         ''~v192R~
                Case SORT_H2_L2R         '*=7 vertical 2 part in page L2R''~v199I~
                    '*                  If (type And LS_HORIZONTAL) <> 0 Then              ''~v199R~
                    If (type And LS_VERTICAL) = 0 Then                   ''~v199I~
                        '*                      key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:small(Left)->Right''~v199I~
                        key = brect.Y * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:small(Left)->Right''~v199I~
                    Else                                               ''~v199I~
                        '*                      key = CType((SORT_SCALE + brect.X) / chW, Integer) * SORT_SCALE + brect.Y   '*Y:small(Up)->Down, X:large minus(Right)->Left      '*after vertical line''~v199I~
                        key = (SORT_SCALE + brect.X) * SORT_SCALE + brect.Y   '*Y:small(Up)->Down, X:large minus(Right)->Left      '*after vertical line''~v199I~
                    End If                                             ''~v199I~
                    Dim pos As Integer                                 ''~v199I~
                    pos = line.posLayout                               ''~v199I~
                    If pos = LP_LEFT Then                                    ''~v199I~
                        key += -SORT_SCALE * 2 * SORT_SCALE            ''~v199R~
                    End If                                             ''~v199I~
                    '*Trace.W("Class10:sortLines H2L2R pos=" & pos & ",lineStyle=0x" & Hex(line.lineStyle) & ",key=" & key & ",text=" & line.Text)''~v199R~''~v201R~
                Case SORT_H2_R2L         '*=8 vertical 2 part in page L2R''~v199I~
                    '*                  If (type And LS_HORIZONTAL) <> 0 Then              ''~v199R~
                    If (type And LS_VERTICAL) = 0 Then                ''~v199R~
                        '*                      key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:large minus(Right)->Left''~v199I~
                        key = brect.Y * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:large minus(Right)->Left''~v199I~
                    Else                                               ''~v199I~
                        '*                      key = CType((SORT_SCALE - brect.X) / chW, Integer) * SORT_SCALE + brect.Y   '*X:small(Right)->Left Y:small(Up)-->Down''~v199I~
                        key = (SORT_SCALE - brect.X) * SORT_SCALE + brect.Y   '*X:small(Right)->Left Y:small(Up)-->Down''~v199I~
                    End If                                             ''~v199I~
                    Dim pos As Integer                                 ''~v199I~
                    pos = line.posLayout                               ''~v199I~
                    If pos = LP_LEFT Then                                    ''~v199I~
                        key += SORT_SCALE * 2 * SORT_SCALE             ''~v199R~
                    End If                                             ''~v199I~
                Case SORT_H2_ENGLISH     '*=9 vertical 2 part in page L2R for English''~v200I~
                    '*                  If (type And LS_HORIZONTAL) <> 0 Then''~v200I~
                    If (type And LS_VERTICAL) = 0 Then                 ''~v200I~
                        '*                      key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:small(Left)->Right''~v200I~
                        key = brect.Y * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:small(Left)->Right''~v200I~
                    Else                                               ''~v200I~
                        '*                      key = CType((SORT_SCALE + brect.X) / chW, Integer) * SORT_SCALE + brect.Y   '*Y:small(Up)->Down, X:large minus(Right)->Left      '*after vertical line''~v200I~
                        key = (SORT_SCALE + brect.X) * SORT_SCALE + brect.Y   '*Y:small(Up)->Down, X:large minus(Right)->Left      '*after vertical line''~v200I~
                    End If                                             ''~v200I~
                    Dim pos As Integer                                 ''~v200I~
                    pos = line.posLayout                               ''~v200I~
                    If pos = LP_LEFT Then                              ''~v200I~
                        key += -SORT_SCALE * 2 * SORT_SCALE            ''~v200I~
                    End If                                             ''~v200I~
                    '*Trace.W("Class10:sortLines H2L2R pos=" & pos & ",lineStyle=0x" & Hex(line.lineStyle) & ",key=" & key & ",text=" & line.Text)''~v200I~''~v201R~
            End Select                                                   ''~v141I~
            Dim intkey As Integer = CType(key, Integer)                   ''~v140I~
            '*          Trace.W("sort key=" & intkey & ",chW=" & chW & ",chH=" & chH & ",text=" & line.Text) ''~v140R~''~v155R~''~v178R~
            Try                                                        ''~v140I~
                sList.Add(intkey, lineno)                              ''~v140R~
            Catch ex As Exception                                      ''~v140I~
                Form1.showStatus("OCR:sortLine :" & ex.Message)        ''~v140I~
            End Try                                                    ''~v140I~
            lineno += 1                                                ''~v140I~
        Next                                                           ''~v140I~
        '*recreate sorted listLines                                        ''~v140I~
        '*      Dim listlines As List(Of OcrLine) = New List(Of OcrLine)(PlistLines) ''~v140R~''~v192R~
        Dim listlines As List(Of OcrLineV2) = New List(Of OcrLineV2)(PlistLines) ''~v192I~
        Dim ls() As Integer = CType(Pls.Clone(), Integer())             ''~v140I~
        Dim linesz() As Double = CType(Plinesz.Clone(), Double())       ''~v140I~
        Dim charH() As Double = CType(PcharH.Clone(), Double())         ''~v140I~
        Dim charW() As Double = CType(PcharW.Clone(), Double())         ''~v140I~
        Dim bound1() As Double = CType(Pbound1.Clone(), Double())      ''~v176I~
        Dim bound2() As Double = CType(Pbound2.Clone(), Double())      ''~v176I~
        Dim axis() As Double = Nothing                                   ''~v176R~
        If Paxis IsNot Nothing Then                                    ''~v176I~
            axis = CType(Paxis.Clone(), Double())                      ''~v176R~
        End If                                                         ''~v176I~
        Dim ii As Integer = 0                                            ''~v140I~
        Dim baseline() As Double = CType(Pbaseline.Clone(), Double())  ''~v147I~
        For Each ent As KeyValuePair(Of Integer, Integer) In sList       ''~v140I~
            Dim idx As Integer = CType(ent.Value, Integer)             ''~v140R~
            PlistLines(ii) = listlines.Item(idx)                         ''~v140I~
            Pls(ii) = ls(idx)                                            ''~v140I~
            Plinesz(ii) = linesz(idx)                                    ''~v140I~
            PcharH(ii) = charH(idx)                                      ''~v140I~
            PcharW(ii) = charW(idx)                                      ''~v140I~
            Pbaseline(ii) = baseline(idx)                              ''~v147I~
            Pbound1(ii) = bound1(idx)                                  ''~v176I~
            Pbound2(ii) = bound2(idx)                                  ''~v176I~
            If Paxis IsNot Nothing Then                                ''~v176I~
                Paxis(ii) = axis(idx)                                  ''~v176R~
                '*              Trace.W("axis=" & Paxis(ii))                           ''~v176I~''~v178R~
            End If                                                     ''~v176I~
            '*          Trace.W("Sorted ii=" & ii & ",type=" & Pls(ii) & ",linesz=" & Plinesz(ii) & ",charH=" & PcharH(ii) & ",charW=" & PcharW(ii)) ''~v140R~''~v155R~
            '*          Trace.W("type=" & Pls(ii) & ",bound1=" & Pbound1(ii) & ",bound2=" & Pbound2(ii) & ",text=" & PlistLines(ii).Text) ''~v146I~''~v147R~''~v155R~''~v176R~''~v178R~
            ii += 1                                                      ''~v140I~
        Next                                                           ''~v140I~
        '*printLinesWords("sortLines exit", PlistLines)                       ''~v196I~''~v201R~
    End Sub                                                    ''~v140I~
    '*************************************************************     ''~v196I~
    Private Function getCenterY(Plist As IReadOnlyList(Of OcrLine), PminTop As Double, PmaxBottom As Double) As Double ''~v196I~
        Dim centerY, lowerTop, upperBottom, lowerTopMin, upperBottomMax As Double ''~v196I~
        Dim hhMax, wwMax As Double                                      ''~v196R~
        Dim wordTop, wordLast As OcrWord                                ''~v196I~
        Dim brect, brectTop, brectLast, brectPrev As Rect                 ''~v196I~
        '*************************************                         ''~v196I~
        Dim boundary As Double = PminTop + (PmaxBottom - PminTop) * 0.45    '*chk line contains  upper and lower''~v196I~
        '*Trace.W("class10:getCenterY minTop=" & PminTop & ",maxBottom=" & PmaxBottom & ",boundary=" & boundary) ''~v196I~''~v201R~
        upperBottomMax = 0.0                                             ''~v196I~
        lowerTopMin = 0.0                                                ''~v196I~
        For Each line As OcrLine In Plist                              ''~v196I~
            Dim ctrword As Integer = line.Words.Count                  ''~v196I~
            If ctrword = 1 Then                                               ''~v196I~
                Continue For                                           ''~v196I~
            End If                                                     ''~v196I~
            wordTop = line.Words.Item(0)                                 ''~v196I~
            wordLast = line.Words.Item(ctrword - 1)                        ''~v196I~
            brectTop = wordTop.BoundingRect                              ''~v196I~
            brectLast = wordLast.BoundingRect                            ''~v196I~
            If brectTop.Y > boundary Then                                     ''~v196I~
                lowerTop = brectTop.Y                                    ''~v196I~
                upperBottom = 0.0                                        ''~v196I~
                '*Trace.W("class10:getCenterY lower line top=" & lowerTop & ",boundary=" & boundary) ''~v196I~''~v201R~
                '*printLineRect("getCenterY", line)                      ''~v196I~''~v201R~
            ElseIf brectTop.Y < boundary And brectLast.Y > boundary Then 'override boundary''~v196I~
                '*search max gap                                       ''~v196I~
                Dim maxGap As Double = 0.0                               ''~v196I~
                lowerTop = 0.0                                           ''~v196I~
                upperBottom = 0.0                                        ''~v196I~
                '*  			getAverageCharSize(line,hhAverage,wwAverage,true)           ''~v196R~''~v197R~
                getMaxCharSize(line, hhMax, wwMax)                       ''~v196I~
                For Each word As OcrWord In line.Words                 ''~v196I~
                    brect = word.BoundingRect                            ''~v196I~
                    If maxGap = 0.0 Then                                      ''~v196I~
                        maxGap = brect.Height '*temporally             ''~v196I~
                    Else                                               ''~v196I~
                        Dim diff = Abs(brect.Y - brectPrev.Y)          ''~v196R~
                        '*                      If diff > hhMax * 2 And diff > maxGap Then    ''~v196R~''~v197R~
                        If diff > hhMax * RATE_SPACE_HEADER And diff > maxGap Then ''~v197I~
                            maxGap = diff                              ''~v196R~
                            lowerTop = brect.Y                           ''~v196I~
                            upperBottom = brectPrev.Y + brectPrev.Height   ''~v196I~
                        End If                                         ''~v196I~
                    End If                                             ''~v196I~
                    brectPrev = brect                                  ''~v196I~
                Next                                                   ''~v196I~
                '*Trace.W("class10:getCenterY upper and lower line top=" & lowerTop & ",upperBotom=" & upperBottom & ",boundaty=" & boundary) ''~v196I~''~v201R~
                '*printLineRect("getCenterY", line)                      ''~v196I~''~v201R~
            Else                                                       ''~v196I~
                Continue For                                           ''~v196I~
            End If                                                      ''~v196I~
            If upperBottom > upperBottomMax Then                               ''~v196I~
                upperBottomMax = upperBottom                             ''~v196I~
                '*Trace.W("class10:getCenterY upperBottomMax=" & upperBottomMax) ''~v196I~''~v201R~
            End If                                                     ''~v196I~
            If lowerTopMin = 0.0 Or lowerTop < lowerTopMin Then                 ''~v196I~
                lowerTopMin = lowerTop                                   ''~v196I~
                '*Trace.W("class10:getCenterY lowerTopMin=" & lowerTopMin) ''~v196I~''~v201R~
            End If                                                     ''~v196I~
        Next                                                           ''~v196I~
        If lowerTopMin > upperBottomMax Then     '* gap found                 ''~v196I~
            centerY = (lowerTopMin + upperBottomMax) / 2                     ''~v196I~
        Else                                                           ''~v196I~
            centerY = (PminTop + PmaxBottom) / 2                             ''~v196I~
        End If                                                         ''~v196I~
        '*Trace.W("Class10:getCenterY centerY=" & centerY & ",lowerTopMin=" & lowerTopMin & ",upperbottomMax=" & upperBottomMax & ",PminTop=" & PminTop & ",PmaxBottom=" & PmaxBottom) ''~v196I~''~v201R~
        Return centerY                                                 ''~v196I~
    End Function                                                       ''~v196I~
    '*************************************************************     ''~v192M~
    Private Sub setVertical2(Plist As IReadOnlyList(Of OcrLine), PlistV2 As List(Of OcrLineV2)) ''~v192R~
        Dim brect As Rect                                              ''~v192M~
        Dim ww As Double                                               ''~v192M~
        Dim hh As Double                                               ''~v192M~
        Dim diffMin As Double                                          ''~v192M~
        Dim topY As Double                                             ''~v192M~
        Dim ctrTopY As Integer = 0                                     ''~v192R~
        Dim topYAll As Double                                          ''~v192M~
        Dim topYAve As Double                                          ''~v192I~
        Dim minLeft As Double = 0.0                                      ''~v192R~
        Dim maxRight As Double = 0.0                                     ''~v192R~
        Dim minTop As Double = 0.0                                       ''~v192R~
        Dim maxBottom As Double = 0.0                                    ''~v192R~
        Dim centerX As Double                                          ''~v192R~
        Dim centerY As Double                                          ''~v192I~
        '*************************************                         ''~v192M~
        '*** determine boundary of upper and lower page of vertical 2  ''~v192R~
        '*count chars                                                  ''~v192M~
        Dim ctrCharsMax As Integer = 0                                 ''~v192R~
        For Each line As OcrLine In Plist                              ''~v192R~
            Dim ctrword As Integer = line.Words.Count                  ''~v192M~
            Dim ctrChars As Integer = 0                                ''~v192M~
            For Each word As OcrWord In line.Words                     ''~v192M~
                ctrChars += word.Text.Length                           ''~v192M~
                brect = word.BoundingRect                              ''~v192I~
                If minLeft = 0.0 Or minLeft > brect.X Then                      ''~v192R~
                    minLeft = brect.X                                    ''~v192R~
                End If                                                 ''~v192I~
                Dim right As Double = brect.X + brect.Width                ''~v192R~
                If right > maxRight Then                                    ''~v192I~
                    maxRight = right                                     ''~v192R~
                End If                                                 ''~v192I~
                Dim bottom As Double = brect.Y + brect.Height              ''~v192R~
                If bottom > maxBottom Then                                  ''~v192I~
                    maxBottom = bottom                                   ''~v192I~
                End If                                                 ''~v192I~
                If minTop = 0.0 Or minTop > brect.Y Then                        ''~v192R~
                    minTop = brect.Y                                     ''~v192I~
                End If                                                 ''~v192I~
            Next                                                       ''~v192M~
            If ctrChars > ctrCharsMax Then                             ''~v192R~
                ctrCharsMax = ctrChars                                 ''~v192R~
            End If                                                     ''~v192M~
        Next                                                           ''~v192M~
        '*Trace.W("Class10:setVertical2 maxRight=" & maxRight & ",minLeft=" & minLeft & ",minTop=" & minTop & ",maxBottom=" & maxBottom) ''~v192R~''~v201R~
        '*      centerX = (maxRight + minLeft) / 2                                   ''~v192R~''~v199R~
        centerX = getCenterXVertical(Plist, minLeft, maxRight)            ''~v199I~
        '*      centerY = (minTop + maxBottom) / 2                                   ''~v192I~''~v196R~
        centerY = getCenterY(Plist, minTop, maxBottom)                   ''~v196R~
        '*Trace.W("Class10:setVertical2 center X=" & centerX & ",Y=" & centerY) ''~v192I~''~v201R~
        If False Then                                                          ''~v192I~
            Dim ctrCharsHalf As Integer = CType(ctrCharsMax / 2, Integer)   ''~v192R~
            '*** search boundary                                           ''~v192M~
            For Each line As OcrLine In Plist                              ''~v192R~
                '*Trace.W("Class10:setVertical2 line text=" & line.Text)     ''~v192R~''~v201R~
                Dim ctrword As Integer = line.Words.Count                  ''~v192M~
                Dim ctrChars As Integer = 0                                ''~v192M~
                For Each word As OcrWord In line.Words                     ''~v192M~
                    ctrChars += word.Text.Length                           ''~v192M~
                Next                                                       ''~v192M~
                If ctrChars < ctrCharsHalf Then                            ''~v192R~
                    '*Trace.W("Class10:setVertical2 ctrChar=" & ctrChars & ",text=" & line.Text) ''~v192R~''~v201R~
                    Continue For                                           ''~v192M~
                End If                                                     ''~v192M~
                '*average char hight                                           ''~v192I~
                Dim hhTotal As Double = 0.0                                ''~v192R~
                Dim ctrCharTotal As Integer = 0                              ''~v192I~
                For Each word As OcrWord In line.Words                     ''~v192I~
                    brect = word.BoundingRect                              ''~v192I~
                    hhTotal += brect.Height                                  ''~v192I~
                    ctrCharTotal += word.Text.Length                         ''~v192I~
                Next                                                       ''~v192I~
                '*              diffMin = hhTotal / ctrCharTotal * 2                       ''~v192R~''~v197R~
                diffMin = hhTotal / ctrCharTotal * RATE_SPACE_HEADER   ''~v197I~
                Dim idxWord As Integer = 0                                 ''~v192M~
                Dim idxWordTop As Integer = 0                                ''~v192I~
                Dim prevBottom As Double = 0.0                             ''~v192M~
                topY = 0.0                                                 ''~v192M~
                For Each word As OcrWord In line.Words                     ''~v192M~
                    brect = word.BoundingRect                              ''~v192M~
                    hh = brect.Height                                      ''~v192M~
                    ww = brect.Width                                       ''~v192M~
                    Dim wordBottom As Double = brect.Y + hh                ''~v192M~
                    hh /= word.Text.Length                                 ''~v192M~
                    '*Trace.W("Class10:setVertical2 X=" & brect.X & ",Y=" & brect.Y & ",wordBottom=" & wordBottom & ",prev=" & prevBottom & ",text=" & word.Text) ''~v192R~''~v201R~
                    If idxWord <> 0 Then                                   ''~v192M~
                        Dim diffY As Double = Abs(brect.Y - prevBottom)         ''~v192R~''~v196R~
                        '*Trace.W("Class10:setVertical2 diffY=" & diffY & ",Min=" & diffMin) ''~v192R~''~v201R~
                        If diffY > diffMin Then                                 ''~v192M~
                            If (topY = 0.0) Then                                ''~v192M~
                                idxWordTop = idxWord                         ''~v192I~
                                topY = brect.Y                             ''~v192M~
                                '*Trace.W("Class10:setVertical2 topY=" & topY & ",idxWordTop=" & idxWordTop) ''~v192R~''~v201R~
                            Else                                           ''~v192M~
                                If idxWordTop <= 1 Then           '*title       ''~v192I~
                                    '*Trace.W("Class10:setVertical2 reset after header topY=" & brect.Y & ",oldTop=" & topY) ''~v192R~''~v201R~
                                    idxWordTop = idxWord                     ''~v192I~
                                    topY = brect.Y                         ''~v192I~
                                Else                                       ''~v192I~
                                    If idxWordTop + 1 >= line.Words.Count Then         '*footer''~v192R~
                                        '*Trace.W("Class10:setVertical2 ignore footer topY=" & brect.Y & ",oldTop=" & topY) ''~v192R~''~v201R~
                                    Else                                   ''~v192I~
                                        '*Trace.W("Class10:setVertical2 Dup topY=" & topY) ''~v192R~''~v201R~
                                        topY = 0.0                         ''~v192R~
                                        Exit For                           ''~v192R~
                                    End If                                 ''~v192R~
                                End If                                     ''~v192R~
                            End If                                         ''~v192M~
                        End If                                             ''~v192M~
                    End If                                                 ''~v192M~
                    idxWord += 1                                             ''~v192R~
                    prevBottom = wordBottom                                  ''~v192R~
                Next                                                       ''~v192M~
                If topY <> 0.0 Then                                        ''~v192R~
                    ctrTopY += 1                                             ''~v192R~
                    topYAll += topY                                        ''~v192M~
                    '*Trace.W("Class10:setVertical2 ctrTopY=" & ctrTopY & ",text=" & line.Text) ''~v192R~''~v201R~
                End If                                                     ''~v192M~
            Next                                                           ''~v192M~
            topYAve = topYAll / ctrTopY                                    ''~v192M~
        Else                                                             ''~v192I~
            topYAve = centerY                                              ''~v192I~
        End If    '*false                                                ''~v192I~
        '*Trace.W("Class10:setVertical2 topYAve=" & topYAve & ",ctrTopY=" & ctrTopY) ''~v192R~''~v201R~
        Dim listLinesUpper As New List(Of OcrLineV2)                   ''~v192R~
        Dim listLinesLower As New List(Of OcrLineV2)                   ''~v192R~
        For Each line As OcrLine In Plist                              ''~v192R~
            Dim wordsUpper As New List(Of OcrWord)                     ''~v192I~
            Dim wordsLower As New List(Of OcrWord)                     ''~v192I~
            Dim swUpper As Boolean = False                               ''~v192I~
            Dim swLower As Boolean = False                               ''~v192I~
            Dim swLeft As Boolean = False                              ''~v192I~
            '*Trace.W("Class10:setVertical2 split Lower/Upper text=" & line.Text) ''~v192I~''~v201R~
            For Each word As OcrWord In line.Words                     ''~v192M~
                brect = word.BoundingRect                              ''~v192R~
                If (brect.Y > topYAve) Then                                 ''~v192M~
                    wordsLower.Add(word)                               ''~v192R~
                    swLower = True                                       ''~v192I~
                    '*Trace.W("Class10:setVertical2 Lower text=" & word.Text & ",topYAve=" & topYAve & ",rectY=" & brect.Y & ",rectX=" & brect.X) ''~v192R~''~v201R~
                Else                                                   ''~v192M~
                    wordsUpper.Add(word)                               ''~v192R~
                    swUpper = True                                       ''~v192I~
                    '*Trace.W("Class10:setVertical2 Upper text=" & word.Text & ",topYAve=" & topYAve & ",rectY=" & brect.Y & ",rectX=" & brect.X) ''~v192R~''~v201R~
                End If                                                 ''~v192M~
                If brect.X < centerX Then                              ''~v192I~
                    swLeft = True                                        ''~v192I~
                End If                                                 ''~v192I~
            Next                                                       ''~v192M~
            If swUpper Then                                            ''~v192I~
                Dim v2 As New OcrLineV2                                ''~v192I~
                v2.Words = wordsUpper                                  ''~v192I~
                v2.posLayout = LP_UPPER                                  ''~v192R~
                If swLeft Then                                              ''~v192I~
                    v2.posLayout += LP_LEFT                              ''~v192I~
                End If                                                 ''~v192I~
                listLinesUpper.Add(v2)                                 ''~v192I~
            End If                                                     ''~v192I~
            If swLower Then                                            ''~v192I~
                Dim v2 As New OcrLineV2                                ''~v192I~
                v2.Words = wordsLower                                  ''~v192I~
                v2.posLayout = LP_LOWER                                  ''~v192R~
                If swLeft Then                                              ''~v192I~
                    v2.posLayout += LP_LEFT                              ''~v192I~
                End If                                                 ''~v192I~
                listLinesLower.Add(v2)                                 ''~v192I~
            End If                                                     ''~v192I~
        Next                                                           ''~v192M~
        Dim listLinesV2 As New List(Of OcrLineV2)                      ''~v192R~
        For Each line As OcrLineV2 In listLinesUpper                   ''~v192R~
            PlistV2.Add(line)                                          ''~v192R~
        Next                                                           ''~v192M~
        For Each line As OcrLineV2 In listLinesLower                   ''~v192R~
            PlistV2.Add(line)                                          ''~v192R~
        Next                                                           ''~v192M~
        For Each line As OcrLineV2 In PlistV2                          ''~v192R~
            Dim sbText As System.Text.StringBuilder = New System.Text.StringBuilder() ''~v192R~
            For Each word As OcrWord In line.Words                     ''~v192M~
                sbText.Append(word.Text)                               ''~v192M~
            Next                                                       ''~v192M~
            line.Text = sbText.ToString()                                ''~v192M~
            '*Trace.W("Class10:setVertical2 line Text=" & line.Text)     ''~v192R~''~v201R~
        Next                                                           ''~v192M~
    End Sub                                                            ''~v192M~
    '*************************************************************     ''~v192I~
    Private Sub setVertical2Copy(Plist As IReadOnlyList(Of OcrLine), PlistV2 As List(Of OcrLineV2)) ''~v192R~
        For Each line As OcrLine In Plist                              ''~v192I~
            '*Trace.W("Class10:setVertical2Copy line wordCount=" & line.Words.Count & ",Text=" & line.Text) ''~v192R~''~v201R~
            Dim v2 As New OcrLineV2                                    ''~v192I~
            v2.Text = line.Text                                          ''~v192I~
            '*      	v2.Words=CType(line.Words,List(Of OCRWord))                ''~v192R~
            '*          Dim wordV2 As New List(Of OcrWord)(New OcrWord(line.Words.Count) {})''~v192R~
            Dim wordV2 As New List(Of OcrWord)                         ''~v192R~
            Dim idxWord As Integer = 0                                   ''~v192I~
            For Each word As OcrWord In line.Words                     ''~v192I~
                '*              wordV2(idxWord) = word                                 ''~v192R~
                wordV2.Add(word)                                       ''~v192I~
                '*Trace.W("Class10:setVertical2Copy countWord=" & wordV2.Count & ",idxWord=" & idxWord & ",words=" & word.Text) ''~v192R~''~v201R~
                idxWord += 1                                             ''~v192I~
            Next                                                       ''~v192I~
            v2.Words = wordV2                                          ''~v192I~
            PlistV2.Add(v2)                                            ''~v192I~
        Next                                                           ''~v192I~
    End Sub                                                            ''~v192I~
    '************************************************************************************''~v163I~''~v192I~
    Class OcrLineV2                                                    ''~v192I~
        Public Text As String                                          ''~v192I~
        Public Words As List(Of OcrWord)                              ''~v192I~
        Public lineStyle As Integer                                    ''~v196R~
        Public posLayout As Integer         '* layout position      ''~v196I~
    End Class                                                          ''~v163I~''~v192I~
    '************************************************************************************''~v196I~
    '************************************************************************************''~v196I~
    '************************************************************************************''~v196I~
    '*************************************************************     ''~v196M~
    Private Sub printLinesWords(Ptitle As String, Plist As List(Of OcrLine)) ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printLinesWords*StartL " & Ptitle & " *** ctr=" & Plist.Count) ''~v196M~
        Dim lineNo As Integer = 0                                        ''~v196M~
        For Each line As OcrLine In Plist                              ''~v196M~
            Trace.W("Class10:printLines lineNo=" & lineNo & ",Text=" & line.Text) ''~v196M~
            printWords(CType(line.Words, List(Of OcrWord)))            ''~v196M~
            lineNo += 1                                                  ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printLinesWords*EndL")                        ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v197I~
    Private Sub printLinesWordsRect(Ptitle As String, Plist As List(Of OcrLine)) ''~v197I~
#If DEBUG Then                                                         ''~v197I~
        Trace.W("Class10:printLinesWordsRect*StartL " & Ptitle & " *** ctr=" & Plist.Count) ''~v197I~
        Dim lineNo As Integer = 0                                      ''~v197I~
        For Each line As OcrLine In Plist                              ''~v197I~
            Trace.W("Class10:printLines lineNo=" & lineNo & ",Text=" & line.Text) ''~v197I~
            '*printLineRect("printLinesWordsRect", line)                  ''~v197R~''~v201R~
            lineNo += 1                                                ''~v197I~
        Next                                                           ''~v197I~
        Trace.W("Class10:printLinesWords*EndL")                        ''~v197I~
#End If                                                                ''~v197I~
    End Sub                                                            ''~v197I~
    '*************************************************************     ''~v196M~
    Private Sub printLinesWords(Ptitle As String, Plist As IReadOnlyList(Of OcrLine)) ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printLinesWords*ReadOnly*StartL " & Ptitle & " *** ctr=" & Plist.Count) ''~v196M~
        Dim lineNo As Integer = 0                                        ''~v196M~
        For Each line As OcrLine In Plist                              ''~v196M~
            Trace.W("Class10:printLines lineNo=" & lineNo & ",Text=" & line.Text) ''~v196M~
            '*            printWords(CType(line.Words, List(Of OcrWord)))''~v196M~
            printWords(line.Words)                                     ''~v196M~
            lineNo += 1                                                  ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printLinesWords*ReadOnly*EndL")               ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v197I~
    Private Sub printLinesWordsRect(Ptitle As String, Plist As IReadOnlyList(Of OcrLine)) ''~v197I~
#If DEBUG Then                                                         ''~v197I~
        Trace.W("Class10:printLinesWordsRect*ReadOnly*StartL " & Ptitle & " *** ctr=" & Plist.Count) ''~v197I~
        Dim lineNo As Integer = 0                                      ''~v197I~
        For Each line As OcrLine In Plist                              ''~v197I~
            Trace.W("Class10:printLines lineNo=" & lineNo & ",Text=" & line.Text) ''~v197I~
            '*            printWords(CType(line.Words, List(Of OcrWord)))''~v197I~
            '*printLineRect("printLinesWordsRect", line)                  ''~v197R~''~v201R~
            lineNo += 1                                                ''~v197I~
        Next                                                           ''~v197I~
        Trace.W("Class10:printLinesWords*ReadOnly*EndL")               ''~v197I~
#End If                                                                ''~v197I~
    End Sub                                                            ''~v197I~
    '*************************************************************     ''~v196M~
    Private Sub printLinesWords(Ptitle As String, Plist As List(Of OcrLineV2)) ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printLinesWords*StartL " & Ptitle & " *** ctr=" & Plist.Count) ''~v196M~
        Dim lineNo As Integer = 0                                        ''~v196M~
        For Each line As OcrLineV2 In Plist                            ''~v196M~
            Trace.W("Class10:printLines lineNO=" & lineNo & ",lineStyle=0x" & Hex(line.lineStyle) & ",poslayout=" & line.posLayout & ",Text=" & line.Text) ''~v196M~
            printWords(line.Words)                                     ''~v196M~
            lineNo += 1                                                  ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printLinesWords*EndL")                        ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printLine(Ptitle As String, Pline As OcrLineV2)        ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printLine*** " & Ptitle & ",lineStyle=0x" & Hex(Pline.lineStyle) & ",posLayout=" & Pline.posLayout & ",Text=" & Pline.Text) ''~v196M~
        printWords(Pline.Words)                                        ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printLineRect(Ptitle As String, Pline As OcrLine)      ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printLineRect " & Ptitle & ",Text=" & Pline.Text) ''~v196M~
        printWordsRect(Pline.Words)                                    ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printLineRect(Ptitle As String, Pline As OcrLineV2)    ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printLineRect*** " & Ptitle & " Text=" & Pline.Text) ''~v196M~
        printWordsRect(Pline.Words)                                    ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printWords(Plist As List(Of OcrWord))                  ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printWords*StartW ctr=" & Plist.Count)        ''~v196M~
        For Each word As OcrWord In Plist                              ''~v196M~
            Trace.W("Class10:printWords Text=" & word.Text)            ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printWords*EndW")                             ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printWordsRect(Plist As List(Of OcrWord))              ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printWordsRect*StartW ctr=" & Plist.Count)    ''~v196M~
        For Each word As OcrWord In Plist                              ''~v196M~
            Dim brect As Rect = word.BoundingRect                        ''~v196M~
            Trace.W("Class10:printWords Text=" & word.Text & ",X=" & brect.X & ",Y=" & brect.Y & ",ww=" & brect.Width & ",hh=" & brect.Height) ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printWordsRect*EndW")                         ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printWordsRect(Plist As IReadOnlyList(Of OcrWord))     ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printWordsRect*StartW ctr=" & Plist.Count)    ''~v196M~
        For Each word As OcrWord In Plist                              ''~v196M~
            Dim brect As Rect = word.BoundingRect                        ''~v196M~
            Trace.W("Class10:printWords Text=" & word.Text & ",X=" & brect.X & ",Y=" & brect.Y & ",ww=" & brect.Width & ",hh=" & brect.Height) ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printWordsRect*EndW")                         ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printWords(Plist As IReadOnlyList(Of OcrWord))         ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printWords*ReadOnly*StartW ctr=" & Plist.Count) ''~v196M~
        For Each word As OcrWord In Plist                              ''~v196M~
            Trace.W("Class10:printWords Text=" & word.Text)            ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printWords*ReadOnly*EndW")                    ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub printWords(Ptitle As String, Plist As List(Of OcrWord)) ''~v196M~
#If DEBUG Then                                                         ''~v196M~
        Trace.W("Class10:printWords*StartW " & Ptitle & ",ctr=" & Plist.Count) ''~v196M~
        For Each word As OcrWord In Plist                              ''~v196M~
            Trace.W("Class10:printWords Text=" & word.Text)            ''~v196M~
        Next                                                           ''~v196M~
        Trace.W("Class10:printWords*EndW")                             ''~v196M~
#End If                                                                ''~v196M~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196I~
    '*************************************************************     ''~v196I~
    '*************************************************************     ''~v196I~
    Private Function getLineStyle(Pline As OcrLine, PswChkText As Boolean) As Integer         ''~v196R~''~v197R~
        Dim rc As Integer = 0                                          ''~v196I~
        Dim brect0 As Rect                                             ''~v196I~
        Dim brect1 As Rect                                             ''~v196I~
        Dim word0 As OcrWord                                           ''~v196I~
        Dim word1 As OcrWord                                           ''~v196I~
        If Pline.Words.Count = 1 Then                                  ''~v196I~
            rc = LS_SINGLE                                             ''~v196I~
            If PswChkText Then                                              ''~v197I~
                word0 = Pline.Words.Item(0)                                ''~v196I~''~v197R~
                If word0.Text.Length > 1 Then                                     ''~v196I~''~v197R~
                    brect0 = word0.BoundingRect                            ''~v196I~''~v197R~
                    If brect0.Height < brect0.Width Then                          ''~v196I~''~v197R~
                        rc = LS_HORIZONTAL                                 ''~v196I~''~v197R~
                    Else                                                   ''~v196I~''~v197R~
                        rc = LS_VERTICAL                                   ''~v196I~''~v197R~
                    End If                                                 ''~v196I~''~v197R~
                End If                                                     ''~v196I~''~v197R~
            End If                                                     ''~v197I~
        Else                                                           ''~v196I~
            word0 = Pline.Words.Item(0)                                ''~v196I~
            brect0 = word0.BoundingRect                                ''~v196I~
            word1 = Pline.Words.Item(Pline.Words.Count - 1)                                ''~v196I~''~v197R~
            brect1 = word1.BoundingRect                                ''~v196I~
            Dim diffX As Double = Abs(brect0.X - brect1.X) + brect1.Width             ''~v196I~''~v197R~
            Dim diffY As Double = Abs(brect0.Y - brect1.Y) + brect1.Height             ''~v196I~''~v197R~
            If diffX > diffY Then                                           ''~v197R~
                rc = LS_HORIZONTAL                                     ''~v196I~
            Else                                                       ''~v197R~
                rc = LS_VERTICAL                                   ''~v196I~''~v197R~
            End If                                                     ''~v196I~
        End If                                                         ''~v196I~
        '*Trace.W("Class10:getLineStyle swChkText=" & PswChkText & ",rc=0x" & Hex(rc) & ",text=" & Pline.Text) ''~v196R~''~v197R~''~v201R~
        Return rc                                                      ''~v196I~
    End Function                                                       ''~v196I~
    Private Function getLineStyle(Pline As OcrLineV2, PswChkText As Boolean) As Integer ''~v199I~
        Dim rc As Integer = 0                                          ''~v199I~
        Dim brect0 As Rect                                             ''~v199I~
        Dim brect1 As Rect                                             ''~v199I~
        Dim word0 As OcrWord                                           ''~v199I~
        Dim word1 As OcrWord                                           ''~v199I~
        If Pline.Words.Count = 1 Then                                  ''~v199I~
            rc = LS_SINGLE                                             ''~v199I~
            If PswChkText Then                                         ''~v199I~
                word0 = Pline.Words.Item(0)                            ''~v199I~
                If word0.Text.Length > 1 Then                          ''~v199I~
                    brect0 = word0.BoundingRect                        ''~v199I~
                    If brect0.Height < brect0.Width Then               ''~v199I~
                        rc = LS_HORIZONTAL                             ''~v199I~
                    Else                                               ''~v199I~
                        rc = LS_VERTICAL                               ''~v199I~
                    End If                                             ''~v199I~
                End If                                                 ''~v199I~
            End If                                                     ''~v199I~
        Else                                                           ''~v199I~
            word0 = Pline.Words.Item(0)                                ''~v199I~
            brect0 = word0.BoundingRect                                ''~v199I~
            word1 = Pline.Words.Item(Pline.Words.Count - 1)            ''~v199I~
            brect1 = word1.BoundingRect                                ''~v199I~
            Dim diffX As Double = Abs(brect0.X - brect1.X) + brect1.Width ''~v199I~
            Dim diffY As Double = Abs(brect0.Y - brect1.Y) + brect1.Height ''~v199I~
            If diffX > diffY Then                                      ''~v199I~
                rc = LS_HORIZONTAL                                     ''~v199I~
            Else                                                       ''~v199I~
                rc = LS_VERTICAL                                       ''~v199I~
            End If                                                     ''~v199I~
        End If                                                         ''~v199I~
        '*Trace.W("Class10:getLineStyle ocrLineV2 swChkText=" & PswChkText & ",rc=0x" & Hex(rc) & ",text=" & Pline.Text) ''~v199I~''~v201R~
        Return rc                                                      ''~v199I~
    End Function                                                       ''~v199I~
    '*************************************************************     ''~v196I~
    Private Sub getLineHeight(Pline As OcrLine, ByRef PhhMax As Double, ByRef PbottomY As Double) ''~v196R~''~v197R~
        '*printLineRect("getLineHeight", Pline)                           ''~v196I~''~v201R~
        Dim hhMax As Double = 0.0                                      ''~v196I~
        Dim bottomY As Double = 0.0                                    ''~v196I~
        Dim brect As Rect                                              ''~v196I~
        For Each word As OcrWord In Pline.Words                        ''~v196I~
            brect = word.BoundingRect                                  ''~v196I~
            If hhMax < brect.Height Then                               ''~v196I~
                hhMax = brect.Height                                   ''~v196I~
            End If                                                     ''~v196I~
            If bottomY < brect.Height + brect.Y Then                     ''~v196I~
                bottomY = brect.Height + brect.Y                          ''~v196I~
            End If                                                     ''~v196I~
        Next                                                           ''~v196I~
        '*Trace.W("Class10:getLineHeight hhMax=" & hhMax & ",bottomY=" & bottomY & ",text=" & Pline.Text) ''~v196R~''~v201R~
    End Sub                                                            ''~v196R~
    '*************************************************************     ''~v197I~
    Private Sub getLineRect(Pline As OcrLine, ByRef PX1 As Double, ByRef PY1 As Double, ByRef PX2 As Double, ByRef PY2 As Double) ''~v197R~
        '*printLineRect("getLineRect", Pline)                            ''~v197R~''~v201R~
        Dim topY As Double = 0.0                                       ''~v197I~
        Dim bottomY As Double = 0.0                                    ''~v197I~
        Dim leftX As Double = 0.0                                      ''~v197I~
        Dim rightX As Double = 0.0                                     ''~v197I~
        Dim brect As Rect                                              ''~v197I~
        For Each word As OcrWord In Pline.Words                        ''~v197I~
            brect = word.BoundingRect                                  ''~v197I~
            If topY = 0.0 Or brect.Y < topY Then                                ''~v197I~
                topY = brect.Y                                           ''~v197I~
            End If                                                     ''~v197I~
            If bottomY < brect.Y + brect.Height Then                          ''~v197I~
                bottomY = brect.Y + brect.Height                       ''~v197I~
            End If                                                     ''~v197I~
            If leftX = 0.0 Or brect.X < leftX Then                              ''~v197I~
                leftX = brect.X                                          ''~v197I~
            End If                                                     ''~v197I~
            If rightX < brect.X + brect.Width Then                            ''~v197I~
                rightX = brect.X + brect.Width                         ''~v197I~
            End If                                                     ''~v197I~
        Next                                                           ''~v197I~
        '*      If PX1 IsNot Nothing Then                                      ''~v197R~
        PX1 = leftX                                                  ''~v197I~
        '*      End If                                                         ''~v197R~
        '*      If PX2 IsNot Nothing Then                                      ''~v197R~
        PX2 = rightX                                                 ''~v197I~
        '*      End If                                                         ''~v197R~
        '*      If PY1 IsNot Nothing Then                                      ''~v197R~
        PY1 = topY                                                   ''~v197I~
        '*      End If                                                         ''~v197R~
        '*      If PY2 IsNot Nothing Then                                      ''~v197R~
        PY2 = bottomY                                              ''~v197R~
        '*      End If                                                         ''~v197R~
        '*Trace.W("Class10:getLineRect x1=" & leftX & ",y1=" & topY & ",x2=" & rightX & ",y2=" & bottomY) ''~v197R~''~v201R~
    End Sub                                                            ''~v197I~
    '*************************************************************     ''~v199I~
    Private Sub getLineRect(Pline As OcrLineV2, ByRef PX1 As Double, ByRef PY1 As Double, ByRef PX2 As Double, ByRef PY2 As Double) ''~v199I~
        '*printLineRect("getLineRect", Pline)                            ''~v199I~''~v201R~
        Dim topY As Double = 0.0                                       ''~v199I~
        Dim bottomY As Double = 0.0                                    ''~v199I~
        Dim leftX As Double = 0.0                                      ''~v199I~
        Dim rightX As Double = 0.0                                     ''~v199I~
        Dim brect As Rect                                              ''~v199I~
        For Each word As OcrWord In Pline.Words                        ''~v199I~
            brect = word.BoundingRect                                  ''~v199I~
            If topY = 0.0 Or brect.Y < topY Then                       ''~v199I~
                topY = brect.Y                                         ''~v199I~
            End If                                                     ''~v199I~
            If bottomY < brect.Y + brect.Height Then                   ''~v199I~
                bottomY = brect.Y + brect.Height                       ''~v199I~
            End If                                                     ''~v199I~
            If leftX = 0.0 Or brect.X < leftX Then                     ''~v199I~
                leftX = brect.X                                        ''~v199I~
            End If                                                     ''~v199I~
            If rightX < brect.X + brect.Width Then                     ''~v199I~
                rightX = brect.X + brect.Width                         ''~v199I~
            End If                                                     ''~v199I~
        Next                                                           ''~v199I~
        '*      If PX1 IsNot Nothing Then                              ''~v199I~
        PX1 = leftX                                                    ''~v199I~
        '*      End If                                                 ''~v199I~
        '*      If PX2 IsNot Nothing Then                              ''~v199I~
        PX2 = rightX                                                   ''~v199I~
        '*      End If                                                 ''~v199I~
        '*      If PY1 IsNot Nothing Then                              ''~v199I~
        PY1 = topY                                                     ''~v199I~
        '*      End If                                                 ''~v199I~
        '*      If PY2 IsNot Nothing Then                              ''~v199I~
        PY2 = bottomY                                                  ''~v199I~
        '*      End If                                                 ''~v199I~
        '*Trace.W("Class10:getLineRect x1=" & leftX & ",y1=" & topY & ",x2=" & rightX & ",y2=" & bottomY) ''~v199I~''~v201R~
    End Sub                                                            ''~v199I~
    '*************************************************************     ''~v196I~
    Private Function getMostFrequentValue(Ptbl As Double(), Pctr As Integer, Pmax As Double, Pmin As Double, Pvias As Integer) As Integer ''~v196I~
        Dim intMin As Integer = CType(Pmin, Integer)                   ''~v196I~
        Dim intMax As Integer = CType(Pmax + 1, Integer)               ''~v196I~
        Dim ctr = CType((intMax - intMin) / Pvias, Integer) + 1          ''~v196I~
        '*Trace.W("getMostFrequentValue max=" & Pmax & ",min=" & Pmin & ",ctr=" & ctr & ",vias=" & Pvias) ''~v196I~''~v201R~
        Dim wk As Integer() = New Integer(ctr) {}                      ''~v196I~
        For ii As Integer = 0 To Pctr - 1                              ''~v196I~
            Dim intV = CType((Ptbl(ii) - intMin) / Pvias, Integer)     ''~v196I~
            '*Trace.W("getMostFrequentValue ii=" & ii & ",Ptbl=" & Ptbl(ii) & ",intV=" & intV) ''~v196I~''~v201R~
            wk(intV) += 1                                              ''~v196I~
            '*Trace.W("getMostFrequentValue wk=" & wk(intV))             ''~v196I~''~v201R~
        Next                                                           ''~v196I~
        Dim idxMax As Integer = 0                                      ''~v196I~
        Dim intMaxV As Integer = 0                                     ''~v196I~
        For ii As Integer = 0 To ctr - 1                               ''~v196I~
            Dim intV = wk(ii)                                          ''~v196I~
            '*Trace.W("getMostFrequentValue ii=" & ii & ",level=" & (ii * Pvias + Pmin) & ",intV=" & intV) ''~v196R~''~v201R~
            If intV > intMaxV Then                                     ''~v196I~
                intMaxV = intV                                         ''~v196I~
                idxMax = ii                                            ''~v196I~
                '*Trace.W("getMostFrequentValue max ii=" & ii & ",value level=" & (ii * Pvias + Pmin) & ",intV=" & intV) ''~v196R~''~v201R~
            End If                                                     ''~v196I~
        Next                                                           ''~v196I~
        Dim rc As Integer = idxMax * Pvias + intMin                      ''~v196I~
        '*Trace.W("getMostFrequentValue rc=" & rc)                       ''~v196I~''~v201R~
        Return rc                                                      ''~v196I~
    End Function                                                       ''~v196I~
    '*************************************************************     ''~v196I~
    '*get topY under header line                                       ''~v196I~
    '*************************************************************     ''~v196I~
#If False Then      '*TODO test, OK for 2dan                           ''~v197R~
    '*************************************************************     ''~v197I~
    '*get header boundary                                              ''~v197I~
    '*************************************************************     ''~v197I~
    Private Function getHeaderBottomY(Plist As IReadOnlyList(Of OcrLine), PscrHeight As Integer) As Integer''~v197R~
        Dim bottomY As Integer = 0                                     ''~v197I~
        Dim brect0, brect1 As Rect                                      ''~v197R~
        Dim ctrWord As Integer                                         ''~v197I~
        Dim top0Min As Double = PscrHeight                             ''~v197R~
        Dim top1Min As Double = PscrHeight                             ''~v197I~
        Dim bottom0Min As Double = PscrHeight                          ''~v197R~
        Dim bottom0Max As Double = 0.0                                 ''~v197I~
        Dim top0, top1, bottom0, diff As Double                           ''~v197R~
        '************************                                      ''~v197I~
        '*printLinesWords("getHeaderBottomY Entry", Plist)               ''~v197R~''~v201R~
        '* get topword bottom with double space to 2nd word            ''~v197R~
        For Each line As OcrLine In Plist                              ''~v197I~
            brect0 = line.Words.Item(0).BoundingRect                   ''~v197I~
            bottom0 = brect0.Y + brect0.Height                             ''~v197I~
            If bottom0Min > bottom0 Then                                      ''~v197I~
                '*Trace.W("Class10:getHeaderBottomY OLD bottom0Min=" & bottom0Min & ",bottom0=" & bottom0)''~v197R~''~v201R~
                '*printLineRect("getHeaderBottomY ", line)               ''~v197R~''~v201R~
                bottom0Min = bottom0                                     ''~v197I~
            End If                                                     ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getHeaderBottomY Final bottom0Min=" & bottom0Min)''~v197R~''~v201R~
        For Each line As OcrLine In Plist                              ''~v197I~
            brect0 = line.Words.Item(0).BoundingRect                   ''~v197I~
            top0 = brect0.Y                                            ''~v197I~
            If top0 >= bottom0Min Then                                 ''~v197I~
                Continue For                                           ''~v197I~
            End If                                                     ''~v197I~
            '*Trace.W("Class10:getHeaderBottomY top0=" & top0 & " < bottom0Min=" & bottom0Min)''~v197R~''~v201R~
            '*printLineRect("getHeaderBottomY ", line)                   ''~v197R~''~v201R~
            bottom0 = brect0.Y + brect0.Height                         ''~v197I~
            If bottom0Max < bottom0 Then                                      ''~v197I~
                '*Trace.W("Class10:getHeaderBottomY OLD bottom0Max=" & bottom0Max & ",bottom0=" & bottom0)''~v197R~''~v201R~
                '*printLineRect("getHeaderBottomY ", line)               ''~v197R~''~v201R~
                bottom0Max = bottom0                                     ''~v197I~
            End If                                                     ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getHeaderBottomY Final bottom0Max=" & bottom0Max)''~v197R~''~v201R~
        ''~v197I~
        For Each line As OcrLine In Plist                              ''~v197I~
            brect0 = line.Words.Item(0).BoundingRect                   ''~v197I~
            top0 = brect0.Y                                              ''~v197I~
            bottom0 = brect0.Y + brect0.Height                             ''~v197I~
            If top0 >= bottom0Min Then                                 ''~v197R~
                Continue For                                           ''~v197I~
            End If                                                     ''~v197I~
            '*Trace.W("Class10:getHeaderBottomY top0=" & top0 & " < bottom0Min=" & bottom0Min)''~v197R~''~v201R~
            '*printLineRect("getHeaderBottomY ", line)                   ''~v197R~''~v201R~
            ctrWord = line.Words.Count                                 ''~v197I~
            If ctrWord = 1 Then                                               ''~v197I~
                Continue For                                           ''~v197I~
            End If                                                     ''~v197I~
            brect1 = line.Words.Item(1).BoundingRect                   ''~v197I~
            top1 = brect1.Y                                              ''~v197I~
            diff = top1 - bottom0                                          ''~v197I~
            '*Trace.W("Class10:getHeaderBottomY top1=" & top1 & ",bottom0=" & bottom0 & ",diff=" & diff & ",brect0.Height=" & brect0.Height)''~v197R~''~v201R~
'*          If diff >= brect0.Height * 2 Then                          ''~v197R~
            If diff >= brect0.Height * RATE_SPACE_HEADER Then          ''~v197I~
                If top1Min > top1 Then                                 ''~v197R~
                    '*Trace.W("Class10:getHeaderBottomY OLD top1Min=" & top1Min & ",top1Min=" & top1Min & ",bottom0=" & bottom0 & ",diff=" & diff)''~v197R~''~v201R~
                    top1Min = top1                                       ''~v197I~
                    '*printLineRect("getHeaderBottomY ", line)           ''~v197R~''~v201R~
                End If                                                 ''~v197I~
            End If                                                     ''~v197I~
        Next                                                           ''~v197I~
        diff = top1Min - bottom0Max                                        ''~v197R~
        If diff > 0.0 Then                                                    ''~v197R~
            '*          bottomY = CType(bottom0Max + diff / 2, Integer)            ''~v197R~
            bottomY = CType(top1Min - 1, Integer)                        ''~v197I~
        End If                                                         ''~v197I~
        '*Trace.W("Class10:getHeaderBottomY return top1Min=" & top1Min & ",bottom0Max=" & bottom0Max & ",diff=" & diff & ",bottomY=" & bottomY)''~v197R~''~v201R~
        Return bottomY                                                 ''~v197R~
    End Function                                                       ''~v197I~
#Else                                                                  ''~v197I~
    '*************************************************************     ''~v197I~
    '*************************************************************     ''~v197I~
    '*************************************************************     ''~v197I~
    '*************************************************************     ''~v197I~
    Private Function getHeaderBottomY(Plist As IReadOnlyList(Of OcrLine), PscrHeight As Integer) As Integer ''~v197I~
        Dim bottomY As Integer = 0                                     ''~v197I~
        Dim swHeader As Boolean                                        ''~v197I~
        Dim top, bottom, height, topTop, floor, floorTop, bottomTop, heightTop, bottomTopMax, floorTopMax As Double ''~v197R~
        Dim LSs As Integer()                                           ''~v197I~
        Dim Y1s, Y2s, HHs As Double()                                    ''~v197I~
        Dim lineStyle, lineno, ctrLine, lineNoTop As Integer              ''~v197I~
        Dim brect, brect1 As Rect                                              ''~v197I~
        Dim topMin As Double = PscrHeight                              ''~v197R~
        Dim heightTopMax As Double = 0.0                               ''~v197I~
        '************************                                      ''~v197I~
        '*printLinesWords("getHeaderBottomY Entry", Plist)               ''~v197I~''~v201R~
        ctrLine = Plist.Count                                            ''~v197I~
        Y1s = New Double(ctrLine) {}                                        ''~v197I~
        Y2s = New Double(ctrLine) {}                                        ''~v197I~
        HHs = New Double(ctrLine) {}                                        ''~v197I~
        LSs = New Integer(ctrLine) {}                                  ''~v197I~
        lineno = 0                                                       ''~v197I~
        '* search min top ****************************************     ''~v197I~''~v198R~
        For Each line As OcrLine In Plist                              ''~v197I~
            '*Trace.W("Class10:searchTop lineNo=" & lineno)              ''~v197I~''~v201R~
            '*printLineRect("getHeaderBottomY", line)                    ''~v197I~''~v201R~
'*          lineStyle = getLineStyle(line, False) '*chk text in word=False''~v197I~''~v201R~
            lineStyle = getLineStyle(line, True) '*chk text in word=False''~v201I~
            If lineStyle = LS_HORIZONTAL Then                                 ''~v197I~
                getLineRect(line, Nothing, top, Nothing, bottom)       ''~v197I~
                height = bottom - top                                      ''~v197I~
            Else                                                       ''~v197I~
                brect = line.Words.Item(0).BoundingRect                ''~v197I~
                top = brect.Y                                          ''~v197I~
                height = brect.Height                                    ''~v197I~
                bottom = top + height                                  ''~v197I~
            End If                                                     ''~v197I~
            LSs(lineno) = lineStyle                                      ''~v197I~
            Y1s(lineno) = top                                             ''~v197I~
            Y2s(lineno) = bottom                                          ''~v197I~
            HHs(lineno) = height                                         ''~v197I~
            If top < topMin Or topMin = 0.0 Then                                ''~v197I~
                lineNoTop = lineno                                       ''~v197M~
                '*Trace.W("Class10:getHeaderBottomY new topMin=" & top & ",old topMin=" & topMin & "linenoTop=" & lineNoTop) ''~v197I~''~v201R~
                topMin = top                                             ''~v197I~
                heightTopMax = height                                   ''~v197I~
            ElseIf top = topMin Then                                           ''~v197I~
                If height > heightTopMax Then '*select taller if same top Y   ''~v197I~
                    '*Trace.W("Class10:getHeaderBottomY new heightTop=" & height & ",old=" & heightTop & ",linenoTop=" & lineNoTop) ''~v197R~''~v201R~
                    heightTopMax = height                                ''~v197I~
                    lineNoTop = lineno                                 ''~v197I~
                End If                                                 ''~v197I~
            End If                                                     ''~v197I~
            lineno += 1                                                  ''~v197I~
        Next                                                           ''~v197I~
        topTop = topMin                                                  ''~v197I~
        bottomTop = Y2s(lineNoTop)                                        ''~v197I~
        heightTop = HHs(lineNoTop)                                       ''~v197I~
        floorTop = bottomTop + heightTop                                   ''~v197I~
        '*Trace.W("Class10:getHeaderBottomY Final lineNoTop=" & lineNoTop & ",topTop=" & topTop & ",bottomTop=" & bottomTop & ",heightTop=" & heightTop & ",floorTop=" & floorTop & ",heightTopMax=" & heightTopMax) ''~v197R~''~v201R~
        '* get max bottom of top word *****************************    ''~v197I~''~v198R~
        swHeader = True                                                  ''~v197I~
        bottomTopMax = bottomTop                                         ''~v197I~
        floorTopMax = floorTop                                           ''~v197I~
        lineno = 0                                                        ''~v197M~
        For Each line As OcrLine In Plist                              ''~v197I~
            '*printLineRect("getHeaderBottomY", line)                    ''~v197I~''~v201R~
            top = Y1s(lineno)                                           ''~v197I~
            bottom = Y2s(lineno)                                        ''~v197I~
            height = HHs(lineno)                                         ''~v197I~
            '*Trace.W("Class10:getHeaderBottomY lineStyle=0x" & Hex(lineStyle) & ",top=" & top & ",bottom=" & bottom & ",floorTop=" & floorTop) ''~v197R~''~v201R~
            If top < bottomTop Then '*top may be on header line               ''~v197I~
                '*Trace.W("Class10:getHeaderBottomY top=" & top & " < bottomTop=" & bottomTop & ",bottomTopMax=" & bottomTopMax) ''~v197R~''~v201R~
                If bottom > bottomTopMax Then                                 ''~v197I~
                    '*Trace.W("Class10:getHeaderBottomY new bottomTopMax=" & bottom & ",old=" & bottomTopMax) ''~v197R~''~v201R~
                    bottomTopMax = bottom                                ''~v197I~
                End If                                                 ''~v197I~
                '*              floor = bottom + height * 2                            ''~v197R~
                floor = bottom + height * RATE_SPACE_HEADER            ''~v197I~
                '*Trace.W("Class10:getHeaderBottomY floor=" & floor & ",bottom=" & bottom & ",height=" & height & ",floorTopMax=" & floorTopMax) ''~v197I~''~v201R~
                If floor > floorTopMax Then                            ''~v197M~
                    floorTopMax = floor                                ''~v197M~
                    '*Trace.W("Class10:getHeaderBottomY new floorTopMax=" & floorTopMax) ''~v197M~''~v201R~
                End If                                                  ''~v197I~
            Else
                If top < floorTop Then                                 ''~v197R~
                    '*Trace.W("Class10:getHeaderBottomY Failed top=" & top & " < floorTop=" & floorTop) ''~v197R~''~v201R~
                    swHeader = False                                   ''~v197R~
                    Exit For                                           ''~v197R~
                End If                                                 ''~v197R~
            End If ''~v197I~
            lineno += 1                                                  ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getHeaderFinal floorTopMax=" & floorTopMax & ",bottomTopMax=" & bottomTopMax) ''~v197I~''~v201R~
        If swHeader Then                                                    ''~v197I~
            '* chk vertical line 2nd word is under the floor **********    ''~v198I~
            lineno = 0                                                    ''~v197I~
            For Each line As OcrLine In Plist                          ''~v197I~
                '*printLineRect("getHeaderBottomY", line)                ''~v197I~''~v201R~
                lineStyle = LSs(lineno)                                  ''~v197I~
                top = Y1s(lineno)                                       ''~v197I~
                bottom = Y2s(lineno)                                    ''~v197I~
                height = HHs(lineno)                                     ''~v197I~
                '*Trace.W("Class10:getHeaderBottomY lineStyle=0x" & Hex(lineStyle) & ",top=" & top & ",bottom=" & bottom) ''~v197R~''~v201R~
                If lineStyle <> LS_VERTICAL Then                          ''~v197I~
                    Continue For                                       ''~v197I~
                End If                                                 ''~v197I~
                brect1 = line.Words.Item(1).BoundingRect               ''~v197I~
                '*Trace.W("Class10:getHeaderBottomY 2ndword top=" & brect1.Y & ",floorTopMax=" & floorTopMax) ''~v197I~''~v201R~
                If brect1.Y < floorTopMax Then                              ''~v197I~
                    Dim swOK = False                                     ''~v197I~
                    If lineStyle = LS_VERTICAL And line.Words.Count <= 3 Then 'special consideration for Japanese char recognized as 2 vertical word''~v197I~
                        getLineRect(line, Nothing, top, Nothing, bottom) ''~v197I~
                        height = bottom - top                            ''~v197R~
                        floor = top + height + height * RATE_SPACE_HEADER      ''~v197I~
                        If floor <= floorTopMax Then                          ''~v197I~
                            swOK = True                                  ''~v197I~
                        End If                                         ''~v197I~
                        '*Trace.W("Class10:getHeaderBottomY 2 word vertical chk swOK=" & swOK & "height=" & height & ",floor=" & floor & ",floorTopMax=" & floorTopMax) ''~v197I~''~v201R~
                    End If                                             ''~v197I~
                    If Not swOK Then                                   ''~v197R~
                        '*Trace.W("Class10:getHeaderBottomY Failed 2ndword top=" & brect1.Y & " < floorTopMax=" & floorTopMax) ''~v197R~''~v201R~
                        swHeader = False                               ''~v197R~
                        Exit For                                       ''~v197R~
                    End If                                             ''~v197R~
                End If                                                 ''~v197I~
                lineno += 1                                              ''~v197I~
            Next                                                       ''~v197I~
        End If                                                         ''~v197I~
        If swHeader Then                                                    ''~v197I~
            '*          bottomY = CType(bottomTopMax + 1, Integer)                 ''~v197R~
            '*                      bottomY = CType((bottomTopMax+floorTopMax)/2, Integer)''~v197R~
            bottomY = CType(floorTopMax - 1, Integer)                  ''~v197R~
        End If                                                         ''~v197I~
        '*Trace.W("Class10:getHeaderBottomY return swHeader=" & swHeader & ",bottomY=" & bottomY & ",floorTopMax=" & floorTopMax & ",bottomTopMax=" & bottomTopMax) ''~v197R~''~v201R~
        Return bottomY                                                 ''~v197I~
    End Function                                                       ''~v197I~
#End If                                                                ''~v197I~
    '*************************************************************     ''~v197R~
    '*get footer topY                                                  ''~v197R~
    '*************************************************************     ''~v197R~
#If False Then                                                              ''~v197I~
    Private Function getFooterTopY(Plist As IReadOnlyList(Of OcrLine)) As Integer ''~v197I~
        Dim lineNo As Integer = 0                                        ''~v197I~
        Dim bottomY As Double = 0.0                                      ''~v197I~
        Dim topMinM As Double = 0.0                                      ''~v197I~
        Dim topMinS As Double = 0.0                                      ''~v197I~
        Dim bottomMaxM As Double = 0.0                                   ''~v197I~
        Dim topY As Integer = 0                                          ''~v197R~
        Dim word As OcrWord                                            ''~v197I~
        Dim brect As Rect                                              ''~v197I~
        Dim ctrWord As Integer                                         ''~v197I~
        Dim top, bottom, diff As Double                                  ''~v197I~
        Dim topMax As Double = 0.0                                     ''~v197R~
        Dim topMinMHeight As Double = 0.0                              ''~v197I~
        Dim topMinSHeight As Double = 0.0                                ''~v197I~
        Dim lineBottomM As OcrLine = Nothing                             ''~v197R~
        '************************                                      ''~v197I~
        '*printLinesWords("getFooterTopY Entry", Plist)                  ''~v197I~''~v201R~
        For Each line As OcrLine In Plist                              ''~v197I~
            '*printLineRect("getFooterTopY", line)                       ''~v197I~''~v201R~
            ctrWord = line.Words.Count                                 ''~v197I~
            word = line.Words.Item(ctrWord - 1)                          ''~v197I~
            brect = word.BoundingRect                                   ''~v197I~
            top = brect.Y                                                ''~v197I~
            bottom = brect.Y + brect.Height                            ''~v197I~
            If topMax = 0.0 Or topMax < top Then                                ''~v197I~
                '*Trace.W("Class10:getFooterTopY top=" & top & ",topMax=" & topMax) ''~v197I~''~v201R~
                topMax = top                                             ''~v197I~
                If ctrWord = 1 Then                                    ''~v197R~
                    If topMinS = 0.0 Or top < topMinS Then             ''~v197R~
                        topMinS = top                                  ''~v197R~
                        topMinSHeight = brect.Height                   ''~v197R~
                        '*Trace.W("Class10:getFooterTopY-Single topMinS=" & topMinS) ''~v197I~''~v201R~
                    End If                                             ''~v197R~
                Else                                                   ''~v197R~
                    If topMinM = 0.0 Or top < topMinM Then             ''~v197R~
                        topMinM = top                                  ''~v197R~
                        lineBottomM = line                             ''~v197R~
                        topMinMHeight = brect.Height                   ''~v197R~
                        '*Trace.W("Class10:getFooterTopY-Multi topMinM=" & topMinM) ''~v197I~''~v201R~
                    End If                                             ''~v197R~
                    If bottom > bottomMaxM Then                        ''~v197R~
                        bottomMaxM = bottom                            ''~v197R~
                    End If                                             ''~v197R~
                End If                                                 ''~v197R~
            End If                                                     ''~v197I~
            lineNo += 1                                                ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getFooterTopY topMinS=" & topMinS & ",topMinM=" & topMinM & ",topMinMHeight=" & topMinMHeight & ",topMinSHeight=" & topMinSHeight & ",bottomMaxM=" & bottomMaxM) ''~v197R~''~v201R~
        If topMinS > topMinM Then                                             ''~v197I~
            diff = topMinS - bottomMaxM                                    ''~v197R~
'*          If diff > topMinSHeight * 2 Then                           ''~v197R~
            If diff > topMinSHeight * RATE_SPACE_HEADER Then           ''~v197I~
                topY = CType(topMinS - diff / 2, Integer)                     ''~v197R~
            End If                                                     ''~v197R~
        ElseIf lineBottomM IsNot Nothing Then                               ''~v197R~
            ctrWord = lineBottomM.Words.Count                          ''~v197R~
            word = lineBottomM.Words.Item(ctrWord - 2)                   ''~v197R~
            brect = word.BoundingRect                                   ''~v197I~
            bottom = brect.Y + brect.Height                                ''~v197I~
            diff = topMinM - bottom                                        ''~v197R~
'*          If diff > topMinMHeight * 2 Then                           ''~v197R~
            If diff > topMinMHeight * RATE_SPACE_HEADER Then           ''~v197I~
                topY = CType(topMinM - diff / 2, Integer)                     ''~v197R~
            End If                                                     ''~v197R~
        End If                                                         ''~v197I~
        '*Trace.W("Class10:getFooterTopY footerTopY=" & topY)            ''~v197I~''~v201R~
        Return topY                                                    ''~v197I~
    End Function                                                       ''~v197I~
#Else                                                                  ''~v197I~
#If False Then                                                              ''~v197I~
    Private Function getFooterTopY(Plist As IReadOnlyList(Of OcrLine), PscrHeight As Double) As Integer ''~v197I~
        Dim ctrWord As Integer                                         ''~v197I~
        Dim topMax As Double = 0.0                                     ''~v197I~
        Dim topMin As Double = PscrHeight                              ''~v197R~
        Dim top1Min As Double = PscrHeight                             ''~v197I~
        Dim bottomMin As Double = PscrHeight                           ''~v197R~
        Dim bottomMax As Double = 0.0                                  ''~v197I~
        Dim bottom1MAx As Double = 0.0                                 ''~v197I~
        Dim top, bottom, diff, footerHeight As Double                   ''~v197R~
        Dim brect As Rect                                      ''~v197I~
        Dim topY As Integer = 0                                        ''~v197I~
        '************************                                      ''~v197I~
        '*printLinesWords("getFooterTopY Entry", Plist)                  ''~v197I~''~v201R~
        For Each line As OcrLine In Plist                              ''~v197I~
            ctrWord = line.Words.Count                                 ''~v197I~
            brect = line.Words.Item(ctrWord - 1).BoundingRect            ''~v197I~
            top = brect.Y                                              ''~v197I~
            bottom = brect.Y + brect.Height                               ''~v197I~
            If topMax < top Then                                         ''~v197I~
                '*Trace.W("Class10:getFooterTopY top=" & top & ",OLD topMax=" & topMax) ''~v197I~''~v201R~
                '*printLineRect("getFooterTopY ", line)                  ''~v197I~''~v201R~
                topMax = top                                           ''~v197I~
            End If                                                     ''~v197I~
            If bottomMax < bottom Then                                        ''~v197R~
                bottomMax = bottom                                       ''~v197I~
            End If                                                     ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getFooterTopY Final topMax=" & topMax & ",bottomMax=" & bottomMax) ''~v197R~''~v201R~
        ''~v197I~
        For Each line As OcrLine In Plist                              ''~v197I~
            ctrWord = line.Words.Count                                 ''~v197I~
            brect = line.Words.Item(ctrWord - 1).BoundingRect            ''~v197I~
            top = brect.Y                                                ''~v197R~
            bottom = brect.Y + brect.Height                                ''~v197I~
            If bottom > topMax Then                                     ''~v197R~
                '*Trace.W("Class10:getFooterTopY topMax=" & topMax & " < bottom=" & bottom) ''~v197R~''~v201R~
                '*printLineRect("getFooterTopY ", line)                  ''~v197R~''~v201R~
                If topMin > top Then                                          ''~v197R~
                    '*Trace.W("Class10:getFooterY OLD topMin=" & topMin & ",top=" & top) ''~v197R~''~v201R~
                    '*printLineRect("getFooterTopY ", line)              ''~v197R~''~v201R~
                    topMin = top                                         ''~v197R~
                End If                                                 ''~v197R~
            End If                                                     ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getFooterTopY Final topMin=" & topMin)        ''~v197R~''~v201R~
        If topMin <> PscrHeight Then                                          ''~v197R~
            footerHeight = bottomMax - topMin                              ''~v197I~
            bottomMax = 0.0                                              ''~v197I~
            For Each line As OcrLine In Plist                          ''~v197R~
                ctrWord = line.Words.Count                             ''~v197R~
                brect = line.Words.Item(ctrWord - 1).BoundingRect        ''~v197R~
                bottom = brect.Y + brect.Height                        ''~v197R~
                diff = topMin - bottom                                 ''~v197R~
                '*Trace.W("Class10:getFooterTopY topMin=" & topMin & ",bottom=" & bottom & ",diff=" & diff & ",bottomMax=" & bottomMax & ",topMin=" & topMin & ",footerHeight=" & footerHeight) ''~v197R~''~v201R~
                '*              printLineRect("getFooterTopY ", line)                  ''~v197R~
'*              If diff >= footerHeight * 2 Then                       ''~v197R~
                If diff >= footerHeight * RATE_SPACE_HEADER Then       ''~v197I~
                    If bottomMax < bottom Then                          ''~v197R~
                        '*Trace.W("Class10:getFooterTopY OLD bottomMax=" & bottomMax & ",bottom=" & bottom & ",diff=" & diff) ''~v197R~''~v201R~
                        bottomMax = bottom                               ''~v197R~
                        '*printLineRect("getFooterTopY ", line)          ''~v197R~''~v201R~
                    End If                                             ''~v197R~
                End If                                                 ''~v197R~
            Next                                                       ''~v197R~
            diff = topMin - bottomMax                                  ''~v197R~
            If diff > 0.0 Then                                                ''~v197R~
                topY = CType(bottomMax + 1, Integer)                        ''~v197R~
            End If                                                     ''~v197R~
        End If                                                         ''~v197I~
        '*Trace.W("Class10:getFooterTopY return topMin=" & topMin & ",bottomMax=" & bottomMax & ",diff=" & diff & ",topY=" & topY) ''~v197R~''~v201R~
        Return topY                                                    ''~v197I~
    End Function                                                       ''~v197I~
#Else                                                                  ''~v197I~
#If False Then                                                              ''~v197I~
	'**************************************************************************''~v197I~
    Private Function getFooterTopY(Plist As IReadOnlyList(Of OcrLine), PscrHeight As Double) As Integer''~v197I~
        Dim ctrWord As Integer                                         ''~v197M~
        Dim brect1,brect2 As Rect                                      ''~v197I~
        Dim topY As Integer = 0                                        ''~v197M~
        Dim top, bottom, diff,hhWord as Double                         ''~v197I~
        Dim bottomMax As Double = 0.0                                  ''~v197M~
        Dim bottomLine As Double = 0.0                                 ''~v197I~
        Dim topMin As Double = PscrHeight                              ''~v197M~
        '************************                                      ''~v197I~
        '*printLinesWords("getFooterTopY Entry", Plist)                  ''~v197I~''~v201R~
        For Each line As OcrLine In Plist                              ''~v197I~
            ctrWord = line.Words.Count                                 ''~v197I~
'*          If ctrWord = 1 Or getLineStyle(line,False) <> LS_VERTICAL Then''~v197R~''~v201R~
            If ctrWord = 1 Or getLineStyle(line,True) <> LS_VERTICAL Then''~v201I~
                Continue For                                           ''~v197I~
            End If                                                      ''~v197I~
            brect1 = line.Words.Item(ctrWord - 1).BoundingRect         ''~v197R~
            brect2 = line.Words.Item(ctrWord - 2).BoundingRect         ''~v197I~
            top=brect1.Y                                               ''~v197I~
            bottom=brect2.Y+brect2.Height                              ''~v197I~
            diff=top-bottom                                            ''~v197I~
            hhWord=brect1.Height                                       ''~v197I~
'*          if diff>hhWord*2                                           ''~v197R~
            if diff>hhWord*RATE_SPACE_HEADER                           ''~v197I~
                '*Trace.W("Class10:getFooterTopY Multiword diff=" & diff & ",top=" & top & ",bottom=" & bottom)''~v197I~''~v201R~
            	if bottom>bottomMax                                    ''~v197I~
	                '*Trace.W("Class10:getFooterTopY New bottomMax=" & bottom & ",Old bottomMax=" & bottomMax)''~v197I~''~v201R~
                	bottomMax=bottom                                   ''~v197I~
                end if                                                 ''~v197I~
            	if top<topMin                                          ''~v197I~
	                '*Trace.W("Class10:getFooterTopY New topMin=" & top & ",Old topMin=" & topMin)''~v197I~''~v201R~
                	topMin=top                                         ''~v197I~
                end if                                                 ''~v197I~
                '*printLineRect("getFooterTopY ", line)                  ''~v197I~''~v201R~
            end If                                                     ''~v197R~
            If bottomLine < top + hhWord Then                                   ''~v197I~
                bottomLine = top + hhWord                                  ''~v197I~
            End If                                                      ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getFooterTopY multiword Final topMin=" & topMin & ",bottomMax=" & bottomMax & ",bottomLine=" & bottomLine)''~v197I~''~v201R~
        For Each line As OcrLine In Plist                              ''~v197I~
            ctrWord = line.Words.Count                                 ''~v197I~
            If ctrWord = 1 Then                                              ''~v197I~
                brect1 = line.Words.Item(0).BoundingRect               ''~v197I~
                top = brect1.Y                                           ''~v197I~
                bottom = brect1.Y + brect1.Height                          ''~v197I~
                hhWord = brect1.Height                                    ''~v197I~
'*          ElseIf getLineStyle(line,False) = LS_HORIZONTAL Then       ''~v197R~''~v201R~
            ElseIf getLineStyle(line,True ) = LS_HORIZONTAL Then       ''~v201I~
                getLineRect(line, Nothing, top, Nothing, bottom) ''~v197I~
                hhWord = bottom - top                                       ''~v197I~
            Else                                                       ''~v197I~
                Continue For                                           ''~v197I~
            End If                                                      ''~v197I~
            If bottom < bottomMax Then                                      ''~v197I~
                Continue For                                           ''~v197I~
            End If                                                      ''~v197I~
            If top > bottomLine Then                                          ''~v197I~
                diff = top - bottomLine                                    ''~v197I~
                bottom=bottomLine                                      ''~v197I~
            ElseIf top > bottomMax Then                                       ''~v197I~
                diff = top - bottomMax                                     ''~v197I~
                bottom=bottomMax                                       ''~v197I~
            Else                                                       ''~v197I~
                diff =0                                                 ''~v197I~
            end If                                                     ''~v197I~
'*          If diff > hhWord * 2 Then                                  ''~v197R~
            If diff > hhWord * RATE_SPACE_HEADER Then                  ''~v197I~
                '*Trace.W("Class10:getFooterTopY SingleWord diff=" & diff & ",top=" & top & ",bottom=" & bottom & ",bottomLine=" & bottomLine & ",bottomMax=" & bottomMax & ",hhWord=" & hhWord) ''~v197I~''~v201R~
                If bottom > bottomMax Then                             ''~v197R~
                    '*Trace.W("Class10:getFooterTopY New bottomMax=" & bottom & ",Old bottomMax=" & bottomMax) ''~v197I~''~v201R~
                    bottomMax = bottom                                   ''~v197I~
                End If                                                 ''~v197I~
                If top < topMin Then                                          ''~v197I~
                    '*Trace.W("Class10:getFooterTopY New topMin=" & top & ",Old topMin=" & topMin) ''~v197I~''~v201R~
                    topMin = top                                         ''~v197I~
                End If                                                 ''~v197I~
                '*printLineRect("getFooterTopY ", line)                  ''~v197I~''~v201R~
            End If                                                     ''~v197I~
        Next                                                           ''~v197I~
        topY = CType(bottomMax + 1, Integer)                           ''~v197I~
        '*Trace.W("Class10:getFooterTopY return topMin=" & topMin & ",bottomMax=" & bottomMax & ",topY=" & topY)''~v197R~''~v201R~
        Return topY                                                    ''~v197I~
    End Function                                                       ''~v197I~
#Else                                                                  ''~v197I~
#If False Then                                                              ''~v198I~
    '**************************************************************************''~v197I~
    Private Function getFooterTopY(Plist As IReadOnlyList(Of OcrLine), PscrHeight As Double) As Integer ''~v197I~
        ''~v197I~
        Dim topY As Integer = 0                                        ''~v197M~
        Dim ls As Integer = 0                                          ''~v197I~
        Dim brect1, brect2 As Rect                                      ''~v197M~
        Dim ctrWord, lineStyle As Integer                              ''~v197I~
        Dim top, bottom As Double                                      ''~v197R~
        Dim bottom2, diff, hhBottom As Double                          ''~v197I~
        Dim topMaxSingle = 0.0                                           ''~v197I~
        Dim topMaxVertical = 0.0                                            ''~v197I~
        Dim bottomMaxSingle As Double = 0.0                            ''~v197I~
        Dim bottom2Max As Double = 0.0                                 ''~v197I~
        Dim bottomMaxVertical As Double = 0.0                          ''~v197I~
        Dim bottomMax As Double = 0.0                                  ''~v197I~
        Dim swFooter As Boolean                                        ''~v197I~
        '************************                                      ''~v197I~
        '*printLinesWords("getFooterTopY Entry", Plist)                  ''~v197I~''~v201R~
        For Each line As OcrLine In Plist                              ''~v197I~
            '*printLineRect("getFooterTopY", line)                       ''~v197I~''~v201R~
'*          lineStyle = getLineStyle(line, False)                       ''~v197R~''~v201R~
            lineStyle = getLineStyle(line, True )                      ''~v201I~
            If lineStyle <> LS_VERTICAL Then                                  ''~v197I~
                If lineStyle = LS_SINGLE Then                                 ''~v197I~
                    brect1 = line.Words.Item(0).BoundingRect           ''~v197I~
                    top = brect1.Y                                       ''~v197I~
                    bottom = top + brect1.Height                           ''~v197I~
                Else                                                   ''~v197I~
                    getLineRect(line, Nothing, top, Nothing, bottom)   ''~v197I~
                End If                                                 ''~v197I~
                If top > topMaxSingle Then                                    ''~v197I~
                    '*Trace.W("Class10:getFooterTopY Single top=" & top & ",old topMaxSingle=" & topMaxSingle) ''~v197I~''~v201R~
                    topMaxSingle = top                                   ''~v197I~
                End If                                                 ''~v197I~
                If bottom > bottomMaxSingle Then                            ''~v197I~
                    '*Trace.W("Class10:getFooterTopY Single Vertical bottom=" & bottom & ",Old bottomMaxSingle=" & bottomMaxSingle) ''~v197M~''~v201R~
                    bottomMaxSingle = bottom                           ''~v197M~
                End If                                                 ''~v197M~
            Else    '*Vertical                                         ''~v197I~
                ctrWord = line.Words.Count                             ''~v197I~
                brect1 = line.Words.Item(ctrWord - 1).BoundingRect     ''~v197I~
                brect2 = line.Words.Item(ctrWord - 2).BoundingRect     ''~v197I~
                top = brect1.Y                                           ''~v197I~
                bottom = top + brect1.Height                               ''~v197I~
                bottom2 = brect2.Y + brect2.Height                         ''~v197I~
                diff = top - bottom2                                     ''~v197R~
                '*Trace.W("Class10:getFooterTopY Vertical diff=" & diff & ",top=" & top & ",bottom2=" & bottom2 & ",brect1.Height=" & brect1.Height) ''~v197R~''~v201R~
                '*              If diff > brect1.Height * 2 Then                       ''~v197R~
                If diff > brect1.Height * RATE_SPACE_HEADER Then       ''~v197I~
                    '*Trace.W("Class10:getFooterTopY Vertical diff=" & diff & ",top=" & top & ",bottom=" & bottom) ''~v197I~''~v201R~
                    If top > topMaxVertical Then                              ''~v197I~
                        '*Trace.W("Class10:getFooterTopY Vertical Old topMaxVertical=" & topMaxVertical) ''~v197I~''~v201R~
                        topMaxVertical = top                                 ''~v197I~
                    End If                                             ''~v197I~
                    If bottom2 > bottom2Max Then                              ''~v197I~
                        '*Trace.W("Class10:getFooterTopY Vertical new bottom2Max=" & bottom2 & ",old=" & bottom2Max) ''~v197R~''~v201R~
                        bottom2Max = bottom2                             ''~v197I~
                    End If                                             ''~v197I~
                End If                                                 ''~v197I~
                If bottom > bottomMaxVertical Then                     ''~v197I~
                    '*Trace.W("Class10:getFooterTopY Vertical bottom=" & bottom & ",Old bottomMaxVertical=" & bottomMaxVertical) ''~v197M~''~v201R~
                    bottomMaxVertical = bottom                         ''~v197I~
                End If                                                 ''~v197I~
            End If                                                  ''~v197I~
        Next                                                           ''~v197I~
        '*Trace.W("Class10:getFooterTopY multiword Final topMaxVertical=" & topMaxVertical & ",topMaxSingle=" & topMaxSingle & ",bottomMaxSingle=" & bottomMaxSingle & ",bottomMaxVertical=" & bottomMaxVertical & ",bottom2Max=" & bottom2Max) ''~v197R~''~v201R~
        swFooter = True                                                  ''~v197I~
        If topMaxSingle > bottomMaxVertical Then    '* detected single under vertical''~v197I~
            '*Trace.W("Class10:getFooterTopY topMaxSingle=" & topMaxSingle & " > bottomMaxVertical=" & bottomMaxVertical) ''~v197I~''~v201R~
            For Each line As OcrLine In Plist                          ''~v197I~
'*              lineStyle = getLineStyle(line, False)                   ''~v197R~''~v201R~
                lineStyle = getLineStyle(line, True)                   ''~v201I~
                If lineStyle = LS_VERTICAL Then                              ''~v197I~
                    Continue For                                       ''~v197I~
                End If                                                 ''~v197I~
                '*printLineRect("getFooterTopY chk Single", line)        ''~v197I~''~v201R~
                If lineStyle = LS_SINGLE Then                             ''~v197I~
                    brect1 = line.Words.Item(0).BoundingRect           ''~v197I~
                    top = brect1.Y                                       ''~v197I~
                    bottom = top + brect1.Height                       ''~v197I~
                    hhBottom = brect1.Height                              ''~v197I~
                Else                                                   ''~v197I~
                    getLineRect(line, Nothing, top, Nothing, bottom)   ''~v197I~
                    hhBottom = Abs(bottom - top)                       ''~v197R~
                End If                                                 ''~v197I~
                If bottom <= bottomMaxVertical Then                           ''~v197I~
                    '*Trace.W("Class10:getFooterTopY Single is upper of vertical bottom=" & bottom & " bottomMaxVertical=" & bottomMaxVertical) ''~v197I~''~v201R~
                    Continue For                                       ''~v197I~
                End If                                                 ''~v197I~
                If top > bottomMaxVertical Then '* top>bottomMaxVertical''~v197R~
                    '*Trace.W("Class10:getFooterTopY Single top=" & top & " > bottomMaxVertical=" & bottomMaxVertical) ''~v197R~''~v201R~
                    diff = top - bottomMaxVertical                     ''~v197R~
                    '*Trace.W("Class10:getFooterTopY Single diff=" & diff & ",hhBottom=" & hhBottom) ''~v197I~''~v201R~
                    '*                  If diff <= hhBottom * 2 Then                       ''~v197R~
                    If diff <= hhBottom * RATE_SPACE_HEADER Then       ''~v197I~
                        '*Trace.W("Class10:getFooterTopY Single Failed diff=" & diff & ",top=" & top & ",hhBottom=" & hhBottom) ''~v197I~''~v201R~
                        swFooter = False                                 ''~v197I~
                        Exit For                                       ''~v197I~
                    End If                                             ''~v197I~
                Else    '* top<=bottomMaxVertical                      ''~v197R~
                    If bottom > bottomMaxVertical Then                        ''~v197I~
                        '*Trace.W("Class10:getFooterTopY Single Failed top=" & top & ",bottom=" & bottom & " <= bottomMaxVertical=" & bottomMaxVertical) ''~v197R~''~v201R~
                        swFooter = False                                ''~v197R~
                        Exit For                                       ''~v197R~
                    End If                                             ''~v197I~
                End If                                                 ''~v197I~
            Next                                                       ''~v197I~
            If swFooter Then                                                ''~v197R~
                topY = CType(bottomMaxVertical + 1, Integer)           ''~v197I~
            End If                                                     ''~v197I~
        Else  '*                                                       ''~v197I~
            '*Trace.W("Class10:getFooterTopY topMaxSingle=" & topMaxSingle & " <= bottomMaxVertical=" & bottomMaxVertical & ",bottom2Max=" & bottom2Max) ''~v197R~''~v201R~
            If bottom2Max <> 0.0 Then '*vertical line may be with appended footer''~v197R~
                '*Trace.W("Class10:getFooterTopY bottom2Max=" & bottom2Max & " <> 0.0") ''~v197I~''~v201R~
                For Each line As OcrLine In Plist                      ''~v197I~
                    '*printLineRect("getFooterTopY chk Single vs Vertical", line) ''~v197I~''~v201R~
'*                  lineStyle = getLineStyle(line, False)               ''~v197R~''~v201R~
                    lineStyle = getLineStyle(line, True)               ''~v201I~
                    If lineStyle = LS_VERTICAL Then                          ''~v197I~
                        Continue For                                   ''~v197I~
                    End If                                             ''~v197I~
                    If lineStyle = LS_SINGLE Then                             ''~v197I~
                        brect1 = line.Words.Item(0).BoundingRect       ''~v197I~
                        top = brect1.Y                                   ''~v197I~
                        bottom = top + brect1.Height                       ''~v197I~
                        hhBottom = brect1.Height                          ''~v197I~
                    Else                                               ''~v197I~
                        getLineRect(line, Nothing, top, Nothing, bottom) ''~v197I~
                        hhBottom = Abs(bottom - top)                   ''~v197R~
                    End If                                             ''~v197I~
                    If top > bottom2Max Then   '* top>bottom2Max              ''~v197R~
                        '*Trace.W("Class10:getFooterTopY Single top=" & top & " > bottom2Max=" & bottom2Max) ''~v197R~''~v201R~
                        diff = top - bottom2Max                            ''~v197R~
                        '*Trace.W("Class10:getFooterTopY Single diff=" & diff & ",hhBottom=" & hhBottom) ''~v197I~''~v201R~
                        '*                      If diff <= hhBottom * 2 Then                   ''~v197R~
                        If diff <= hhBottom * RATE_SPACE_HEADER Then   ''~v197I~
                            '*Trace.W("Class10:getFooterTopY Single failed diff=" & diff & ",top=" & top & ",bottom2Max=" & bottom2Max) ''~v197R~''~v201R~
                            swFooter = False                             ''~v197I~
                            Exit For                                   ''~v197I~
                        End If                                         ''~v197I~
                    Else        '*top<=bottom2Max                      ''~v197I~
                        If bottom > bottom2Max Then       '*single word override bottom2Max''~v197R~
                            '*Trace.W("Class10:getFooterTopY Single failed bottom=" & bottom & ",bottom2Max=" & bottom2Max) ''~v197R~''~v201R~
                            swFooter = False                             ''~v197I~
                            Exit For                                   ''~v197I~
                        End If                                         ''~v197I~
                    End If                                             ''~v197I~
                Next                                                   ''~v197I~
                If swFooter Then                                            ''~v197I~
                    topY = CType(bottom2Max + 1, Integer)              ''~v197R~
                End If                                                 ''~v197I~
            End If                                                     ''~v197I~
        End If                                                         ''~v197I~
        '*Trace.W("Class10:getFooterTopY return swFooter=" & swFooter & ",bottomMaxVertical=" & bottomMaxVertical & ",bottom2Max=" & bottom2Max & ",topY=" & topY) ''~v197R~''~v201R~
        Return topY                                                    ''~v197I~
    End Function                                                       ''~v197I~
#Else                                                                  ''~v198I~
    '**************************************************************************''~v198I~
    '**************************************************************************''~v198I~
    '**************************************************************************''~v198I~
    Private Function getFooterTopY(Plist As IReadOnlyList(Of OcrLine), PscrHeight As Integer) As Integer ''~v198R~
        '*      Dim bottomY As Integer = 0                                     ''~v198R~
        Dim topY As Integer = 0                                        ''~v198I~
        '*      Dim swHeader As Boolean                                        ''~v198R~
        Dim swFooter As Boolean                                        ''~v198I~
        '*      Dim top, bottom, height, topTop, floor,   floorTop,   bottomTop, heightTop, bottomTopMax, floorTopMax   As Double''~v198R~
        Dim top, bottom, height, topTop, ceiling, ceilingTop, bottomTop, heightTop, topTopMin, ceilingTopMin As Double ''~v198R~
        Dim LSs As Integer()                                           ''~v198I~
        Dim Y1s, Y2s, HHs As Double()                                  ''~v198I~
        Dim lineStyle, lineno, ctrLine, lineNoTop As Integer           ''~v198I~
        Dim brect, brect1 As Rect                                      ''~v198I~
        '*      Dim topMin As Double = PscrHeight                              ''~v198R~
        Dim topMax As Double = 0.0                                     ''~v198I~
        Dim heightTopMax As Double = 0.0                               ''~v198I~
        '************************                                      ''~v198I~
        '*printLinesWords("getFooterTopY Entry", Plist)                  ''~v198R~''~v201R~
        ctrLine = Plist.Count                                          ''~v198I~
        Y1s = New Double(ctrLine) {}                                   ''~v198I~
        Y2s = New Double(ctrLine) {}                                   ''~v198I~
        HHs = New Double(ctrLine) {}                                   ''~v198I~
        LSs = New Integer(ctrLine) {}                                  ''~v198I~
        lineno = 0                                                     ''~v198I~
        '* search lowest bottom word ****************************************''~v198R~
        For Each line As OcrLine In Plist                              ''~v198I~
            '*Trace.W("Class10:searchTop lineNo=" & lineno)              ''~v198I~''~v201R~
            '*printLineRect("getFooterTopY", line)                       ''~v198R~''~v201R~
'*          lineStyle = getLineStyle(line, False) '*chk text in word=False''~v198I~''~v201R~
            lineStyle = getLineStyle(line, True ) '*chk text in word=False''~v201I~
            If lineStyle = LS_HORIZONTAL Then                          ''~v198I~
                getLineRect(line, Nothing, top, Nothing, bottom)       ''~v198I~
                height = bottom - top                                  ''~v198I~
            Else                                                       ''~v198I~
                '*              brect = line.Words.Item(0).BoundingRect                ''~v198R~
                Dim ctrWord As Integer = line.Words.Count              ''~v198I~
                brect = line.Words.Item(ctrWord - 1).BoundingRect        ''~v198I~
                top = brect.Y                                          ''~v198I~
                height = brect.Height                                  ''~v198I~
                bottom = top + height                                  ''~v198I~
            End If                                                     ''~v198I~
            LSs(lineno) = lineStyle                                    ''~v198I~
            Y1s(lineno) = top                                          ''~v198I~
            Y2s(lineno) = bottom                                       ''~v198I~
            HHs(lineno) = height                                       ''~v198I~
            '*          If top < topMin Or topMin = 0.0 Then                       ''~v198R~
            If top > topMax Then                                            ''~v198I~
                lineNoTop = lineno                                     ''~v198I~
                '*Trace.W("Class10:getFooterTopY new topMin=" & top & ",old topMax=" & topMax & "linenoTop=" & lineNoTop) ''~v198R~''~v201R~
                '*              topMin = top                                           ''~v198R~
                topMax = top                                           ''~v198I~
                heightTopMax = height                                  ''~v198I~
                '*          ElseIf top = topMin Then                                   ''~v198R~
            ElseIf top = topMax Then                                   ''~v198I~
                If height > heightTopMax Then '*select taller if same top Y''~v198I~
                    '*Trace.W("Class10:getFooterTopY new heightTop=" & height & ",old=" & heightTop & ",linenoTop=" & lineNoTop) ''~v198R~''~v201R~
                    heightTopMax = height                              ''~v198I~
                    lineNoTop = lineno                                 ''~v198I~
                End If                                                 ''~v198I~
            End If                                                     ''~v198I~
            lineno += 1                                                ''~v198I~
        Next                                                           ''~v198I~
        '*      topTop = topMin                                                ''~v198R~
        topTop = topMax                                                ''~v198I~
        bottomTop = Y2s(lineNoTop)                                     ''~v198I~
        heightTop = HHs(lineNoTop)                                     ''~v198I~
        '*      floorTop = bottomTop + heightTop                               ''~v198R~
        ceilingTop = topTop - heightTop                                  ''~v198I~
        '*Trace.W("Class10:getFooterTopY Final lineNoTop=" & lineNoTop & ",topTop=" & topTop & ",bottomTop=" & bottomTop & ",heightTop=" & heightTop & ",ceilingTop=" & ceilingTop & ",heightTopMax=" & heightTopMax) ''~v198R~''~v201R~
        '* get min ceiling of bottom word *****************************''~v198R~
        '*      swHeader = True                                                ''~v198R~
        swFooter = True                                                ''~v198I~
        '*      bottomTopMax = bottomTop                                       ''~v198R~
        topTopMin = topTop                                             ''~v198I~
        '*      floorTopMax = floorTop                                         ''~v198R~
        ceilingTopMin = ceilingTop                                     ''~v198I~
        lineno = 0                                                     ''~v198I~
        For Each line As OcrLine In Plist                              ''~v198I~
            '*printLineRect("getFooterTopY", line)                       ''~v198R~''~v201R~
            top = Y1s(lineno)                                          ''~v198I~
            bottom = Y2s(lineno)                                       ''~v198I~
            height = HHs(lineno)                                       ''~v198I~
            '*Trace.W("Class10:getFooterTopY lineStyle=0x" & Hex(lineStyle) & ",top=" & top & ",bottom=" & bottom & ",ceilingTop=" & ceilingTop) ''~v198R~''~v201R~
            '*          If top < bottomTop Then '*top may be on header line        ''~v198R~
            If bottom > topTop Then '*bottom may be on footer line     ''~v198I~
                '*Trace.W("Class10:getFooterTopY bottom=" & bottom & " > topTop=" & topTop & ",topTopMin=" & topTopMin) ''~v198R~''~v201R~
                '*              If bottom > bottomTopMax Then                          ''~v198R~
                If top < topTopMin Then                                       ''~v198I~
                    '*Trace.W("Class10:getFooterTopY new topTopMin=" & top & ",old=" & topTopMin) ''~v198R~''~v201R~
                    '*                  bottomTopMax = bottom                              ''~v198R~
                    topTopMin = top                                     ''~v198I~
                End If                                                 ''~v198I~
                '*              floor = bottom + height * 2            ''~v198I~
                '*              floor = bottom + height * RATE_SPACE_HEADER            ''~v198R~
                ceiling = top - height * RATE_SPACE_HEADER             ''~v198I~
                '*Trace.W("Class10:getFooterTopY ceiling=" & ceiling & ",top=" & top & ",height=" & height & ",ceilingTopMin=" & ceilingTopMin) ''~v198R~''~v201R~
                '*              If floor > floorTopMax Then                            ''~v198R~
                If ceiling < ceilingTopMin Then                        ''~v198I~
                    '*                  floorTopMax = floor                                ''~v198R~
                    ceilingTopMin = ceiling                            ''~v198I~
                    '*Trace.W("Class10:getFooterTopY new ceilingTopMin=" & ceilingTopMin) ''~v198R~''~v201R~
                End If                                                 ''~v198I~
            Else                                                       ''~v198I~
                '*              If top < floorTop Then                                 ''~v198R~
                If bottom > ceilingTop Then                            ''~v198I~
                    '*Trace.W("Class10:getFooterTopY Failed bottom=" & bottom & " > ceilingTop=" & ceilingTop) ''~v198R~''~v201R~
                    '*                  swHeader = False                                   ''~v198R~
                    swFooter = False                                   ''~v198I~
                    Exit For                                           ''~v198I~
                End If                                                 ''~v198I~
            End If                                                     ''~v198I~
            lineno += 1                                                ''~v198I~
        Next                                                           ''~v198I~
        '*Trace.W("Class10:getHeaderTopY Final ceilingTopMin=" & ceilingTopMin & ",topTopMin=" & topTopMin) ''~v198R~''~v201R~
        '*      If swHeader Then                                               ''~v198R~
        If swFooter Then                                               ''~v198I~
            '* chk vertical line bottom 2nd word is upper of the ceiling **********''~v198R~
            lineno = 0                                                 ''~v198I~
            For Each line As OcrLine In Plist                          ''~v198I~
                '*printLineRect("getFooterTopY", line)                   ''~v198R~''~v201R~
                lineStyle = LSs(lineno)                                ''~v198I~
                top = Y1s(lineno)                                      ''~v198I~
                bottom = Y2s(lineno)                                   ''~v198I~
                height = HHs(lineno)                                   ''~v198I~
                '*Trace.W("Class10:getFooterTopY lineStyle=0x" & Hex(lineStyle) & ",top=" & top & ",bottom=" & bottom) ''~v198R~''~v201R~
                If lineStyle <> LS_VERTICAL Then                       ''~v198I~
                    Continue For                                       ''~v198I~
                End If                                                 ''~v198I~
                '*              brect1 = line.Words.Item(1).BoundingRect               ''~v198R~
                Dim ctrWord As Integer = line.Words.Count              ''~v198I~
                brect1 = line.Words.Item(ctrWord - 1).BoundingRect       ''~v198I~
                '*Trace.W("Class10:getFooterTopY 2ndword top=" & brect1.Y & ",ceilingTopMin=" & ceilingTopMin) ''~v198R~''~v201R~
                '*              If brect1.Y < floorTopMax Then                         ''~v198R~
                If brect1.Y > ceilingTopMin Then                       ''~v198I~
                    Dim swOK = False                                   ''~v198I~
                    If lineStyle = LS_VERTICAL And line.Words.Count <= 3 Then 'special consideration for Japanese char recognized as 2 vertical word''~v198I~
                        getLineRect(line, Nothing, top, Nothing, bottom) ''~v198I~
                        height = bottom - top                          ''~v198I~
                        '*                      floor = top + height + height * RATE_SPACE_HEADER''~v198R~
                        ceiling = top - height * RATE_SPACE_HEADER     ''~v198I~
                        '*                      If floor <= floorTopMax Then                   ''~v198R~
                        If ceiling >= ceilingTopMin Then               ''~v198I~
                            swOK = True                                ''~v198I~
                        End If                                         ''~v198I~
                        '*Trace.W("Class10:getFooterTopY bottom 2nd word vertical chk swOK=" & swOK & "height=" & height & ",ceiling=" & ceiling & ",ceilingTopMin=" & ceilingTopMin) ''~v198R~''~v201R~
                    End If                                             ''~v198I~
                    If Not swOK Then                                   ''~v198I~
                        '*Trace.W("Class10:getFooterTopY Failed 2ndword top=" & brect1.Y & " < ceilingTopMin=" & ceilingTopMin) ''~v198R~''~v201R~
                        '*                      swHeader = False                               ''~v198R~
                        swFooter = False                               ''~v198I~
                        Exit For                                       ''~v198I~
                    End If                                             ''~v198I~
                End If                                                 ''~v198I~
                lineno += 1                                            ''~v198I~
            Next                                                       ''~v198I~
        End If                                                         ''~v198I~
        '*      If swHeader Then                                               ''~v198R~
        If swFooter Then                                               ''~v198I~
            '*          bottomY = CType(bottomTopMax + 1, Integer)     ''~v198I~
            '*                      bottomY = CType((bottomTopMax+floorTopMax)/2, Integer)''~v198I~
            '*          bottomY = CType(floorTopMax - 1, Integer)                  ''~v198R~
            topY = CType(ceilingTopMin + 1, Integer)                    ''~v198I~
        End If                                                         ''~v198I~
        '*Trace.W("Class10:getFooterTopY return swFooter=" & swFooter & ",topY=" & topY & ",ceilingTopMin=" & ceilingTopMin & ",topTopMin=" & topTopMin) ''~v198R~''~v201R~
        '*      Return bottomY                                                 ''~v198R~
        Return topY                                                    ''~v198I~
    End Function                                                       ''~v198I~
#End If                                                                ''~v198I~
#End If                                                                ''~v197I~
#End If                                                                ''~v197I~
#End If                                                                ''~v197I~
    '*************************************************************     ''~v196I~
    Private Function getHeaderBottom(Plist As IReadOnlyList(Of OcrLine)) As Integer ''~v196R~
        '*Trace.W("Class10:getHeaderBottom bmp ww=" & bmpWidth & ",hh=" & bmpHeight) ''~v196R~''~v201R~
        '*printLinesWords("getHeaderBottom Entry", Plist)                 ''~v196R~''~v201R~
        Dim rc As Integer = getHeaderBottomY(Plist, bmpHeight)             ''~v196R~''~v197R~
        '*Trace.W("Class10:getHeaderBottom rc=" & rc)                    ''~v196I~''~v201R~
        Return rc                                                      ''~v196I~
    End Function                                                       ''~v196I~
    '*************************************************************     ''~v196I~
    Private Function issueRun(Pfnm As String, Ptag As String) As Boolean ''~v196I~
        Dim rc As Boolean                                              ''~v196I~
        xText = ""                                                     ''~v196I~
        result = Nothing                                               ''~v196I~
        statusMsg = Nothing                                            ''~v196I~
        Dim t As Task = Task.Run(Async Function()                      ''~v196I~
                                     rc = Await extractTextAsync(Pfnm, Ptag) ''~v196R~
                                 End Function)                         ''~v196I~
        t.Wait()                                                       ''~v196I~
        '*Trace.W("Class10:issueRun rc=" & rc)                           ''~v196R~''~v201R~
        Return rc                                                      ''~v196I~
    End Function                                                       ''~v196I~
    '*************************************************************     ''~v196I~
    Private Function extractHeaderAndText(Pfnm As String, Ptag As String) As Boolean ''~v196R~
        Dim saveFileBMP As Bitmap = fileBMP                              ''~v196I~
        Dim saveClipRect As Rectangle = clipRect                         ''~v196I~
        Dim saveHeaderBottomY As Integer = headerBottomY               ''~v196R~
        Dim saveSortOption As Integer = sortOption                      ''~v196I~
        '**********************                                        ''~v196I~
        '*Trace.W("Class10:extractHeaderAndText headerBottomY=" & headerBottomY & ",scaleNew=" & scaleNew) ''~v196R~''~v201R~
        swOK = True                                                      ''~v197I~
        xTextHeader = ""                                                 ''~v197I~
        xTextFooter = ""                                                 ''~v197I~
        If headerBottomY <> 0 Then                                              ''~v197I~
            phaseGetHeader = PGH_GETHEADER                                 ''~v196I~''~v197M~
            clipRect = New Rectangle()                                      ''~v196I~
            clipRect.Width = bmpWidth                                        ''~v196R~
            clipRect.Height = headerBottomY                                ''~v196R~
            fileBMP = cutBMPRect(fileBMP, clipRect)                         ''~v196R~
            bmpWidth = fileBMP.Width                                       ''~v196R~
            bmpHeight = fileBMP.Height                                     ''~v196M~
            '*Trace.W("Class10:extractHeaderAndText clipRect hh=" & clipRect.Height & ",ww=" & clipRect.Width) ''~v196I~''~v201R~
            ''~v196I~
            sortOption = SORT_HLINE_LTR '* = 4       '*sort horizontal line by Y:smallY-->largeY''~v196I~
            swOK = issueRun(Pfnm, Ptag)                                    ''~v196I~
            sortOption = saveSortOption                                    ''~v196I~
            '*Trace.W("Class10:extractHeaderAndText header xText=" & xText)  ''~v196R~''~v201R~
            xTextHeader = xText + vbCrLf                               ''~v197R~
            resultTextAngleHeader = resultTextAngle '*use at markWords   ''~v197I~
            clipRectHeader = clipRect                                    ''~v197I~
            fileBMP.Dispose()                                              ''~v197I~
            fileBMP = saveFileBMP                                          ''~v197I~
            If xTextHeader.Length = 0 Then                             ''~v197I~
                headerBottomY = 0                                      ''~v197I~
                saveHeaderBottomY = 0                                  ''~v197I~
            Else                                                       ''~v197I~
                linesHeader = result.Lines  '*for markWords            ''~v197R~
            End If                                                     ''~v197I~
        End If                                                           ''~v197I~
        If swOK And footerTopY <> 0 Then                                        ''~v197I~
            phaseGetHeader = PGH_GETHEADER                                ''~v197R~
            clipRect = New Rectangle()                                     ''~v197I~
            clipRect.Width = bmpWidth                                      ''~v197I~
            clipRect.Y = footerTopY                                        ''~v197R~
            clipRect.Height = saveFileBMP.Height - footerTopY                ''~v197R~
            fileBMP = cutBMPRect(fileBMP, clipRect)                        ''~v197I~
            bmpWidth = fileBMP.Width                                       ''~v197I~
            bmpHeight = fileBMP.Height                                     ''~v197I~
            '*Trace.W("Class10:extractHeaderAndText clipRect hh=" & clipRect.Height & ",ww=" & clipRect.Width) ''~v197I~''~v201R~
            sortOption = SORT_HLINE_LTR '* = 4       '*sort horizontal line by Y:smallY-->largeY''~v197I~
            swOK = issueRun(Pfnm, Ptag)                                    ''~v197I~
            sortOption = saveSortOption                                    ''~v197I~
            '*Trace.W("Class10:extractHeaderAndText footer xText=" & xText)  ''~v197I~''~v201R~
            xTextFooter = xText                                            ''~v197I~
            resultTextAngleFooter = resultTextAngle '*use at markWords   ''~v197I~
            clipRectFooter = clipRect                                    ''~v197I~
            fileBMP.Dispose()                                              ''~v197I~
            fileBMP = saveFileBMP                                          ''~v197I~
            If xTextFooter.Length = 0 Then                                        ''~v197I~
                '*Trace.W("Class10:extractHeaderAndText footer xText=" & xText & " is null reset footerTopY to 0") ''~v197I~''~v201R~
                footerTopY = 0                                               ''~v197I~
            Else                                                       ''~v197I~
                linesFooter = result.Lines  '*for markWords            ''~v197I~
            End If                                                         ''~v197I~
        End If                                                           ''~v197I~
        If swOK Then                                                        ''~v196I~
            '*          xTextHeader = xText                                          ''~v196I~''~v197R~
            '*          fileBMP.Dispose()                                          ''~v196I~''~v197R~
            fileBMP = saveFileBMP                                        ''~v196I~
            clipRect.Y = saveHeaderBottomY                             ''~v196R~
            clipRect.Height = saveFileBMP.Height - saveHeaderBottomY   ''~v196R~
            If footerTopY <> 0 Then                                           ''~v197I~
                clipRect.Height -= saveFileBMP.Height - footerTopY      ''~v197R~
            End If                                                     ''~v197I~
            fileBMP = cutBMPRect(fileBMP, clipRect)                     ''~v196I~
            bmpWidth = fileBMP.Width                                   ''~v196I~
            bmpHeight = fileBMP.Height                                 ''~v196I~
            phaseGetHeader = PGH_GETTEXT                               ''~v196M~
            swOK = issueRun(Pfnm, Ptag)                                ''~v196I~
            phaseGetHeader = 0                                         ''~v196I~
            '*Trace.W("Class10:extractHeaderAndText NonHeader xText=" & xText) ''~v196I~''~v201R~
            fileBMP.Dispose()                                          ''~v196I~
            xText = xTextHeader & xText & vbCrLf & vbCrLf & xTextFooter                                  ''~v196R~''~v197R~
            '*Trace.W("Class10:extractHeaderAndText hdr and text xText=" & xText) ''~v196I~''~v201R~
        End If                                                         ''~v196I~
        fileBMP = saveFileBMP                                            ''~v196I~
        bmpWidth = fileBMP.Width                                       ''~v196I~
        bmpHeight = fileBMP.Height                                     ''~v196I~
        clipRect = saveClipRect                                         ''~v196R~
        headerBottomY = saveHeaderBottomY                                ''~v196I~
        Return swOK                                                    ''~v196I~
    End Function                                                       ''~v196I~
    '*************************************************************     ''~v196M~
    Private Sub getAverageCharSize(Pline As OcrLineV2, ByRef PhhAverage As Double, ByRef PwwAverage As Double, PswIgnoreSmall As Boolean) ''~v196M~''~v197R~
        '*Trace.W("Class10:getAvarageCharSize swIgnoreSmall=" & PswIgnoreSmall) ''~v197I~''~v201R~
        '*printLineRect("getAverageCharSize", Pline)                     ''~v197I~''~v201R~
        Dim hhTotal, wwTotal As Double                                 ''~v196M~
        Dim ctrChar As Integer = 0                                     ''~v196M~
        hhTotal = 0.0                                                  ''~v196M~
        wwTotal = 0.0                                                  ''~v196M~
        For Each word As OcrWord In Pline.Words                        ''~v196M~
            Dim brect As Rect = word.BoundingRect                      ''~v196M~
            hhTotal += brect.Height                                    ''~v196M~
            wwTotal += brect.Width                                     ''~v196M~
            ctrChar += word.Text.Length                                ''~v196M~
        Next                                                           ''~v196M~
        PhhAverage = hhTotal / ctrChar                                 ''~v196M~
        PwwAverage = wwTotal / ctrChar                                 ''~v196M~
        '*Trace.W("Class10:getAvarageCharSize ctrChar=" & ctrChar & ",hh=" & PhhAverage & ",ww=" & PwwAverage & ",text=" & Pline.Text) ''~v197I~''~v201R~
        If PswIgnoreSmall Then                                              ''~v197I~
            Dim ctrH2 As Integer = 0                                     ''~v197I~
            Dim ctrW2 As Integer = 0                                     ''~v197I~
            Dim avh2 As Double = PhhAverage * RATE_SMALL_CHAR          ''~v197I~
            Dim avw2 As Double = PwwAverage * RATE_SMALL_CHAR          ''~v197I~
            Dim hh2 As Double = 0                                     ''~v197I~
            Dim ww2 As Double = 0                                     ''~v197I~
            For Each word As OcrWord In Pline.Words                    ''~v197I~
                Dim brect As Rect = word.BoundingRect                  ''~v197I~
                ''~v197I~
                Dim hh As Double = brect.Height                          ''~v197R~
                If hh >= avh2 Then                                     ''~v197R~
                    hh2 += brect.Height                                  ''~v197I~
                    ctrH2 += word.Text.Length                            ''~v197I~
                End If                                                 ''~v197I~
                Dim ww As Double = brect.Width / word.Text.Length          ''~v197I~
                If ww >= avw2 Then                                     ''~v197R~
                    ww2 += brect.Width                                   ''~v197I~
                    ctrW2 += word.Text.Length                            ''~v197I~
                End If                                                 ''~v197I~
            Next                                                       ''~v197I~
            If ctrH2 <> 0 Then                                                ''~v197I~
                PhhAverage = hh2 / ctrH2                                   ''~v197I~
            End If                                                     ''~v197I~
            If ctrW2 <> 0 Then                                                ''~v197I~
                PwwAverage = ww2 / ctrW2                                   ''~v197I~
            End If                                                     ''~v197I~
            '*Trace.W("Class10:getAvarageCharSize except small avh2=" & avh2 & ",avw2=" & avw2 & ",ctrW2=" & ctrW2 & ",ctrH2=" & ctrH2 & ",hh2=" & hh2 & ",ww2=" & ww2 & ",text=" & Pline.Text) ''~v197R~''~v201R~
        End If                                                         ''~v197I~
        '*Trace.W("Class10:getAvarageCharSize ctrChar=" & ctrChar & ",hh=" & PhhAverage & ",ww=" & PwwAverage & ",text=" & Pline.Text) ''~v196M~''~v197R~''~v201R~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub getAverageCharSize(Pline As OcrLine, ByRef PhhAverage As Double, ByRef PwwAverage As Double) ''~v196M~
        Dim hhTotal, wwTotal As Double                                 ''~v196M~
        Dim ctrChar As Integer = 0                                     ''~v196M~
        hhTotal = 0.0                                                  ''~v196M~
        wwTotal = 0.0                                                  ''~v196M~
        For Each word As OcrWord In Pline.Words                        ''~v196M~
            Dim brect As Rect = word.BoundingRect                      ''~v196M~
            hhTotal += brect.Height                                    ''~v196M~
            wwTotal += brect.Width                                     ''~v196M~
            ctrChar += word.Text.Length                                ''~v196M~
        Next                                                           ''~v196M~
        PhhAverage = hhTotal / ctrChar                                 ''~v196M~
        PwwAverage = wwTotal / ctrChar                                 ''~v196M~
        '*Trace.W("Class10:getAverageCharSize hh=" & PhhAverage & ",ww=" & PwwAverage & ",text=" & Pline.Text) ''~v196M~''~v197R~''~v201R~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v196M~
    Private Sub getMaxCharSize(Pline As OcrLine, ByRef PhhMax As Double, ByRef PwwMax As Double) ''~v196M~
        Dim hhMax As Double = 0.0                                      ''~v196M~
        Dim wwMax As Double = 0.0                                      ''~v196M~
        For Each word As OcrWord In Pline.Words                        ''~v196M~
            Dim brect As Rect = word.BoundingRect                      ''~v196M~
            If hhMax < brect.Height Then                                      ''~v196M~
                hhMax = brect.Height                                     ''~v196M~
            End If                                                     ''~v196M~
            If wwMax < brect.Width Then                                       ''~v196M~
                wwMax = brect.Width                                      ''~v196M~
            End If                                                     ''~v196M~
        Next                                                           ''~v196M~
        PhhMax = hhMax                                                   ''~v196M~
        PwwMax = wwMax                                                   ''~v196M~
        '*Trace.W("Class10:getMaxCharSize hhMax=" & hhMax & ",wwMax=" & wwMax) ''~v196M~''~v201R~
    End Sub                                                            ''~v196M~
    '*************************************************************     ''~v199I~
    Private Function getCenterX(Plist As List(Of OcrLineV2), PminLeft As Double, PmaxRight As Double) As Double ''~v199R~
        '*      Dim centerY, lowerTop, upperBottom, lowerTopMin, upperBottomMax As Double''~v199R~
        Dim brect As Rect                                              ''~v199R~
        Dim boundary, gap, left, right, rightPrev, leftMin, rightMax As Double ''~v199R~
        Dim gapMax As Double = 0.0                                     ''~v199R~
        Dim gapMaxCenterX As Double = 0.0                                ''~v199I~
        Dim lineStyle As Integer                                       ''~v199R~
        Dim chkBoundary As Double() = New Double() {0.0, -0.05, 0.05, -0.1, 0.1, -0.15, 0.15, -0.2, 0.2} ''~v199R~
        '*************************************                         ''~v199I~
        '*Trace.W("class10:getCenterX PminLeft=" & PminLeft & ",PmaxRight=" & PmaxRight) ''~v199R~''~v201R~
        For ii As Integer = 0 To chkBoundary.Count - 1                   ''~v199R~
            boundary = (PminLeft + PmaxRight) / 2 + (PmaxRight - PminLeft) * chkBoundary(ii) ''~v199I~
            '*Trace.W("class10:getCenterX ii=" & ii & ",boundary=" & boundary) ''~v199R~''~v201R~
            Dim swOverride As Boolean                                  ''~v199I~
            leftMin = 0.0                                              ''~v199I~
            rightMax = 0.0                                             ''~v199I~
            For Each line As OcrLineV2 In Plist                        ''~v199R~
                '*printLineRect("getCenterX", line)                      ''~v199I~''~v201R~
                lineStyle = line.lineStyle '*chk text in word=False    ''~v199R~
            	swOverride = False                                     ''~v199I~
                If lineStyle <> LS_HORIZONTAL Then                     ''~v199R~
'*                  Continue For                                       ''~v199R~
                    getLineRect(line, left, Nothing, right, Nothing)   ''~v199I~
                    If right < boundary Then                           ''~v199I~
                        If right > rightMax Then                       ''~v199I~
                            rightMax = right                           ''~v199I~
                        End If                                         ''~v199I~
                        Continue For                                   ''~v199I~
                    ElseIf left > boundary Then                        ''~v199I~
                        If leftMin = 0.0 Or left < leftMin Then        ''~v199I~
                            leftMin = left                             ''~v199I~
                        End If                                         ''~v199I~
                        Continue For                                   ''~v199I~
                    Else                                               ''~v199I~
                        If lineStyle <> LS_HORIZONTAL Then             ''~v199I~
                            swOverride = True                          ''~v199I~
                            Exit For                                   ''~v199I~
                        End If                                         ''~v199I~
                    End If                                             ''~v199I~
                End If                                                 ''~v199R~
                rightPrev = 0.0                                        ''~v199R~
                For Each word As OcrWord In line.Words                 ''~v199R~
                    brect = word.BoundingRect                          ''~v199R~
                    left = brect.X                                       ''~v199I~
                    right = brect.X + brect.Width                          ''~v199I~
                    If left = boundary Then                            ''~v199I~
                        swOverride = True                                ''~v199I~
                        Exit For                                       ''~v199I~
                    End If                                             ''~v199I~
                    If left > boundary Then                                   ''~v199R~
                        If leftMin = 0.0 Or left < leftMin Then          ''~v199R~
                            leftMin = left                             ''~v199I~
                        End If                                         ''~v199I~
                        If rightPrev >= boundary Then  '*space between word is on boundary''~v199R~
                            swOverride = True                            ''~v199R~
                        End If                                       ''~v199I~
                        Exit For                                       ''~v199I~
                    End If                                             ''~v199R~
                    If right < boundary Then                                  ''~v199I~
                        If right > rightMax Then                       ''~v199R~
                            rightMax = right                           ''~v199I~
                        End If                                         ''~v199I~
                    End If                                             ''~v199I~
                    rightPrev = right                                    ''~v199I~
                Next                                                   ''~v199R~
                '*Trace.W("class10:getCenterX swOverride=" & swOverride & ",rightMax=" & rightMax & ",leftMin=" & leftMin & ",boundary=" & boundary) ''~v199R~''~v201R~
                If swOverride Then                                          ''~v199I~
                    Exit For                                           ''~v199R~
                End If                                                 ''~v199I~
            Next '*all line                                            ''~v199R~
            '*Trace.W("class10:getCenterX ii=" & ii & ",boundary=" & boundary & ",swOverride=" & swOverride & ",leftMin=" & leftMin & ",rightMax=" & rightMax) ''~v199I~''~v201R~
            If swOverride Then                                              ''~v199I~
                Continue For   '*try next boundary                     ''~v199I~
            End If                                                     ''~v199I~
            If leftMin <> 0 And rightMax <> 0 Then                              ''~v199I~
                gap = leftMin - rightMax                                   ''~v199I~
                '*Trace.W("class10:getCenterX gap=" & gap & ",leftMin=" & leftMin & ",rightMax=" & rightMax & ",gapMaxCenterX=" & gapMaxCenterX)''~v199M~''~v201R~
                If gap > gapMax Then                                   ''~v199R~
                	gapMax = gap                                       ''~v199R~
                	gapMaxCenterX = rightMax + gap / 2                 ''~v199R~
                	'*Trace.W("class10:getCenterX gapMax=" & gapMax & ",leftMin=" & leftMin & ",rightMax=" & rightMax & ",gapMaxCenterX=" & gapMaxCenterX)''~v199R~''~v201R~
                    '*Trace.W("class10:for gap by gapRate="  & ((PmaxRight - PminLeft) * RATE_CENTERX_GAP))''~v199I~''~v201R~
                	If gap > (PmaxRight - PminLeft) * RATE_CENTERX_GAP Then''~v199R~
                    	'*Trace.W("class10:getCenterX exit by gap rate gap=" & gap & ",PmaxRight=" & PmaxRight & ",PminLeft=" & PminLeft)''~v199R~''~v201R~
                    	Exit For                                       ''~v199R~
                	End If                                             ''~v199R~
                End If                                                 ''~v199R~
            End If                                                     ''~v199I~
        Next '*chkBoundary constant                                    ''~v199I~
        '*Trace.W("Class10:getCenterX return centerX=" & gapMaxCenterX)  ''~v199R~''~v201R~
        Return gapMaxCenterX                                           ''~v199R~
    End Function                                                       ''~v199I~
    '*************************************************************     ''~v199I~
    Private Function getCenterXVertical(Plist As IReadOnlyList(Of OcrLine), PminLeft As Double, PmaxRight As Double) As Double ''~v199R~
        Dim brect As Rect                                              ''~v199I~
        Dim boundary, gap, left, right, rightPrev, leftMin, rightMax As Double ''~v199I~
        Dim gapMax As Double = 0.0                                     ''~v199I~
        Dim gapMaxCenterX As Double = 0.0                              ''~v199I~
        Dim lineStyle As Integer                                       ''~v199I~
        Dim chkBoundary As Double() = New Double() {0.0, -0.05, 0.05, -0.1, 0.1, -0.15, 0.15, -0.2, 0.2} ''~v199I~
        '*************************************                         ''~v199I~
        '*Trace.W("class10:getCenterXVertical PminLeft=" & PminLeft & ",PmaxRight=" & PmaxRight) ''~v199R~''~v201R~
        For ii As Integer = 0 To chkBoundary.Count - 1                 ''~v199I~
            boundary = (PminLeft + PmaxRight) / 2 + (PmaxRight - PminLeft) * chkBoundary(ii) ''~v199I~
            '*Trace.W("class10:getCenterXVertical ii=" & ii & ",boundary=" & boundary) ''~v199R~''~v201R~
            Dim swOverride As Boolean                                  ''~v199I~
            leftMin = 0.0                                              ''~v199I~
            rightMax = 0.0                                             ''~v199I~
            For Each line As OcrLine In Plist                        ''~v199I~
                '*printLineRect("getCenterXVertical", line)              ''~v199R~''~v201R~
                '               lineStyle = line.lineStyle '*chk text in word=False    ''~v199R~
                lineStyle = getLineStyle(line, True) '*chk text in word=true''~v199I~
                getLineRect(line, left, Nothing, right, Nothing)        ''~v199I~
                swOverride = False                                     ''~v199M~
                If right < boundary Then                                      ''~v199I~
                    If right > rightMax Then                           ''~v199I~
                        rightMax = right                               ''~v199I~
                    End If                                             ''~v199I~
                    Continue For                                       ''~v199I~
                ElseIf left > boundary Then                                   ''~v199I~
                    If leftMin = 0.0 Or left < leftMin Then            ''~v199I~
                        leftMin = left                                 ''~v199I~
                    End If                                             ''~v199I~
                    Continue For                                       ''~v199I~
                Else                                                   ''~v199I~
                    If lineStyle <> LS_HORIZONTAL Then                 ''~v199I~
                        swOverride = True                                ''~v199I~
                        Exit For                                       ''~v199I~
                    End If                                             ''~v199I~
                End If                                                 ''~v199I~
                '*hline ovrride boundary                               ''~v199I~
                rightPrev = 0.0                                        ''~v199I~
                For Each word As OcrWord In line.Words                 ''~v199I~
                    brect = word.BoundingRect                          ''~v199I~
                    left = brect.X                                     ''~v199I~
                    right = brect.X + brect.Width                      ''~v199I~
                    If left = boundary Then                            ''~v199I~
                        swOverride = True                              ''~v199I~
                        Exit For                                       ''~v199I~
                    End If                                             ''~v199I~
                    If left > boundary Then                            ''~v199I~
                        If leftMin = 0.0 Or left < leftMin Then        ''~v199I~
                            leftMin = left                             ''~v199I~
                        End If                                         ''~v199I~
                        If rightPrev >= boundary Then  '*space between word is on boundary''~v199I~
                            swOverride = True                          ''~v199I~
                        End If                                         ''~v199I~
                        Exit For                                       ''~v199I~
                    End If                                             ''~v199I~
                    If right < boundary Then                           ''~v199I~
                        If right > rightMax Then                       ''~v199I~
                            rightMax = right                           ''~v199I~
                        End If                                         ''~v199I~
                    End If                                             ''~v199I~
                    rightPrev = right                                  ''~v199I~
                Next                                                   ''~v199I~
                '*Trace.W("class10:getCenterXVertical swOverride=" & swOverride & ",rightMax=" & rightMax & ",leftMin=" & leftMin & ",boundary=" & boundary) ''~v199R~''~v201R~
                If swOverride Then                                     ''~v199I~
                    Exit For                                           ''~v199I~
                End If                                                 ''~v199I~
            Next '*all line                                            ''~v199I~
            '*Trace.W("class10:getCenterXVertical ii=" & ii & ",boundary=" & boundary & ",swOverride=" & swOverride & ",leftMin=" & leftMin & ",rightMax=" & rightMax) ''~v199R~''~v201R~
            If swOverride Then                                         ''~v199I~
                Continue For   '*try next boundary                     ''~v199I~
            End If                                                     ''~v199I~
            If leftMin <> 0 And rightMax <> 0 Then                     ''~v199I~
                gap = leftMin - rightMax                               ''~v199I~
                If gap > gapMax Then                                   ''~v199R~
                	gapMax = gap                                       ''~v199R~
                    gapMaxCenterX = rightMax + gap / 2                 ''~v199R~
                    '*Trace.W("class10:getCenterXVertical gap=" & gap & ",leftMin=" & leftMin & ",rightMax=" & rightMax & ",gapMaxCenterX=" & gapMaxCenterX)''~v199R~''~v201R~
                    '*Trace.W("class10:for gap by gapRate="  & ((PmaxRight - PminLeft) * RATE_CENTERX_GAP))''~v199R~''~v201R~
                    If gap > (PmaxRight - PminLeft) * RATE_CENTERX_GAP Then''~v199R~
                        '*Trace.W("class10:getCenterXVertical exit by gap rate gap=" & gap & ",PmaxRight=" & PmaxRight & ",PminLeft=" & PminLeft)''~v199R~''~v201R~
                        Exit For                                       ''~v199R~
                    End If                                             ''~v199R~
                End If                                                 ''~v199R~
            End If                                                     ''~v199I~
        Next '*chkBoundary constant                                    ''~v199I~
        '*Trace.W("Class10:getCenterXVertical return centerX=" & gapMaxCenterX) ''~v199R~''~v201R~
        Return gapMaxCenterX                                           ''~v199I~
    End Function                                                       ''~v199I~
    '**************************************************************************************************''~v199I~
    Private Sub setHorizontal2(Plist As IReadOnlyList(Of OcrLine), PlistV2 As List(Of OcrLineV2)) ''~v199I~
        Dim brect As Rect                                              ''~v199I~
        '*      Dim ww As Double                                               ''~v199R~
        '*      Dim hh As Double                                               ''~v199R~
        '*      Dim diffMin As Double                                          ''~v199R~
        '*      Dim topY As Double                                             ''~v199R~
        '*      Dim ctrTopY As Integer = 0                                     ''~v199R~
        '*      Dim topYAll As Double                                          ''~v199R~
        '*      Dim topYAve As Double                                          ''~v199R~
        Dim minLeft As Double = 0.0                                    ''~v199I~
        Dim maxRight As Double = 0.0                                   ''~v199I~
        '*      Dim minTop As Double = 0.0                                     ''~v199R~
        '*      Dim maxBottom As Double = 0.0                                  ''~v199R~
        Dim centerX,left,right As Double                               ''~v199R~
        '*      Dim centerY As Double
        Dim lineStyle, lineNo As Integer ''~v199R~
        '*************************************                         ''~v199I~
        Dim listLinesV2 As New List(Of OcrLineV2)                      ''~v199M~
        setVertical2Copy(Plist, listLinesV2)                             ''~v199I~
        '*** determine boundary of upper and lower page of vertical 2  ''~v199I~
        Dim ctrCharsMax As Integer = 0                                 ''~v199I~
        '*      For Each line As OcrLine In Plist                              ''~v199R~
        lineNo = 0
        For Each line As OcrLineV2 In listLinesV2                         ''~v199I~
            lineStyle = getLineStyle(line, true) '*chk text in word=true''~v199R~
	        '*Trace.W("Class10:setHorizontal2 lineStyle=0x" & Hex(lineStyle) & ",text=" & line.Text)''~v199I~''~v201R~
            line.lineStyle = lineStyle                                   ''~v199I~
            getLineRect(line,left, Nothing, right, Nothing)            ''~v199R~
            If minLeft = 0.0 Or left<minLeft                           ''~v199R~
                minLeft = left                                         ''~v199R~
            End If                                                     ''~v199R~
            If right > maxRight Then                                   ''~v199R~
                maxRight = right                                       ''~v199R~
            End If                                                     ''~v199R~
            lineNo += 1
        Next                                                           ''~v199I~
        '*Trace.W("Class10:setHorizontal2 maxRight=" & maxRight & ",minLeft=" & minLeft) ''~v199R~''~v201R~
        '*      centerX = (maxRight + minLeft) / 2                             ''~v199R~
        centerX = getCenterX(listLinesV2, minLeft, maxRight)           ''~v199R~
        '*      centerY = (minTop + maxBottom) / 2                     ''~v199I~
        '*      centerY = getCenterY(Plist, minTop, maxBottom)                 ''~v199R~
        '*Trace.W("Class10:setHorizontal2 center X=" & centerX)          ''~v199R~''~v201R~
        '*      topYAve = centerY                                              ''~v199R~
        '*      Dim listLinesUpper As New List(Of OcrLineV2)                   ''~v199R~
        '*      Dim listLinesLower As New List(Of OcrLineV2)                   ''~v199R~
        '*      For Each line As OcrLine In Plist                              ''~v199R~
        For Each line As OcrLineV2 In listLinesV2                         ''~v199I~
            '*          Dim wordsUpper As New List(Of OcrWord)                     ''~v199R~
            '*          Dim wordsLower As New List(Of OcrWord)                     ''~v199R~
            Dim wordsLeft As New List(Of OcrWord)                     ''~v199I~
            Dim wordsRight As New List(Of OcrWord)                     ''~v199I~
            '*          Dim swUpper As Boolean = False                             ''~v199R~
            '*          Dim swLower As Boolean = False                             ''~v199R~
            Dim swLeft As Boolean = False                              ''~v199I~
            '*Trace.W("Class10:setHorizontal2 split Left/Rightr text=" & line.Text) ''~v199R~''~v201R~
            For Each word As OcrWord In line.Words                     ''~v199I~
                brect = word.BoundingRect                              ''~v199I~
                '*              If (brect.Y > topYAve) Then                            ''~v199R~
                If brect.X >= centerX Then                             ''~v199R~
                    '*                  wordsLower.Add(word)                               ''~v199R~
                    wordsRight.Add(word)                               ''~v199I~
                    '*                  swLower = True                                     ''~v199R~
                    swLeft = False                                    ''~v199I~
                    '*Trace.W("Class10:setHorizontal2 right page text=" & word.Text & ",centerX=" & centerX & ",rectY=" & brect.Y & ",rectX=" & brect.X)''~v199R~''~v201R~
                Else                                                   ''~v199I~
                    '*                  wordsUpper.Add(word)                               ''~v199R~
                    wordsLeft.Add(word)                                ''~v199I~
                    '*                  swUpper = True                                     ''~v199R~
                    swLeft = True                                     ''~v199I~
                    '*Trace.W("Class10:setHorizontal2 left page text=" & word.Text & ",centerX=" & centerX & ",rectY=" & brect.Y & ",rectX=" & brect.X)''~v199R~''~v201R~
                End If                                                 ''~v199I~
                '*              If brect.X < centerX Then                              ''~v199R~
                '*                  swLeft = True                                      ''~v199R~
                '*              End If                                                 ''~v199R~
            Next                                                       ''~v199I~
            '*          If swUpper Then                                            ''~v199R~
            If swLeft Then                                            ''~v199I~
                Dim v2 As New OcrLineV2                                ''~v199I~
                '*              v2.Words = wordsUpper                                  ''~v199R~
                v2.Words = wordsLeft                                   ''~v199I~
                '*              v2.posLayout = LP_UPPER                                ''~v199R~
                v2.posLayout = LP_LEFT                                 ''~v199I~
                v2.lineStyle = getLineStyle(line, True) '*chk text in word=True''~v199R~
                '*              If swLeft Then                                         ''~v199R~
                '*                  v2.posLayout += LP_LEFT                            ''~v199R~
                '*              End If                                                 ''~v199R~
                '*              listLinesUpper.Add(v2)                                 ''~v199R~
                PlistV2.Add(v2)                                        ''~v199R~
        		'*printLineRect("setHorizontal2 left line",v2)           ''~v199I~''~v201R~
                '*          End If                                                     ''~v199R~
                '*          If swLower Then                                            ''~v199R~
            Else                                                       ''~v199I~
                Dim v2 As New OcrLineV2                                ''~v199I~
                '*              v2.Words = wordsLower                                  ''~v199R~
                v2.Words = wordsRight                                  ''~v199I~
                v2.lineStyle = getLineStyle(line, True) '*chk text in word=True''~v199R~
                '*              v2.posLayout = LP_LOWER                                ''~v199R~
                '*              If swLeft Then                                         ''~v199R~
                '*                  v2.posLayout += LP_LEFT                            ''~v199R~
                '*              End If                                                 ''~v199R~
                '*              listLinesLower.Add(v2)                                 ''~v199R~
        		'*printLineRect("setHorizontal2 right line",v2)          ''~v199I~''~v201R~
                PlistV2.Add(v2)                                        ''~v199R~
            End If                                                     ''~v199I~
        Next                                                           ''~v199I~
        '*      Dim listLinesV2 As New List(Of OcrLineV2)                      ''~v199R~
        '*      For Each line As OcrLineV2 In listLinesUpper                   ''~v199R~
        '*          PlistV2.Add(line)                                          ''~v199R~
        '*      Next                                                           ''~v199R~
        '*      For Each line As OcrLineV2 In listLinesLower                   ''~v199R~
        '*          PlistV2.Add(line)                                          ''~v199R~
        '*      Next                                                           ''~v199R~
        For Each line As OcrLineV2 In PlistV2                          ''~v199R~
            Dim sbText As System.Text.StringBuilder = New System.Text.StringBuilder()''~v199R~
            Dim sw1st as Boolean=true                                  ''~v200I~
            For Each word As OcrWord In line.Words                     ''~v199R~
        		If sortOption = SORT_H2_ENGLISH                        ''~v200M~
                	if sw1st                                           ''~v200I~
                    	sw1st=false                                    ''~v200I~
                    else                                               ''~v200I~
	                	sbText.Append(" ")                             ''~v200I~
                    end if                                             ''~v200I~
        		End If                                                 ''~v200M~
                sbText.Append(word.Text)                               ''~v199R~
            Next                                                       ''~v199I~
            line.Text = sbText.ToString()                              ''~v199I~
            '*Trace.W("Class10:setHorizontal2 line Text=" & line.Text)   ''~v199I~''~v201R~
        Next                                                           ''~v199I~
        '*printLinesWords("setHorizontal2 exit",PlistV2)                 ''~v199R~''~v201R~
    End Sub                                                            ''~v199I~
End Class
'*************************************************************
' Windows manual
'Public Delegate Sub AsyncOperationCompletehandler(IAsyncOperation, AsyncStatus)
'*************************************************************
Public Module TaskExtensionModule
    '***add to the class/Interface(1st parm of Function/Sub) extended Function/Sub with <Extension> prefix
    '*************************************************************
    <Extension()>
    Public Function AsTask(Of TResult)(Poper As IAsyncOperation(Of TResult)) As Task(Of TResult)
        Dim mTCS = New TaskCompletionSource(Of TResult)()
        '       Dim notifier = New DelegateNotifier(Of T)(AddressOf TaskNotifier(Of T))
        '       Poper.Completed = New AsyncOperationCompletedHandler(Of T)(Sub(Poper2, Pstatus2)
        '                                                                      TaskNotifier(Of T)(Poper2, Pstatus2)
        '                                                                  End Sub)
        Try
            Poper.Completed = New AsyncOperationCompletedHandler(Of TResult)(Sub(Poper2, Pstatus2)
                                                                                 TaskNotifier(Poper2, Pstatus2, mTCS)
                                                                             End Sub)  'void AsyncOperationCompletionHandler(IAsyncOperation,AsyncStatus)
        Catch ex As Exception
            Form1.showStatus("OCR:AsTask :" & ex.Message)              ''~v140R~
        End Try
        Return mTCS.Task
    End Function
    '*************************************************************
    <Extension()>
    Public Function GetAwaiter(Of TResult)(Poper As IAsyncOperation(Of TResult)) As TaskAwaiter(Of TResult)
        '       Return Poper.AsTask().GetAwaiter()
        Dim tsk As Task(Of TResult) = Poper.AsTask()
        Dim w As TaskAwaiter(Of TResult) = tsk.GetAwaiter()
        Return w
    End Function
    '   <Extension()>
    Public Sub TaskNotifier(Of TResult)(Poper As IAsyncOperation(Of TResult), Pstatus As AsyncStatus, PmTCS As TaskCompletionSource(Of TResult))
        Select Case Pstatus
            Case AsyncStatus.Completed
                PmTCS.SetResult(Poper.GetResults())
                Poper.Close() 'IAsyncOperation Interface inherit IAsyncInfo, IAsyncInfo has Close() it is requires adter GetResult()
            Case AsyncStatus.Error
                PmTCS.SetException(Poper.ErrorCode) 'ErrorCode is in IAsyncInfo
            Case AsyncStatus.Canceled
                PmTCS.SetCanceled()
        End Select
    End Sub
End Module
