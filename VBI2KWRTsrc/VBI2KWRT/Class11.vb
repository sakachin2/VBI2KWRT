'CID:''+v106R~:#72                             update#=  171;         ''+v106I~
'************************************************************************************''+v106I~
'v106 2017/12/20 partially extract from image(box by mouse dragging)   ''+v106I~
'************************************************************************************''+v106I~
Imports System.Windows.Forms                                           ''+v106I~
Imports System.Drawing.Imaging                                         ''+v106I~
Public Class ImageCut                                                  ''+v106I~
    Private PB As PictureBox                                           ''+v106I~
    Private X1, Y1, X2, Y2, boxw, boxh As Integer                      ''+v106I~
    Private swMove, swDown As Boolean                                  ''+v106I~
    Private bmpForRect As Bitmap = Nothing                             ''+v106I~
    Private bmpRect As Bitmap = Nothing                                ''+v106I~
    Private boxRect As Rectangle                                       ''+v106I~
    '**********************************                                ''+v106I~
    Public Sub New(Ppb As PictureBox)                                  ''+v106I~
        PB = Ppb                                                       ''+v106I~
    End Sub                                                            ''+v106I~
    '****************************************************************************''+v106I~
    Public Sub mouseDown(e As MouseEventArgs, Pbmp As Bitmap)          ''+v106I~
        X1 = e.X                                                       ''+v106I~
        Y1 = e.Y                                                       ''+v106I~
        X2 = e.X                                                       ''+v106I~
        Y2 = e.Y                                                       ''+v106I~
        Trace.W("mouseDown x1=" & X1 & ",Y1=" & Y1 & ",swMove=" & swMove) ''+v106I~
        swDown = True                                                  ''+v106I~
        swMove = False                                                 ''+v106I~
        bmpForRect = Pbmp  'Zoomed Imaged on PictureBox(not wordBMP)   ''+v106I~
        Trace.W("mouseDown bmp W=" & Pbmp.Width & ",H=" & Pbmp.Height) ''+v106I~
    End Sub                                                            ''+v106I~
    '****************************************************************************''+v106I~
    Public Function mouseMove(e As MouseEventArgs, ByRef Ppbmp As Bitmap, ByRef PpboxRect As Rectangle) As Boolean ''+v106I~
        '* rc :true, rectangle draw to bmp clone                       ''+v106I~
        X2 = e.X                                                       ''+v106I~
        Y2 = e.Y                                                       ''+v106I~
        If Not swDown Then                                             ''+v106I~
            Return False                                               ''+v106I~
        End If                                                         ''+v106I~
        Trace.W("mouseMove x2=" & X2 & ",Y2=" & Y2 & ",swMove=" & swMove & ",swDown=" & swDown) ''+v106I~
        swMove = True                                                  ''+v106I~
        Dim rc As Boolean = drawBox(bmpForRect, Ppbmp)                 ''+v106I~
        PpboxRect = boxRect                                            ''+v106I~
        Trace.W("mouseMove rc=" & rc)                                  ''+v106I~
        Return rc                                                      ''+v106I~
    End Function                                                       ''+v106I~
    '****************************************************************************''+v106I~
    Public Function mouseUp(e As MouseEventArgs, ByRef Ppbmp As Bitmap, PpboxRect As Rectangle) As Boolean ''+v106I~
        X2 = e.X                                                       ''+v106I~
        Y2 = e.Y                                                       ''+v106I~
        Trace.W("mouseUp x2=" & X2 & ",Y2=" & Y2 & ",swMove=" & swMove) ''+v106I~
        swDown = False                                                 ''+v106I~
        If Not swMove Then                                             ''+v106I~
            Return False                                               ''+v106I~
        End If                                                         ''+v106I~
        swMove = False                                                 ''+v106I~
        Dim rc As Boolean = drawBox(bmpForRect, Ppbmp)                 ''+v106I~
        '       Ppbmp.Save("W:\mouseup.bmp", ImageFormat.BMP) '@@@@test        ''+v106I~
        PpboxRect = boxRect     'box on PictureBox image               ''+v106I~
        Trace.W("mouseUp rc=" & rc)                                    ''+v106I~
        Return rc                                                      ''+v106I~
    End Function                                                       ''+v106I~
    '****************************************************************************''+v106I~
    Public Function drawBox(Pbmp As Bitmap, ByRef Ppbmp As Bitmap) As Boolean ''+v106I~
        '* rc :true, rectangle draw to bmp clone                       ''+v106I~
        Ppbmp = Nothing                                                ''+v106I~
        Dim ww, hh, xx1, yy1, xx2, yy2 As Integer                      ''+v106I~
        Trace.W("drawBox X1=" & X1 & ",Y1=" & Y1 & ",X2=" & X2 & ",Y2=" & Y2) ''+v106I~
        Trace.W("drawBox bmpW =" & Pbmp.Width & ",bmpH=" & Pbmp.Height) ''+v106I~
        xx1 = Math.Min(X1, X2)                                         ''+v106I~
        yy1 = Math.Min(Y1, Y2)                                         ''+v106I~
        xx2 = Math.Max(X1, X2)                                         ''+v106I~
        yy2 = Math.Max(Y1, Y2)                                         ''+v106I~
        xx1 = Math.Min(xx1, Pbmp.Width)                                ''+v106I~
        xx2 = Math.Min(xx2, Pbmp.Width)                                ''+v106I~
        yy1 = Math.Min(yy1, Pbmp.Height)                               ''+v106I~
        yy2 = Math.Min(yy2, Pbmp.Height)                               ''+v106I~
        ww = xx2 - xx1                                                 ''+v106I~
        hh = yy2 - yy1                                                 ''+v106I~
        boxRect = New Rectangle(xx1, yy1, ww, hh) 'on PictureBox       ''+v106I~
        Trace.W("drawBox xx1=" & xx1 & ",yy1=" & yy1 & ",xx2=" & xx2 & ",yy2=" & yy2 & ",ww=" & ww & ",hh=" & hh) ''+v106I~
        If ww = 0 OrElse hh = 0 Then                                   ''+v106I~
            Return False                                               ''+v106I~
        End If                                                         ''+v106I~
        Try                                                            ''+v106I~
            Dim bmp As Bitmap = CType(Pbmp.Clone(), Bitmap)            ''+v106I~
            Dim rect As Rectangle = New Rectangle(xx1, yy1, ww, hh)    ''+v106I~
            Trace.W("drawBox ww=" & ww & ",hh=" & hh)                  ''+v106I~
            '            Dim g As Graphics                                         ''+v106I~
            '            Try   ' chk indexd pixcel format                          ''+v106I~
            '                g = Graphics.FromImage(bmp)                           ''+v106I~
            '            Catch ex As Exception                                     ''+v106I~
            '                bmp = Index2NonIndex(bmp)                             ''+v106I~
            '                g = Graphics.FromImage(bmp)                           ''+v106I~
            '            End Try                                                   ''+v106I~
            '            Dim colorpen As Pen = Pens.Blue                           ''+v106I~
            '            g.DrawRectangle(colorpen, rect)                           ''+v106I~
            '            g.Dispose()                                               ''+v106I~
            drawRect(bmp, rect)                                         ''+v106I~
            Ppbmp = bmp                                                ''+v106I~
        Catch ex As Exception                                          ''+v106I~
            MessageBox.Show("drawBox exception:" & ex.Message)         ''+v106I~
        End Try                                                        ''+v106I~
        Return True                                                    ''+v106I~
    End Function                                                       ''+v106I~
    '****************************************************************************''+v106I~
    Public Function drawRect(Pbmp As Bitmap, Prect As Rectangle) As Boolean      ''+v106I~
        Try                                                            ''+v106I~
            Dim g As Graphics                                          ''+v106I~
            Dim colorpen As Pen = Pens.Blue                            ''+v106I~
            g = Graphics.FromImage(Pbmp)                               ''+v106I~
            g.DrawRectangle(colorpen, Prect)                           ''+v106I~
            g.Dispose()                                                ''+v106I~
        Catch ex As Exception                                          ''+v106I~
            MessageBox.Show("drawRect exception:" & ex.Message)        ''+v106I~
            Return False                                               ''+v106I~
        End Try                                                        ''+v106I~
        Return True                                                    ''+v106I~
    End Function                                                       ''+v106I~
    '****************************************************************************''+v106I~
    Private Function Index2NonIndex(Psrc As Bitmap) As Bitmap          ''+v106I~
        Dim r As New Rectangle(0, 0, Psrc.Width, Psrc.Height)          ''+v106I~
        Dim bmpNonIndexed = Psrc.Clone(r, Imaging.PixelFormat.Format32bppArgb) ''+v106I~
        Dim fmt As Imaging.PixelFormat = bmpNonIndexed.PixelFormat     ''+v106I~
        Return bmpNonIndexed                                           ''+v106I~
    End Function                                                       ''+v106I~
End Class
