<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class DocOptions
    Inherits System.Windows.Forms.Form

    'フォームがコンポーネントの一覧をクリーンアップするために dispose をオーバーライドします。
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows フォーム デザイナーで必要です。
    Private components As System.ComponentModel.IContainer

    'メモ: 以下のプロシージャは Windows フォーム デザイナーで必要です。
    'Windows フォーム デザイナーを使用して変更できます。  
    'コード エディターを使って変更しないでください。
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(DocOptions))
        Me.CheckBoxBES99 = New System.Windows.Forms.CheckBox()
        Me.CheckBoxKatakanaDoc = New System.Windows.Forms.CheckBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.ButtonCancel = New System.Windows.Forms.Button()
        Me.ButtonHelp = New System.Windows.Forms.Button()
        Me.ButtonOK = New System.Windows.Forms.Button()
        Me.CheckBoxEnglishDoc = New System.Windows.Forms.CheckBox()
        Me.CheckBoxUTF8 = New System.Windows.Forms.CheckBox()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'CheckBoxBES99
        '
        resources.ApplyResources(Me.CheckBoxBES99, "CheckBoxBES99")
        Me.CheckBoxBES99.Name = "CheckBoxBES99"
        Me.CheckBoxBES99.UseVisualStyleBackColor = True
        '
        'CheckBoxKatakanaDoc
        '
        resources.ApplyResources(Me.CheckBoxKatakanaDoc, "CheckBoxKatakanaDoc")
        Me.CheckBoxKatakanaDoc.Name = "CheckBoxKatakanaDoc"
        Me.CheckBoxKatakanaDoc.UseVisualStyleBackColor = True
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.ButtonCancel)
        Me.Panel1.Controls.Add(Me.ButtonHelp)
        Me.Panel1.Controls.Add(Me.ButtonOK)
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Name = "Panel1"
        '
        'ButtonCancel
        '
        resources.ApplyResources(Me.ButtonCancel, "ButtonCancel")
        Me.ButtonCancel.Name = "ButtonCancel"
        Me.ButtonCancel.UseVisualStyleBackColor = True
        '
        'ButtonHelp
        '
        resources.ApplyResources(Me.ButtonHelp, "ButtonHelp")
        Me.ButtonHelp.Name = "ButtonHelp"
        Me.ButtonHelp.UseVisualStyleBackColor = True
        '
        'ButtonOK
        '
        resources.ApplyResources(Me.ButtonOK, "ButtonOK")
        Me.ButtonOK.Name = "ButtonOK"
        Me.ButtonOK.UseVisualStyleBackColor = True
        '
        'CheckBoxEnglishDoc
        '
        resources.ApplyResources(Me.CheckBoxEnglishDoc, "CheckBoxEnglishDoc")
        Me.CheckBoxEnglishDoc.Name = "CheckBoxEnglishDoc"
        Me.CheckBoxEnglishDoc.UseVisualStyleBackColor = True
        '
        'CheckBoxUTF8
        '
        resources.ApplyResources(Me.CheckBoxUTF8, "CheckBoxUTF8")
        Me.CheckBoxUTF8.Name = "CheckBoxUTF8"
        Me.CheckBoxUTF8.UseVisualStyleBackColor = True
        '
        'DocOptions
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.CheckBoxUTF8)
        Me.Controls.Add(Me.CheckBoxEnglishDoc)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.CheckBoxKatakanaDoc)
        Me.Controls.Add(Me.CheckBoxBES99)
        Me.Name = "DocOptions"
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents CheckBoxBES99 As System.Windows.Forms.CheckBox
    Friend WithEvents CheckBoxKatakanaDoc As System.Windows.Forms.CheckBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents ButtonCancel As System.Windows.Forms.Button
    Friend WithEvents ButtonHelp As System.Windows.Forms.Button
    Friend WithEvents ButtonOK As System.Windows.Forms.Button
    Friend WithEvents CheckBoxEnglishDoc As System.Windows.Forms.CheckBox
    Friend WithEvents CheckBoxUTF8 As System.Windows.Forms.CheckBox
End Class
