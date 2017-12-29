<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form8
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form8))
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.CheckBoxCase = New System.Windows.Forms.CheckBox()
        Me.CheckBoxUp = New System.Windows.Forms.CheckBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.ButtonReplaceAll = New System.Windows.Forms.Button()
        Me.ButtonReplace = New System.Windows.Forms.Button()
        Me.ButtonFind = New System.Windows.Forms.Button()
        Me.ComboBoxSearchWord = New System.Windows.Forms.ComboBox()
        Me.ComboBoxRepWord = New System.Windows.Forms.ComboBox()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'CheckBoxCase
        '
        resources.ApplyResources(Me.CheckBoxCase, "CheckBoxCase")
        Me.CheckBoxCase.Name = "CheckBoxCase"
        Me.CheckBoxCase.UseVisualStyleBackColor = True
        '
        'CheckBoxUp
        '
        resources.ApplyResources(Me.CheckBoxUp, "CheckBoxUp")
        Me.CheckBoxUp.Name = "CheckBoxUp"
        Me.CheckBoxUp.UseVisualStyleBackColor = True
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.ButtonReplaceAll)
        Me.Panel1.Controls.Add(Me.ButtonReplace)
        Me.Panel1.Controls.Add(Me.ButtonFind)
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Name = "Panel1"
        '
        'ButtonReplaceAll
        '
        Me.ButtonReplaceAll.BackColor = System.Drawing.SystemColors.ControlLight
        resources.ApplyResources(Me.ButtonReplaceAll, "ButtonReplaceAll")
        Me.ButtonReplaceAll.Name = "ButtonReplaceAll"
        Me.ButtonReplaceAll.UseVisualStyleBackColor = False
        '
        'ButtonReplace
        '
        Me.ButtonReplace.BackColor = System.Drawing.SystemColors.ControlLight
        resources.ApplyResources(Me.ButtonReplace, "ButtonReplace")
        Me.ButtonReplace.Name = "ButtonReplace"
        Me.ButtonReplace.UseVisualStyleBackColor = False
        '
        'ButtonFind
        '
        Me.ButtonFind.BackColor = System.Drawing.SystemColors.ControlLight
        resources.ApplyResources(Me.ButtonFind, "ButtonFind")
        Me.ButtonFind.Name = "ButtonFind"
        Me.ButtonFind.UseVisualStyleBackColor = False
        '
        'ComboBoxSearchWord
        '
        Me.ComboBoxSearchWord.FormattingEnabled = True
        resources.ApplyResources(Me.ComboBoxSearchWord, "ComboBoxSearchWord")
        Me.ComboBoxSearchWord.Name = "ComboBoxSearchWord"
        '
        'ComboBoxRepWord
        '
        Me.ComboBoxRepWord.FormattingEnabled = True
        resources.ApplyResources(Me.ComboBoxRepWord, "ComboBoxRepWord")
        Me.ComboBoxRepWord.Name = "ComboBoxRepWord"
        '
        'Form8
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.ComboBoxRepWord)
        Me.Controls.Add(Me.ComboBoxSearchWord)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.CheckBoxUp)
        Me.Controls.Add(Me.CheckBoxCase)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.KeyPreview = True
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "Form8"
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents CheckBoxCase As System.Windows.Forms.CheckBox
    Friend WithEvents CheckBoxUp As System.Windows.Forms.CheckBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents ButtonReplaceAll As System.Windows.Forms.Button
    Friend WithEvents ButtonReplace As System.Windows.Forms.Button
    Friend WithEvents ButtonFind As System.Windows.Forms.Button
    Friend WithEvents ComboBoxSearchWord As System.Windows.Forms.ComboBox
    Friend WithEvents ComboBoxRepWord As System.Windows.Forms.ComboBox
End Class
