<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MsgBox
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
        Me.TextBoxMessage = New System.Windows.Forms.TextBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.ButtonOK = New System.Windows.Forms.Button()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TextBoxMessage
        '
        Me.TextBoxMessage.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TextBoxMessage.Location = New System.Drawing.Point(0, 0)
        Me.TextBoxMessage.Multiline = True
        Me.TextBoxMessage.Name = "TextBoxMessage"
        Me.TextBoxMessage.ReadOnly = True
        Me.TextBoxMessage.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.TextBoxMessage.Size = New System.Drawing.Size(580, 741)
        Me.TextBoxMessage.TabIndex = 0
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.ButtonOK)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Panel1.Location = New System.Drawing.Point(0, 703)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(580, 38)
        Me.Panel1.TabIndex = 1
        '
        'ButtonOK
        '
        Me.ButtonOK.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ButtonOK.Location = New System.Drawing.Point(0, 0)
        Me.ButtonOK.Name = "ButtonOK"
        Me.ButtonOK.Size = New System.Drawing.Size(580, 38)
        Me.ButtonOK.TabIndex = 0
        Me.ButtonOK.Text = "OK"
        Me.ButtonOK.UseVisualStyleBackColor = True
        '
        'MsgBox
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(580, 741)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.TextBoxMessage)
        Me.Name = "MsgBox"
        Me.Text = "Form4"
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TextBoxMessage As System.Windows.Forms.TextBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents ButtonOK As System.Windows.Forms.Button
End Class
