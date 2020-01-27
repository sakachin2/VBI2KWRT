<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form6
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form6))
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.PanelTop = New System.Windows.Forms.Panel()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.ToolStripMenuItemOpen = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSave = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSaveAS = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemCut = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemCopy = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemPaste = New System.Windows.Forms.ToolStripMenuItem()
        Me.PanelButtons = New System.Windows.Forms.Panel()
        Me.ButtonOK = New System.Windows.Forms.Button()
        Me.ButtonClose = New System.Windows.Forms.Button()
        Me.ButtonCancel = New System.Windows.Forms.Button()
        Me.ButtonHelp = New System.Windows.Forms.Button()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.DataGridViewAddStr = New System.Windows.Forms.DataGridView()
        Me.ColumnEnable = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.ColumnShift = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.ColumnChars = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ColumnDel = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.StatusStrip1.SuspendLayout()
        Me.PanelTop.SuspendLayout()
        Me.MenuStrip1.SuspendLayout()
        Me.PanelButtons.SuspendLayout()
        Me.Panel1.SuspendLayout()
        CType(Me.DataGridViewAddStr, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'StatusStrip1
        '
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Name = "StatusStrip1"
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        resources.ApplyResources(Me.ToolStripStatusLabel1, "ToolStripStatusLabel1")
        Me.ToolStripStatusLabel1.Spring = True
        '
        'PanelTop
        '
        Me.PanelTop.Controls.Add(Me.MenuStrip1)
        resources.ApplyResources(Me.PanelTop, "PanelTop")
        Me.PanelTop.Name = "PanelTop"
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItemOpen, Me.ToolStripMenuItemSave, Me.ToolStripMenuItemSaveAS, Me.ToolStripMenuItemCut, Me.ToolStripMenuItemCopy, Me.ToolStripMenuItemPaste})
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Name = "MenuStrip1"
        '
        'ToolStripMenuItemOpen
        '
        Me.ToolStripMenuItemOpen.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemOpen, "ToolStripMenuItemOpen")
        Me.ToolStripMenuItemOpen.Name = "ToolStripMenuItemOpen"
        '
        'ToolStripMenuItemSave
        '
        Me.ToolStripMenuItemSave.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemSave, "ToolStripMenuItemSave")
        Me.ToolStripMenuItemSave.Name = "ToolStripMenuItemSave"
        '
        'ToolStripMenuItemSaveAS
        '
        Me.ToolStripMenuItemSaveAS.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemSaveAS, "ToolStripMenuItemSaveAS")
        Me.ToolStripMenuItemSaveAS.Name = "ToolStripMenuItemSaveAS"
        '
        'ToolStripMenuItemCut
        '
        Me.ToolStripMenuItemCut.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemCut, "ToolStripMenuItemCut")
        Me.ToolStripMenuItemCut.Name = "ToolStripMenuItemCut"
        '
        'ToolStripMenuItemCopy
        '
        Me.ToolStripMenuItemCopy.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemCopy, "ToolStripMenuItemCopy")
        Me.ToolStripMenuItemCopy.Name = "ToolStripMenuItemCopy"
        '
        'ToolStripMenuItemPaste
        '
        Me.ToolStripMenuItemPaste.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemPaste, "ToolStripMenuItemPaste")
        Me.ToolStripMenuItemPaste.Name = "ToolStripMenuItemPaste"
        '
        'PanelButtons
        '
        Me.PanelButtons.Controls.Add(Me.ButtonOK)
        Me.PanelButtons.Controls.Add(Me.ButtonClose)
        Me.PanelButtons.Controls.Add(Me.ButtonCancel)
        Me.PanelButtons.Controls.Add(Me.ButtonHelp)
        resources.ApplyResources(Me.PanelButtons, "PanelButtons")
        Me.PanelButtons.Name = "PanelButtons"
        '
        'ButtonOK
        '
        resources.ApplyResources(Me.ButtonOK, "ButtonOK")
        Me.ButtonOK.Name = "ButtonOK"
        Me.ButtonOK.UseVisualStyleBackColor = True
        '
        'ButtonClose
        '
        resources.ApplyResources(Me.ButtonClose, "ButtonClose")
        Me.ButtonClose.Name = "ButtonClose"
        Me.ButtonClose.UseVisualStyleBackColor = True
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
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.DataGridViewAddStr)
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Name = "Panel1"
        '
        'DataGridViewAddStr
        '
        Me.DataGridViewAddStr.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridViewAddStr.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.ColumnEnable, Me.ColumnShift, Me.ColumnChars, Me.ColumnDel})
        resources.ApplyResources(Me.DataGridViewAddStr, "DataGridViewAddStr")
        Me.DataGridViewAddStr.Name = "DataGridViewAddStr"
        Me.DataGridViewAddStr.RowTemplate.Height = 21
        '
        'ColumnEnable
        '
        resources.ApplyResources(Me.ColumnEnable, "ColumnEnable")
        Me.ColumnEnable.Name = "ColumnEnable"
        '
        'ColumnShift
        '
        resources.ApplyResources(Me.ColumnShift, "ColumnShift")
        Me.ColumnShift.Name = "ColumnShift"
        '
        'ColumnChars
        '
        resources.ApplyResources(Me.ColumnChars, "ColumnChars")
        Me.ColumnChars.Name = "ColumnChars"
        Me.ColumnChars.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.ColumnChars.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ColumnDel
        '
        resources.ApplyResources(Me.ColumnDel, "ColumnDel")
        Me.ColumnDel.Name = "ColumnDel"
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        '
        'Form6
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = Global.WindowsApplication1.My.MySettings.Default.CFG_Form6ClientSize
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.PanelButtons)
        Me.Controls.Add(Me.PanelTop)
        Me.DataBindings.Add(New System.Windows.Forms.Binding("ClientSize", Global.WindowsApplication1.My.MySettings.Default, "CFG_Form6ClientSize", True, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged))
        Me.MinimizeBox = False
        Me.Name = "Form6"
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.PanelTop.ResumeLayout(False)
        Me.PanelTop.PerformLayout()
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.PanelButtons.ResumeLayout(False)
        Me.Panel1.ResumeLayout(False)
        CType(Me.DataGridViewAddStr, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents StatusStrip1 As StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As ToolStripStatusLabel
    Friend WithEvents PanelTop As Panel
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents PanelButtons As Panel
    Friend WithEvents ButtonHelp As Button
    Friend WithEvents ButtonCancel As Button
    Friend WithEvents ButtonOK As Button
    Friend WithEvents Panel1 As Panel
    Friend WithEvents DataGridViewAddStr As DataGridView
    Friend WithEvents ToolStripMenuItemOpen As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSave As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSaveAS As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemCut As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemCopy As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemPaste As ToolStripMenuItem
    Friend WithEvents OpenFileDialog1 As OpenFileDialog
    Friend WithEvents SaveFileDialog1 As SaveFileDialog
    Friend WithEvents ColumnEnable As DataGridViewCheckBoxColumn
    Friend WithEvents ColumnShift As DataGridViewCheckBoxColumn
    Friend WithEvents ColumnChars As DataGridViewTextBoxColumn
    Friend WithEvents ColumnDel As DataGridViewCheckBoxColumn
    Friend WithEvents ButtonClose As Button
End Class
