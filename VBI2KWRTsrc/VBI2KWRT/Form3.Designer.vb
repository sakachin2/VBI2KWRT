<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form3
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form3))
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.ContextMenuWordSelection = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.CMCut = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMCopy = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMPaste = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMSelectAll = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSpecialChar = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemFind = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem_DocOptions = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemDictionary = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemWords = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabelCharType = New System.Windows.Forms.ToolStripStatusLabel()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.ToolStripMenuItemFile = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSave = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSaveAs = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemPrint = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemUndo = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemRedo = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemKanaConv = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemHelp = New System.Windows.Forms.ToolStripMenuItem()
        Me.ContextMenuWordSelection.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.MenuStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TextBox1
        '
        Me.TextBox1.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.TextBox1.ContextMenuStrip = Me.ContextMenuWordSelection
        Me.TextBox1.Cursor = System.Windows.Forms.Cursors.Default
        resources.ApplyResources(Me.TextBox1, "TextBox1")
        Me.TextBox1.HideSelection = False
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.ShortcutsEnabled = False
        '
        'ContextMenuWordSelection
        '
        Me.ContextMenuWordSelection.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CMCut, Me.CMCopy, Me.CMPaste, Me.CMSelectAll, Me.ToolStripMenuItemSpecialChar, Me.ToolStripMenuItemFind, Me.ToolStripMenuItem_DocOptions, Me.ToolStripMenuItemDictionary, Me.ToolStripMenuItemWords})
        Me.ContextMenuWordSelection.Name = "ContextMenuStrip1"
        resources.ApplyResources(Me.ContextMenuWordSelection, "ContextMenuWordSelection")
        '
        'CMCut
        '
        Me.CMCut.Name = "CMCut"
        resources.ApplyResources(Me.CMCut, "CMCut")
        '
        'CMCopy
        '
        Me.CMCopy.Name = "CMCopy"
        resources.ApplyResources(Me.CMCopy, "CMCopy")
        '
        'CMPaste
        '
        Me.CMPaste.Name = "CMPaste"
        resources.ApplyResources(Me.CMPaste, "CMPaste")
        '
        'CMSelectAll
        '
        Me.CMSelectAll.Name = "CMSelectAll"
        resources.ApplyResources(Me.CMSelectAll, "CMSelectAll")
        '
        'ToolStripMenuItemSpecialChar
        '
        Me.ToolStripMenuItemSpecialChar.Name = "ToolStripMenuItemSpecialChar"
        resources.ApplyResources(Me.ToolStripMenuItemSpecialChar, "ToolStripMenuItemSpecialChar")
        '
        'ToolStripMenuItemFind
        '
        Me.ToolStripMenuItemFind.Name = "ToolStripMenuItemFind"
        resources.ApplyResources(Me.ToolStripMenuItemFind, "ToolStripMenuItemFind")
        '
        'ToolStripMenuItem_DocOptions
        '
        Me.ToolStripMenuItem_DocOptions.Name = "ToolStripMenuItem_DocOptions"
        resources.ApplyResources(Me.ToolStripMenuItem_DocOptions, "ToolStripMenuItem_DocOptions")
        '
        'ToolStripMenuItemDictionary
        '
        Me.ToolStripMenuItemDictionary.Name = "ToolStripMenuItemDictionary"
        resources.ApplyResources(Me.ToolStripMenuItemDictionary, "ToolStripMenuItemDictionary")
        '
        'ToolStripMenuItemWords
        '
        Me.ToolStripMenuItemWords.Name = "ToolStripMenuItemWords"
        resources.ApplyResources(Me.ToolStripMenuItemWords, "ToolStripMenuItemWords")
        '
        'ToolStripSeparator4
        '
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        resources.ApplyResources(Me.ToolStripSeparator4, "ToolStripSeparator4")
        '
        'StatusStrip1
        '
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabelCharType, Me.ToolStripStatusLabel1})
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Name = "StatusStrip1"
        '
        'ToolStripStatusLabelCharType
        '
        Me.ToolStripStatusLabelCharType.Name = "ToolStripStatusLabelCharType"
        resources.ApplyResources(Me.ToolStripStatusLabelCharType, "ToolStripStatusLabelCharType")
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        resources.ApplyResources(Me.ToolStripStatusLabel1, "ToolStripStatusLabel1")
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItemFile, Me.ToolStripMenuItemSave, Me.ToolStripMenuItemSaveAs, Me.ToolStripMenuItemPrint, Me.ToolStripMenuItemUndo, Me.ToolStripMenuItemRedo, Me.ToolStripMenuItemKanaConv, Me.ToolStripMenuItemHelp})
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Name = "MenuStrip1"
        '
        'ToolStripMenuItemFile
        '
        Me.ToolStripMenuItemFile.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemFile, "ToolStripMenuItemFile")
        Me.ToolStripMenuItemFile.Name = "ToolStripMenuItemFile"
        '
        'ToolStripMenuItemSave
        '
        Me.ToolStripMenuItemSave.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemSave, "ToolStripMenuItemSave")
        Me.ToolStripMenuItemSave.Name = "ToolStripMenuItemSave"
        '
        'ToolStripMenuItemSaveAs
        '
        Me.ToolStripMenuItemSaveAs.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemSaveAs, "ToolStripMenuItemSaveAs")
        Me.ToolStripMenuItemSaveAs.Name = "ToolStripMenuItemSaveAs"
        '
        'ToolStripMenuItemPrint
        '
        Me.ToolStripMenuItemPrint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemPrint, "ToolStripMenuItemPrint")
        Me.ToolStripMenuItemPrint.Name = "ToolStripMenuItemPrint"
        '
        'ToolStripMenuItemUndo
        '
        Me.ToolStripMenuItemUndo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemUndo, "ToolStripMenuItemUndo")
        Me.ToolStripMenuItemUndo.Name = "ToolStripMenuItemUndo"
        '
        'ToolStripMenuItemRedo
        '
        Me.ToolStripMenuItemRedo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemRedo, "ToolStripMenuItemRedo")
        Me.ToolStripMenuItemRedo.Name = "ToolStripMenuItemRedo"
        '
        'ToolStripMenuItemKanaConv
        '
        Me.ToolStripMenuItemKanaConv.BackColor = System.Drawing.Color.Yellow
        Me.ToolStripMenuItemKanaConv.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.ToolStripMenuItemKanaConv, "ToolStripMenuItemKanaConv")
        Me.ToolStripMenuItemKanaConv.Name = "ToolStripMenuItemKanaConv"
        '
        'ToolStripMenuItemHelp
        '
        Me.ToolStripMenuItemHelp.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemHelp, "ToolStripMenuItemHelp")
        Me.ToolStripMenuItemHelp.Name = "ToolStripMenuItemHelp"
        '
        'Form3
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.MenuStrip1)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "Form3"
        Me.ContextMenuWordSelection.ResumeLayout(False)
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents ToolStripSeparator4 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents ContextMenuWordSelection As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents CMCut As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMCopy As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMPaste As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMSelectAll As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSpecialChar As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemFind As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem_DocOptions As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemDictionary As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents ToolStripStatusLabelCharType As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents ToolStripStatusLabel1 As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents ToolStripMenuItemWords As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents ToolStripMenuItemFile As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSave As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSaveAs As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemPrint As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemUndo As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemRedo As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemKanaConv As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemHelp As ToolStripMenuItem
End Class
