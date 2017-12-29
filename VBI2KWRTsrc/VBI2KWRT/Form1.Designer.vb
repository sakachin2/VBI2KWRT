<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ImageToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.KanjiTextToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.KanaTextToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemClose = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemPrint = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitXToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSave = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSaveAs = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemPrintIcon = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemUndo = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemRedo = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemOptions = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemHelp = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuButtonKata = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripMenuButtonHira = New System.Windows.Forms.ToolStripButton()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.FontDialog1 = New System.Windows.Forms.FontDialog()
        Me.NotifyIcon1 = New System.Windows.Forms.NotifyIcon(Me.components)
        Me.TBBES = New System.Windows.Forms.TextBox()
        Me.ContextMenuSpecialChar = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.CMCut = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMCopy = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMPaste = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMSelectAll = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSpecialChar = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMFind = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemWords = New System.Windows.Forms.ToolStripMenuItem()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.MenuStrip1.SuspendLayout()
        Me.ContextMenuSpecialChar.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem, Me.ToolStripMenuItemSave, Me.ToolStripMenuItemSaveAs, Me.ToolStripMenuItemPrintIcon, Me.ToolStripMenuItemUndo, Me.ToolStripMenuItemRedo, Me.ToolStripMenuItemOptions, Me.ToolStripMenuItemHelp, Me.ToolStripMenuButtonKata, Me.ToolStripMenuButtonHira})
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Name = "MenuStrip1"
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ImageToolStripMenuItem, Me.KanjiTextToolStripMenuItem, Me.KanaTextToolStripMenuItem, Me.ToolStripMenuItemClose, Me.SaveToolStripMenuItem, Me.SaveAsToolStripMenuItem, Me.ToolStripMenuItemPrint, Me.ExitXToolStripMenuItem})
        resources.ApplyResources(Me.FileToolStripMenuItem, "FileToolStripMenuItem")
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        '
        'ImageToolStripMenuItem
        '
        Me.ImageToolStripMenuItem.Name = "ImageToolStripMenuItem"
        resources.ApplyResources(Me.ImageToolStripMenuItem, "ImageToolStripMenuItem")
        '
        'KanjiTextToolStripMenuItem
        '
        Me.KanjiTextToolStripMenuItem.Name = "KanjiTextToolStripMenuItem"
        resources.ApplyResources(Me.KanjiTextToolStripMenuItem, "KanjiTextToolStripMenuItem")
        '
        'KanaTextToolStripMenuItem
        '
        Me.KanaTextToolStripMenuItem.Name = "KanaTextToolStripMenuItem"
        resources.ApplyResources(Me.KanaTextToolStripMenuItem, "KanaTextToolStripMenuItem")
        '
        'ToolStripMenuItemClose
        '
        Me.ToolStripMenuItemClose.Name = "ToolStripMenuItemClose"
        resources.ApplyResources(Me.ToolStripMenuItemClose, "ToolStripMenuItemClose")
        '
        'SaveToolStripMenuItem
        '
        Me.SaveToolStripMenuItem.Name = "SaveToolStripMenuItem"
        resources.ApplyResources(Me.SaveToolStripMenuItem, "SaveToolStripMenuItem")
        '
        'SaveAsToolStripMenuItem
        '
        Me.SaveAsToolStripMenuItem.Name = "SaveAsToolStripMenuItem"
        resources.ApplyResources(Me.SaveAsToolStripMenuItem, "SaveAsToolStripMenuItem")
        '
        'ToolStripMenuItemPrint
        '
        Me.ToolStripMenuItemPrint.Name = "ToolStripMenuItemPrint"
        resources.ApplyResources(Me.ToolStripMenuItemPrint, "ToolStripMenuItemPrint")
        '
        'ExitXToolStripMenuItem
        '
        Me.ExitXToolStripMenuItem.Name = "ExitXToolStripMenuItem"
        resources.ApplyResources(Me.ExitXToolStripMenuItem, "ExitXToolStripMenuItem")
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
        'ToolStripMenuItemPrintIcon
        '
        Me.ToolStripMenuItemPrintIcon.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemPrintIcon, "ToolStripMenuItemPrintIcon")
        Me.ToolStripMenuItemPrintIcon.Name = "ToolStripMenuItemPrintIcon"
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
        'ToolStripMenuItemOptions
        '
        Me.ToolStripMenuItemOptions.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemOptions, "ToolStripMenuItemOptions")
        Me.ToolStripMenuItemOptions.Name = "ToolStripMenuItemOptions"
        '
        'ToolStripMenuItemHelp
        '
        Me.ToolStripMenuItemHelp.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemHelp, "ToolStripMenuItemHelp")
        Me.ToolStripMenuItemHelp.Name = "ToolStripMenuItemHelp"
        '
        'ToolStripMenuButtonKata
        '
        Me.ToolStripMenuButtonKata.BackColor = System.Drawing.Color.Yellow
        Me.ToolStripMenuButtonKata.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.ToolStripMenuButtonKata, "ToolStripMenuButtonKata")
        Me.ToolStripMenuButtonKata.Name = "ToolStripMenuButtonKata"
        '
        'ToolStripMenuButtonHira
        '
        Me.ToolStripMenuButtonHira.BackColor = System.Drawing.Color.Turquoise
        Me.ToolStripMenuButtonHira.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.ToolStripMenuButtonHira, "ToolStripMenuButtonHira")
        Me.ToolStripMenuButtonHira.Name = "ToolStripMenuButtonHira"
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        '
        'SaveFileDialog1
        '
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        '
        'NotifyIcon1
        '
        resources.ApplyResources(Me.NotifyIcon1, "NotifyIcon1")
        '
        'TBBES
        '
        Me.TBBES.BackColor = System.Drawing.SystemColors.Info
        Me.TBBES.ContextMenuStrip = Me.ContextMenuSpecialChar
        Me.TBBES.Cursor = System.Windows.Forms.Cursors.Default
        resources.ApplyResources(Me.TBBES, "TBBES")
        Me.TBBES.HideSelection = False
        Me.TBBES.Name = "TBBES"
        Me.TBBES.ShortcutsEnabled = False
        '
        'ContextMenuSpecialChar
        '
        Me.ContextMenuSpecialChar.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CMCut, Me.CMCopy, Me.CMPaste, Me.CMSelectAll, Me.ToolStripMenuItemSpecialChar, Me.CMFind, Me.ToolStripMenuItemWords})
        Me.ContextMenuSpecialChar.Name = "ContextMenuStrip1"
        resources.ApplyResources(Me.ContextMenuSpecialChar, "ContextMenuSpecialChar")
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
        'CMFind
        '
        Me.CMFind.Name = "CMFind"
        resources.ApplyResources(Me.CMFind, "CMFind")
        '
        'ToolStripMenuItemWords
        '
        Me.ToolStripMenuItemWords.Name = "ToolStripMenuItemWords"
        resources.ApplyResources(Me.ToolStripMenuItemWords, "ToolStripMenuItemWords")
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
        '
        'Form1
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.TBBES)
        Me.Controls.Add(Me.MenuStrip1)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "Form1"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ContextMenuSpecialChar.ResumeLayout(False)
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents FileToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveAsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents MRUImage As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MRUText As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MRUKanaText As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MRUBESText As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExitXToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ImageToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents KanjiTextToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents KanaTextToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FontDialog1 As System.Windows.Forms.FontDialog
    Friend WithEvents ToolStripMenuItemOptions As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NotifyIcon1 As System.Windows.Forms.NotifyIcon
    Friend WithEvents TBBES As System.Windows.Forms.TextBox
    Friend WithEvents ToolStripMenuItemHelp As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemUndo As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemRedo As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemPrint As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ContextMenuSpecialChar As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents CMCut As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMCopy As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMPaste As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMSelectAll As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuButtonKata As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripMenuButtonHira As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripMenuItemSpecialChar As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CMFind As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemClose As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSave As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemSaveAs As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemPrintIcon As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents ToolStripMenuItemWords As ToolStripMenuItem
    '   Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
End Class
