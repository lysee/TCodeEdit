## TCodeEdit

A lightweight, syntax hilight UNI code editor（一个小巧、语法加亮、基于UNICODE的代码编辑器）.

### Why it is here

TCodeEdit was created in Jan 26th, 2017. I wrote this component is to get a small and eay-to-use code editor (according to SynEdit/AtSynEdit..) which can be used in the latest Lazarus/Delphi XE projects without changing a letter. I am happy it realy works, and I hope it would be useful for someone else, so I released it open source on Github.（我在2017年1月26日开始编写TEditCode，为的是得到一个能够不经修改就可以直接用在最新的Lazarus和Delphi XE项目中，跨Windows/Linux/Mac OS等各类操作系统运行的代码编辑器，我的一个代码生成程序需要这个东西。我很高兴把它发布到Github供需要的朋友使用）.

### Develping status

TCodeEdit is in development and lack of some helpful functions（TCodeEdit仍在开发完善中，目前尚缺乏一些辅助性的功能）:

  - print and preview（打印和打印预览）.
  - convert code to HTML（将代码转换为HTML编码）.
  - bookmark（书签，按Ctrl+数字键跳转的那种）.
  - code autocomplete(代码辅助完成).
  - more language syntax(更多的的编程语言语法)
  - ....
  
### License

TCodeEdit was release under the MIT license. Just use it as you will（TCodeEdit使用MIT协议，可以放心使用）.

### Version

Maybe has completed 60%, so current version is 0.6.0(也许刚刚完成60%，所以版本暂定为0.6.0).

### How to use it.

1. Download TCodeEdit and add it to your Lazarus/Delphi project(下载TCodeEdit后加入你的开发项目).
2. Use codeedit.pas in your form unit and declare class field to hold TCodeEdit(将codeedit.pas加入需要使用TCodeEdit的窗口单元并声明类成员变量).

    `uses`
    `  ..., codeedit;`
    ` `
    `type`
    `  TMainForm = class(TForm)`
    `    FEdit: TCodeEdit;`
    `    procedure EditStatus(Sender: TObject);`
    `  end;`

3. Call PlaceACodeEdit() to palce a code editor on the form or panel(调用PlaceACode函数把TCodeEdit放置到需要的位置).

    `procedure TMainForm.FormCreate(Sender: TObject);`
    `begin'
    `  FEdit := PlaceACodeEdit(Self);`
    `  FEdit.OnStatus := @EditStatus;`
    `end;`

4. Reponds OnStatus event to display editor status and enable/disable menu or buttons（响应OnStatus事件显示编辑器状态，修改相关菜单、按钮的状态）.

    `procedure TMainForm.EditStatus(Sender: TObject);`
    `begin`
    `  EditUndoMenu.Enabled := (FEdit.Undos.Last <> nil);`
    `  EditRedoMenu.Enabled := (FEdit.Redos.Last <> nil);`
    `  EditCutMenu.Enabled := FEdit.Selection.Selected;`
    `  EditCopyMenu.Enabled := FEdit.Selection.Selected;`
    `  EditPasteMenu.Enabled := HasTextFormat;`
    `  StatusBar.Panels[0].Text := Format('%d, %d', [FEdit.Caret.LineIndex + 1, FEdit.Caret.TextIndex]);`
    `  if FEdit.Modified then`
    `    StatusBar.Panels[1].Text := 'Changed' else`
    `    StatusBar.Panels[1].Text := '';`
    `  StatusBar.Panels[2].Text := FEdit.Syntax.Language;`
    `end;`
    ```

5. How to open a file（如何打开文件）
 
    `procedure TMainForm.FileOpenMenuClick(Sender: TObject);`
    `begin`
    `  CloseDialogs;`
    `  if OpenDialog.Execute then`
    `    FEdit.Lines.LoadFromFile(OpenDialog.FileName);`
    `end;`

  Base on file extension, TCodeEdit will choose prefered language syntax automatically(打开文件后，TCodeEdit根据文件后缀自动选择匹配的语法加亮类).

6. How to use a language syntax manually(如何手动设置语法加亮类).

  - `FEdit.Syntax.SyntaxClass := TPascalSyntax;`
  - `FEdit.Syntax.SyntaxClass := FindSyntax('Pascal');`
  - `FEdit.Syntax.SyntaxClass := FindSyntaxByFileExt('.pas');`

