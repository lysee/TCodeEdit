## TCodeEdit

A lightweight and syntax hilighted UNICODE editor.<br>
一个小巧、语法加亮、使用UNICODE编码的代码编辑器。<br>

![](/resource/run_in_fedora.png)

### Why it is here

TCodeEdit is a component of my lysee project for editing pascal and lysee codes, and it is easy to be extended to fit other developing languages. On the whole, TCodeEdit is small, simple, easy to use and might be useful to other coders, so I put it in here open source.<br>
TCodeEdit起初是为我的Lysee项目开发的Pascal/Lysee代码编辑器，后来发现可以很轻松的扩展它用于编辑其他编程语言。总体上，TCodeEdit具有小巧、简单、易用的特点，对其他程序员也许也有用处，因此我把它放在这里，开放源代码供大家使用。<br>

### Develping status

TCodeEdit is in development and lack of some helpful functions.<br>
TCodeEdit还在开发完善中，除了基本的语法加亮编辑功能，其他一些辅助功能还有待添加:<br>

  - print and preview（打印和打印预览）.
  - convert code to HTML（将代码转换为HTML编码）.
  - bookmark（书签，按Ctrl+数字键跳转的那种）.
  - autocomplete(代码辅助完成).
  - more language syntax(更多的的编程语言语法)
  - ....
  
### License

TCodeEdit was release under the MIT license. Just use it as you will.<br>
TCodeEdit使用MIT许可，可以放心使用.<br>

### Version

Maybe has just completed 60%, so current version is 0.6.0.<br>
也许刚刚完成60%，所以版本暂定为0.6.0.

### How to use it.

1. Get TCodeEdit and add it to your Lazarus/Delphi project.<br>
   下载TCodeEdit后加入你的开发项目。<br>
2. Use codeedit.pas in your form unit and declare class field to hold TCodeEdit<br>
   将codeedit.pas加入需要使用TCodeEdit的窗口单元并声明类成员变量。<br>

   ```Pascal
   uses codeedit;
   
   type
     TMainForm = class(TForm)
     private
       FEdit: TCodeEdit;
       procedure EditStatus(Sender: TObject);
     end;
   ```

3. Palce a TCodeEdit at where you want by calling PlaceACodeEdit().<br>
   调用PlaceACodeEdit函数把TCodeEdit放置到需要的位置。<br>

   ```Pascal
   procedure TMainForm.FormCreate(Sender: TObject);
   begin
     FEdit := PlaceACodeEdit(Self);
     FEdit.OnStatus := @EditStatus;
   end;
   ```

4. Reponds OnStatus event to display editor status and enable/disable menu or buttons.<br>
   响应OnStatus事件显示编辑器状态，修改相关菜单、按钮等组件的属性。<br>

   ```Pascal
   procedure TMainForm.EditStatus(Sender: TObject);
   begin
     EditUndoMenu.Enabled := (FEdit.Undos.Last <> nil);
     EditRedoMenu.Enabled := (FEdit.Redos.Last <> nil);
     EditCutMenu.Enabled := FEdit.Selection.Selected;
     EditCopyMenu.Enabled := FEdit.Selection.Selected;
     EditPasteMenu.Enabled := HasTextFormat;
     StatusBar.Panels[0].Text := Format('%d, %d', [FEdit.Caret.LineIndex + 1, FEdit.Caret.TextIndex]);
     if FEdit.Modified then
       StatusBar.Panels[1].Text := 'Changed' else
       StatusBar.Panels[1].Text := '';
     StatusBar.Panels[2].Text := FEdit.Syntax.Language;
   end;
   ```

5. How to open a file.<br>
   如何打开文件。<br>

   ```Pascal
   procedure TMainForm.FileOpenMenuClick(Sender: TObject);
   begin
     CloseDialogs;
     if OpenDialog.Execute then
       FEdit.Lines.LoadFromFile(OpenDialog.FileName);
   end;
   ```

   Base on file extension, TCodeEdit will choose prefered language syntax automatically.<br>
   打开文件后，TCodeEdit根据文件后缀自动选择匹配的语法加亮类。

6. How to use syntax class manually.<br>
   如何手动设置语法加亮类。<br>
   
   ```Pascal
   FEdit.Syntax.SyntaxClass := TPascalSyntax;
   FEdit.Syntax.SyntaxClass := FindSyntax('Pascal');
   FEdit.Syntax.SyntaxClass := FindSyntaxByFileExt('.pas');
   FEdit.Syntax.SyntaxClass := FindFileSyntax('~/editor/frmmain.pas');
   ```

