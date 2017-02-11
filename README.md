# TCodeEdit

A lightweight, syntax hilight UNI code editor.

## Why it is here

TCodeEdit was created in Jan 26th, 2017. I write tis component is to get a small
and eay-to-use code editor which can work in the latest Lazarus/Delphi XE without
changing a letter.

## Develping status

TCodeEdit is in development and lack of some helpful functions:

  - print and preview.
  - convert code to HTML.
  - bookmark.
  - code auto-complete.
  - ......

## License

TCodeEdit was release under the MIT license. Just use it as you will.

## Version

Current version is 0.6.0. I hope it grow up to 1.0 in the next few months.

## How to use it.

 1. download to your harddisk and add codeedit.pas and codesyns.pas to your project.
 2. use codeedit.pas in your form unit and declare class field to hold TCodeEdit.

    uses
      Classes, SysUtils, codeedit;

    type

      { TMainForm }

      TMainForm = class(TForm)
      .....
        FEdit: TCodeEdit;
        procedure EditStatus(Sender: TObject);
      .....
      end;

 3. call PlaceACodeEdit() function to palce a code editor to your form or panel.

    procedure TMainForm.FormCreate(Sender: TObject);
    begin
      FEdit := PlaceACodeEdit(Self);
      FEdit.OnStatus := @EditStatus;
    end;

 4. reponds to TCodeEdit.OnStatus event to enable/disable menu or buttons.

    procedure TMainForm.EditStatus(Sender: TObject);
    begin
      EditUndoMenu.Enabled := (FEdit.Undos.Last <> nil);
      EditRedoMenu.Enabled := (FEdit.Redos.Last <> nil);
      EditCutMenu.Enabled := FEdit.Selection.Selected;
      EditCopyMenu.Enabled := FEdit.Selection.Selected;
      EditPasteMenu.Enabled := HasTextFormat;
      StatusBar.Panels[0].Text := Format('%d, %d',
        [FEdit.Caret.LineIndex + 1, FEdit.Caret.TextIndex]);
      if FEdit.Modified then
        StatusBar.Panels[1].Text := 'Changed' else
        StatusBar.Panels[1].Text := '';
      StatusBar.Panels[2].Text := FEdit.Syntax.Language;
    end;

 5. open a file

    procedure TMainForm.FileOpenMenuClick(Sender: TObject);
    begin
      CloseDialogs;
      if OpenDialog.Execute then
        FEdit.Lines.LoadFromFile(OpenDialog.FileName);
    end;

    Base on file extension, TCodeEdit will change its language syntax automatically.

 6. select a language syntax manually.

    - FEdit.Syntax.SyntaxClass := TPascalSyntax;
    - FEdit.Syntax.SyntaxClass := FindSyntax('Pascal');
    - FEdit.Syntax.SyntaxClass := FindSyntaxByFileExt('.pas');

