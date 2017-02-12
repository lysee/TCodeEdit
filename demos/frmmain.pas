unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, ComCtrls, Clipbrd, codeedit;

type

  { TMainForm }

  TMainForm = class(TForm)
    FindDialog: TFindDialog;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewMenu: TMenuItem;
    FileOpenMenu: TMenuItem;
    FileSaveMenu: TMenuItem;
    MenuItem1: TMenuItem;
    FileExitMenu: TMenuItem;
    EditPanel: TPanel;
    EditMenu: TMenuItem;
    EditCutMenu: TMenuItem;
    EditCopyMenu: TMenuItem;
    EditPasteMenu: TMenuItem;
    MenuItem2: TMenuItem;
    EditSelectAllMenu: TMenuItem;
    EditUndoMenu: TMenuItem;
    EditRedoMenu: TMenuItem;
    MenuItem3: TMenuItem;
    ReplaceDialog: TReplaceDialog;
    SearchReplaceMenu: TMenuItem;
    SearchFindMenu: TMenuItem;
    SearchMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FileNewMenuClick(Sender: TObject);
    procedure FileOpenMenuClick(Sender: TObject);
    procedure FileSaveMenuClick(Sender: TObject);
    procedure FileExitMenuClick(Sender: TObject);
    procedure EditMenuClick(Sender: TObject);
    procedure EditUndoMenuClick(Sender: TObject);
    procedure EditRedoMenuClick(Sender: TObject);
    procedure EditCutMenuClick(Sender: TObject);
    procedure EditCopyMenuClick(Sender: TObject);
    procedure EditPasteMenuClick(Sender: TObject);
    procedure EditSelectAllMenuClick(Sender: TObject);
    procedure SearchFindMenuClick(Sender: TObject);
    procedure SearchReplaceMenuClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
  private
    FEdit: TCodeEdit;
    procedure EditStatus(Sender: TObject);
    procedure CloseDialogs;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FEdit := PlaceACodeEdit(EditPanel);
  FEdit.OnStatus := @EditStatus;
end;

procedure TMainForm.FileNewMenuClick(Sender: TObject);
begin
  CloseDialogs;
  FEdit.Lines.BeginUpdate;
  try
    FEdit.Lines.ClearToOneEmptyLine;
    FEdit.Modified := false;
  finally
    FEdit.Lines.EndUpdate;
  end;
end;

procedure TMainForm.FileOpenMenuClick(Sender: TObject);
begin
  CloseDialogs;
  if OpenDialog.Execute then
    FEdit.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.FileSaveMenuClick(Sender: TObject);
begin
  CloseDialogs;
  if SaveDialog.Execute then
  begin
    FEdit.Lines.SaveToFile(SaveDialog.FileName);
    FEdit.Modified := false;
  end;
end;

procedure TMainForm.FileExitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditMenuClick(Sender: TObject);
begin
  EditUndoMenu.Enabled := (FEdit.Undos.Last <> nil);
  EditRedoMenu.Enabled := (FEdit.Redos.Last <> nil);
  EditCutMenu.Enabled := FEdit.Selection.Selected;
  EditCopyMenu.Enabled := FEdit.Selection.Selected;
  EditPasteMenu.Enabled := HasTextFormat;
end;

procedure TMainForm.EditUndoMenuClick(Sender: TObject);
begin
  FEdit.Undos.Apply;
end;

procedure TMainForm.EditRedoMenuClick(Sender: TObject);
begin
  FEdit.Redos.Apply;
end;

procedure TMainForm.EditCutMenuClick(Sender: TObject);
begin
  FEdit.Selection.CutToClipboard;
end;

procedure TMainForm.EditCopyMenuClick(Sender: TObject);
begin
  FEdit.Selection.CopyToClipboard;
end;

procedure TMainForm.EditPasteMenuClick(Sender: TObject);
begin
  FEdit.Selection.PasteFromClipboard;
end;

procedure TMainForm.EditSelectAllMenuClick(Sender: TObject);
begin
  FEdit.Selection.SelectAll;
end;

procedure TMainForm.SearchFindMenuClick(Sender: TObject);
begin
  ReplaceDialog.CloseDialog;
  FindDialog.Execute;
end;

procedure TMainForm.SearchReplaceMenuClick(Sender: TObject);
begin
  FindDialog.CloseDialog;
  FEdit.Selection.Unselect;
  ReplaceDialog.Execute;
end;

procedure TMainForm.FindDialogFind(Sender: TObject);
begin
  ReplaceDialog.FindText := FindDialog.FindText;
  if not FEdit.FindNext(StrToCode(FindDialog.FindText)) then
    ShowMessage('text not found!');
end;

procedure TMainForm.ReplaceDialogFind(Sender: TObject);
begin
  FindDialog.FindText := ReplaceDialog.FindText;
  if not FEdit.FindNext(StrToCode(ReplaceDialog.FindText)) then
    ShowMessage('text not found!');
end;

procedure TMainForm.ReplaceDialogReplace(Sender: TObject);
begin
  if FEdit.Selection.Selected then
    FEdit.Selection.Text := StrToCode(ReplaceDialog.ReplaceText);
  if frReplaceAll in ReplaceDialog.Options then
  begin
    while FEdit.FindNext(StrToCode(ReplaceDialog.FindText)) do
      FEdit.Selection.Text := StrToCode(ReplaceDialog.ReplaceText);
  end
  else ReplaceDialogFind(nil);
end;

procedure TMainForm.EditStatus(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Format('%d, %d',
    [FEdit.Caret.LineIndex + 1, FEdit.Caret.TextIndex]);
  if FEdit.Modified then
    StatusBar.Panels[1].Text := 'Changed' else
    StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := FEdit.Syntax.Language;
end;

procedure TMainForm.CloseDialogs;
begin
  FindDialog.CloseDialog;
  ReplaceDialog.CloseDialog;
end;

end.

