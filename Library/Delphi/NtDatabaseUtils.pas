{
  @abstract Implements routines that help you internationalize database applications.

  Use these routins to hide and show fields and to set display label.
  All fields are visible by default. However the form file does not contain
  the display label if the display label equals to the name of the field.
  This leads into the situation where there is no resource string for the label
  and it can not be localized. This is why you should add a resource string
  that contains the display label and then call @link(TNtDatabaseUtils.ShowField).

  The following sample hides Id and Lang fields and shows Name, Fieldplayers,
  Goalie, Origin and Description fields.

  @longCode(#
procedure TMainForm.UpdateItems;
resourcestring
  SName = 'Name';
  SFieldplayers = 'Fieldplayers';
  SGoalie = 'Goalie';
  SOrigin = 'Origin';
  SDescription = 'Description';
begin
  TNtDatabaseUtils.HideField(Query1, 'Id');
  TNtDatabaseUtils.HideField(Query1, 'Lang');

  TNtDatabaseUtils.ShowField(Query1, 'Name', SName, 15);
  TNtDatabaseUtils.ShowField(Query1, 'Fieldplayers', SFieldplayers);
  TNtDatabaseUtils.ShowField(Query1, 'Goalie', SGoalie);
  TNtDatabaseUtils.ShowField(Query1, 'Origin', SOrigin, 15);
  TNtDatabaseUtils.ShowField(Query1, 'Description', SDescription, 100);
end;
#)

  See @italic(Samples\Delphi\VCL\Sport) or @italic(Samples\Delphi\FMX\Sport)
  samples to see how to use the unit.
}
unit NtDatabaseUtils;

{$I NtVer.inc}

interface

uses
  Db;

type
  { @abstract Static class that contains database routines.
    @seealso(NtDatabaseUtils) }
  TNtDatabaseUtils = class
  public
    { Hide the field.
      @param dataSet  Dataset that contains the field.
      @param name     Name of the field to hide. }
    class procedure HideField(dataSet: TDataSet; const name: String);

    { Show the field and set the display label.
      @param dataSet  Dataset that contains the field.
      @param name     Name of the field to hide.
      @param caption  Display label of the field.
      @param width    Display width of the field in characters. }
    class procedure ShowField(
      dataSet: TDataSet;
      const name, caption: String;
      width: Integer = 0);
  end;

implementation

class procedure TNtDatabaseUtils.HideField(
  dataSet: TDataSet;
  const name: String);
begin
  if dataSet.Active then
    dataSet.FindField(name).Visible := False;
end;

class procedure TNtDatabaseUtils.ShowField(
  dataSet: TDataSet;
  const name, caption: String;
  width: Integer);
var
  field: TField;
begin
  if not dataSet.Active then
    Exit;

  field := dataSet.FindField(name);
  field.DisplayLabel := caption;

  if width > 0 then
    field.DisplayWidth := width;
end;

end.
