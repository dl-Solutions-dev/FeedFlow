unit UTestPagination;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework,
  UPagination;

type
  [ TestFixture ]
  TTestPagination = class
  private
    FPagination: TPagination;
  public
    [ Setup ]
    procedure Setup;
    [ TearDown ]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [ Test ]
    [ TestCase( 'TestSansSaut', '50, 10, 2' ) ]
    procedure Test1( NbRecords, NbPerPage, aActualPage: SmallInt );
    // Test with TestCase Attribute to supply parameters.
  end;

implementation

procedure TTestPagination.Setup;
begin
  FPagination := TPagination.Create;
end;

procedure TTestPagination.TearDown;
begin
  FreeAndNil( FPagination );
end;

procedure TTestPagination.Test1( NbRecords, NbPerPage, aActualPage: SmallInt );
begin
  FPagination.GeneratePagesList( NbRecords, NbPerPage, aActualPage, '', '', '', '' );

  Assert.AreEqual( Int64( 5 ), FPagination.PagesList.Count );
end;

initialization
  TDUnitX.RegisterTestFixture( TTestPagination );

end.

