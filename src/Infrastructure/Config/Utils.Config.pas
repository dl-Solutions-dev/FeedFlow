/// <summary>
///   Configuration
/// </summary>
/// <remarks>
///   Cette class propose un objet singleton
/// </remarks>
unit Utils.Config;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Inifiles;

type
  /// <summary>
  ///   Class contenant les éléments configurables
  /// </summary>
  TConfig = class( TIniFile )
  private
    FLocation: string;
    FVersion: string;
    FEdition: string;
    FTemplateFolder: string;
    FCompany: string;
    FResource: string;
    FApp_Name: string;
    FDatabaseName: string;
    FResourcePath: string;
    FFilesFolder: string;

    class var FInstance: TConfig;
    class var FSynchro: TMultiReadExclusiveWriteSynchronizer;

    procedure Initialisation;
    procedure SetFilesFolder(const Value: string);
  public
    destructor Destroy; override;

    class constructor CreateClass;
    class destructor DestroyClass;

    class function GetInstance: TConfig;

    /// <summary>
    ///   Nm de l'alias de la base de données
    /// </summary>
    property DatabaseName: string read FDatabaseName;
    /// <summary>
    ///   Nom de l'application
    /// </summary>
    property App_Name: string read FApp_Name;
    /// <summary>
    ///   Version de l'application
    /// </summary>
    property Version: string read FVersion;
    property Edition: string read FEdition;
    /// <summary>
    ///   Nom de la société
    /// </summary>
    property Company: string read FCompany;
    property Resource: string read FResource;
    /// <summary>
    ///   Répertoire contenant les templates HTML
    /// </summary>
    property TemplateFolder: string read FTemplateFolder;
    property Location: string read FLocation;
    property ResourcePath:string read FResourcePath;
    /// <summary>
    ///   Chemin du dossier contenant les fichiers uploadés
    /// </summary>
    property FilesFolder:string read FFilesFolder write SetFilesFolder;
  end;

implementation

uses
  System.IOUtils;

const
  /// <summary>
  ///   Nom du fichier de configuration
  /// </summary>
  CONFIG_FILE_NAME: string = 'Config.ini';

  { TConfig }

class constructor TConfig.CreateClass;
begin
  FSynchro := TMultiReadExclusiveWriteSynchronizer.Create;
  FInstance := nil;
end;

destructor TConfig.Destroy;
begin

  inherited;
end;

class destructor TConfig.DestroyClass;
begin
  if Assigned( TConfig.FInstance ) then
    TConfig.FInstance.Free;

  FreeAndNil( FSynchro );
end;

class function TConfig.GetInstance: TConfig;
begin
  FSynchro.BeginRead;
  try
    if not ( Assigned( FInstance ) ) then
    begin
      FSynchro.BeginWrite;
      try
{$IFDEF DEBUG}
        FInstance := TConfig.Create( ExtractFilePath( ParamStr( 0 ) ) + CONFIG_FILE_NAME );
{$ELSE}
        FInstance := TConfig.Create( 'c:\ServeursDelphi\Config\FeedFlow\' + CONFIG_FILE_NAME );
{$ENDIF}
        FInstance.Initialisation;
      finally
        FSynchro.EndWrite;
      end;
    end;

    Result := FInstance;
  finally
    FSynchro.EndRead;
  end;
end;

procedure TConfig.Initialisation;
begin
  FDatabaseName := ReadString( 'Parametres', 'Database', '' );
  FLocation := ReadString( 'Parametres', 'Location', '' );
  FVersion := ReadString( 'Parametres', 'Version', '' );
  FEdition := ReadString( 'Parametres', 'Edition', '' );
  FTemplateFolder := ReadString( 'Parametres', 'TemplateFolder', '' );
  FCompany := ReadString( 'Parametres', 'Company', '' );
  FResource := ReadString( 'Parametres', 'Resource', '' );
  FApp_Name := ReadString( 'Parametres', 'AppName', '' );
  FResourcePath := ReadString( 'Parametres', 'ResourcePath', '' );
  FFilesFolder  := ReadString( 'Parametres', 'FilesFolder', '' );
end;

procedure TConfig.SetFilesFolder(const Value: string);
begin
  FFilesFolder := Value;
end;

end.

