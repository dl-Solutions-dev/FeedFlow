/// <summary>
///   Jeton
/// </summary>
unit Utils.Token;

interface

uses
  System.Classes,
  System.SysUtils,
  Web.HTTPApp;

type
  /// <summary>
  ///   Token
  /// </summary>
  TToken = class
  private
    FRole: string;
    FCountry: string;
    FCategory: string;
    FUserName: string;
    FLang: string;
    FSubCatgegory: string;

    procedure SetCategory( const Value: string );
    procedure SetCountry( const Value: string );
    procedure SetLang( const Value: string );
    procedure SetRole( const Value: string );
    procedure SetSubCatgegory( const Value: string );
    procedure SetUserName( const Value: string );
  public
    /// <summary>
    ///   Contrôle la validité d'un token reçu
    /// </summary>
    function Valid( aToken: string; aLoadData: Boolean ): Boolean;
    /// <summary>
    ///   Créer le token qui sera encoyé au navigateur pour stockage dans la
    ///   session
    /// </summary>
    function CreateToken( aUserName, aCountry, aLang, aCategory, aSubCategory,
      aRole: string ): string;

    /// <summary>
    ///   Nom d'utilisateur
    /// </summary>
    property UserName: string read FUserName write SetUserName;
    /// <summary>
    ///   Pays d'affectation de l'utilisateur
    /// </summary>
    property Country: string read FCountry write SetCountry;
    /// <summary>
    ///   Langue de l'utilisateur
    /// </summary>
    property Lang: string read FLang write SetLang;
    /// <summary>
    ///   Catégorie d'affectation de l'utilisateur
    /// </summary>
    property Category: string read FCategory write SetCategory;
    /// <summary>
    ///   Sous-catégorie d'affectation de l'utilisateur
    /// </summary>
    property SubCatgegory: string read FSubCatgegory write SetSubCatgegory;
    /// <summary>
    ///   Role de l'utilisateur
    /// </summary>
    property Role: string read FRole write SetRole;
  end;

implementation

uses
  System.JSON,
  System.DateUtils,
  JOSE.Core.JWA,
  JOSE.Core.JWT,
  JOSE.Core.Builder;

const
  /// <summary>
  ///   Contante contenant la clé de cryptage
  /// </summary>
  SECRETKEY: string = 'mysecretkey256bitwide(32characters)';

  { TToken }

function TToken.CreateToken( aUserName, aCountry, aLang, aCategory, aSubCategory,
  aRole: string ): string;
var
  Header, Payload: TJSONObject;
  ExpirationTime: Double;
  LToken: TJWT;
begin
  // Définir l'en-tête du JWT
  LToken := TJWT.Create;
  Header := LToken.Header.JSON;
  try
    Header.AddPair( 'alg', 'HS256' );
    Header.AddPair( 'typ', 'JWT' );

    // Définir le payload (contenu) du JWT
    Payload := LToken.Claims.JSON;
    try
      Payload.AddPair( 'sub', aUserName );
      ExpirationTime := IncMinute( NOw, 60 ); // Temps d'expiration en secondes
      Payload.AddPair( 'exp', ExpirationTime );
      Payload.AddPair( 'Country', aCountry );
      Payload.AddPair( 'Lang', aLang );
      Payload.AddPair( 'UserName', aUserName );
      Payload.AddPair( 'Role', aRole );
      Payload.AddPair( 'Category', aCategory );
      Payload.AddPair( 'SubCategory', aSubCategory );

      // Générer le JWT
      Result := TJOSE.SerializeCompact( SECRETKEY, TJOSEAlgorithmId.HS256, LToken );
    finally
    end;
  finally
    FreeAndNil( LToken );
  end;
end;

procedure TToken.SetCategory( const Value: string );
begin
  FCategory := Value;
end;

procedure TToken.SetCountry( const Value: string );
begin
  FCountry := Value;
end;

procedure TToken.SetLang( const Value: string );
begin
  FLang := Value;
end;

procedure TToken.SetRole( const Value: string );
begin
  FRole := Value;
end;

procedure TToken.SetSubCatgegory( const Value: string );
begin
  FSubCatgegory := Value;
end;

procedure TToken.SetUserName( const Value: string );
begin
  FUserName := Value;
end;

function TToken.Valid( aToken: string; aLoadData: Boolean ): Boolean;
var
  LToken: TJWT;
  LExpirationTime: Double;
begin
  Result := False;

  // Unpack and verify the token
  LToken := TJOSE.DeserializeCompact( SECRETKEY, aToken );
  try
    if Assigned( LToken ) then
    begin
      LExpirationTime := LToken.Claims.JSON.GetValue<Double>( 'exp' );

      if ( LExpirationTime >= Now ) then
      begin
        Result := True;

        if aLoadData then
        begin
          FUserName := LToken.Claims.JSON.GetValue<string>( 'UserName' );
          FRole := LToken.Claims.JSON.GetValue<string>( 'Role' );
          FCountry := LToken.Claims.JSON.GetValue<string>( 'Country' );
          FCategory := LToken.Claims.JSON.GetValue<string>( 'Category' );
          FLang := LToken.Claims.JSON.GetValue<string>( 'Lang' );
          FSubCatgegory := LToken.Claims.JSON.GetValue<string>( 'SubCategory' );
        end;
      end;
    end;
  finally
    LToken.Free;
  end;
end;

end.

