unit C2WRegistration;

interface

uses Classes, C2WHttpService;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('C2W', [TC2WHttpService]);
  end;

end.
