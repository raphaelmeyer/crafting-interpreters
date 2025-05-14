with AUnit.Reporter.Text;
with AUnit.Run;
with Alox_Suite;

procedure Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Alox_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Tests;
