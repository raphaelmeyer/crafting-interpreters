with Lox_VM;

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Main is
   procedure Main;

private
   type String_Access is access String;

   type Managed_Source is new Ada.Finalization.Controlled with record
      Content : String_Access := null;
   end record;

   overriding
   procedure Finalize (Source : in out Managed_Source);
   function Make (Content : String) return Managed_Source;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Repl (VM : in out Lox_VM.VM_Context);
   procedure Run_File (VM : in out Lox_VM.VM_Context; Path : String);

   function Read_File (Path : String) return String;

end Main;
