with Lox_VM;

package Main is
   procedure Main;

private
   procedure Repl (VM : in out Lox_VM.VM_Context);
   procedure Run_File (VM : in out Lox_VM.VM_Context; Path : String);

   function Read_File (Path : String) return String;

end Main;
