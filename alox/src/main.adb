with Exceptions;

with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Main is

   procedure Main is
      package Unbounded renames Ada.Strings.Unbounded;

      File_Name : Unbounded.Unbounded_String :=
        Unbounded.Null_Unbounded_String;
      VM        : Lox_VM.VM_Context;
   begin
      Lox_VM.Init (VM);

      VM.Trace_Execution := False;
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if Ada.Command_Line.Argument (I) = "--debug" then
            VM.Trace_Execution := True;
         elsif Unbounded.Length (File_Name) = 0 then
            File_Name :=
              Unbounded.To_Unbounded_String (Ada.Command_Line.Argument (I));
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Usage: "
               & Ada.Command_Line.Command_Name
               & " [--debug] [path]");
            raise Exceptions.Usage_Error;
         end if;
      end loop;

      if Unbounded.Length (File_Name) = 0 then
         Repl (VM);
      else
         Run_File (VM, Unbounded.To_String (File_Name));
      end if;

   end Main;

   procedure Repl (VM : in out Lox_VM.VM_Context) is
   begin
      loop
         begin
            Ada.Text_IO.Put ("> ");
            declare
               Line   : constant String := Ada.Text_IO.Get_Line;
               Unused : Lox_VM.InterpretResult;
            begin
               Unused := Lox_VM.Interpret (VM, Line);
            end;

         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;
   end Repl;

   procedure Run_File (VM : in out Lox_VM.VM_Context; Path : String) is
      Result : Lox_VM.InterpretResult;
      Source : constant String := Read_File (Path);

      use type Lox_VM.InterpretResult;
   begin
      Result := Lox_VM.Interpret (VM, Source);

      if Result = Lox_VM.Interpret_Compile_Error then
         raise Exceptions.Compile_Error;
      end if;

      if Result = Lox_VM.Interpret_Runtime_Error then
         raise Exceptions.Runtime_Error;
      end if;
   end Run_File;

   function Read_File (Path : String) return String is
      File : Ada.Streams.Stream_IO.File_Type;
      Size : Ada.Streams.Stream_IO.Count;
   begin

      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      Size := Ada.Streams.Stream_IO.Size (File);

      declare
         Data :
           Ada.Streams.Stream_Element_Array
             (1 .. Ada.Streams.Stream_Element_Offset (Size));
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (File, Data, Last);
         Ada.Streams.Stream_IO.Close (File);

         declare
            Source : String (1 .. Integer (Size));
            I      : Positive := Source'First;
         begin
            for C of Data loop
               Source (I) := Character'Val (C);
               I := Positive'Succ (I);
            end loop;
            return Source;
         end;
      end;

   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("Could not open file """ & Path & """.");
         raise Exceptions.File_Not_Found;
   end Read_File;

end Main;
