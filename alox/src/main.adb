with Debug;
with Exceptions;
with Lox_Chunk;
with Lox_Scanner;
with Lox_VM;

with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Main is
   Main_Chunk : aliased Lox_Chunk.Chunk;

   procedure Main is
      package Unbounded renames Ada.Strings.Unbounded;

      File_Name : Unbounded.Unbounded_String :=
        Unbounded.Null_Unbounded_String;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if Ada.Command_Line.Argument (I) = "--debug" then
            Debug.Enable_Print_Code;
            Debug.Enable_Trace_Execution;
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
         Repl;
      else
         Run_File (Unbounded.To_String (File_Name));
      end if;

   end Main;

   overriding
   procedure Finalize (Source : in out Managed_Source) is
   begin
      if Source.Content /= null then
         Free (Source.Content);
      end if;
   end Finalize;

   function Make (Content : String) return Managed_Source is
   begin
      return Source : Managed_Source do
         Source.Content := new String'(Content);
      end return;
   end Make;

   procedure Repl is
      VM : Lox_VM.VM_Context;
   begin
      Lox_VM.Init (VM);

      loop
         begin
            Ada.Text_IO.Put ("> ");
            declare
               Line   : constant String := Ada.Text_IO.Get_Line;
               Source : constant Managed_Source := Make (Line);
               Unused : Lox_VM.Interpret_Result;
            begin
               Unused :=
                 Lox_VM.Interpret
                   (VM,
                    Lox_Scanner.Source_Code (Source.Content),
                    Main_Chunk'Access);
            end;

         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;
   end Repl;

   procedure Run_File (Path : String) is
      File_Content : constant String := Read_File (Path);
      Source       : constant Managed_Source := Make (File_Content);
      VM           : Lox_VM.VM_Context;
      Result       : Lox_VM.Interpret_Result;

      use type Lox_VM.Interpret_Result;
   begin
      Lox_VM.Init (VM);

      Result :=
        Lox_VM.Interpret
          (VM, Lox_Scanner.Source_Code (Source.Content), Main_Chunk'Access);

      if Result = Lox_VM.INTERPRET_COMPILE_ERROR then
         raise Exceptions.Compile_Error;
      end if;

      if Result = Lox_VM.INTERPRET_RUNTIME_ERROR then
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

         use type Ada.Streams.Stream_IO.Count;
      begin
         Ada.Streams.Stream_IO.Read (File, Data, Last);
         Ada.Streams.Stream_IO.Close (File);

         if Size < 1 then
            return "";
         end if;

         declare
            Source : String (1 .. Positive (Size));
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
