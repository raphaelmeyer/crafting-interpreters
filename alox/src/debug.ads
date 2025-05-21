with Chunk;
with Value;

package Debug is

   procedure DisassembleChunk (C : Chunk.Chunk; Name : String);

private
   function DisassembleInstruction
     (C : Chunk.Chunk; Offset : Natural) return Natural;

   function ConstantInstruction
     (Name : String; C : Chunk.Chunk; Offset : Natural) return Natural;
   function SimpleInstruction (Name : String; Offset : Natural) return Natural;

   procedure PrintValue (V : Value.Value);

end Debug;
