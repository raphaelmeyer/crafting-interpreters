limited with Lox_Object;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Lox_Value is
   package Unbounded renames Ada.Strings.Unbounded;
   subtype Unbounded_String is Unbounded.Unbounded_String;

   type Value_Type is
     (VAL_BOOL, VAL_NIL, VAL_NUMBER, VAL_STRING, VAL_OBJECT, VAL_NATIVE);

   type Lox_Float (Is_Valid : Boolean := True) is record
      case Is_Valid is
         when True =>
            Value : Long_Float;

         when False =>
            null;
      end case;
   end record;

   overriding
   function "=" (Left, Right : Lox_Float) return Boolean;
   function "+" (Left, Right : Lox_Float) return Lox_Float;
   function "-" (Left, Right : Lox_Float) return Lox_Float;
   function "*" (Left, Right : Lox_Float) return Lox_Float;
   function "/" (Left, Right : Lox_Float) return Lox_Float;
   function "<" (Left, Right : Lox_Float) return Boolean;
   function ">" (Left, Right : Lox_Float) return Boolean;
   function "-" (Right : Lox_Float) return Lox_Float;

   NaN : constant Lox_Float := (Is_Valid => False);

   type Value;

   type Native_Fn is access function (First_Arg : Natural) return Value;

   type Native is record
      Arity : Natural;
      Func  : Native_Fn;
   end record;

   type Value (Kind : Value_Type := VAL_NIL) is record
      case Kind is
         when VAL_BOOL =>
            Bool_Value : Boolean;

         when VAL_NUMBER =>
            Number_Value : Lox_Float;

         when VAL_STRING =>
            String_Value : Unbounded_String;

         when VAL_OBJECT =>
            Object_Value : access Lox_Object.Object;

         when VAL_NATIVE =>
            Native_Value : Native;

         when others =>
            null;
      end case;
   end record;

   package Value_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Value);

   subtype Value_Array is Value_Vectors.Vector;

   function Make_Nil return Value;
   function Is_Nil (V : Value) return Boolean;

   function Make_Bool (Bool : Boolean) return Value;
   function Is_Bool (V : Value) return Boolean;

   function Make_Number (Number : Lox_Value.Lox_Float) return Value;
   function Is_Number (V : Value) return Boolean;

   function Make_String (Str : Unbounded_String) return Value;
   function Is_String (V : Value) return Boolean;

   function Is_Object (V : Value) return Boolean;

   function Make_Closure (Closure : Lox_Object.Object_Access) return Value;
   function Is_Closure (V : Value) return Boolean;

   function Make_Function (Func : Lox_Object.Object_Access) return Value;
   function Is_Function (V : Value) return Boolean;

   function Make_Native (Arity : Natural; Func : Native_Fn) return Value;
   function Is_Native (V : Value) return Boolean;

   procedure Write (VA : in out Value_Array; V : Value);

   procedure Print_Value (V : Value);

   function Values_Equal (A : Value; B : Value) return Boolean;

private
   function To_String (V : Lox_Value.Lox_Float) return String;

end Lox_Value;
