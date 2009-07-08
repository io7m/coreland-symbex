with Interfaces;

use type Interfaces.Unsigned_32;

package body Symbex.Parse is

  --
  -- Basic wide string hashing function.
  --

  function UBW_String_Hash
    (Item : UBW_Strings.Unbounded_Wide_String) return Ada.Containers.Hash_Type
  is
    Temp_Hash : Interfaces.Unsigned_32 := 0;
  begin
    for Index in 1 .. UBW_Strings.Length (Item) loop
      Temp_Hash := Interfaces.Rotate_Left (Temp_Hash, 3) +
        Wide_Character'Pos (UBW_Strings.Element (Item, Index));
    end loop;

    return Ada.Containers.Hash_Type (Temp_Hash);
  end UBW_String_Hash;

  --
  -- Basic integer hashing function.
  --

  function Node_String_Cache_ID_Hash
    (Key : Node_String_Cache_ID_t) return Ada.Containers.Hash_Type
  is
    Temp_Hash : Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Key);
  begin
    Temp_Hash := (Temp_Hash +   16#7ed55d16#) +   (Interfaces.Rotate_Left (Temp_Hash, 12));
    Temp_Hash := (Temp_Hash xor 16#c761c23c#) xor (Interfaces.Shift_Right (Temp_Hash, 19));
    Temp_Hash := (Temp_Hash +   16#165667b1#) +   (Interfaces.Rotate_Left (Temp_Hash, 5));
    Temp_Hash := (Temp_Hash +   16#d3a2646c#) xor (Interfaces.Rotate_Left (Temp_Hash, 9));
    Temp_Hash := (Temp_Hash +   16#fd7046c5#) +   (Interfaces.Rotate_Left (Temp_Hash, 3));
    Temp_Hash := (Temp_Hash xor 16#b55a4f09#) xor (Interfaces.Shift_Right (Temp_Hash, 16));

    return Ada.Containers.Hash_Type (Temp_Hash);
  end Node_String_Cache_ID_Hash;

  --
  -- Return ID of Key.
  -- Hash (Element) = Hash (Key (Element))
  --

  function Node_String_Cache_ID_Key
    (Element : UBW_Strings.Unbounded_Wide_String) return Node_String_Cache_ID_t is
  begin
    return Node_String_Cache_ID_t (UBW_String_Hash (Element));
  end Node_String_Cache_ID_Key;

  --
  -- Public API.
  --

  function Initialized
    (Tree : in     Tree_t) return Boolean is
  begin
    return Tree.Inited;
  end Initialized;

  --
  -- Initialize parser state.
  --

  procedure Initialize
    (Tree   : in out Tree_t;
     Status :    out Tree_Status_t) is
  begin
    Tree :=
      Tree_t'(Inited     => True,
              List_Depth => 0,
              Lists      => <>);
    Status := Tree_OK;
  end Initialize;

  --
  -- Process token.
  --

  procedure Process_Token
    (Tree   : in out Tree_t;
     Token  : in     Lex.Token_t;
     Status :    out Tree_Status_t) is
  begin
    case Token.Kind is
      when Lex.Token_Quoted_String => null;
      when Lex.Token_Symbol        => null;
      when Lex.Token_List_Open     => null;
      when Lex.Token_List_Close    => null;
      when Lex.Token_EOF           => null;
    end case;
  end Process_Token;

end Symbex.Parse;
