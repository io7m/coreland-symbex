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
  -- Append Node to list List_ID
  --

  procedure Append_Node
    (Tree    : in out Tree_t;
     Node    : in     Node_t;
     List_ID : in     List_ID_t)
  is
    procedure Process (List : in out List_t) is
    begin
      Lists.Append
        (Container => List.Nodes,
         New_Item  => Node);
    end Process;
  begin
    List_Arrays.Update_Element
      (Container => Tree.Lists,
       Index     => List_ID,
       Process   => Process'Access);
  end Append_Node;

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
      Tree_t'(Inited       => True,
              List_Stack   => <>,
              Lists        => <>,
              Current_List => List_ID_t'First);
    Status := Tree_OK;
  end Initialize;

  --
  -- Process token.
  --

  procedure Process_Token
    (Tree   : in out Tree_t;
     Token  : in     Lex.Token_t;
     Status :    out Tree_Status_t)
  is
    procedure Quoted_String is
    begin
      null;
    end Quoted_String;

    procedure Symbol is
    begin
      null;
    end Symbol;

    procedure List_Open is
      List   : List_t;
      New_ID : List_ID_t;
      Node   : Node_t (Kind => Node_List);
    begin
      -- The ID of this new list will be the previous list + 1.
      New_ID := List_Arrays.Last_Index (Tree.Lists) + 1;

      -- Fetch list parent, if available.
      if List_ID_Stack.Size (Tree.List_Stack) > 0 then
        List_ID_Stack.Peek
          (Stack   => Tree.List_Stack,
           Element => List.Parent);

        -- Append node to parent pointing to this list.
        Node.List := New_ID;
        Append_Node
          (Tree    => Tree,
           List_ID => List.Parent,
           Node    => Node);
      else
        -- Root list has self as own parent.
        List.Parent := New_ID;
      end if;

      -- Add list to tree.
      List_Arrays.Append
        (Container => Tree.Lists,
         New_Item  => List);

      -- Push list onto stack.
      List_ID_Stack.Push
        (Stack   => Tree.List_Stack,
         Element => New_ID);

      pragma Assert (List_Arrays.Last_Index (Tree.Lists) = New_ID);
    end List_Open;

    procedure List_Close is
    begin
      Status := Tree_OK;
      List_ID_Stack.Pop_Discard (Tree.List_Stack);
    exception
      -- Stack underflow.
      when Constraint_Error =>
        Status := Tree_Error_Unbalanced_Parenthesis;
    end List_Close;

    procedure EOF is
    begin
      if List_ID_Stack.Size (Tree.List_Stack) /= 0 then
        Status := Tree_Error_Early_EOF;
      end if;
      Status := Tree_OK;
    end EOF;
  begin
    case Token.Kind is
      when Lex.Token_Quoted_String => Quoted_String;
      when Lex.Token_Symbol        => Symbol;
      when Lex.Token_List_Open     => List_Open;
      when Lex.Token_List_Close    => List_Close;
      when Lex.Token_EOF           => EOF;
    end case;
  end Process_Token;

end Symbex.Parse;
