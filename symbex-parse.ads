with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Unbounded;
with Symbex.Lex;
with Stack;

pragma Elaborate_All (Stack);

package Symbex.Parse is

  type Tree_t is private;

  --
  -- Tree status value.
  --

  type Tree_Status_t is
    (Tree_OK,
     Tree_Error_Unbalanced_Parenthesis,
     Tree_Error_Early_EOF);

  --
  -- Tree node kind.
  --

  type Node_Kind_t is
    (Node_String,
     Node_Symbol,
     Node_List);

  type Node_t is private;

  --
  -- Status values corresponding to error conditions.
  --

  subtype Tree_Error_Status_t is Tree_Status_t
    range Tree_Error_Unbalanced_Parenthesis .. Tree_Status_t'Last;

  --
  -- Tree is initialized?
  --

  function Initialized
    (Tree : in     Tree_t) return Boolean;

  --
  -- Initialize parser state.
  --

  procedure Initialize
    (Tree   : in out Tree_t;
     Status :    out Tree_Status_t);
  pragma Precondition (not Initialized (Tree));
--  pragma Postcondition
--    (((Status  = Tree_OK) and     Initialized (Tree)) or
--     ((Status /= Tree_OK) and not Initialized (Tree)));

  --
  -- Process token.
  --

  procedure Process_Token
    (Tree   : in out Tree_t;
     Token  : in     Lex.Token_t;
     Status :    out Tree_Status_t);
  pragma Precondition (Initialized (Tree));

private
  package UBW_Strings renames Ada.Strings.Wide_Unbounded;

  -- Allow '=' operator to be visible.
  use type UBW_Strings.Unbounded_Wide_String;

  -- String hashing function for unbounded wide strings.
  function UBW_String_Hash
    (Item : UBW_Strings.Unbounded_Wide_String) return Ada.Containers.Hash_Type;

  -- Hashed set of unbounded strings.
  package Node_String_Cache is new Ada.Containers.Hashed_Sets
    (Element_Type        => UBW_Strings.Unbounded_Wide_String,
     Hash                => UBW_String_Hash,
     Equivalent_Elements => UBW_Strings."=");

  -- Integer index/key of unbounded wide string in hashed set.
  type Node_String_Cache_ID_t is mod 2 ** 32;

  -- Integer hashing function for hashed set keys.
  function Node_String_Cache_ID_Hash
    (Key : Node_String_Cache_ID_t) return Ada.Containers.Hash_Type;

  -- Return ID value of Element.
  function Node_String_Cache_ID_Key
    (Element : UBW_Strings.Unbounded_Wide_String) return Node_String_Cache_ID_t;

  -- Retrieve elements from hashed set by key.
  package Node_String_Cache_By_Key is new Node_String_Cache.Generic_Keys
    (Key_Type        => Node_String_Cache_ID_t,
     Key             => Node_String_Cache_ID_Key,
     Hash            => Node_String_Cache_ID_Hash,
     Equivalent_Keys => "=");

  -- Index of list in array of lists.
  type List_ID_t is new Positive;

  -- Node type, element of list.
  type Node_t (Kind : Node_Kind_t := Node_Symbol) is record
    case Kind is
      when Node_Symbol => Name : Node_String_Cache_ID_t;
      when Node_String => Data : Node_String_Cache_ID_t;
      when Node_List   => List : List_ID_t;
    end case;
  end record;

  --
  -- Node list.
  --

  package Lists is new Ada.Containers.Vectors
    (Index_Type   => Positive,
     Element_Type => Node_t);

  subtype List_Nodes_t is Lists.Vector;

  type List_t is record
    Parent : List_ID_t;
    Nodes  : List_Nodes_t;
  end record;

  --
  -- Array of node lists.
  --

  package List_Arrays is new Ada.Containers.Vectors
    (Index_Type   => List_ID_t,
     Element_Type => List_t);

  subtype List_Array_t is List_Arrays.Vector;

  --
  -- List ID stack.
  --

  package List_ID_Stack is new Stack
    (Element_Type => List_ID_t);

  --
  -- Tree type.
  --

  type Tree_t is record
    Inited       : Boolean;
    List_Stack   : List_ID_Stack.Stack_t;
    Lists        : List_Array_t;
    Current_List : List_ID_t;
  end record;

end Symbex.Parse;
