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
     Tree_Error_Excess_Closing_Parentheses,
     Tree_Error_Unterminated_List);

  --
  -- Tree node types.
  --

  -- Kind of list element.
  type Node_Kind_t is
    (Node_String,
     Node_Symbol,
     Node_List);

  -- Element of list.
  type Node_t is private;

  subtype Node_Symbol_Name_t is Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
  subtype Node_String_Data_t is Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

  -- Return kind of node.
  function Node_Kind (Node : in Node_t) return Node_Kind_t;

  -- Key referring to data in node.
  type Node_String_Data_ID_t is private;

  --
  -- Tree list types.
  --

  -- List, containing nodes.
  type List_t is private;

  -- Unique list identifier.
  type List_ID_t is new Positive;

  --
  -- Status values corresponding to error conditions.
  --

  subtype Tree_Error_Status_t is Tree_Status_t
    range Tree_Error_Excess_Closing_Parentheses .. Tree_Status_t'Last;

  --
  -- Tree is initialized?
  --

  function Initialized
    (Tree : in     Tree_t) return Boolean;

  --
  -- Tree parsing is completed?
  --

  function Completed
    (Tree : in     Tree_t) return Boolean;

  --
  -- Initialize parser state.
  --

  procedure Initialize_Tree
    (Tree   : in out Tree_t;
     Status :    out Tree_Status_t);
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
  pragma Precondition
    (Initialized (Tree) and
     Token.Is_Valid     and
     not Completed (Tree));

  --
  -- Subprograms only of practical use to the rest of Symbex.
  --

  package Internal is

    --
    -- Fetch cached string data.
    --

    function Get_String
      (Tree : in Tree_t;
       ID   : in Node_String_Data_ID_t)
      return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
    pragma Precondition (Completed (Tree));

    --
    -- Fetch node data ID (only valid for strings and symbols).
    --

    function Get_Data_ID (Node : in Node_t) return Node_String_Data_ID_t;
    pragma Precondition (Node_Kind (Node) /= Node_List);

    --
    -- Fetch node list ID (only valid for lists).
    --

    function Get_List_ID (Node : in Node_t) return List_ID_t;
    pragma Precondition (Node_Kind (Node) = Node_List);

    --
    -- Retrieve list.
    --

    function Get_List
      (Tree    : in Tree_t;
       List_ID : in List_ID_t) return List_t;
    pragma Precondition (Completed (Tree));

    --
    -- Iterate over nodes in list.
    --

    procedure List_Iterate
      (List    : in List_t;
       Process : access procedure (Node : in Node_t));

  end Internal;

private
  package UBW_Strings renames Ada.Strings.Wide_Unbounded;

  -- Allow '=' operator to be visible.
  use type UBW_Strings.Unbounded_Wide_String;

  -- String hashing function for unbounded wide strings.
  function UBW_String_Hash (Item : UBW_Strings.Unbounded_Wide_String) return Ada.Containers.Hash_Type;

  -- Hashed set of unbounded strings.
  package Node_String_Cache is new Ada.Containers.Hashed_Sets
    (Element_Type        => UBW_Strings.Unbounded_Wide_String,
     Hash                => UBW_String_Hash,
     Equivalent_Elements => UBW_Strings."=");

  -- Integer index/key of unbounded wide string in hashed set.
  type Node_String_Data_ID_t is mod 2 ** 32;

  -- Integer hashing function for hashed set keys.
  function Node_String_Cache_ID_Hash
    (Key : Node_String_Data_ID_t) return Ada.Containers.Hash_Type;

  -- Return ID value of Element.
  function Node_String_Cache_ID_Key
    (Element : UBW_Strings.Unbounded_Wide_String) return Node_String_Data_ID_t;

  -- Retrieve elements from hashed set by key.
  package Node_String_Cache_By_Key is new Node_String_Cache.Generic_Keys
    (Key_Type        => Node_String_Data_ID_t,
     Key             => Node_String_Cache_ID_Key,
     Hash            => Node_String_Cache_ID_Hash,
     Equivalent_Keys => "=");

  -- Node type, element of list.
  type Node_t (Kind : Node_Kind_t := Node_Symbol) is record
    case Kind is
      when Node_Symbol => Name : Node_String_Data_ID_t;
      when Node_String => Data : Node_String_Data_ID_t;
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
    Completed    : Boolean;
    List_Stack   : List_ID_Stack.Stack_t;
    Lists        : List_Array_t;
    Current_List : List_ID_t;
    String_Cache : Node_String_Cache.Set;
  end record;

end Symbex.Parse;
