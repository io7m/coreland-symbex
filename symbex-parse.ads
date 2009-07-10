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

  -- Status values corresponding to error conditions.
  subtype Tree_Error_Status_t is Tree_Status_t
    range Tree_Error_Excess_Closing_Parentheses .. Tree_Status_t'Last;

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

  --
  -- Tree list types.
  --

  -- List, containing nodes.
  type List_t is private;

  -- Unique list identifier.
  type List_ID_t       is new Positive;
  type List_Length_t   is new Natural;
  type List_Position_t is new Positive;
  type List_Depth_t    is new Natural;

  -- Retrieve number of nodes in list.
  function List_Length (List : in List_t) return List_Length_t;

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
    -- Fetch node data (only valid for strings and symbols).
    --

    function Get_Data (Node : in Node_t)
      return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
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

  -- Node type, element of list.
  type Node_t (Kind : Node_Kind_t := Node_Symbol) is record
    case Kind is
      when Node_Symbol => Name : Node_Symbol_Name_t;
      when Node_String => Data : Node_String_Data_t;
      when Node_List   => List : List_ID_t;
    end case;
  end record;

  --
  -- Node list.
  --

  package Lists is new Ada.Containers.Vectors
    (Index_Type   => List_Position_t,
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
  end record;

end Symbex.Parse;
