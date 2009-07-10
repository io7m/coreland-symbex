with Interfaces;

use type Interfaces.Unsigned_32;

package body Symbex.Parse is

  package body Internal is

    function Get_Data (Node : in Node_t)
      return UBW_Strings.Unbounded_Wide_String is
    begin
      case Node.Kind is
        when Node_Symbol => return Node.Name;
        when Node_String => return Node.Data;
        when others      => raise Constraint_Error with "invalid node type";
      end case;
    end Get_Data;

    function Get_List_ID (Node : in Node_t) return List_ID_t is
    begin
      case Node.Kind is
        when Node_List => return Node.List;
        when others    => raise Constraint_Error with "invalid node type";
      end case;
    end Get_List_ID;

    function Get_List_Length (List : in List_t) return List_Length_t is
    begin
      return List_Length_t (Lists.Length (List.Nodes));
    end Get_List_Length;

    procedure List_Iterate
      (List    : in List_t;
       Process : access procedure (Node : in Node_t))
    is
      procedure Inner_Process (Cursor : in Lists.Cursor) is
      begin
        Lists.Query_Element (Cursor, Process);
      end Inner_Process;
    begin
      Lists.Iterate
        (Container => List.Nodes,
         Process   => Inner_Process'Access);
    end List_Iterate;

    function Get_List
      (Tree    : in Tree_t;
       List_ID : in List_ID_t) return List_t is
    begin
      return List_Arrays.Element
        (Container => Tree.Lists,
         Index     => List_ID);
    end Get_List;

  end Internal;

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
  -- Append list to list array and push list ID onto stack.
  --

  procedure Append_List
    (Tree    : in out Tree_t;
     List    : in     List_t;
     List_ID : in     List_ID_t) is
  begin
    -- Add list to tree.
    List_Arrays.Append
      (Container => Tree.Lists,
       New_Item  => List);
    pragma Assert (List_Arrays.Last_Index (Tree.Lists) = List_ID);

    -- Push list onto stack.
    List_ID_Stack.Push
      (Stack   => Tree.List_Stack,
       Element => List_ID);
  end Append_List;

  --
  -- Token processors.
  --

  --
  -- Add quoted string to current list.
  --

  procedure Process_Quoted_String
    (Tree   : in out Tree_t;
     Token  : in     Lex.Token_t)
  is
    Current_List : List_ID_t;
    Node         : Node_t (Kind => Node_String);
  begin
    Node.Data := Token.Text;

    -- Fetch current list.
    List_ID_Stack.Peek
      (Stack   => Tree.List_Stack,
       Element => Current_List);

    -- Add node to list.
    Append_Node
      (Tree    => Tree,
       List_ID => Current_List,
       Node    => Node);
  end Process_Quoted_String;

  --
  -- Add symbol to current list.
  --

  procedure Process_Symbol
    (Tree   : in out Tree_t;
     Token  : in     Lex.Token_t)
  is
    Current_List : List_ID_t;
    Node         : Node_t (Kind => Node_Symbol);
  begin
    Node.Name := Token.Text;

    -- Fetch current list.
    List_ID_Stack.Peek
      (Stack   => Tree.List_Stack,
       Element => Current_List);

    -- Add node to list.
    Append_Node
      (Tree    => Tree,
       List_ID => Current_List,
       Node    => Node);
  end Process_Symbol;

  --
  -- Open new list. Create new node pointing to new list in current.
  -- Push list onto stack.
  --

  procedure Process_List_Open (Tree : in out Tree_t) is
    List   : List_t;
    New_ID : List_ID_t;
    Node   : Node_t (Kind => Node_List);
  begin
    New_ID := List_Arrays.Last_Index (Tree.Lists) + 1;

    -- Fetch list parent, if available.
    List_ID_Stack.Peek
      (Stack   => Tree.List_Stack,
       Element => List.Parent);

    -- Append node to parent pointing to this list.
    Node.List := New_ID;
    Append_Node
      (Tree    => Tree,
       List_ID => List.Parent,
       Node    => Node);

    -- Add list to tree.
    Append_List
      (Tree    => Tree,
       List    => List,
       List_ID => New_ID);

    pragma Assert (List_Arrays.Last_Index (Tree.Lists) = New_ID);
  end Process_List_Open;

  --
  -- Close list and remove from stack.
  --

  procedure Process_List_Close
    (Tree   : in out Tree_t;
     Status : in out Tree_Status_t) is
  begin
    if List_ID_Stack.Size (Tree.List_Stack) > 1 then
      List_ID_Stack.Pop_Discard (Tree.List_Stack);
    else
      Status := Tree_Error_Excess_Closing_Parentheses;
    end if;
  end Process_List_Close;

  --
  -- Check for premature EOF.
  --

  procedure Process_EOF
    (Tree   : in out Tree_t;
     Status : in out Tree_Status_t) is
  begin
    if List_ID_Stack.Size (Tree.List_Stack) > 1 then
      Status := Tree_Error_Unterminated_List;
    end if;
    Tree.Completed := True;
  end Process_EOF;

  --
  -- Add initial empty root list.
  --

  procedure Add_Root_List (Tree : in out Tree_t) is
    List   : List_t;
    New_ID : List_ID_t;
  begin
    New_ID := List_ID_t'First;

    -- Root list is parent of itself.
    List.Parent := New_ID;

    -- Add list to tree.
    Append_List
      (Tree    => Tree,
       List    => List,
       List_ID => New_ID);
  end Add_Root_List;

  --
  -- Public API.
  --

  function Initialized
    (Tree : in     Tree_t) return Boolean is
  begin
    return Tree.Inited;
  end Initialized;

  function Completed
    (Tree : in     Tree_t) return Boolean is
  begin
    return Tree.Completed;
  end Completed;

  --
  -- Initialize tree state.
  --

  procedure Initialize_Tree
    (Tree   : in out Tree_t;
     Status :    out Tree_Status_t) is
  begin
    Tree := Tree_t'
      (Inited       => True,
       Completed    => False,
       List_Stack   => <>,
       Lists        => <>,
       Current_List => List_ID_t'First);

    Add_Root_List (Tree);

    Status := Tree_OK;
  end Initialize_Tree;

  --
  -- Process token.
  --

  procedure Process_Token
    (Tree   : in out Tree_t;
     Token  : in     Lex.Token_t;
     Status :    out Tree_Status_t) is
  begin
    -- Status is OK by default.
    Status := Tree_OK;

    case Token.Kind is
      when Lex.Token_Quoted_String =>
        Process_Quoted_String
          (Tree   => Tree,
           Token  => Token);
      when Lex.Token_Symbol =>
        Process_Symbol
          (Tree   => Tree,
           Token  => Token);
      when Lex.Token_List_Open =>
        Process_List_Open (Tree);
      when Lex.Token_List_Close =>
        Process_List_Close
          (Tree   => Tree,
           Status => Status);
      when Lex.Token_EOF =>
        Process_EOF
          (Tree   => Tree,
           Status => Status);
    end case;
  end Process_Token;

  --
  -- Node accessors.
  --

  function Node_Kind (Node : in Node_t) return Node_Kind_t is
  begin
    return Node.Kind;
  end Node_Kind;

end Symbex.Parse;
