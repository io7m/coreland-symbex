package body Symbex.Walk is

  procedure Walk_Tree
    (Tree   : in     Parse.Tree_t;
     Status :    out Walk_Status_t)
  is
    Finish_Tree  : exception;
    Finish_Error : exception;
    Depth        : Natural := 0;

    -- Recursive list walking procedure.
    procedure Walk_List
      (Tree    : in Parse.Tree_t;
       List_ID : in Parse.List_ID_t)
    is
      use type Parse.List_Position_t;

      procedure Process_List (List : in Parse.List_t) is
        Current_Status : Walk_Status_t;
        Length         : Parse.List_Length_t;
        Position       : Parse.List_Position_t;

        -- Process node, call handler based on node type.
        procedure Process_Node (Node : in Parse.Node_t) is
        begin
          case Parse.Node_Kind (Node) is
            when Parse.Node_Symbol =>
              Handle_Symbol
                (Name          => Parse.Internal.Get_Data (Node),
                 List_ID       => List_ID,
                 List_Position => Position,
                 List_Length   => Length,
                 Status        => Current_Status);
            when Parse.Node_String =>
              Handle_String
                (Data          => Parse.Internal.Get_Data (Node),
                 List_ID       => List_ID,
                 List_Position => Position,
                 List_Length   => Length,
                 Status        => Current_Status);
            when Parse.Node_List =>
              Walk_List
                (Tree    => Tree,
                 List_ID => Parse.Internal.Get_List_ID (Node));
          end case;
          Position := Position + 1;
        end Process_Node;
      begin
        Length   := Parse.Internal.Get_List_Length (List);
        Position := Parse.List_Position_t'First;

        -- Open list callback.
        Handle_List_Open
          (List_ID => List_ID,
           Depth   => Parse.List_Depth_t (Depth),
           Status  => Current_Status);
        case Current_Status is
          when Walk_Continue    => null;
          when Walk_Finish_List => return;
          when Walk_Finish_Tree => raise Finish_Tree;
          when Walk_Error       => raise Finish_Error;
        end case;

        -- Iterate over all nodes in list.
        Parse.Internal.List_Iterate
          (List    => List,
           Process => Process_Node'Access);
        case Current_Status is
          when Walk_Continue    => null;
          when Walk_Finish_List => return;
          when Walk_Finish_Tree => raise Finish_Tree;
          when Walk_Error       => raise Finish_Error;
        end case;

        -- Close list callback.
        Handle_List_Close
          (List_ID => List_ID,
           Depth   => Parse.List_Depth_t (Depth),
           Status  => Current_Status);
        case Current_Status is
          when Walk_Continue    => null;
          when Walk_Finish_List => return;
          when Walk_Finish_Tree => raise Finish_Tree;
          when Walk_Error       => raise Finish_Error;
        end case;
      end Process_List;
    begin
      Depth := Depth + 1;
      Process_List
        (Parse.Internal.Get_List
          (Tree    => Tree,
           List_ID => List_ID));
      Depth := Depth - 1;
    end Walk_List;
  begin
    Walk_List
      (Tree    => Tree,
       List_ID => Parse.List_ID_t'First);
  exception
    when Finish_Tree  => Status := Walk_Finish_Tree;
    when Finish_Error => Status := Walk_Error;
  end Walk_Tree;

end Symbex.Walk;
