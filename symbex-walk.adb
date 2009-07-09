package body Symbex.Walk is

  procedure Walk_Tree
    (Tree   : in     Parse.Tree_t;
     Status :    out Status_t)
  is
    Finish_Tree  : exception;
    Finish_Error : exception;

    -- Recursive list walking procedure.
    procedure Walk_List
      (Tree    : in     Parse.Tree_t;
       List_ID : in     Parse.List_ID_t;
       Status  :    out Status_t)
    is
      procedure Process_List (List : in Parse.List_t) is
        Current_Status : Status_t;

        -- Process node, call handler based on node type.
        procedure Process_Node (Node : in Parse.Node_t) is
        begin
          case Parse.Node_Kind (Node) is
            when Parse.Node_Symbol =>
              Handle_Symbol
                (Name    => Parse.Internal.Get_String
                  (Tree => Tree,
                   ID   => Parse.Internal.Get_Data_ID (Node)),
                 List_ID => List_ID,
                 Status  => Current_Status);
            when Parse.Node_String =>
              Handle_String
                (Data    => Parse.Internal.Get_String
                  (Tree => Tree,
                   ID   => Parse.Internal.Get_Data_ID (Node)),
                 List_ID => List_ID,
                 Status  => Current_Status);
            when Parse.Node_List =>
              Walk_List
                (Tree    => Tree,
                 List_ID => List_ID,
                 Status  => Current_Status);
          end case;
        end Process_Node;
      begin
        -- Open list callback.
        Handle_List_Open
          (List_ID => List_ID,
           Status  => Current_Status);
        case Current_Status is
          when Walk_Continue    => null;
          when Walk_Finish_List => return;
          when Walk_Finish_Tree => raise Finish_Tree;
          when Walk_Error       => raise Finish_Error;
        end case;

        -- Iterate over all nodes in list.
        Parse.Lists.Iterate
          (Container => List.Nodes,
           Process   => Process_Node'Access);
        case Current_Status is
          when Walk_Continue    => null;
          when Walk_Finish_List => return;
          when Walk_Finish_Tree => raise Finish_Tree;
          when Walk_Error       => raise Finish_Error;
        end case;

        -- Close list callback.
        Handle_List_Close
          (List_ID => List_ID,
           Status  => Current_Status);
        case Current_Status is
          when Walk_Continue    => null;
          when Walk_Finish_List => return;
          when Walk_Finish_Tree => raise Finish_Tree;
          when Walk_Error       => raise Finish_Error;
        end case;
      end Process_List;
    begin
      List_Arrays.Query_Element
        (Container => Tree.Lists,
         Index     => List_ID,
         Process   => Process_Element'Access);
    end Walk_List;
  begin
    Walk_List
      (Tree    => Tree,
       List_ID => Parse.List_ID_t'First,
       Status  => Status);
  exception
    when Finish_Tree  => Status := Walk_Finish_Tree;
    when Finish_Error => Status := Walk_Error;
  end Walk_Tree;

end Symbex.Walk;