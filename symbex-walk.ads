with Symbex.Parse;

package Symbex.Walk is

  type Walk_Status_t is
    (Walk_Continue,
     Walk_Finish_List,
     Walk_Finish_Tree,
     Walk_Error);

  generic
    with procedure Handle_List_Open
      (List_ID : in     Parse.List_ID_t;
       Status  :    out Walk_Status_t);

    with procedure Handle_Symbol
      (Name    : in     Parse.Node_Symbol_Name_t;
       List_ID : in     Parse.List_ID_t;
       Status  :    out Walk_Status_t);

    with procedure Handle_String
      (Data    : in     Parse.Node_String_Data_t;
       List_ID : in     Parse.List_ID_t;
       Status  :    out Walk_Status_t);

    with procedure Handle_List_Close
      (List_ID : in     Parse.List_ID_t;
       Status  :    out Walk_Status_t);

  procedure Walk_Tree
    (Tree   : in     Parse.Tree_t;
     Status :    out Walk_Status_t);
  -- pragma Precondition (Parse.Completed (Tree));

end Symbex.Walk;
