package body Central_Coordinator is

   task body Coordinator is
      use Swarm_Vectors;
      Destination : Vector_3D;
   begin
      delay 2.0;
      accept Receive (Msg : in Inter_Vehicle_Messages) do
         Destination := Msg.Globe_Position;
      end Receive;

      -- following code sends messages to all vehicles.

      for Element_Index in First_Index (Swarm_State) .. Last_Index (Swarm_State) loop
         declare
            This_Element : constant Swarm_Element_State := Element (Swarm_State, Element_Index);
         begin
            null;
--                 This_Element.Controls.all.Set_Throttle (0.0);
         end;
      end loop;
   end Coordinator;

end Central_Coordinator;
