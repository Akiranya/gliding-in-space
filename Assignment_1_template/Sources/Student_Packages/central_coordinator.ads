with Ada.Text_IO; use Ada.Text_IO;
with Swarm_Data; use Swarm_Data;
with Swarm_Structures;      use Swarm_Structures;
with Vehicle_Task_Type; use Vehicle_Task_Type;
with Vehicle_Message_Type; use Vehicle_Message_Type;
with Vectors_3D; use Vectors_3D;

package Central_Coordinator is

   task type Coordinator is
      entry Receive (Msg : Inter_Vehicle_Messages);
   end Coordinator;

end Central_Coordinator;
