--  with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vectors_3D;                 use Vectors_3D;
with Vehicle_Interface;          use Vehicle_Interface;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
--  with Swarm_Structures;           use Swarm_Structures;
with Swarm_Structures_Base; use Swarm_Structures_Base;
--  with Ada.Text_IO; use Ada.Text_IO;
--  with Central_Coordinator; use Central_Coordinator;

package body Vehicle_Task_Type is

--     God : Coordinator;

   task body Vehicle_Task is

      Vehicle_No : Positive;
--        Recent_Globe_Info : Energy_Globe;
      type Tick is mod Natural'Last;
      Time : Tick; -- time for drawing circle


      -- helper function: get a element from array
      -- grabs the first element in this array from the time being.
      function Grab_A_Globe (Globes : Energy_Globes) return Energy_Globe is (Globes (1));
      function Has_Energy_Nearby (Globes : Energy_Globes) return Boolean is (Globes'Length > 0);

      -- helper function: group all ships
      function From_Even return Boolean is (Vehicle_No mod 2 = 0);

      procedure Report (Info : String) is
      begin
         Put_Line (Vehicle_No'Image & " " & Info);
      end Report;

   begin

      -- You need to react to this call and provide your task_id.
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;
         Time := 0;
      end Identify;

      Report ("spawned.");

      -- Replace the rest of this task with your own code.
      -- Maybe synchronizing on an external event clock like "Wait_For_Next_Physics_Update",
      -- yet you can synchronize on e.g. the real-time clock as well.

      -- Without control this vehicle will go for its natural swarming

      select

         Flight_Termination.Stop;

      then abort

         Outer_task_loop : loop

            Wait_For_Next_Physics_Update;

            -- Your vehicle should respond to the world here: sense, listen, talk, act?

--              Set_Throttle (1.0);
--              Set_Destination (Zero_Vector_3D);
--              Target : constant Vector_3D := (5.0, 5.0, 5.0); -- the way how to create a vector.

            -- send message if found the globe(s).
            if Has_Energy_Nearby (Energy_Globes_Around) then
               declare
                  Lucky_Globe : constant Energy_Globe := Grab_A_Globe (Energy_Globes_Around);
                  Lucky_Info : constant Inter_Vehicle_Messages := (No => Vehicle_No,
                                                                   Globe_Loc => Lucky_Globe.Position,
                                                                   Globe_Vel => Lucky_Globe.Velocity,
                                                                   Towards_Globe => False);
               begin
                  Send (Message => Lucky_Info);
               end;
            end if;

            -- try to receive message.
            -- if this globe receives a message,
            -- it should spread this message to its nearby globes.
            if Messages_Waiting then
               declare
                  Incomming_Message : Inter_Vehicle_Messages;
                  Globe_Loc : Positions; -- helper local var
                  Globe_Vel : Velocities; -- helper local var
                  Distance : Real; -- distance between this ship and globe found
               begin
                  Receive (Incomming_Message); -- retrives the message.
                  Globe_Loc := Incomming_Message.Globe_Loc;
                  Globe_Vel := Incomming_Message.Globe_Vel;
--                    Put_Line (Vehicle_No'Image & " got message! " & Image (Globe_Vel));

                  -- the distance between this ship and the globe
                  Distance := Vectors_3D."abs" (Globe_Loc - Position);
--                    Report ("Distance: " & Distance'Image);
                  if Distance > 0.4 and From_Even then
--                       Report ("In control.");
                     -- let the ship follow the globe:
                     Set_Throttle (1.0);
                     Set_Destination (Globe_Loc);
                  end if;

                  -- spread incomming message to nearby ships:
                  Send (Incomming_Message);
               end;
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;
