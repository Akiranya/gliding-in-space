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
with Ada.Numerics; use Ada.Numerics;

--  with Ada.Text_IO; use Ada.Text_IO;
--  with Central_Coordinator; use Central_Coordinator;

package body Vehicle_Task_Type is

--     God : Coordinator;

   task body Vehicle_Task is

      Vehicle_No : Positive;
      Recent_Messages : Inter_Vehicle_Messages;
      Local_Charging : Boolean := False;
      Time : Real := 0.0; -- time for drawing circle
      Radius : constant Real := 0.25; -- orbit radius
      Orbit : Vector_3D;

      -- helper function: get a element from array
      -- grabs the first element in this array from the time being.
      function Grab_A_Globe (Globes : Energy_Globes) return Energy_Globe is (Globes (1));
      function Has_Energy_Nearby (Globes : Energy_Globes) return Boolean is (Globes'Length > 0);

      procedure Update_Charging (State : Boolean) is
      begin
         Local_Charging := State;
         Recent_Messages.Charging := State;
      end;

      procedure Orbiting (Throttle : Real; Tick : Real) is
      begin
         Orbit := (Real_Elementary_Functions.Cos (Time),
                   Real_Elementary_Functions.Sin (Time),
                   0.0);
         Orbit := Orbit * Radius; -- (cos(Time) + sin(Time)) * Radius
         Orbit := Orbit + Recent_Messages.Globe_Loc; -- adjusts orbiting origin
         Orbit := Orbit + Recent_Messages.Globe_Vel; -- predicits oribiting origin
         Time := Time + Pi / Tick; -- increment Time for next calculation
         Set_Destination (Orbit);
         Set_Throttle (Throttle);
      end Orbiting;

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

         Recent_Messages := (Source_ID => 999, -- non-existent no, placeholder
                             Forwarder_ID => Vehicle_No,
                             Globe_Loc => Zero_Vector_3D,
                             Globe_Vel => Zero_Vector_3D,
                             Charging => False);
      end Identify;

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

            -- send message if found the globe(s).
            if Has_Energy_Nearby (Energy_Globes_Around) then
               declare
                  Lucky_Globe : constant Energy_Globe := Grab_A_Globe (Energy_Globes_Around);
                  Lucky_Info : constant Inter_Vehicle_Messages := (Source_ID => Vehicle_No,
                                                                   Forwarder_ID => Vehicle_No,
                                                                   Globe_Loc => Lucky_Globe.Position,
                                                                   Globe_Vel => Lucky_Globe.Velocity,
                                                                   Charging => False);
               begin
                  Recent_Messages := Lucky_Info;
                  Send (Recent_Messages);
--                    Report ("globbe nearby.");
               end;
            end if;

            -- try to receive message:
            -- if this ship receives a message,
            -- it should spread this message to its nearby globes.
            if Messages_Waiting then
               declare
--                    Distance : Real; -- distance between this ship and globe found
                  Incomming_Message : Inter_Vehicle_Messages;
               begin
                  Receive (Incomming_Message);
                  -- only updates info if the message is from new source, or
                  -- some ship's going to charge (Charging = True)
--                    if not (Incomming_Message.Source_ID = Vehicle_No)
--                      and then not (Incomming_Message.Forwarder_ID = Vehicle_No)
                  if not (Incomming_Message.Forwarder_ID = Vehicle_No)
                  then
                     Recent_Messages := Incomming_Message;
--                       Report ("incomming new message. Source: " & Recent_Messages.Source_ID'Image);
                  end if;

                  -- spread incomming message to nearby ships:
                  Send (Recent_Messages);
               end;
            end if;

            -- if this ship is not going to charge, then
            -- let it orbit around the globe.
            if not Local_Charging
            then
               Orbiting (Throttle => 0.5,
                         Tick     => 100.0);
            end if;

            -- try to charge:
            if Current_Charge < 0.75 and then not Recent_Messages.Charging then
               Update_Charging (True); -- tells other ships i'm going to charge.
               Send (Recent_Messages);

               Set_Destination (Recent_Messages.Globe_Loc);
               Set_Throttle (1.0); -- as faster as possible
               Report ("charging!");
            end if;

            -- after recharging, go back to orbit by using *local* globe info
            if Recent_Messages.Charging and then Current_Charge >= 0.75 then
               Update_Charging (False); -- tells other ships I've finished charging.

               -- go back to orbit
               Orbiting (Throttle => 1.0,
                         Tick     => 100.0);
               Report ("back to orbit.");

               delay 0.2; -- wait until the ship is on orbit.
               Send (Recent_Messages); -- !!! no guarantee to be received. !!!
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;

--              if abs (Recent_Messages.Globe_Loc - Position) > 0.4 then
--                 Report ("In control.");
--                 Set_Throttle (1.0);
--                 Set_Destination (Recent_Messages.Globe_Loc);
--              end if;
