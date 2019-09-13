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

package body Vehicle_Task_Type is

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
      end Update_Charging;

      procedure Orbiting (Throttle : Real; Tick : Real) is
      begin
         Orbit := (Real_Elementary_Functions.Cos (Time),
                   Real_Elementary_Functions.Sin (Time),
                   0.0);
         Orbit := Orbit * Radius; -- (cos(Time) + sin(Time)) * Radius
         Orbit := Orbit + Recent_Messages.Globe_Loc; -- sets orbiting origin
         Orbit := Orbit + Recent_Messages.Globe_Vel; -- adds velocity to generate more roboust orbit
         Time := Time + Pi / Tick; -- increment Time for next calculation
         Set_Destination (Orbit);
         Set_Throttle (Throttle);
      end Orbiting;

      procedure Report (Info : String) is
      begin
         Put_Line (Vehicle_No'Image & " " & Info);
      end Report;

      task type Message_Sender is
         entry Async_Send (Incomming_Message_In : Inter_Vehicle_Messages;
                           Send_Interval_In : Duration;
                           Send_Count_In : Positive);
      end Message_Sender;
      type Message_Sender_Pt is access Message_Sender;
      task body Message_Sender is
         Incomming_Message : Inter_Vehicle_Messages;
         Send_Interval : Duration;
         Send_Count : Positive;
      begin
--           Report ("sender task created.");
         accept Async_Send (Incomming_Message_In : Inter_Vehicle_Messages;
                            Send_Interval_In : Duration;
                            Send_Count_In : Positive)
         do
            Incomming_Message := Incomming_Message_In;
            Send_Interval := Send_Interval_In;
            Send_Count := Send_Count_In;
         end Async_Send;

         for I in 1 .. Send_Count loop
            Send (Incomming_Message);
            delay Send_Interval;
         end loop;
      end Message_Sender;

   begin

      -- You need to react to this call and provide your task_id.
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;
         Recent_Messages := (Source_ID => 999, -- non-existent no, as placeholder
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
                  Lucky_Info : constant Inter_Vehicle_Messages := (Source_ID => Vehicle_No, -- to be used?
                                                                   Forwarder_ID => Vehicle_No,
                                                                   Globe_Loc => Lucky_Globe.Position,
                                                                   Globe_Vel => Lucky_Globe.Velocity,
                                                                   Charging => False);
               begin
                  Recent_Messages := Lucky_Info;
                  Send (Recent_Messages);
--                    Report ("globe nearby.");
               end;
            end if;

            -- try to receive message:
            -- if this ship receives a message,
            -- it should spread this message to its nearby globes.
            if Messages_Waiting then
               declare
                  Incomming_Message : Inter_Vehicle_Messages;
                  -- i know the declare-block is redundant, but in case for need.
                  -- Incomming_Message is here to decide if the ship
                  -- should update local message (and send it out).
               begin
                  Receive (Incomming_Message);
                  -- only updates info if the message is from new source, or
                  -- some ship's going to charge (Charging = True)
                  Recent_Messages := Incomming_Message;
                  Send (Recent_Messages); -- spread incomming message to nearby ships.
--                    Report ("incomming new message. Source: " & Recent_Messages.Source_ID'Image);
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
               Update_Charging (True);
               Send (Recent_Messages); -- tells other ships i'm going to charge.

               Set_Destination (Recent_Messages.Globe_Loc);
               Set_Throttle (1.0); -- as faster as possible
--                 Report ("charging!");
            end if;

            -- after recharging, go back to orbit by using *local* globe info
            if Recent_Messages.Charging and then Current_Charge >= 0.75 then
               Update_Charging (False);

               -- go back to orbit
               Orbiting (Throttle => 1.0,
                         Tick     => 100.0);
--                 Report ("back to orbit.");

               -- wait a little while to allow this ship to go back to orbit,
               -- so that message could be received by the ships on orbiting,
               -- not only the ships near globe.
--                 delay 0.1;
--                 -- tells other ships I've finished charging.
--                 Send (Recent_Messages); -- no guarantee to be received?
--
--                 delay 0.1;
--                 -- sends message again to let more ships know I've finished charging.
--                 Send (Recent_Messages); -- no guarantee to be received?

               -- sends message without blocking Vehicle_Task.
               -- this allows to have full control of the vehicle,
               -- i.e. it can avoid the ship being off the orbit.
               declare
                  Message_Sender_Instance : constant Message_Sender_Pt := new Message_Sender;
               begin
                  Message_Sender_Instance.all.Async_Send (Incomming_Message_In => Recent_Messages,
                                                          Send_Interval_In     => 0.1,
                                                          Send_Count_In        => 3);
               end;

            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;

--  Vectors_3D."abs" (Vector_1 - Vector_2) -- calculates distance between two Points_3D
