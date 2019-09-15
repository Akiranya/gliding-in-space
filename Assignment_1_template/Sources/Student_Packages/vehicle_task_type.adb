-- Suggestions for packages which might be useful:

with Ada.Numerics;               use Ada.Numerics;
with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vectors_3D;                 use Vectors_3D;
with Vehicle_Interface;          use Vehicle_Interface;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
--  with Swarm_Structures;           use Swarm_Structures;
with Swarm_Structures_Base;      use Swarm_Structures_Base;
--  with Real_Time_IO; use Real_Time_IO;

package body Vehicle_Task_Type is

   task body Vehicle_Task is

      Vehicle_No : Positive;

      ----------------
      -- local storage:
      ----------------

      Recent_Messages : Inter_Vehicle_Messages; -- local message
      Local_Charging : Boolean := False;

      ----------------
      -- orbit parameters:
      ----------------

      Time : Real := 0.0; -- time, constantly increasing while the game is running, for drawing circle
      Orbit : Vector_3D; -- the orbit where ships fly along

      ----------------
      -- funcs/procs:
      ----------------

      -- helper function: gets a element from array
      -- p.s. from the time being, just grabs the first element in this array,
      -- it might be expanded when i have new good idea.
      function Grab_A_Globe (Globes : Energy_Globes) return Energy_Globe is (Globes (1));

      -- helper function: checks if there's any globe nearby.
      function Has_Energy_Nearby (Globes : Energy_Globes) return Boolean is (Globes'Length > 0);

      -- helper function: updates both charging variables.
      procedure Update_Charging_States (State : Boolean) is
      begin
         Local_Charging := State;
         Recent_Messages.Charging := State;
      end Update_Charging_States;

      -- let the ship fly along an orbit! this helps message spread out!
      -- p.s. info needed for calculating is Recent_Message which
      -- is the most recent message received by this ship.
      procedure Orbiting (Throttle : Real; Radius : Real) is
         Tick_Per_Update : constant Real := 32.0; -- orbiting speed. **greater means slower**
      begin
         Orbit := (x => Real_Elementary_Functions.Cos (Time),
                   y => Real_Elementary_Functions.Sin (Time),
                   z => 0.0);
         Orbit := Orbit * Radius; -- Orbit := (Radius * Cos (Time), R * Sin (Time), 0)
         Orbit := Orbit + Recent_Messages.Globe.Position; -- sets orbiting origin
         Orbit := Orbit + Recent_Messages.Globe.Velocity; -- adds velocity to generate more roboust orbit track
         Time := Time + Pi / Tick_Per_Update; -- increment Time for next calculation
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
         ----------------
         -- initializes:
         ----------------
         Recent_Messages := (Sender => Vehicle_No,
                             Globe => (Zero_Vector_3D, Zero_Vector_3D),
                             Charging => False);
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

            ----------------
            -- send message if found the globe(s):
            ----------------

            if Has_Energy_Nearby (Energy_Globes_Around) then
               declare
                  Lucky_Globe : constant Energy_Globe := Grab_A_Globe (Energy_Globes_Around);
                  Lucky_Info : constant Inter_Vehicle_Messages := (Sender => Vehicle_No, -- to be used?
                                                                   Globe => Lucky_Globe,
                                                                   Charging => False); -- don't update
               begin
                  Recent_Messages := Lucky_Info;
                  Send (Recent_Messages);
               end;
            end if;

            ----------------
            -- try to receive message:
            ----------------

            -- if this ship receives a message,
            -- it should then spread this message to its nearby ships.
            if Messages_Waiting then
               declare
                  Incomming_Message : Inter_Vehicle_Messages;
                  -- Incomming_Message is here to decide if the ship
                  -- should update (partial) local rencent message (and send it out).
               begin
                  Receive (Incomming_Message);

                  -- TODO: calculates no. of vehicles existent

                  Recent_Messages := Incomming_Message; -- updates all local info
                  Recent_Messages.Sender := Vehicle_No; -- sends this ship no. out.

                  Send (Recent_Messages); -- spread incomming message to nearby ships.
               end;
            end if;

            ----------------
            -- normal orbiting:
            ----------------

            -- if this ship is not going to charge, then
            -- let it orbit around the globe.
            if not Local_Charging then
               Orbiting (Throttle => 0.6,
                         Radius   => 0.25);
            end if;

            -----------------
            -- try to charge:
            -----------------

            -- (not Recent_Messages.Charging) indicates that
            -- this ship has not received any charging request yet,
            -- which means that there won't be many of ships, nearby this ship,
            -- also intending to charge.

            -- that is, this avoid too many ships competing for globes.
            if Current_Charge < 0.8 and then not Recent_Messages.Charging then
               Update_Charging_States (True);
               Send (Recent_Messages); -- tells other ships i'm going to charge.
                  -- TODO: go to different globe if too many charging nearby
               Set_Destination (Recent_Messages.Globe.Position);
               Set_Throttle (1.0);
            end if;

            -----------------
            -- after recharging, go back to orbit:
            -----------------

            -- p.s. to figure out what conditions exactly represent 'finished charging',
            -- we can use Current_Charge and Local_Charging flag.

            -- if local charging flag is True, it means that this ship *was* going to charge,
            -- and Current_Charge >= 0.75 means that it *now* resumes its energy.
            if Current_Charge >= 0.75 and then Local_Charging then
               Update_Charging_States (False);
               Orbiting (Throttle => 1.0,
                         Radius   => 0.25); -- go back to orbit by using *local* globe info
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;

--  Vectors_3D."abs" (Point_1 - Point_2) -- calculates distance between two points
