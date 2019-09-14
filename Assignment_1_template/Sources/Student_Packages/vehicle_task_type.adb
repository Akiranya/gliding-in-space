-- Suggestions for packages which might be useful:

with Ada.Numerics;               use Ada.Numerics;
--  with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vectors_3D;                 use Vectors_3D;
with Vehicle_Interface;          use Vehicle_Interface;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
--  with Swarm_Structures;           use Swarm_Structures;
with Swarm_Structures_Base;      use Swarm_Structures_Base;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Ordered_Sets;
--  with Real_Time_IO; use Real_Time_IO;

package body Vehicle_Task_Type is

   task body Vehicle_Task is

      Vehicle_No : Positive;

      ----------------
      -- local storage:
      ----------------

      Recent_Messages : Inter_Vehicle_Messages; -- local message
      Local_Charging : Boolean := False;
      Locally_Known_No_Vehicle : Count_Type;

      package Vehicle_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Positive);
      use Vehicle_Sets;
      Known_Vehicles : Set;

      ----------------
      -- orbit parameters:
      ----------------

      Time : Real := 0.0; -- time, constantly increasing while the game is running, for drawing circle
      Orbit : Vector_3D; -- the orbit where ships fly along

      ----------------
      -- funcs/procs:
      ----------------

      -- helper function: gets a element from array
      -- p.s. grabs the first element in this array from the time being,
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

      -- let the ship fly along a orbit!
      -- p.s. info needed for calculating is Recent_Message which
      -- is the most recent message received by this ship.
      procedure Orbiting (Throttle : Real; Radius : Real) is
         Tick_Per_Update : constant Real := 96.0; -- orbiting speed. **greater means slower**
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

      procedure Smart_Orbiting (Throttle : Real; Vehicle_Size : Count_Type) is
      begin
         case Vehicle_Size is
            when 1 .. 64 => Orbiting (Throttle, 0.2);
            when 65 .. 80 => Orbiting (Throttle, 0.25);
            when 81 .. 96 => Orbiting (Throttle, 0.30);
            when 97 .. 112 => Orbiting (Throttle, 0.35);
            when 113 .. 128 => Orbiting (Throttle, 0.40);
            when others => Orbiting (Throttle, 0.45);
         end case;
      end Smart_Orbiting;

--        procedure Report (Info : String) is
--        begin
--           Put_Line (Vehicle_No'Image & " " & Info);
--        end Report;

   begin

      -- You need to react to this call and provide your task_id.
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;
         Known_Vehicles.Insert (Vehicle_No); -- adds itself to set

         ----------------
         -- initializes:
         ----------------
         Recent_Messages := (Source_ID => 999, -- non-existent no, as placeholder
                             Forwarder_ID => Vehicle_No,
                             Globe => (Zero_Vector_3D, Zero_Vector_3D),
                             Charging => False,
                             Known_No_Vehicle => 1);
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

            ----------------
            -- send message if found the globe(s):
            ----------------

            if Has_Energy_Nearby (Energy_Globes_Around) then
               declare
                  Lucky_Globe : constant Energy_Globe := Grab_A_Globe (Energy_Globes_Around);
                  Lucky_Info : constant Inter_Vehicle_Messages := (Source_ID => Vehicle_No, -- to be used?
                                                                   Forwarder_ID => Vehicle_No,
                                                                   Globe => Lucky_Globe,
                                                                   Charging => False,
                                                                   Known_No_Vehicle => Recent_Messages.Known_No_Vehicle); -- don't update
               begin
                  Recent_Messages := Lucky_Info;
                  Send (Recent_Messages);
--                    Report ("globe nearby.");
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
                  Max_No_Vehicles : Count_Type;
                  -- Incomming_Message is here to decide if the ship
                  -- should update (partial) local rencent message (and send it out).
               begin
                  Receive (Incomming_Message);

                  -- TODO: calculates centroid of polygon by using all info got

                  ----------------
                  -- calculates no. of vehicles:
                  ----------------

                  Known_Vehicles.Include (Incomming_Message.Forwarder_ID); -- *tries* to add vehicle
                  Locally_Known_No_Vehicle := Known_Vehicles.Length;
                  Max_No_Vehicles := Count_Type'Max (Incomming_Message.Known_No_Vehicle, Locally_Known_No_Vehicle);
                  Max_No_Vehicles := Count_Type'Max (Recent_Messages.Known_No_Vehicle, Max_No_Vehicles);
                  Recent_Messages := Incomming_Message; -- updates all local info
                  Recent_Messages.Known_No_Vehicle := Max_No_Vehicles; -- replaces it with longer length.
                  Recent_Messages.Forwarder_ID := Vehicle_No; -- sends this ship no. out.
--                    if Max_No_Vehicles > 48 then
--                       Report ("known no. of vehicles: " & Recent_Messages.Length'Image);
--                    end if;

                  ----------------
                  -- spreads message out:
                  ----------------

                  Send (Recent_Messages); -- spread incomming message to nearby ships.
--                    Report ("incomming new message. source: " & Recent_Messages.Source_ID'Image);
               end;
            end if;

            ----------------
            -- normal orbiting:
            ----------------

            -- if this ship is not going to charge, then
            -- let it orbit around the globe.
            if not Local_Charging then
               Smart_Orbiting (Throttle     => 0.5,
                               Vehicle_Size => Locally_Known_No_Vehicle);
            end if;

            -----------------
            -- try to charge:
            -----------------

            -- (not Recent_Messages.Charging) indicates that
            -- this ship has not received any charging request yet,
            -- which means that there won't be many of ships, nearby this ship,
            -- also intending to charge.

            -- that is, this avoid too many ships competing for globes.
            if Current_Charge < 0.75 and then not Recent_Messages.Charging then
               Update_Charging_States (True);
               Send (Recent_Messages); -- tells other ships i'm going to charge.

               Set_Destination (Recent_Messages.Globe.Position);
               Set_Throttle (1.0); -- as faster as possible
--                 Report ("charging!");
            end if;

            -----------------
            -- after recharging, go back to orbit:
            -----------------

            -- if the ship has finished charging ...

            -- to figure out what conditions exactly represent 'finished charging',
            -- we can use Current_Charge and Local_Charging flag.

            -- if local charging flag is True, it means that this ship WAS going to charge,
            -- and Current_Charge >= 0.75 means that it now resumes its spirits.
            if Current_Charge >= 0.75 and then Local_Charging then
               Update_Charging_States (False);
               Smart_Orbiting (Throttle     => 1.0,
                               Vehicle_Size => Locally_Known_No_Vehicle); -- go back to orbit by using *local* globe info
--                 Report ("back to orbit.");
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;

--  Vectors_3D."abs" (Point_1 - Point_2) -- calculates distance between two points
