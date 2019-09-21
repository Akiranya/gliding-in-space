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
--  with Ada.Real_Time; use Ada.Real_Time;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;

package body Vehicle_Task_Type is

   task body Vehicle_Task is

      Vehicle_No : Positive;

      ----------------
      -- local storage:
      ----------------

      Last_Msg : Inter_Vehicle_Messages;
      Local_Charging : Boolean := False;

      package Vehicle_No_Set is new Ada.Containers.Ordered_Sets (Element_Type => Positive);
      use Vehicle_No_Set;
      Reserved_Vehicles : Vehicle_No_Set.Set;
      Vanished_Vehicles : Vehicle_No_Set.Set;

      ----------------
      -- orbit parameters:
      ----------------

      T : Real := 0.0; -- time, constantly increasing while the game is running, for drawing circle
      Orbit : Vector_3D; -- the orbit where ships fly along

      ----------------
      -- helper funcs/procs:
      ----------------

      -- overview: gets a element from array
      -- p.s. from the time being, just grabs the first element in this array,
      -- it might be expanded when i have new good idea.
      function Grab_A_Globe (Globes : Energy_Globes) return Energy_Globe is (Globes (1));

      -- overview: checks if there's any globe nearby.
      function Has_Energy_Nearby (Globes : Energy_Globes) return Boolean is (Globes'Length > 0);

      -- overview: let the ship fly along an orbit! this helps message spread out!
      -- p.s. info needed for calculating orbit is Recent_Message which
      -- is the *most* recent message received by this ship.
      procedure Orbiting (Throttle : Real; Radius : Real) is
         Tick_Per_Update : constant Real := 64.0; -- orbiting speed. **greater means slower**
      begin
         Orbit := (x => Real_Elementary_Functions.Cos (T),
                   y => Real_Elementary_Functions.Sin (T),
                   z => 0.0);
         Orbit := Orbit * Radius; -- a point on circle: (r*cos(t), r*sin(t), 0)
         Orbit := Orbit + Last_Msg.Globe.Position; -- sets orbiting origin.
         Orbit := Orbit + Last_Msg.Globe.Velocity; -- adds velocity to generate more roboust orbit track.

         T := T + Pi / Tick_Per_Update;

         Set_Destination (Orbit);
         Set_Throttle (Throttle);
      end Orbiting;

      procedure Report (Info : String) is
      begin
         Put_Line (Vehicle_No'Image & " " & Info);
      end Report;

   begin
      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;

         -- initializes Local_Msg & Last_Msg to avoid 'read-before-write' exception
         Last_Msg := (Sender => Vehicle_No,
                      Globe => (Position => Zero_Vector_3D,
                                Velocity => Zero_Vector_3D),
                      -- following three variables just copy Local_Msg
                      Charging => False,
                      Commander => Vehicle_No,
                      Target_Vanished => Positive'Last); -- Positive'Last is a 'null value'
      end Identify;

      Reserved_Vehicles.Insert (Vehicle_No); -- initializes the set

      select

         Flight_Termination.Stop;

      then abort

         Outer_task_loop : loop

            Wait_For_Next_Physics_Update;

            ----------------
            -- send message if found the globe(s):
            ----------------

            if Has_Energy_Nearby (Energy_Globes_Around) then
               declare
                  Lucky_Globe : constant Energy_Globe := Grab_A_Globe (Energy_Globes_Around);
                  Outgoing_Msg : constant Inter_Vehicle_Messages := (Sender => Vehicle_No,
                                                                     Globe => Lucky_Globe,
                                                                     Charging => False,
                                                                     Commander => Last_Msg.Commander,
                                                                     Target_Vanished => Last_Msg.Target_Vanished);
               begin
                  Last_Msg := Outgoing_Msg;
                  Send (Last_Msg);
               end;
            end if;

            ----------------
            -- try to receive message:
            ----------------

            -- if this ship receives a message,
            -- it should then spread this message to its nearby ships.

            if Messages_Waiting then
               declare
                  Incomming_Msg : Inter_Vehicle_Messages;
                  -- Incomming_Message is here to decide if the ship
                  -- should update (partial) local rencent message (and send it out).
               begin
                  Receive (Incomming_Msg);

                  -- TODO: decided whether to vanish itself

                  Last_Msg := Incomming_Msg; -- replaces all local messages with incomming messages

                  -- accumulates reserved ships until Target_No_of_Elements
                  if Reserved_Vehicles.Length < Count_Type (Target_No_of_Elements) then
                     Reserved_Vehicles.Include (Last_Msg.Sender);
                  else
                     -- accumulates the vehicles which will be asked to vanished.
                     if not Reserved_Vehicles.Contains (Last_Msg.Sender) then
                        Vanished_Vehicles.Include (Last_Msg.Sender);
                     end if;
                  end if;

--                    Report ("agrees on:" & Last_Msg.Commander'Image);

                  ----------------
                  -- TODO: select the commander & kill ships
                  ----------------

                  -- fact: the ship which first finds the globe is the commander!
                  if Vanished_Vehicles.Length > 0 and then Last_Msg.Commander = Vehicle_No
                  then
                     -- TODO: coordinate other vehicles to vanish.
                     declare
                        Lucky_Ship : constant Positive := Vanished_Vehicles.First_Element;
                     begin
                        Vanished_Vehicles.Exclude (Lucky_Ship);
                        Last_Msg.Target_Vanished := Lucky_Ship;

                        Report ("destruction starts:" & Lucky_Ship'Image);
                     end;
                  end if;

                  Last_Msg.Sender := Vehicle_No; -- attach this Vehicle_No to outgoing messages
                  Send (Last_Msg); -- spreads *modified* incomming messages to nearby ships.
               end;
            end if;

            if Vehicle_No = Last_Msg.Target_Vanished then
               Report ("vanished.");
               exit Outer_task_loop;
            end if;

            ----------------
            -- normal orbiting:
            ----------------

            -- if this ship is not going to charge, then
            -- let it orbit around the globe.
            if not Local_Charging then
               Orbiting (Throttle => Full_Throttle * 0.5,
                         Radius   => 0.4);
            end if;

            -----------------
            -- try to charge:
            -----------------

            -- (not Recent_Messages.Charging) indicates that
            -- this ship has not received any charging request yet,
            -- which means that there won't be many of ships, nearby this ship,
            -- also intending to charge.

            -- that is, this avoid too many ships competing for globes.

            if Current_Charge < Full_Charge * 0.75 and then not Last_Msg.Charging then
               Last_Msg.Charging := True;
               Local_Charging := True;
               Send (Last_Msg); -- tells other ships i'm going to charge.

               -- TODO: go to different globe if too many charging nearby.

               Set_Destination (Last_Msg.Globe.Position);
               Set_Throttle (Full_Throttle);
            end if;

            -----------------
            -- after recharging, go back to orbit:
            -----------------

            -- p.s. to figure out what conditions exactly represent 'finished charging',
            -- we can use Current_Charge and Local_Charging flag.

            -- if local charging flag is True, it means that this ship *was* going to charge,
            -- and Current_Charge >= 0.75 means that it *now* resumes its energy.

            if Current_Charge >= Full_Charge * 0.9 and then Local_Charging then
               Last_Msg.Charging := False;
               Local_Charging := False;
               Orbiting (Throttle => Full_Throttle,
                         Radius   => 0.4); -- go back to orbit by using *local* globe info.
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;

--  Vectors_3D."abs" (Point_1 - Point_2) -- calculates distance between two points
