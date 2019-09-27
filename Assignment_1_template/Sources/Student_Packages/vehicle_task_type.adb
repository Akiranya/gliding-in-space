with Ada.Numerics;               use Ada.Numerics;
with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
with Real_Type;                  use Real_Type;
with Vectors_3D;                 use Vectors_3D;
with Vehicle_Interface;          use Vehicle_Interface;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
with Swarm_Structures_Base;      use Swarm_Structures_Base;
with Utils;                      use Utils;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Ordered_Sets;

package body Vehicle_Task_Type is

   task body Vehicle_Task is

      Vehicle_No : Positive;

      --------------------------------
      -- config:
      --------------------------------

      Destruction : constant Boolean := True; -- enable stage D mode?

      --------------------------------
      -- local storage:
      --------------------------------

      Last_Msg       : Inter_Vehicle_Messages;
      Local_Charging : Boolean := False;

      package Swarm_Element_Index_Ordered_Sets is
        new Ada.Containers.Ordered_Sets (Element_Type => Swarm_Element_Index);
      use Swarm_Element_Index_Ordered_Sets;
      Reserved_Vehicles : Swarm_Element_Index_Ordered_Sets.Set;

      --------------------------------
      -- orbit parameters:
      --------------------------------

      T : Real := 0.0; -- time, constantly increasing while the game is running, for drawing circle

      --------------------------------
      -- helper funcs/procs:
      --------------------------------

      procedure Orbiting (Throttle : Throttle_T) is
         use Real_Elementary_Functions;
         T_Inrcement : constant Real      := 64.0; -- radian increment. **greater means this ship spins slower**
         R           : constant Distances := 0.15; -- orbit radius, will be expanded if more ships are found
         Orbit       : Vector_3D;                  -- the orbit where ships fly along
      begin
         Orbit := (x => Cos (T), y => Sin (T), z => 0.0);
         Orbit := Orbit * R;                       -- a point on circle: (r*cos(t), r*sin(t), 0)
         Orbit := Orbit + Last_Msg.Globe.Position; -- sets orbiting origin, now the point is (a+r*cos(t), b+r*sin(t), c+0)
         Orbit := Orbit + Last_Msg.Globe.Velocity; -- adds velocity to generate more roboust orbit track.
         T     := T + Pi / T_Inrcement;
         Set_Destination (Orbit); Set_Throttle (Throttle);
      end Orbiting;

      procedure Report (Info : String) is
      begin
         Put_Line (Vehicle_No'Image & " " & Info);
      end Report;

   begin
      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No    := Set_Vehicle_No;
         Local_Task_Id := Current_Task;
         -- initializes Last_Msg to avoid 'read-before-write' exception
         Last_Msg := (Sender          => Vehicle_No,
                      Globe           => (Position => Zero_Vector_3D, Velocity => Zero_Vector_3D),
                      Charging        => False,
                      Leader          => Vehicle_No,     -- at the beginning, every vehicle think itself is leader.
                      Target_Vanished => Positive'Last); -- Positive'Last is a placeholder.
      end Identify;

      Reserved_Vehicles.Insert (Vehicle_No); -- adds itself to set.

      select

         Flight_Termination.Stop;

      then abort

         Outer_task_loop : loop

            Wait_For_Next_Physics_Update;

            --------------------------------
            -- send message if found the globe(s):
            --------------------------------

            if Has_Energy_Nearby (Energy_Globes_Around) then
               declare
                  Lucky_Globe  : constant Energy_Globe           := Grab_A_Globe (Energy_Globes_Around);
                  Outgoing_Msg : constant Inter_Vehicle_Messages := (Sender          => Vehicle_No,
                                                                     Globe           => Lucky_Globe,
                                                                     Charging        => False,
                                                                     Leader          => Last_Msg.Leader,
                                                                     Target_Vanished => Last_Msg.Target_Vanished);
               begin
                  Last_Msg := Outgoing_Msg; Send (Last_Msg);
               end;
            end if;

            --------------------------------
            -- try to receive & forward messages:
            --------------------------------

            if Messages_Waiting then
               declare
                  Incoming_Msg : Inter_Vehicle_Messages;
               begin
                  Receive (Incoming_Msg);
                  Last_Msg := Incoming_Msg; -- replaces all local messages with incomming messages

                  if Last_Msg.Leader = Vehicle_No then

                     -- accumulates reserved vehicles until reaching Target_No_of_Elements.
                     if Reserved_Vehicles.Length < Count_Type (Target_No_of_Elements) then
                        Reserved_Vehicles.Include (Last_Msg.Sender);
                     end if;

                     --------------------------------
                     -- writes messages that kill ships:
                     --------------------------------

                     if Reserved_Vehicles.Length >= Count_Type (Target_No_of_Elements) and then Destruction then
                        if not Reserved_Vehicles.Contains (Last_Msg.Sender) then
                           Last_Msg.Target_Vanished := Last_Msg.Sender;
                           Report ("destruction target:" & Last_Msg.Target_Vanished'Image);
                        end if;
                     end if;

                  end if;

                  Last_Msg.Sender := Vehicle_No; -- attach this Vehicle_No to outgoing messages
                  Send (Last_Msg);               -- spreads *modified* incomming messages to nearby ships.
               end;
            end if;

            --------------------------------
            -- decides whether to vanish itself or not:
            --------------------------------

            if Vehicle_No = Last_Msg.Target_Vanished then
               Report ("vanished."); exit Outer_task_loop;
            end if;

            --------------------------------
            -- normal orbiting:
            --------------------------------

            if not Local_Charging then
               Orbiting (Throttle => Full_Throttle * 0.5);
            end if;

            ---------------------------------
            -- try to charge:
            ---------------------------------

            if Current_Charge < Full_Charge * 0.75 and then not Last_Msg.Charging then
               Last_Msg.Charging := True;
               Local_Charging    := True;
               Send (Last_Msg); -- tells other ships i'm going to charge.
               Set_Destination (Last_Msg.Globe.Position); Set_Throttle (Full_Throttle);
            end if;

            ---------------------------------
            -- after recharging, go back to orbit:
            ---------------------------------

            if Current_Charge >= Full_Charge * 0.9 and then Local_Charging then
               Last_Msg.Charging := False;
               Local_Charging    := False;
               Orbiting (Throttle => Full_Throttle); -- go back to orbit by using *local* globe info.
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;
