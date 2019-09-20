-- Suggestions for packages which might be useful:

with Ada.Real_Time;         use Ada.Real_Time;
with Swarm_Structures_Base; use Swarm_Structures_Base;

package Vehicle_Message_Type is

   -- Replace this record definition by what your vehicles need to communicate.

   type Inter_Vehicle_Messages is record
      Sender : Positive; -- who sent the message
      Globe : Energy_Globe;
      Charging : Boolean; -- whether the sender is going to charge
      Timestamp : Time;
   end record;

end Vehicle_Message_Type;
