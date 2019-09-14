-- Suggestions for packages which might be useful:

--  with Ada.Real_Time;         use Ada.Real_Time;
with Swarm_Structures_Base; use Swarm_Structures_Base;
with Ada.Containers; use Ada.Containers;

package Vehicle_Message_Type is

   -- Replace this record definition by what your vehicles need to communicate.

   type Inter_Vehicle_Messages is record
      Source_ID : Positive; -- who found the globe
      Forwarder_ID : Positive; -- who sent the message
      Globe : Energy_Globe;
      Charging : Boolean; -- whether the sender is going to charge
      Known_No_Vehicle : Count_Type; -- no. of vehicles found
   end record;

end Vehicle_Message_Type;
