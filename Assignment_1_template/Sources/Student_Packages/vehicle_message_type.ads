-- Suggestions for packages which might be useful:

--  with Ada.Real_Time;         use Ada.Real_Time;
with Vectors_3D;            use Vectors_3D;
with Swarm_Structures_Base;

package Vehicle_Message_Type is

   -- Replace this record definition by what your vehicles need to communicate.

   type Inter_Vehicle_Messages is record
      Source_ID : Positive; -- who found the globe
      Forwarder_ID : Positive; -- who sent the message
      Globe_Loc : Swarm_Structures_Base.Positions;
      Globe_Vel : Swarm_Structures_Base.Velocities;
      Charging : Boolean;
   end record;

end Vehicle_Message_Type;
