with Swarm_Structures_Base; use Swarm_Structures_Base;

package Vehicle_Message_Type is

   type Inter_Vehicle_Messages is record
      Sender          : Swarm_Element_Index; -- who sent the message
      Globe           : Energy_Globe;
      Charging        : Boolean;             -- whether the sender is going to charge
      Leader          : Swarm_Element_Index; -- the ship which guides all other vehicles to vanish
      Target_Vanished : Swarm_Element_Index; -- who is asked to vanish
   end record;

end Vehicle_Message_Type;
