with Swarm_Structures_Base; use Swarm_Structures_Base;

package Utils is

   -- overview: gets an energy_globe from array energy_globes.
   function Grab_A_Globe (Globes : Energy_Globes) return Energy_Globe;

   -- overview: checks if there's any globe nearby.
   function Has_Energy_Nearby (Globes : Energy_Globes) return Boolean;

end Utils;
