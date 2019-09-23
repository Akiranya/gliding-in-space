with Swarm_Structures_Base; use Swarm_Structures_Base;

package Utils is

   -- overview: gets a element from array.
   -- p.s. from the time being, just grabs the first element in this array,
   -- it might be expanded when i have new good idea.
   function Grab_A_Globe (Globes : Energy_Globes) return Energy_Globe;

   -- overview: checks if there's any globe nearby.
   function Has_Energy_Nearby (Globes : Energy_Globes) return Boolean;

end Utils;
