package body Utils is

   function Grab_A_Globe (Globes : Energy_Globes) return Energy_Globe is (Globes (Positive'First));
   function Has_Energy_Nearby (Globes : Energy_Globes) return Boolean is (Globes'Length > 0);

end Utils;
