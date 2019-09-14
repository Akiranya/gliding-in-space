package body Real_Time_IO is

   function Since_Start return Time_Span is
   begin
      return Clock - Epoch;
   end Since_Start;

   function Since_Start return Duration is
   begin
      return To_Duration (Since_Start);
   end Since_Start;

   function Since_Start return String is
   begin
      return Duration'Image (Since_Start);
   end Since_Start;

end Real_Time_IO;
