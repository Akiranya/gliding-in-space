-- source: https://stackoverflow.com/questions/30071270/how-to-print-ada-real-time-time-variable

with Ada.Real_Time; use Ada.Real_Time;

package Real_Time_IO is
   function Since_Start return Time_Span;
   function Since_Start return Duration;
   function Since_Start return String;
private
   Epoch : constant Time := Clock;
end Real_Time_IO;
