with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Task_Identification; use Ada.Task_Identification;
with Swarm_Structures_Base; use Swarm_Structures_Base;
with Vehicle_Message_Type;       use Vehicle_Message_Type;

package Vehicle_Task_Type is
   START_TIME : constant Time := Clock;
   type Messages_Arr_Index_Type is mod 99;
   type Messages_Arr is array (Messages_Arr_Index_Type) of Inter_Vehicle_Messages;

   task type Vehicle_Task is
      entry Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id);
   end Vehicle_Task;

   procedure Init_Location (Vehicle_No : Positive; Centre_Pos : Positions; Fast : Boolean);
   procedure Optimised_Send (Message_To_Send : Inter_Vehicle_Messages; Buffer_Arr : in out Messages_Arr; Arr_Index : in out Messages_Arr_Index_Type; Veh_ID : Positive);

end Vehicle_Task_Type;
