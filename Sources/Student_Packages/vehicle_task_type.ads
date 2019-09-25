--
-- Danny (u6611178), Australia, September 2019
--

with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Task_Identification; use Ada.Task_Identification;
with Swarm_Structures_Base; use Swarm_Structures_Base;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
package Vehicle_Task_Type is
   START_TIME : constant Time := Clock;
   type Messages_Arr_Index_Type is mod 99;
   type Messages_Arr is array (Messages_Arr_Index_Type) of Inter_Vehicle_Messages;

    type Globe_Pos_I_Know is record
         Pos : Positions := (0.0, 0.0, 0.0);
         Pos_Time : Time;
    end record;

   task type Vehicle_Task is
      entry Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id);
   end Vehicle_Task;

         package Positive_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Positive);
      use Positive_Sets;

   procedure Init_Location (Vehicle_No : Positive; Centre_Pos : Positions; Fast : Boolean);
   procedure Optimised_Send (Message_To_Send : Inter_Vehicle_Messages; Buffer_Arr : in out Messages_Arr; Arr_Index : in out Messages_Arr_Index_Type; Veh_ID : Positive; Debug_Info : String);
   procedure Update_MyGlobes (My_Globe : in out Globe_Pos_I_Know; Found_Double_Globe : in out Boolean);
   procedure Leader_Message_Handler (Poll_Target : Positive; My_Globe : in out Globe_Pos_I_Know; Found_Double_Globe : in out Boolean; Temp_Message : in out Inter_Vehicle_Messages; My_Message : in out Inter_Vehicle_Messages;  Survivors : in out Positive_Sets.Set; Local_Message_Buffer : in out Messages_Arr; Mes_Arr_Index : in out Messages_Arr_Index_Type; Vehicle_No : Positive);
end Vehicle_Task_Type;
