with Ada.Task_Identification; use Ada.Task_Identification;
with Swarm_Structures_Base; use Swarm_Structures_Base;

package Vehicle_Task_Type is

   task type Vehicle_Task is
      entry Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id);
   end Vehicle_Task;

   procedure Gohome (Vehicle_No : Positive; Centre_Pos : Positions);
end Vehicle_Task_Type;
