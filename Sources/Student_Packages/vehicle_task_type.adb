-- Suggestions for packages which might be useful:

--  with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
--  with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vehicle_Interface;          use Vehicle_Interface;
--  with Vehicle_Message_Type;       use Vehicle_Message_Type;
--  with Swarm_Structures;           use Swarm_Structures;
with Coordinator;                 use Coordinator;
with Swarm_Structures_Base; use Swarm_Structures_Base;
with Vectors_3D; use Vectors_3D;
package body Vehicle_Task_Type is

   task body Vehicle_Task is

      Vehicle_No : Positive;
      -- You will want to take the pragma out, once you use the "Vehicle_No"

   begin

      -- You need to react to this call and provide your task_id.
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;
      end Identify;

      -- Replace the rest of this task with your own code.
      -- Maybe synchronizing on an external event clock like "Wait_For_Next_Physics_Update",
      -- yet you can synchronize on e.g. the real-time clock as well.

      -- Without control this vehicle will go for its natural swarming instinct.

      select

         Flight_Termination.Stop;
      then abort

         Outer_task_loop : loop

            Wait_For_Next_Physics_Update;

            -- Your vehicle should respond to the world here: sense, listen, talk, act?

               if Energy_Globes_Around'Length /= 0 then
                  Put_Line ("Setting Globe");
                  Master.reportGlobe (Globe => Energy_Globes_Around (1).Position);
               end if;

               while Master.getGlobeLocationSet and then Vehicle_No = 1 loop
                  Set_Destination (Master.getGlobeLocation);
                  Set_Throttle (1.0);
                  delay 0.3;
                  if Energy_Globes_Around'Length /= 0 then
                     Master.reportGlobe (Globe => Energy_Globes_Around (1).Position);
                  end if;
               end loop;

               Gohome (Vehicle_No);

               if Current_Charge < 0.5 then
                  --                    Master.goCharge(Master.getGlobeLocation);
                  Set_Destination (Master.getGlobeLocation);
                  Set_Throttle (1.0);
                  Put_Line (Integer'Image (Vehicle_No) & " is going to charge");
                  if Energy_Globes_Around'Length /= 0 then
                     Put_Line ("Setting Globe");
                     Master.reportGlobe (Globe => Energy_Globes_Around (1).Position);
                     Gohome (Vehicle_No);
                     Put_Line (Integer'Image (Vehicle_No) & " is comming back");
                  end if;
               end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

   procedure Gohome (Vehicle_No : Positive) is
      Group      : constant Positive := Vehicle_No mod 8 + 1;
   begin
      case Group is
         when 1 =>
            Set_Destination ((Master.getGlobeLocation (x), 0.2 + Master.getGlobeLocation (y), Master.getGlobeLocation (z)));
         when 2 =>
            Set_Destination ((Master.getGlobeLocation (x), 0.4 + Master.getGlobeLocation (y), Master.getGlobeLocation (z)));
         when 3 =>
            Set_Destination ((Master.getGlobeLocation (x) + 0.2, Master.getGlobeLocation (y), Master.getGlobeLocation (z)));
         when 4 =>
            Set_Destination ((Master.getGlobeLocation (x) + 0.4, Master.getGlobeLocation (y), Master.getGlobeLocation (z)));
         when 5 =>
            Set_Destination ((Master.getGlobeLocation (x), Master.getGlobeLocation (y) - 0.2, Master.getGlobeLocation (z)));
         when 6 =>
            Set_Destination ((Master.getGlobeLocation (x), Master.getGlobeLocation (y) - 0.4, Master.getGlobeLocation (z)));
         when 7 =>
            Set_Destination ((Master.getGlobeLocation (x) - 0.2, Master.getGlobeLocation (y), Master.getGlobeLocation (z)));
         when 8 =>
            Set_Destination ((Master.getGlobeLocation (x) - 0.4, Master.getGlobeLocation (y), Master.getGlobeLocation (z)));
         when others =>
            null;
      end case;
      Set_Throttle (0.5);
   end Gohome;

end Vehicle_Task_Type;
