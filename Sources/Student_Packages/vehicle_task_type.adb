-- Suggestions for packages which might be useful:

with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
--  with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vehicle_Interface;          use Vehicle_Interface;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
--  with Swarm_Structures;           use Swarm_Structures;
with Swarm_Structures_Base; use Swarm_Structures_Base;
with Vectors_3D; use Vectors_3D;
package body Vehicle_Task_Type is

   task body Vehicle_Task is

      Vehicle_No : Positive;
      My_Message : Inter_Vehicle_Messages; -- Make sure this is the latest
      Temp_Message : Inter_Vehicle_Messages;
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

            if Energy_Globes_Around'Length /= 0 then -- Find globes, send message
--             Put_Line ("Setting Globe");
               My_Message := (Sender => Vehicle_No, Message_Time => Clock, Globe_Positions => Energy_Globes_Around (1).Position);
               Send (My_Message);
            end if;
            Message_loop :
            while Messages_Waiting loop -- Retrieve message
               -- First time getting message
               if My_Message.Sender = 6611178 then
                  Receive (My_Message);
                  Temp_Message := My_Message;
               else
                  Receive (Temp_Message);
                  if Temp_Message.Sender = 6611178 then -- Message from sender 1 has the highest priority
                     My_Message := Temp_Message;
                     while Messages_Waiting loop
                        Receive (Temp_Message); -- Empty the message buffer
                     end loop;
                     exit Message_loop;
                  elsif Temp_Message.Message_Time > My_Message.Message_Time then
                     My_Message := Temp_Message; -- Take the latest message
                  end if;
               end if;
            end loop Message_loop;

            while My_Message.Sender /= 6611178 and then Vehicle_No = 1 loop
               Set_Destination (My_Message.Globe_Positions);
               Set_Throttle (1.0); -- Mission for vehicle 1 is to always chase the globe
               delay 0.3;
               if Energy_Globes_Around'Length /= 0 then
                  My_Message := (Sender => Vehicle_No, Message_Time => Clock, Globe_Positions => Energy_Globes_Around (1).Position);
                  Send (My_Message);
               end if;
            end loop;

            if My_Message.Sender /= 6611178 and then Vehicle_No /= 1 then
               Gohome (Vehicle_No, My_Message.Globe_Positions);
            end if;

            if My_Message.Sender /= 6611178 then
               Send (My_Message);
            end if;

            if Current_Charge < 0.6 then
               if My_Message.Sender /= 6611178 then
                  Set_Destination (My_Message.Globe_Positions);
               end if;
               Set_Throttle (1.0);
--                Put_Line (Integer'Image (Vehicle_No) & " is going to charge");
               if Energy_Globes_Around'Length /= 0 then
                  Put_Line ("Setting Globe");
                  My_Message := (Sender => Vehicle_No, Message_Time => Clock, Globe_Positions => Energy_Globes_Around (1).Position);
                  Send (My_Message);
                  Gohome (Vehicle_No, My_Message.Globe_Positions);
--                    Put_Line (Integer'Image (Vehicle_No) & " is comming back");
               end if;
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

   procedure Gohome (Vehicle_No : Positive; Centre_Pos : Positions) is
      Group      : constant Positive := Vehicle_No mod 8 + 1;
   begin
      case Group is
         when 1 =>
            Set_Destination ((Centre_Pos (x), 0.2 + Centre_Pos (y), Centre_Pos (z)));
         when 2 =>
            Set_Destination ((Centre_Pos (x), 0.4 + Centre_Pos (y), Centre_Pos (z)));
         when 3 =>
            Set_Destination ((Centre_Pos (x) + 0.2, Centre_Pos (y), Centre_Pos (z)));
         when 4 =>
            Set_Destination ((Centre_Pos (x) + 0.4, Centre_Pos (y), Centre_Pos (z)));
         when 5 =>
            Set_Destination ((Centre_Pos (x), Centre_Pos (y) - 0.2, Centre_Pos (z)));
         when 6 =>
            Set_Destination ((Centre_Pos (x), Centre_Pos (y) - 0.4, Centre_Pos (z)));
         when 7 =>
            Set_Destination ((Centre_Pos (x) - 0.2, Centre_Pos (y), Centre_Pos (z)));
         when 8 =>
            Set_Destination ((Centre_Pos (x) - 0.4, Centre_Pos (y), Centre_Pos (z)));
         when others =>
            null;
      end case;
      Set_Throttle (0.5);
   end Gohome;

end Vehicle_Task_Type;
