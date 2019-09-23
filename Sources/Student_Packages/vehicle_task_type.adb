-- Suggestions for packages which might be useful:

with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
with Real_Type;                  use Real_Type;
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
      Group      : Positive;
      Start_Time : constant Time := Clock;
      type Messages_Arr_Index_Type is mod 9;
      type Messages_Arr is array (Messages_Arr_Index_Type) of Inter_Vehicle_Messages;
      Mes_Arr_Index : Messages_Arr_Index_Type := 0;
      Local_Message_Buffer : Messages_Arr; -- To prevent junk message that keeping sending each other like a loop
      My_Message : Inter_Vehicle_Messages; -- Make sure this is the latest
      Temp_Message : Inter_Vehicle_Messages;
      type Globe_Pos_I_Know is record
         Pos : Positions;
         Pos_Time : Time;
      end record;
      Globe_Pos : Globe_Pos_I_Know;

      Iam_Leader : Boolean := False;
      Group_ID : Positive := 6611178; -- don't have a group
      Head : Positive;
      Tail : Positive;
      Pre : Positive;
      -- You will want to take the pragma out, once you use the "Vehicle_No"

   begin

      -- You need to react to this call and provide your task_id.
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;
         Group          := Vehicle_No mod 4 + 1;
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

            -- First 10 Second
            --              if Real (to_Duration (Clock - Start_Time)) < 10.0 then
            if Energy_Globes_Around'Length /= 0 then
               Globe_Pos.Pos := Energy_Globes_Around (1).Position;
               Globe_Pos.Pos_Time := Clock;
               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => 1,
                              Target_Group => 6611178,
                              Manager_ID => 6611178,
                              Turn_Back => False,
                              Message_Time => Clock,
                              Predecessor_Positions => Position,
                              Globe_Update_Time => Globe_Pos.Pos_Time,
                              Globe_Positions => Globe_Pos.Pos);
               Send (My_Message);
            end if;
            Message_loop :
            while Messages_Waiting loop -- Retrieve message

               if My_Message.Original_Sender = 6611178 then
                  -- This task getting a message for the very first time.
                  Receive (Temp_Message);
                  My_Message := Temp_Message;
                  Globe_Pos.Pos := My_Message.Globe_Positions;
                  Globe_Pos.Pos_Time := My_Message.Globe_Update_Time;
                  if Temp_Message.Target_Receiver = Vehicle_No then
                       if Vehicle_No = 1 then -- Manager, go to the globe
                          Set_Destination (My_Message.Globe_Positions);
                          Set_Throttle (1.0);
                       end if;

                     --                          if Vehicle_No = 3 then
                     --                                   My_Message := (Original_Sender => Vehicle_No,
                     --                                   Target_Receiver => 1,
                     --                                   Target_Group => 6611178,
                     --                                   Manager_ID => 6611178,
                     --                                   Turn_Back => False,
                     --                                   Message_Time => Clock,
                     --                                   Predecessor_Positions => Position,
                     --                                   Globe_Update_Time => Globe_Pos.Pos_Time,
                     --                                                  Globe_Positions => Globe_Pos.Pos);
                     --                             end if;
                     null;
                  elsif Temp_Message.Target_Receiver /= Vehicle_No or else Temp_Message.Target_Receiver = 6611178 then
                     Local_Message_Buffer (Mes_Arr_Index) := Temp_Message;
                     Mes_Arr_Index := Mes_Arr_Index + 1;
                     Send (Temp_Message); -- forward this message
                  end if;

               else -- Not the very first time getting a message
                  Receive (Temp_Message);
                  -- No matter who the message is sendting to, I will steal its globe position if it is newer
                  if Temp_Message.Globe_Update_Time > Globe_Pos.Pos_Time then
                     Globe_Pos.Pos := Temp_Message.Globe_Positions;
                     Globe_Pos.Pos_Time := Temp_Message.Globe_Update_Time;
                  end if;

                  -- Check if this message is for me
                  if Temp_Message.Target_Receiver = Vehicle_No then
                     -- Check if this is the latest
                     if Temp_Message.Message_Time > My_Message.Message_Time then
                        My_Message := Temp_Message; -- Take the latest message
                          if Vehicle_No = 1 then -- Manager, go to the globe
                             Set_Destination (My_Message.Globe_Positions);
                             Set_Throttle (1.0);
                          end if;
                     end if;

                  else
                     -- Message for others

                     declare
                        Found : Boolean := False;
                     begin
                        for mess of Local_Message_Buffer loop
                           if mess = Temp_Message then
                              Found := True;
                           end if;
                        end loop;
                        if not Found then
                           Local_Message_Buffer (Mes_Arr_Index) := Temp_Message;
                           Mes_Arr_Index := Mes_Arr_Index + 1;
                           Send (Temp_Message); -- forward this message only when the message is not been sent before
                        end if;
                     end;
                  end if;

               end if;
            end loop Message_loop;
            --              end if;

            if My_Message.Original_Sender /= 6611178 and then Vehicle_No = 1 then
               Set_Destination ((Globe_Pos.Pos (x) + 0.01, Globe_Pos.Pos (y) + 0.01, Globe_Pos.Pos (z) + 0.01));
               Set_Throttle (1.0); -- Mission for vehicle 1 is to always chase the globe
               if Energy_Globes_Around'Length /= 0 then
                  Globe_Pos.Pos := Energy_Globes_Around (1).Position;
                  Globe_Pos.Pos_Time := Clock;
                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => 6611178,
                                 Target_Group => 6611178,
                                 Manager_ID => 6611178,
                                 Turn_Back => False,
                                 Message_Time => Clock,
                                 Predecessor_Positions => Position,
                                 Globe_Update_Time => Globe_Pos.Pos_Time,
                                 Globe_Positions => Globe_Pos.Pos);
                  Send (My_Message);
               end if;
            end if;

            if My_Message.Original_Sender /= 6611178 and then Vehicle_No /= 1 then
               Init_Location (Vehicle_No, Globe_Pos.Pos);
            end if;

            if My_Message.Original_Sender /= 6611178 then
               Send (My_Message);
            end if;

            if Current_Charge < 0.6 then
               if My_Message.Original_Sender /= 6611178 then
                  Set_Destination (Globe_Pos.Pos);
               end if;
               Set_Throttle (1.0);
               --                Put_Line (Integer'Image (Vehicle_No) & " is going to charge");
               if Energy_Globes_Around'Length /= 0 then
                  Globe_Pos.Pos := Energy_Globes_Around (1).Position;
                  Globe_Pos.Pos_Time := Clock;
                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => 6611178,
                                 Target_Group => 6611178,
                                 Manager_ID => 6611178,
                                 Turn_Back => False,
                                 Message_Time => Clock,
                                 Predecessor_Positions => Position,
                                 Globe_Update_Time => Globe_Pos.Pos_Time,
                                 Globe_Positions => Globe_Pos.Pos);
                  Send (My_Message);
                  Init_Location (Vehicle_No, My_Message.Globe_Positions);
                  --                    Put_Line (Integer'Image (Vehicle_No) & " is comming back");
               end if;
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

   procedure Init_Location (Vehicle_No : Positive; Centre_Pos : Positions) is
      Temp_Group      : constant Positive := Vehicle_No mod 12 + 1;
   begin
      case Temp_Group is
         when 1 =>
            Set_Destination ((Centre_Pos (x), Centre_Pos (y) + 0.2, Centre_Pos (z)));
         when 2 =>
            Set_Destination ((Centre_Pos (x), Centre_Pos (y) + 0.4, Centre_Pos (z)));
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
         when 9 =>
            Set_Destination ((Centre_Pos (x) - 0.2, Centre_Pos (y) + 0.2, Centre_Pos (z)));
         when 10 =>
            Set_Destination ((Centre_Pos (x) + 0.2, Centre_Pos (y) + 0.2, Centre_Pos (z)));
         when 11 =>
            Set_Destination ((Centre_Pos (x) + 0.2, Centre_Pos (y) - 0.2, Centre_Pos (z)));
         when 12 =>
            Set_Destination ((Centre_Pos (x) - 0.2, Centre_Pos (y) - 0.2, Centre_Pos (z)));
         when others =>
            null;
      end case;
      Set_Throttle (0.5);
   end Init_Location;

end Vehicle_Task_Type;
