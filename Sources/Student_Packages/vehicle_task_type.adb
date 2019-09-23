-- Suggestions for packages which might be useful:

--  with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vehicle_Interface;          use Vehicle_Interface;
-- with Vehicle_Message_Type;       use Vehicle_Message_Type;
-- with Swarm_Structures;           use Swarm_Structures;
-- with Swarm_Structures_Base; use Swarm_Structures_Base;
with Vectors_3D; use Vectors_3D;
package body Vehicle_Task_Type is

   task body Vehicle_Task is
      BATTERY_LEVEL_HIGH : constant Vehicle_Charges := 0.9;
      BATTERY_LEVEL_LOW : constant Vehicle_Charges := 0.5;
      BATTERY_LEVEL_CRITICAL : constant Vehicle_Charges := 0.3;
      GLOBE_MOVED_NOTIFY_DISTANCE : constant Real := 0.05;
      MANAGER_GLOBE1 : constant Positive := 1;
      MANAGER_GLOBE2 : constant Positive := 2;

      Vehicle_No : Positive;
      Group      : Positive;
      Mes_Arr_Index : Messages_Arr_Index_Type := 0;
      -- To prevent junk message that keeping sending each other like a loop
      Local_Message_Buffer : Messages_Arr;
      My_Message : Inter_Vehicle_Messages; -- Make sure this is the latest
      Temp_Message : Inter_Vehicle_Messages;
      type Globe_Pos_I_Know is record
         Pos : Positions := (0.0, 0.0, 0.0);
         Pos_Time : Time;
      end record;
      Globe_Pos : Globe_Pos_I_Know;
      Go_Charging : Boolean := False;
      Skip_Request_Sent : Boolean := False;

      Found_Dual_Globe : Boolean := False;

      Iam_Leader : Boolean := False;
      Group_ID : Positive := NA; -- don't have a group
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

            if Energy_Globes_Around'Length /= 0 and then abs (Energy_Globes_Around (1).Position - Globe_Pos.Pos) > GLOBE_MOVED_NOTIFY_DISTANCE then
               -- Don't send junk messages if the globe doesn't move much
               Globe_Pos.Pos := Energy_Globes_Around (1).Position;
               Globe_Pos.Pos_Time := Clock;
               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => NA,
                              Target_Group => NA,
                              Manager_ID => NA,
                              Planned_Charge => False,
                              Skip_Queue => False,
                              Message_Time => Clock,
                              Predecessor_Positions => Position,
                              Globe_Update_Time => Globe_Pos.Pos_Time,
                              Globe_Positions => Globe_Pos.Pos);
               Send (My_Message);
--                 Put_Line (Imagenteger'Image (Vehicle_No) & " found a globe, telling everyone");
            end if;

            Message_loop :
            while Messages_Waiting loop -- Retrieve message

               if My_Message.Original_Sender = NA then
                  -- This task getting a message for the very first time.
                  Receive (Temp_Message);
                  -- Put_Line (Integer'Image (Vehicle_No) & " got a messge from" & Integer'Image (Temp_Message.Original_Sender) & " at very first");
                  My_Message := Temp_Message;
                  Globe_Pos.Pos := My_Message.Globe_Positions;
                  Globe_Pos.Pos_Time := My_Message.Globe_Update_Time;
                  Init_Location (Vehicle_No, Globe_Pos.Pos, False);

                  if Temp_Message.Target_Receiver = Vehicle_No then

                     if Vehicle_No = MANAGER_GLOBE1 then -- globe manager will always follow the globe
                        Set_Destination (Globe_Pos.Pos);
                        Set_Throttle (1.0);
                     end if;

                     if Temp_Message.Planned_Charge then
                        if Current_Charge > BATTERY_LEVEL_HIGH then
                           -- No need to charge
                           Put_Line ("I'm fine this time, let others go, reported by Veh." & Integer'Image (Vehicle_No));
                        else
                           Put_Line (Integer'Image (Vehicle_No) & " Copy that, seems the app just started and I already need to charge");
                           Set_Destination (Globe_Pos.Pos);
                           Set_Throttle (1.0);
                           Go_Charging := True;
                        end if;
                        -- delay (0.2);
                     end if;

                  else
                     Optimised_Send (Message_To_Send => Temp_Message,
                                     Buffer_Arr => Local_Message_Buffer,
                                     Arr_Index => Mes_Arr_Index,
                                     Veh_ID => Vehicle_No);
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
                     -- Put_Line (Integer'Image (Vehicle_No) & " got a messge from" & Integer'Image (Temp_Message.Original_Sender));

                     -- Check if this is the latest
                     if Temp_Message.Message_Time > My_Message.Message_Time then
--                          Put_Line (Integer'Image (Vehicle_No) & " find out this is a NEW message");
                        My_Message := Temp_Message; -- Take the latest message
                        if Vehicle_No = MANAGER_GLOBE1 then
                           Set_Destination (Globe_Pos.Pos);
                           Set_Throttle (1.0);
                           Put_Line (Integer'Image (Vehicle_No) & " Manager A in position");
                        end if;
                        if My_Message.Planned_Charge then
                           if Current_Charge > BATTERY_LEVEL_HIGH then
                              -- No need to charge
                              Put_Line ("I'm fine this time, let others go, reported by Veh." & Integer'Image (Vehicle_No));
                           else
                              Put_Line (Integer'Image (Vehicle_No) & " Copy that, coming");
                              Set_Destination (Globe_Pos.Pos);
                              Set_Throttle (1.0);
                              Go_Charging := True;
                           end if;
--                             delayay (0.2);
                        end if;
                     end if;

                  else
                     -- Message for others
                     Optimised_Send (Message_To_Send => Temp_Message,
                                     Buffer_Arr => Local_Message_Buffer,
                                     Arr_Index => Mes_Arr_Index,
                                     Veh_ID => Vehicle_No);
                  end if;

               end if;
            end loop Message_loop;

            if My_Message.Original_Sender /= NA and then Vehicle_No = MANAGER_GLOBE1 then
               for i in 2 .. 64 loop
                  while Messages_Waiting loop
                     Receive (Temp_Message);
                     if Temp_Message.Globe_Update_Time > Globe_Pos.Pos_Time then
                        Globe_Pos.Pos := Temp_Message.Globe_Positions;
                        Globe_Pos.Pos_Time := Temp_Message.Globe_Update_Time;
                     end if;

                     if Temp_Message.Target_Receiver = Vehicle_No and then Temp_Message.Skip_Queue and then Temp_Message.Original_Sender /= My_Message.Original_Sender then
                        -- If same sender as the previous message, ignore
                        declare
                           Assembled_Message : Inter_Vehicle_Messages;
                        begin
                           My_Message := Temp_Message;
                           Assembled_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => My_Message.Original_Sender,
                                 Target_Group => NA,
                                 Manager_ID => NA,
                                 Planned_Charge => True,
                                 Skip_Queue => False,
                                 Message_Time => Clock,
                                 Predecessor_Positions => Position,
                                 Globe_Update_Time => Globe_Pos.Pos_Time,
                                             Globe_Positions => Globe_Pos.Pos);

                          Optimised_Send (Message_To_Send => Assembled_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                                          Veh_ID => Vehicle_No);
                            Put_Line (Integer'Image (My_Message.Original_Sender) & " your queue skip request has been approved. Come now");
                        end;

                     end if;
                     delay (0.01);
                  end loop;

                  if Energy_Globes_Around'Length /= 0 then
                     Globe_Pos.Pos := Energy_Globes_Around (1).Position;
                     Globe_Pos.Pos_Time := Clock;
                  end if;
                  Set_Destination ((Globe_Pos.Pos (x) + 0.01, Globe_Pos.Pos (y) + 0.01, Globe_Pos.Pos (z) + 0.01));
                  Set_Throttle (1.0); -- Mission for vehicle 1 is to always chase the globe

                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => i,
                                 Target_Group => NA,
                                 Manager_ID => NA,
                                 Planned_Charge => True,
                                 Skip_Queue => False,
                                 Message_Time => Clock,
                                 Predecessor_Positions => Position,
                                 Globe_Update_Time => Globe_Pos.Pos_Time,
                                 Globe_Positions => Globe_Pos.Pos);
                  Send (My_Message);
                  Put_Line (Integer'Image (i) & " please come to charge, it's on the schedule");
                  delay (0.1);
               end loop;
            end if;

              if My_Message.Original_Sender /= NA and then not Go_Charging and then Vehicle_No /= MANAGER_GLOBE1 then
                 Init_Location (Vehicle_No, Globe_Pos.Pos, False);
              end if;

            if My_Message.Original_Sender /= NA then
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No);
            end if;

            if not Skip_Request_Sent and then Current_Charge < BATTERY_LEVEL_LOW and then Current_Charge > BATTERY_LEVEL_CRITICAL then

               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => MANAGER_GLOBE1,
                              Target_Group => NA,
                              Manager_ID => NA,
                              Planned_Charge => False,
                              Skip_Queue => True,
                              Message_Time => Clock,
                              Predecessor_Positions => Position,
                              Globe_Update_Time => Globe_Pos.Pos_Time,
                              Globe_Positions => Globe_Pos.Pos);
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No);
               Skip_Request_Sent := True;
               Put_Line (Integer'Image (Vehicle_No) & " send a queue skipping request!");
            end if;

            if Current_Charge < BATTERY_LEVEL_CRITICAL or else Go_Charging then
               Go_Charging := True;
               Set_Destination (Globe_Pos.Pos);
               Set_Throttle (1.0);

               if Energy_Globes_Around'Length /= 0 then
                  Globe_Pos.Pos := Energy_Globes_Around (1).Position;
                  Globe_Pos.Pos_Time := Clock;
                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => NA,
                                 Target_Group => NA,
                                 Manager_ID => NA,
                                 Planned_Charge => False,
                                 Skip_Queue => False,
                                 Message_Time => Clock,
                                 Predecessor_Positions => Position,
                                 Globe_Update_Time => Globe_Pos.Pos_Time,
                                 Globe_Positions => Globe_Pos.Pos);
                    Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No);
                  Go_Charging := False;
                  Skip_Request_Sent := False;
                  Init_Location (Vehicle_No, My_Message.Globe_Positions, True);
                  Put_Line (Integer'Image (Vehicle_No) & " is now charged. Thanks");
                  delay (1.0);
               end if;
            end if;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

   procedure Init_Location (Vehicle_No : Positive; Centre_Pos : Positions; Fast : Boolean) is
      Temp_Group      : constant Positive := Vehicle_No mod 12 + 1;
   begin
      case Temp_Group is
      when 1 =>
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) + 0.1, Centre_Pos (z)));
      when 2 =>
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) + 0.2, Centre_Pos (z)));
      when 3 =>
         Set_Destination ((Centre_Pos (x) + 0.1, Centre_Pos (y), Centre_Pos (z)));
      when 4 =>
         Set_Destination ((Centre_Pos (x) + 0.2, Centre_Pos (y), Centre_Pos (z)));
      when 5 =>
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) - 0.1, Centre_Pos (z)));
      when 6 =>
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) - 0.2, Centre_Pos (z)));
      when 7 =>
         Set_Destination ((Centre_Pos (x) - 0.1, Centre_Pos (y), Centre_Pos (z)));
      when 8 =>
         Set_Destination ((Centre_Pos (x) - 0.2, Centre_Pos (y), Centre_Pos (z)));
      when 9 =>
         Set_Destination ((Centre_Pos (x) - 0.1, Centre_Pos (y) + 0.1, Centre_Pos (z)));
      when 10 =>
         Set_Destination ((Centre_Pos (x) + 0.1, Centre_Pos (y) + 0.1, Centre_Pos (z)));
      when 11 =>
         Set_Destination ((Centre_Pos (x) + 0.1, Centre_Pos (y) - 0.1, Centre_Pos (z)));
      when 12 =>
         Set_Destination ((Centre_Pos (x) - 0.1, Centre_Pos (y) - 0.1, Centre_Pos (z)));
      when others =>
         null;
      end case;
      if Fast then
         Set_Throttle (0.8);
      else
         Set_Throttle (0.4);
      end if;

   end Init_Location;

   procedure Optimised_Send (Message_To_Send : Inter_Vehicle_Messages; Buffer_Arr : in out Messages_Arr; Arr_Index : in out Messages_Arr_Index_Type; Veh_ID : Positive) is

      Found : Boolean := False;
      Similar : Boolean := False;
      Fresh : Boolean := Real (To_Duration (Clock - Message_To_Send.Message_Time)) < 5.0;
   begin
      for msg of Buffer_Arr loop
         if msg = Message_To_Send then
            Found := True;
         end if;

         if Message_To_Send.Original_Sender = msg.Original_Sender and then Message_To_Send.Target_Receiver = msg.Target_Receiver and then Real (abs (To_Duration (Message_To_Send.Message_Time - msg.Message_Time))) < 0.1 then
            Similar := True;
         end if;
      end loop;
      --  ONLY forward this message when the message is not been sent before and it's fresh
      if not Found and then not Similar and then Fresh then
         Buffer_Arr (Arr_Index) := Message_To_Send;
         Arr_Index := Arr_Index + 1;
         Send (Message_To_Send);
--           Put_Line (Integerteger'Image (Veh_ID) & " send a messge from" & Integer'Image (Message_To_Send.Original_Sender) & " to" & Integer'Image (Message_To_Send.Target_Receiver) & ". Mes Time:" & Duration'Image (To_Duration (Message_To_Send.Message_Time - START_TIME)));
      end if;

   end Optimised_Send;
end Vehicle_Task_Type;
