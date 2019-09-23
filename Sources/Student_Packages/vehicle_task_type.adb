--
-- Danny (u6611178), Australia, September 2019
--

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

      Leader1 : Positive := 1;
      Leader2 : Positive := 2;

      Vehicle_No : Positive;
      Group      : Positive;
      Mes_Arr_Index : Messages_Arr_Index_Type := 0;
      -- To prevent junk message that keeping sending each other like a loop
      Local_Message_Buffer : Messages_Arr;
      My_Message : Inter_Vehicle_Messages; -- Make sure this is the latest
      Temp_Message : Inter_Vehicle_Messages;

      My_Globe : Globe_Pos_I_Know;
      Go_Charging : Boolean := False;

      Found_Double_Globe : Boolean := False;

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
            if Energy_Globes_Around'Length /= 0 then
               Update_MyGlobes (My_Globe, Found_Double_Globe);

               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => NA,
                              Target_Group => NA,
                              Leader_ID => NA,
                              Planned_Charge => False,
                              Skip_Queue => False,
                              Double_Globe => Found_Double_Globe,
                              Message_Time => Clock,
                              Predecessor_Positions => Position,
                              Globe_Update_Time => My_Globe.Pos_Time,
                              Globe_Positions => My_Globe.Pos);
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "Found globe, telling everyone.");
            end if;

            Message_loop :
            while Messages_Waiting loop -- Retrieve message

               if My_Message.Original_Sender = NA then
                  -- This task getting a message for the very first time.
                  Receive (Temp_Message);
                  -- Put_Line (Integer'Image (Vehicle_No) & " got a messge from" & Integer'Image (Temp_Message.Original_Sender) & " at very first");
                  My_Message := Temp_Message;
                  My_Globe.Pos := My_Message.Globe_Positions;
                  My_Globe.Pos_Time := My_Message.Globe_Update_Time;
                  Init_Location (Vehicle_No, My_Globe.Pos, False);

                  if Temp_Message.Target_Receiver = Vehicle_No then

                     if Vehicle_No = Leader1 or else (Vehicle_No = Leader2 and then Found_Double_Globe) then -- leader will always follow the globe
                        Set_Destination (My_Globe.Pos);
                        Set_Throttle (1.0);
                     end if;

                     if Temp_Message.Planned_Charge then
                        if Current_Charge > BATTERY_LEVEL_HIGH then
                           -- No need to charge
                           -- r                         Put_Line ("I'm fine this time, let others go, reported by Veh. " & Integer'Image (Vehicle_No));
                           null;
                        else
                           -- r                         Put_Line (Integer'Image (Vehicle_No) & " Copy that, seems the app just started and I already need to charge");
                           Set_Destination (My_Globe.Pos);
                           Set_Throttle (1.0);
                           Go_Charging := True;
                        end if;
                        -- delay (0.2);
                     end if;

                  else
                     Optimised_Send (Message_To_Send => Temp_Message,
                                     Buffer_Arr => Local_Message_Buffer,
                                     Arr_Index => Mes_Arr_Index,
                                     Veh_ID => Vehicle_No,
                                     Debug_Info => "Forward a message from begining");
                  end if;

               else -- Not the very first time getting a message
                  Receive (Temp_Message);
                  -- No matter who the message is sendting to, I will steal its globe position if it is newer
                  if Temp_Message.Globe_Update_Time > My_Globe.Pos_Time then
                     My_Globe.Pos := Temp_Message.Globe_Positions;
                     My_Globe.Pos_Time := Temp_Message.Globe_Update_Time;
                  end if;

                  -- Check if this message is for me
                  if Temp_Message.Target_Receiver = Vehicle_No then
                     -- Put_Line (Integer'Image (Vehicle_No) & " got a messge from" & Integer'Image (Temp_Message.Original_Sender));

                     -- Check if this is the latest
                     if Temp_Message.Message_Time > My_Message.Message_Time then
                        -- Put_Line (Integer'Image (Vehicle_No) & " find out this is a NEW message");
                        My_Message := Temp_Message; -- Take the latest message
                        if Vehicle_No = Leader1 or else (Vehicle_No = Leader2 and then Found_Double_Globe) then
                           Set_Destination (My_Globe.Pos);
                           Set_Throttle (1.0);
                           -- r                    Put_Line (Integer'Image (Vehicle_No) & " Leader A in position");
                        end if;
                        if My_Message.Planned_Charge then
                           if Current_Charge > BATTERY_LEVEL_HIGH then
                              -- No need to charge
                              -- r                       Put_Line ("I'm fine this time, let others go, reported by Veh." & Integer'Image (Vehicle_No));
                              null;
                           else
                              -- r                       Put_Line (Integer'Image (Vehicle_No) & " Copy that, coming");
                              Set_Destination (My_Globe.Pos);
                              Set_Throttle (1.0);
                              Go_Charging := True;
                           end if;
                           -- delayay (0.2);
                        end if;
                     end if;

                  else
                     -- Message for others
                     Optimised_Send (Message_To_Send => Temp_Message,
                                     Buffer_Arr => Local_Message_Buffer,
                                     Arr_Index => Mes_Arr_Index,
                                     Veh_ID => Vehicle_No,
                                     Debug_Info => "Forward a message");
                  end if;

               end if;
            end loop Message_loop;

            if My_Message.Original_Sender /= NA and then (Vehicle_No = Leader1 or else (Vehicle_No = Leader2 and then Found_Double_Globe)) then
               for i in 2 .. 64 loop
                  while Messages_Waiting loop
                     Receive (Temp_Message);
                     if Temp_Message.Globe_Update_Time > My_Globe.Pos_Time then
                        My_Globe.Pos := Temp_Message.Globe_Positions;
                        My_Globe.Pos_Time := Temp_Message.Globe_Update_Time;
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
                                                 Leader_ID => NA,
                                                 Planned_Charge => True,
                                                 Skip_Queue => False,
                                                 Double_Globe => Found_Double_Globe,
                                                 Message_Time => Clock,
                                                 Predecessor_Positions => Position,
                                                 Globe_Update_Time => My_Globe.Pos_Time,
                                                 Globe_Positions => My_Globe.Pos);

                           Optimised_Send (Message_To_Send => Assembled_Message,
                                           Buffer_Arr => Local_Message_Buffer,
                                           Arr_Index => Mes_Arr_Index,
                                           Veh_ID => Vehicle_No,
                                           Debug_Info => "Queue Skip approved by leader");
                        end;
                        delay (0.01);
                     end if;
                  end loop;

                  Update_MyGlobes (My_Globe, Found_Double_Globe);
                  Set_Destination ((My_Globe.Pos (x) + 0.01, My_Globe.Pos (y) + 0.01, My_Globe.Pos (z) + 0.01));
                  Set_Throttle (1.0); -- Mission for vehicle 1 is to always chase the globe

                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => i,
                                 Target_Group => NA,
                                 Leader_ID => NA,
                                 Planned_Charge => True,
                                 Skip_Queue => False,
                                 Double_Globe => Found_Double_Globe,
                                 Message_Time => Clock,
                                 Predecessor_Positions => Position,
                                 Globe_Update_Time => My_Globe.Pos_Time,
                                 Globe_Positions => My_Globe.Pos);
                  -- Send (My_Message);
                  Optimised_Send (Message_To_Send => My_Message,
                                  Buffer_Arr => Local_Message_Buffer,
                                  Arr_Index => Mes_Arr_Index,
                                  Veh_ID => Vehicle_No,
                                  Debug_Info => "Scheduled charging call from leader");
                  delay (0.1);
               end loop;
            end if;

            if My_Message.Original_Sender /= NA and then not Go_Charging and then (Vehicle_No /= Leader1 or else (Vehicle_No /= Leader2 and then Found_Double_Globe)) then
               Init_Location (Vehicle_No, My_Globe.Pos, False);
            end if;

            if My_Message.Original_Sender /= NA then
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "Normal message. Backed to init loc.");
            end if;

            if Current_Charge < BATTERY_LEVEL_LOW and then Current_Charge > BATTERY_LEVEL_CRITICAL then

               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => Leader1,
                              Target_Group => NA,
                              Leader_ID => NA,
                              Planned_Charge => False,
                              Skip_Queue => True,
                              Double_Globe => Found_Double_Globe,
                              Message_Time => Clock,
                              Predecessor_Positions => Position,
                              Globe_Update_Time => My_Globe.Pos_Time,
                              Globe_Positions => My_Globe.Pos);
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "Queue skipping request");

               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => Leader2,
                              Target_Group => NA,
                              Leader_ID => NA,
                              Planned_Charge => False,
                              Skip_Queue => True,
                              Double_Globe => Found_Double_Globe,
                              Message_Time => Clock,
                              Predecessor_Positions => Position,
                              Globe_Update_Time => My_Globe.Pos_Time,
                              Globe_Positions => My_Globe.Pos);
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "Queue skipping request");
            end if;

            if Current_Charge < BATTERY_LEVEL_CRITICAL or else Go_Charging then
               Go_Charging := True;
               Set_Destination (My_Globe.Pos);
               Set_Throttle (1.0);

               if Energy_Globes_Around'Length /= 0 then
                  Update_MyGlobes (My_Globe, Found_Double_Globe);
                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => NA,
                                 Target_Group => NA,
                                 Leader_ID => NA,
                                 Planned_Charge => False,
                                 Skip_Queue => False,
                                 Double_Globe => Found_Double_Globe,
                                 Message_Time => Clock,
                                 Predecessor_Positions => Position,
                                 Globe_Update_Time => My_Globe.Pos_Time,
                                 Globe_Positions => My_Globe.Pos);
                  Optimised_Send (Message_To_Send => My_Message,
                                  Buffer_Arr => Local_Message_Buffer,
                                  Arr_Index => Mes_Arr_Index,
                                  Veh_ID => Vehicle_No,
                                  Debug_Info => "Charged. Update globe");
                  Go_Charging := False;
                  Init_Location (Vehicle_No, My_Message.Globe_Positions, True);
                  delay (0.2);
                  -- fully charge, have fun to go around for 1s, maybe you will find a new globe?
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
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) + 0.1, Centre_Pos (z) + 0.1));
      when 2 =>
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) + 0.2, Centre_Pos (z) + 0.2));
      when 3 =>
         Set_Destination ((Centre_Pos (x) + 0.1, Centre_Pos (y), Centre_Pos (z)));
      when 4 =>
         Set_Destination ((Centre_Pos (x) + 0.2, Centre_Pos (y), Centre_Pos (z)));
      when 5 =>
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) - 0.1, Centre_Pos (z) + 0.1));
      when 6 =>
         Set_Destination ((Centre_Pos (x), Centre_Pos (y) - 0.2, Centre_Pos (z)));
      when 7 =>
         Set_Destination ((Centre_Pos (x) - 0.1, Centre_Pos (y), Centre_Pos (z) - 0.2));
      when 8 =>
         Set_Destination ((Centre_Pos (x) - 0.2, Centre_Pos (y), Centre_Pos (z)));
      when 9 =>
         Set_Destination ((Centre_Pos (x) - 0.1, Centre_Pos (y) + 0.1, Centre_Pos (z) + 0.2));
      when 10 =>
         Set_Destination ((Centre_Pos (x) + 0.1, Centre_Pos (y) + 0.1, Centre_Pos (z) - 0.1));
      when 11 =>
         Set_Destination ((Centre_Pos (x) + 0.1, Centre_Pos (y) - 0.1, Centre_Pos (z) - 0.1));
      when 12 =>
         Set_Destination ((Centre_Pos (x) - 0.1, Centre_Pos (y) - 0.1, Centre_Pos (z) - 0.2));
      when others =>
         null;
      end case;
      if Fast then
         Set_Throttle (0.8);
      else
         Set_Throttle (0.4);
      end if;

   end Init_Location;

   procedure Optimised_Send (Message_To_Send : Inter_Vehicle_Messages; Buffer_Arr : in out Messages_Arr; Arr_Index : in out Messages_Arr_Index_Type; Veh_ID : Positive; Debug_Info : String) is

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
         --           if Debug_Info /= "Forward a message" and then Debug_Info /= "Forward a message from begining" then
         --              Put_Line (Integer'Image (Veh_ID) & " send a messge from" & Integer'Image (Message_To_Send.Original_Sender) & " to" & Integer'Image (Message_To_Send.Target_Receiver) & ". Mes Time:" & Duration'Image (To_Duration (Message_To_Send.Message_Time - START_TIME)) & ". DEBUG_INFO: " & Debug_Info);
         --           end if;
         --           if Debug_Info = "Found a globe, telling everyone." or else Debug_Info = "Charged. Update globe" then
         --              Put_Line (Integer'Image (Veh_ID) & " send a messge from" & Integer'Image (Message_To_Send.Original_Sender) & " to" & Integer'Image (Message_To_Send.Target_Receiver) & ". Mes Time:" & Duration'Image (To_Duration (Message_To_Send.Message_Time - START_TIME)) & ". DEBUG_INFO: " & Debug_Info);
         --           end if;
      end if;

   end Optimised_Send;

   procedure Update_MyGlobes (My_Globe : in out Globe_Pos_I_Know; Found_Double_Globe : in out Boolean) is
   begin
      if Energy_Globes_Around'Length = 2 then
         Put_Line ("Find 2 globes!!!!");
         Found_Double_Globe := True;
         if abs (Energy_Globes_Around (1).Position - My_Globe.Pos) < abs (Energy_Globes_Around (2).Position - My_Globe.Pos)  then
            My_Globe.Pos := Energy_Globes_Around (1).Position;
            My_Globe.Pos_Time := Clock;
         else
            My_Globe.Pos := Energy_Globes_Around (2).Position;
            My_Globe.Pos_Time := Clock;
         end if;

      elsif Energy_Globes_Around'Length = 1 then
         My_Globe.Pos := Energy_Globes_Around (1).Position;
         My_Globe.Pos_Time := Clock;
      end if;
   end Update_MyGlobes;
end Vehicle_Task_Type;
