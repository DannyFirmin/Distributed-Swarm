--
-- Danny (u6611178), Australia, September 2019
--

--  with genericeric_Sliding_Statistics;
with Swarm_Size;                 use Swarm_Size;
with Exceptions;                 use Exceptions;
with Real_Type;                  use Real_Type;
with Vectors_3D; use Vectors_3D;
with Vehicle_Interface;          use Vehicle_Interface;
with Ada.Text_IO;                use Ada.Text_IO;

package body Vehicle_Task_Type is

   task body Vehicle_Task is
      BATTERY_LEVEL_HIGH : constant Vehicle_Charges := 0.9;
      BATTERY_LEVEL_LOW : constant Vehicle_Charges := 0.5;
      BATTERY_LEVEL_CRITICAL : constant Vehicle_Charges := 0.3;

      -- It will send a message if it's charged again. Otherwise, will be treated as RIP
      BATTERY_LEVEL_DEAD : constant Vehicle_Charges := 0.05;

      My_Leader : Positive := 1;
      Vehicle_No : Positive;

      Mes_Arr_Index : Messages_Arr_Index_Type := 0;
      -- To prevent junk message that keeping sending each other like a loop
      Local_Message_Buffer : Messages_Arr;
      My_Message : Inter_Vehicle_Messages; -- Make sure this is the latest
      Temp_Message : Inter_Vehicle_Messages;

      My_Globe : Globes_I_Know;
      Go_Charging : Boolean := False;
      Skip_Request_Sent : Boolean := False;
      Multi_Globes : Boolean := False;
      Followers : Positive_Sets.Set;
      Leading_Time : Time := Clock;

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
               Update_MyGlobes (My_Globe, Multi_Globes);

               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => NA,
                              Message_Time => Clock,
                              New_LeaderID => NA,
                              Planned_Charge => False,
                              Skip_Queue => False,
                              Multi_Globes => Multi_Globes,
                              Go_Suicide => False,
                              Dying => False,
                              Globes_Info => My_Globe);
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
                  My_Globe := My_Message.Globes_Info;
                  Init_Location (Vehicle_No, My_Globe.Pos, False);
                  if Temp_Message.Target_Receiver = Vehicle_No then

                     if Vehicle_No = My_Leader then
                        Set_Destination (My_Globe.Pos);
                        Set_Throttle (1.0);
                        Put_Line (Integer'Image (Vehicle_No) & ": Leader in position. ---");
                     end if;

                     if Temp_Message.Planned_Charge then
                        if Current_Charge > BATTERY_LEVEL_HIGH then
                           -- No need to charge
                           Put_Line ("I'm fine this time, let others go, reported by Veh. " & Integer'Image (Vehicle_No));
                        else
                           Put_Line (Integer'Image (Vehicle_No) & " Copy that, seems the app just started and I already need to charge");
                           Set_Destination (My_Globe.Pos);
                           Set_Throttle (1.0);
                           Go_Charging := True;
                        end if;
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
                  if Temp_Message.Globes_Info.Pos_Time > My_Globe.Pos_Time then
                     My_Globe := Temp_Message.Globes_Info;
                  end if;
                  -- Put_Line (Integer'Image (Vehicle_No) & " got a messge from" & Integer'Image (Temp_Message.Original_Sender));
                  -- Check if this is the latest
                  if Temp_Message.Message_Time > My_Message.Message_Time then
                     -- Put_Line (Integer'Image (Vehicle_No) & " find out this is a NEW message");
                     My_Message := Temp_Message; -- Take the latest message

                     -- Check if this message is for me
                     if My_Message.Target_Receiver = Vehicle_No then

                        if My_Message.New_LeaderID = Vehicle_No then
                           -- Tell everyone I am the new leader
                           My_Leader := Vehicle_No;
                           My_Message := (Original_Sender => Vehicle_No,
                                          Target_Receiver => NA,
                                          Message_Time => Clock,
                                          New_LeaderID => Vehicle_No,
                                          Planned_Charge => False,
                                          Skip_Queue => False,
                                          Multi_Globes => Multi_Globes,
                                          Go_Suicide => False,
                                          Dying => False,
                                          Globes_Info => My_Globe);
                           Optimised_Send (Message_To_Send => My_Message,
                                           Buffer_Arr => Local_Message_Buffer,
                                           Arr_Index => Mes_Arr_Index,
                                           Veh_ID => Vehicle_No,
                                           Debug_Info => "I am the new leader");
                           Leading_Time := Clock;
                        elsif My_Message.New_LeaderID /= My_Leader and then My_Message.New_LeaderID /= NA then
                           My_Leader := My_Message.New_LeaderID;
                           Put_Line (Integer'Image (Vehicle_No) & " has a new leader" & Integer'Image (My_Leader));
                           Leading_Time := Clock;
                        end if;

                        if Vehicle_No = My_Leader then -- leader will always follow the globe
                           Set_Destination (My_Globe.Pos);
                           Set_Throttle (1.0);
                           Put_Line (Integer'Image (Vehicle_No) & ": Leader in position");
                        end if;

                        if My_Message.Go_Suicide then
                           Put_Line (Integer'Image (Vehicle_No) & " suicided itself as leader wished");
                           My_Message := (Original_Sender => Vehicle_No,
                                          Target_Receiver => My_Leader,
                                          Message_Time => Clock,
                                          New_LeaderID => NA,
                                          Planned_Charge => False,
                                          Skip_Queue => False,
                                          Multi_Globes => Multi_Globes,
                                          Go_Suicide => False,
                                          Dying => True,
                                          Globes_Info => My_Globe);
                           Optimised_Send (Message_To_Send => My_Message,
                                           Buffer_Arr => Local_Message_Buffer,
                                           Arr_Index => Mes_Arr_Index,
                                           Veh_ID => Vehicle_No,
                                           Debug_Info => "Dead. R.I.P");
                           Flight_Termination.Stop;
                        end if;

                        if My_Message.Planned_Charge then
                           if Current_Charge > BATTERY_LEVEL_HIGH then
                              -- No need to charge
                              Put_Line ("I'm fine this time, let others go, reported by Veh." & Integer'Image (Vehicle_No));
                           else
                              Put_Line (Integer'Image (Vehicle_No) & " Copy that, coming");
                              Set_Destination (My_Globe.Pos);
                              Set_Throttle (1.0);
                              Go_Charging := True;
                           end if;
                        end if;

                     end if;

                  else
                     -- Message for others, forward it

                     Optimised_Send (Message_To_Send => Temp_Message,
                                     Buffer_Arr => Local_Message_Buffer,
                                     Arr_Index => Mes_Arr_Index,
                                     Veh_ID => Vehicle_No,
                                     Debug_Info => "Forward a message");
                  end if;

               end if;
            end loop Message_loop;

            if My_Message.Original_Sender /= NA and then Vehicle_No = My_Leader then
               Followers.Include (Vehicle_No);
               if Real (To_Duration (Clock - Leading_Time)) < 20.0 then
                  -- I just start leading, still building followers set, so by default poll 2 .. 120
                  for i in 2 .. 120 loop
                     Leader_Message_Handler (Poll_Target => i,
                                             My_Globe => My_Globe,
                                             Multi_Globes => Multi_Globes,
                                             Temp_Message => Temp_Message,
                                             My_Message => My_Message,
                                             Followers => Followers,
                                             Local_Message_Buffer => Local_Message_Buffer,
                                             Mes_Arr_Index => Mes_Arr_Index,
                                             Vehicle_No => Vehicle_No);
                  end loop;

               else
                  declare
                     -- In case modifing Followers set during iteration
                     Temp_Set : Positive_Sets.Set := Followers;
                  begin
                     for i of Followers loop -- Leader polling all followers
                        Leader_Message_Handler (Poll_Target => i,
                                                My_Globe => My_Globe,
                                                Multi_Globes => Multi_Globes,
                                                Temp_Message => Temp_Message,
                                                My_Message => My_Message,
                                                Followers => Temp_Set,
                                                Local_Message_Buffer => Local_Message_Buffer,
                                                Mes_Arr_Index => Mes_Arr_Index,
                                                Vehicle_No => Vehicle_No);
                     end loop;
                     Followers := Temp_Set;
                  end;
               end if;

               -- The answer to stage D.
               Put_Line ("The leader think there are" & Count_Type'Image (Followers.Length) & " followers");
               --                 for E of Followers loop
               --                    Put_Line ("- " & Integer'Image (E));
               --                 end loop;

            end if;

            if Vehicle_No = My_Leader and then Current_Charge < BATTERY_LEVEL_DEAD then
               Followers.Exclude (Vehicle_No);
               -- Pass leadership to the last follower in my set when dying
               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => Positive (Followers.Last_Element),
                              Message_Time => Clock,
                              New_LeaderID => Positive (Followers.Last_Element),
                              Planned_Charge => False,
                              Skip_Queue => False,
                              Multi_Globes => Multi_Globes,
                              Go_Suicide => False,
                              Dying => False,
                              Globes_Info => My_Globe);
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "You are the new leader");
            end if;

            if My_Message.Original_Sender /= NA and then not Go_Charging and then Vehicle_No /= My_Leader then
               Init_Location (Vehicle_No, My_Globe.Pos, False);
            end if;

            if My_Message.Original_Sender /= NA then
               Optimised_Send (Message_To_Send => My_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "Normal message");
            end if;

            if Current_Charge < BATTERY_LEVEL_LOW and then Current_Charge > BATTERY_LEVEL_CRITICAL and then not Skip_Request_Sent then
               Skip_Request_Sent := True;
               My_Message := (Original_Sender => Vehicle_No,
                              Target_Receiver => My_Leader,
                              Message_Time => Clock,
                              New_LeaderID => NA,
                              Planned_Charge => False,
                              Skip_Queue => True,
                              Multi_Globes => Multi_Globes,
                              Go_Suicide => False,
                              Dying => False,
                              Globes_Info => My_Globe);
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

               if Energy_Globes_Around'Length /= 0 and then Current_Charge > BATTERY_LEVEL_CRITICAL then
                  Update_MyGlobes (My_Globe, Multi_Globes);
                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => My_Leader,
                                 Message_Time => Clock,
                                 New_LeaderID => NA,
                                 Planned_Charge => False,
                                 Skip_Queue => False,
                                 Multi_Globes => Multi_Globes,
                                 Go_Suicide => False,
                                 Dying => False,
                                 Globes_Info => My_Globe);
                  Optimised_Send (Message_To_Send => My_Message,
                                  Buffer_Arr => Local_Message_Buffer,
                                  Arr_Index => Mes_Arr_Index,
                                  Veh_ID => Vehicle_No,
                                  Debug_Info => "Charged. Update globe");
                  Go_Charging := False;
                  Skip_Request_Sent := False;
                  Init_Location (Vehicle_No, My_Message.Globes_Info.Pos, True);
                  delay (0.2);
                  -- Fully charged, have fun
               end if;

               if Current_Charge < BATTERY_LEVEL_DEAD then
                  My_Message := (Original_Sender => Vehicle_No,
                                 Target_Receiver => My_Leader,
                                 Message_Time => Clock,
                                 New_LeaderID => NA,
                                 Planned_Charge => False,
                                 Skip_Queue => False,
                                 Multi_Globes => Multi_Globes,
                                 Go_Suicide => False,
                                 Dying => True,
                                 Globes_Info => My_Globe);
                  Optimised_Send (Message_To_Send => My_Message,
                                  Buffer_Arr => Local_Message_Buffer,
                                  Arr_Index => Mes_Arr_Index,
                                  Veh_ID => Vehicle_No,
                                  Debug_Info => "Dead. R.I.P");
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
         Set_Destination ((Centre_Pos (x) - 0.05, Centre_Pos (y), Centre_Pos (z) - 0.2));
      when 8 =>
         Set_Destination ((Centre_Pos (x) - 0.1, Centre_Pos (y), Centre_Pos (z)));
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
      -- pragma Unreferenced (Debug_Info, Veh_ID);
      Found : Boolean := False;
      Similar : Boolean := False;
      Fresh : constant Boolean := Real (To_Duration (Clock - Message_To_Send.Message_Time)) < 5.0;
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
         if Debug_Info = "Queue skipping request" or else Debug_Info = "Queue Skip approved by leader" then
            Put_Line (Integer'Image (Veh_ID) & " send a messge from" & Integer'Image (Message_To_Send.Original_Sender) & " to" & Integer'Image (Message_To_Send.Target_Receiver) & ". Mes Time:" & Duration'Image (To_Duration (Message_To_Send.Message_Time - START_TIME)) & ". DEBUG_INFO: " & Debug_Info);
         end if;
      end if;

   end Optimised_Send;

   procedure Update_MyGlobes (My_Globe : in out Globes_I_Know; Multi_Globes : in out Boolean) is
   begin
      if Energy_Globes_Around'Length = 2 then
         Multi_Globes := True;
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

   procedure Leader_Message_Handler (Poll_Target : Positive; My_Globe : in out Globes_I_Know; Multi_Globes : in out Boolean; Temp_Message : in out Inter_Vehicle_Messages; My_Message : in out Inter_Vehicle_Messages;  Followers : in out Positive_Sets.Set; Local_Message_Buffer : in out Messages_Arr; Mes_Arr_Index : in out Messages_Arr_Index_Type; Vehicle_No : Positive) is
   begin
      while Messages_Waiting loop
         Receive (Temp_Message);
         if Temp_Message.Globes_Info.Pos_Time > My_Globe.Pos_Time then
            My_Globe := Temp_Message.Globes_Info;
         end if;

         if Temp_Message.Target_Receiver = Vehicle_No and then Temp_Message.Skip_Queue then
            -- If Followers are more than Target_No_of_Elements, tell them die
            if Integer (Followers.Length) > Target_No_of_Elements then
               Followers.Exclude (Temp_Message.Original_Sender);
               Temp_Message := (Original_Sender => Vehicle_No,
                                Target_Receiver => Temp_Message.Original_Sender,
                                Message_Time => Clock,
                                New_LeaderID => NA,
                                Planned_Charge => False,
                                Skip_Queue => False,
                                Multi_Globes => Multi_Globes,
                                Go_Suicide => True,
                                Dying => False,
                                Globes_Info => My_Globe);

               Optimised_Send (Message_To_Send => Temp_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "Leader want u die, queue skip denied");
            else
               Followers.Include (Temp_Message.Original_Sender);
               Temp_Message := (Original_Sender => Vehicle_No,
                                Target_Receiver => Temp_Message.Original_Sender,
                                Message_Time => Clock,
                                New_LeaderID => NA,
                                Planned_Charge => True,
                                Skip_Queue => False,
                                Multi_Globes => Multi_Globes,
                                Go_Suicide => False,
                                Dying => False,
                                Globes_Info => My_Globe);

               Optimised_Send (Message_To_Send => Temp_Message,
                               Buffer_Arr => Local_Message_Buffer,
                               Arr_Index => Mes_Arr_Index,
                               Veh_ID => Vehicle_No,
                               Debug_Info => "Queue Skip approved by leader");
            end if;
         elsif Temp_Message.Target_Receiver = Vehicle_No and then Temp_Message.Dying then
            Followers.Exclude (Temp_Message.Original_Sender);
         elsif Temp_Message.Target_Receiver = Vehicle_No then
            Followers.Include (Temp_Message.Original_Sender);
            delay (0.01);
         end if;
      end loop;

      Update_MyGlobes (My_Globe, Multi_Globes);
      Set_Destination ((My_Globe.Pos (x) + 0.01, My_Globe.Pos (y) + 0.01, My_Globe.Pos (z) + 0.01));
      Set_Throttle (1.0); -- Mission for vehicle 1 is to always chase the globe

      if Integer (Followers.Length) > Target_No_of_Elements then
         Followers.Exclude (Poll_Target);
         My_Message := (Original_Sender => Vehicle_No,
                        Target_Receiver => Poll_Target,
                        Message_Time => Clock,
                        New_LeaderID => NA,
                        Planned_Charge => False,
                        Skip_Queue => False,
                        Multi_Globes => Multi_Globes,
                        Go_Suicide => True,
                        Dying => False,
                        Globes_Info => My_Globe);

         Optimised_Send (Message_To_Send => My_Message,
                         Buffer_Arr => Local_Message_Buffer,
                         Arr_Index => Mes_Arr_Index,
                         Veh_ID => Vehicle_No,
                         Debug_Info => "Leader want u die");
      else
         My_Message := (Original_Sender => Vehicle_No,
                        Target_Receiver => Poll_Target,
                        Message_Time => Clock,
                        New_LeaderID => NA,
                        Planned_Charge => True,
                        Skip_Queue => False,
                        Multi_Globes => Multi_Globes,
                        Go_Suicide => False,
                        Dying => False,
                        Globes_Info => My_Globe);

         Optimised_Send (Message_To_Send => My_Message,
                         Buffer_Arr => Local_Message_Buffer,
                         Arr_Index => Mes_Arr_Index,
                         Veh_ID => Vehicle_No,
                         Debug_Info => "Scheduled charging call from leader");
      end if;
      delay (0.1);
   end Leader_Message_Handler;
end Vehicle_Task_Type;
