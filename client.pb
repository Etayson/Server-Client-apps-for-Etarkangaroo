EnableExplicit

Enumeration
#sendgetjob_id
#login_id
#sendpart_id
#sendfileend_id
#sendkey_id
#File
#JOBFILE
#RESULTFILE
EndEnumeration


#TH32CS_SNAPHEAPLIST = $1 
#TH32CS_SNAPPROCESS = $2 
#TH32CS_SNAPTHREAD = $4 
#TH32CS_SNAPMODULE = $8 
#TH32CS_SNAPALL = #TH32CS_SNAPHEAPLIST | #TH32CS_SNAPPROCESS | #TH32CS_SNAPTHREAD | #TH32CS_SNAPMODULE 
#TH32CS_INHERIT = $80000000 
#INVALID_HANDLE_VALUE = -1 
#MAX_PATH = 260 
#PROCESS32LIB = 9999 


#PROCESS_TERMINATE = $1 
#PROCESS_CREATE_THREAD = $2 
#PROCESS_VM_OPERATION = $8 
#PROCESS_VM_READ = $10 
#PROCESS_VM_WRITE = $20 
#PROCESS_DUP_HANDLE = $40 
#PROCESS_CREATE_PROCESS = $80 
#PROCESS_SET_QUOTA = $100 
#PROCESS_SET_INFORMATION = $200 
#PROCESS_QUERY_INFORMATION = $400 
#PROCESS_ALL_ACCESS = #STANDARD_RIGHTS_REQUIRED | #SYNCHRONIZE | $FFF 


Structure settingsStructure
  host.s
  port.i
  name.s   
  pass.s
  file$
  programmname.s
  getworkrangeBegin.s
  getworkrangeEnd.s
  getworkPub.s
  getworkDP.i  
  jobsaveinterval.i
  resultfile.s
  htpow.i
  minimumFileSizeMB.i
  wi.i
EndStructure

Structure kangarooStructure
  params$
EndStructure
#colorBlue=1
#colorGreen=2
#colorCyan=3
#colorRed=4
#colorMagenta=5
#colorBrown=6
#colorDefault=7
#colorDarkgrey=8
#colorYellow=14
#colorWhite=15
#colorbrightmagenta=13
#colorBrightGreen = 10

#maxtcp = 65536
#MB = 1024*1024
#GB = 1024*1024*1024

If #PB_Compiler_Unicode = 1
  Debug "#PB_Compiler_Unicode"
  End
EndIf  

Define NewMap settings.settingsStructure()
Define MutexConsole, getworkMutex
Define *MemoryBuffer
Define NewList Proc32.PROCESSENTRY32 () 
Define NewList  winkeylist.s()
Define IsFindKey=#False
Define CompilerKangaroo
MutexConsole = CreateMutex()
getworkMutex = CreateMutex()

Procedure SPrint(text$, cl)
 Shared MutexConsole
  LockMutex(MutexConsole)
  ConsoleColor(cl,0)
  Debug FormatDate("%hh:%ii:%ss ", Date())+" "+text$
  PrintN(FormatDate("%hh:%ii:%ss ", Date())+" "+text$)  
  ConsoleColor(#colorDefault,0)
  UnlockMutex(MutexConsole)
EndProcedure

Procedure FileExists(filename.s)
  Protected r=0,f 
  f=ReadFile(#PB_Any, filename,#PB_File_SharedRead)  
  If f  
    CloseFile(f)
    r=1
  EndIf
  ProcedureReturn r
EndProcedure

Procedure FilePutContents(filename.s, *mem, size)
  Protected r=0,f,res
  f=CreateFile(#PB_Any,filename)
  If f
    res=WriteData(f,*mem,size)
    CloseFile(f)
  
    If res=size
      r=1
    Else
      r=0
    EndIf
  EndIf
  ProcedureReturn r
EndProcedure

Procedure.s cutHex(a$)
  a$=Trim(UCase(a$)) 
  If Left(a$,2)="0X" 
    a$=Mid(a$,3,Len(a$)-2)
  EndIf 
  If Len(a$)=1
    a$="0"+a$
  EndIf
ProcedureReturn a$
EndProcedure

Procedure.s getprogparam()
  Protected parametrscount, datares$, i, params$, msginit$="[SETUP] "
  Shared  settings()
  parametrscount=CountProgramParameters()
  
  i=0
  While i<parametrscount  
    Select LCase(ProgramParameter(i))
       Case "-wi"
         Debug "found -wi"
        i+1
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\wi = Val(datares$)
          Sprint( msginit$+"-wi ["+ Str(settings("1")\wi)+"]",#colordefault)
        EndIf
        
      Case "-min"
        Debug "found -min"
        i+1
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\minimumFileSizeMB = Val(datares$) * #MB
          Sprint( msginit$+"-min ["+ Str(settings("1")\minimumFileSizeMB)+"]Mb",#colordefault)
        EndIf
        
      Case "-name"
        Debug "found -name"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\name = datares$
          Sprint( msginit$+"-name ["+settings("1")\name+"]",#colordefault)
        EndIf
        
      Case "-pool"
        Debug "found -pool"
        
        i+1  
        datares$ = ProgramParameter(i)         
        If datares$<>"" And Left(datares$,1)<>"-"
          If GetURLPart(datares$, #PB_URL_Protocol)=""
             datares$="http://"+datares$
          EndIf          
          settings("1")\host =GetURLPart(datares$, #PB_URL_Site)
          settings("1")\port = Val(GetURLPart(datares$, #PB_URL_Port))
          Sprint( msginit$+"-pool "+settings("1")\host+":"+settings("1")\port,#colordefault)
        EndIf              
      
      Case "-grid"
        Debug "found -grid"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          params$ +"-grid "+ Trim(datares$)+" "
          Sprint(msginit$+ "-grid "+Trim(datares$),#colordefault)
        EndIf
      Case "-d"
        Debug "found -d"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          params$ +"-d "+ Trim(datares$)+" "
          Sprint(msginit$+ "-d "+Trim(datares$),#colordefault)
        EndIf 
       Case "-wmerge"
        Debug "found -wmerge"   
          params$ +"-wmerge "
          Sprint(msginit$+ "-wmerge ",#colordefault)
         
    EndSelect
    i+1 
  Wend
  
  Debug "all params["+params$+"]"
ProcedureReturn params$
EndProcedure

Procedure InitProcess32 () 
    ProcedureReturn OpenLibrary (#PROCESS32LIB, "kernel32.dll") 
EndProcedure 

Procedure CloseProcess32 () 
    ProcedureReturn CloseLibrary (#PROCESS32LIB) 
EndProcedure 

; ----------------------------------------------------------------------------- 
; Get/free snapshot of process list... 
; ----------------------------------------------------------------------------- 

Procedure CreateProcessList () 
  Shared Proc32 ()
    ClearList (Proc32 ()) 
    ProcedureReturn CallFunction (#PROCESS32LIB, "CreateToolhelp32Snapshot", #TH32CS_SNAPPROCESS, 0) 
EndProcedure 

Procedure FreeProcessList (snapshot) 
  Shared Proc32 ()
    ; Free process list (.PROCESSENTRY32 structures)... 
    ClearList (Proc32 ()) 
    ; Close snapshot handle... 
    ProcedureReturn CloseHandle_ (snapshot) 
EndProcedure 

; ----------------------------------------------------------------------------- 
; Iterate processes... 
; ----------------------------------------------------------------------------- 

Procedure GetFirstProcess (snapshot) 
  Shared Proc32 ()
    ; Allocate a new .PROCESSENTRY32 structure and fill in SizeOf (structure)... 
    AddElement (Proc32 ()) 
    Proc32 ()\dwSize = SizeOf (PROCESSENTRY32) 
    ; Call Process32First with snapshot handle and pointer to structure... 
    If CallFunction (#PROCESS32LIB, "Process32First", snapshot, @Proc32 ()) 
        ProcedureReturn #True 
    Else 
        ; Free the structure if function call failed... 
        DeleteElement (Proc32 ()) 
        ProcedureReturn #False 
    EndIf 
EndProcedure 

Procedure GetNextProcess (snapshot) 
  Shared Proc32 ()
    ; Allocate a new .PROCESSENTRY32 structure and fill in SizeOf (structure)... 
    AddElement (Proc32 ()) 
    Proc32 ()\dwSize = SizeOf (PROCESSENTRY32) 
    ; Call Process32Next with snapshot handle and pointer to structure... 
    If CallFunction (#PROCESS32LIB, "Process32Next", snapshot, @Proc32 ()) 
        ProcedureReturn #True 
    Else 
        ; Free the structure if function call failed... 
        DeleteElement (Proc32 ()) 
        ProcedureReturn #False 
    EndIf 
EndProcedure 



Procedure.l RunProgramEx(Filename.s, Parameter.s, Directory.s, ShowFlag.l) 
  Protected.STARTUPINFO  Info
  Protected.PROCESS_INFORMATION  ProcessInfo
  Protected ProcessPriority, ProcessID.l, win, pid.l, quit,WinHandle
  
   Info.STARTUPINFO 
   Info\cb          =SizeOf(STARTUPINFO)    
   Info\dwFlags     =1                    ;#STARTF_USESHOWWINDOW 
   Info\wShowWindow =ShowFlag 
   ProcessInfo.PROCESS_INFORMATION    
   ProcessPriority=$20                    ;NORMAL_PRIORITY 
  ;Create a window and retrieve its process 
  If CreateProcess_(@Filename, @Parameter, 0, 0, 0, ProcessPriority, 0, @Directory, @Info, @ProcessInfo) 
      ;Process Values      
      ProcessID.l =ProcessInfo\dwProcessId 
      ;Find Window Handle of Process 
      Repeat 
         win=FindWindow_(0,0) 
         While win<>0 And quit=0 
            GetWindowThreadProcessId_(win, @pid.l) 
            If pid=ProcessID 
               WinHandle=win 
               quit=1 
            EndIf 
            win=GetWindow_(win, #GW_HWNDNEXT)          
         Wend 
      Until WinHandle 
   EndIf 
   ;Retrieve Window Handle 
   ProcedureReturn WinHandle 
 EndProcedure  



Procedure.s getsnapshot(winToClose$)
  Protected result$, snapshot, result
  Shared Proc32 ()
ClearList (Proc32 ()) 
If InitProcess32 () 

    ; ------------------------------------------------------------------------- 
    ; Get a snapshot of all running processes... 
    ; ------------------------------------------------------------------------- 
    
    snapshot = CreateProcessList () 
    
    If snapshot 
    
        ; --------------------------------------------------------------------- 
        ; Get list of processes... 
        ; --------------------------------------------------------------------- 
        
        If GetFirstProcess (snapshot) 
            Repeat 
                result = GetNextProcess (snapshot) 
            Until result = #False 
        EndIf 

        ; --------------------------------------------------------------------- 
        ; Iterate through Proc32 () list, and act on process data here... 
        ; --------------------------------------------------------------------- 

        ResetList (Proc32 ()) 
        
        While NextElement (Proc32 ()) 
        
            ; Example of accessing PROCESSENTRY32 structure... 
            
            ;Debug "Process ID: " + Str (Proc32 ()\th32ProcessID) + " (" + PeekS (@Proc32 ()\szExeFile) + ")" 
            If winToClose$=PeekS (@Proc32 ()\szExeFile)
              Debug "FOUND "+winToClose$+" with Process ID: " + Str (Proc32 ()\th32ProcessID)
              result$+Str (Proc32 ()\th32ProcessID)+","
              
            EndIf
        Wend 
        RTrim(result$, ",")
        ; --------------------------------------------------------------------- 
        ; Free snapshot/list of processes... 
        ; --------------------------------------------------------------------- 

        FreeProcessList (snapshot) 
        
    EndIf 

    ; ------------------------------------------------------------------------- 
    ; Close kernel32.dll... 
    ; ------------------------------------------------------------------------- 
        
    CloseProcess32 () 
    
  EndIf 
ProcedureReturn result$
EndProcedure

Procedure KillProcess (pid) 
 Protected phandle, result
    phandle = OpenProcess_ (#PROCESS_TERMINATE, #False, pid) 
    If phandle <> #Null 
        If TerminateProcess_ (phandle, 1) 
            result = #True 
        EndIf 
        CloseHandle_ (phandle) 
    EndIf 
    ProcedureReturn result 
EndProcedure 
  
Procedure shutdownRunningMiners(minername.s)
Protected processName.s, pidresult$,i, result=#False

    processName=minername
 
  pidresult$ = getsnapshot(processName)
  If pidresult$=""
    Debug(minername+" is not running..")
    result = #True 
  Else
    ; **********KILL TASK************
    For i=1 To CountString(pidresult$, ",")
      Sprint("Kill "+minername+" with PID:"+StringField(pidresult$, i, ","), #colorDefault) 
      result=KillProcess (Val(StringField(pidresult$, i, ",")))
      If result = #True 
        Sprint("Kill "+minername+" with PID:"+StringField(pidresult$, i, ",")+" success!", #colorDefault) 
      EndIf
    Next i
  EndIf
  
  
 ProcedureReturn result 
EndProcedure

Procedure.s getElem(js.i,pname.s="",pelem.l=0,aelem.l=0)
  Protected result$,jsFloat_g
  
  result$=""
  If IsJSON(js) And GetJSONMember(JSONValue(js), pname)
  Select JSONType(GetJSONMember(JSONValue(js), pname))
      
      Case #PB_JSON_String
          result$= GetJSONString(GetJSONMember(JSONValue(js), pname))          
        Case #PB_JSON_Array
         
              
              
       If JSONArraySize(GetJSONMember(JSONValue(js), pname))>pelem
         Select JSONType(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
           Case #PB_JSON_String
             result$= GetJSONString(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
           Case #PB_JSON_Number            
             result$= Str(GetJSONInteger(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem)))    
             jsFloat_g=GetJSONDouble(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
             
           Case #PB_JSON_Array
             If JSONArraySize(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))>aelem             
                result$+ GetJSONString(GetJSONElement(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem),aelem))
             EndIf
          Case #PB_JSON_Boolean
             result$=Str(GetJSONBoolean(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem)))
             
         EndSelect
          
        EndIf
        
      Case #PB_JSON_Boolean
        result$=Str(GetJSONBoolean(GetJSONMember(JSONValue(js), pname)))
        
        
      Case #PB_JSON_Number
        
        result$= Str(GetJSONInteger(GetJSONMember(JSONValue(js), pname)))

        
    EndSelect
  
  EndIf

  ProcedureReturn result$
EndProcedure

Procedure.i SendQuestion(con_id,string$)
  Protected err
  If con_id
    SendNetworkString(con_id,string$+#LF$,#PB_Ascii)    
    Debug "send to socket :"+Str(con_id)+">"+string$
  EndIf
ProcedureReturn err
EndProcedure



Procedure ReadFileToMemory(*MemoryBuffer, Filename$, full_size.q)
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, i, err
  
      If ReadFile(#File,Filename$,#PB_File_SharedRead)   
          ;Load BIN if exist
          ;Sprint("Load file:"+Filename$, #colorDarkgrey)  
          totalloadbytes=0
          maxloadbytes=full_size
          If full_size>1024*1024*1024
            maxloadbytes = 1024*1024*1024
          EndIf   
          *pp= *MemoryBuffer
          i=0
          Repeat
            ;Sprint("["+Str(i)+"] chunk:"+Str(maxloadbytes)+" b", #colorDarkgrey)
            loadedbytes=ReadData(#File, *pp, maxloadbytes)
            totalloadbytes + maxloadbytes
            
            If maxloadbytes<>loadedbytes
              Sprint("Error when loading: need:"+Str(maxloadbytes)+" b, got:"+Str(loadedbytes)+"b",#colorRed)              
              err=1
              Break
            EndIf
            
            *pp+maxloadbytes
            
            If totalloadbytes<full_size
              If totalloadbytes+maxloadbytes>full_size
                maxloadbytes = full_size-totalloadbytes
                ;Sprint("Last chunk:"+Str(maxloadbytes)+" b", #colorDarkgrey)
              EndIf
            EndIf
            i+1
          Until totalloadbytes>=full_size
          
          CloseFile(#File)          
        Else
          Sprint("Can`t open file:"+Filename$, #colorRed)
          err=1
        EndIf
ProcedureReturn err 
EndProcedure


Procedure sendfiletohost(*MemoryBuffer,full_size.q, computeCRC32file)
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, i, err, batchCRC32, get_work_sendbatch_string.s, quit=#False, isSending=#False, timeout
  Protected Connect, dis=1, pars_res, *Buffer, ReceivedBytes, answer_t$, pos, pos2, answer_f$, tempjson, get_work, Values, get_work_authorize_string.s, id_json_answer,msginit$, isAuthorized
  Shared settings(),getworkMutex
  LockMutex(getworkMutex)
  *Buffer = AllocateMemory(65536)
  msginit$ ="[GETWORK] "
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #login_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "Login")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))      
    SetJSONString(AddJSONElement(Values), settings("1")\name)     
    SetJSONString(AddJSONElement(Values), settings("1")\pass)
    get_work_authorize_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
  EndIf
  
  tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #sendpart_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "filetansfer")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))      
      SetJSONInteger(AddJSONElement(Values), full_size)     
      SetJSONString(AddJSONElement(Values), Hex(computeCRC32file))
      get_work_sendbatch_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    
  Repeat
    If dis=1
      isAuthorized =#False
      isSending=#False
      Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
      If Not Connect
        
        While Not Connect And timeout<5
          timeout+1
          Debug "try conect to getwork"
          Delay(1000)
          Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
        Wend
        If Not Connect
          ;cant` connect
           Connect = 0
            err=2
            quit = #True
            Break
        EndIf
      EndIf   
      dis=0
      SendQuestion(Connect,get_work_authorize_string) 
      
    EndIf
  
  If Connect
    Select NetworkClientEvent(Connect) 
        Case #PB_NetworkEvent_Data     
        ReceivedBytes = ReceiveNetworkData(Connect, *Buffer, 65536) 
        If ReceivedBytes>0
          answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Ascii)  
          Debug answer_t$
          pos=FindString(answer_t$, "{")
          While pos                
            pos2=FindString(answer_t$, "}",pos+1)
            If pos2            
              answer_f$=Mid(answer_t$,pos,pos2-pos+1)            
              answer_f$ = RTrim(answer_f$,"}")
              answer_f$ = LTrim(answer_f$,"{")                   
              answer_f$ = "{"+answer_f$+"}"
              Debug">>"+answer_f$        
              pars_res=ParseJSON(#PB_Any, answer_f$)                    
              If pars_res
                id_json_answer=Val(LCase(cutHex(getElem(pars_res,"id",0))))
                If id_json_answer
                  Select id_json_answer                            
                    Case #login_id
                      If Not Val(getElem(pars_res,"result",0))
                        If LCase(getElem(pars_res,"error",0))="invalid_login"
                          Sprint(msginit$+">>Invalid Login<<",#colorRed)
                          Delay(1000)
                          dis=1
                          isAuthorized =#False
                        ElseIf LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        EndIf 
                        
                      Else
                        ;Sprint(msginit$+"Authorized", #colorBrown)  
                        isAuthorized =#True
                        SendQuestion(Connect,get_work_sendbatch_string)                        
                      EndIf                      
                    Case #sendpart_id
                      If Val(getElem(pars_res,"result",0))
                        ;Sprint(msginit$+"Sending...", #colorBrown) 
                        isSending = #True
                        totalloadbytes=0
                        maxloadbytes=full_size
                        If full_size>#maxtcp
                          maxloadbytes = #maxtcp
                        EndIf   
                        *pp= *MemoryBuffer
                        i=0
                      Else
                       If LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                       EndIf   
                      EndIf
                      
                      
                  EndSelect
                EndIf
                If IsJSON(pars_res)
                  FreeJSON(pars_res)
                EndIf 
              Else
                    Sprint(msginit$+"Unknown json",#colorred)
              EndIf
              answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
              pos=FindString(answer_t$, "{")
            Else
              pos=0
            EndIf
          Wend
        EndIf
        
      Case #PB_NetworkEvent_Disconnect
        Debug "getwork disconnected"
        Connect = 0
        err=1
        quit = #True
    EndSelect
    
    If isSending=#True
      Debug "["+Str(i)+"] send chunk:"+Str(maxloadbytes)+" b"
      If Not Connect
        Connect=0
        err=1
        quit = #True
        Break
      EndIf
      SendNetworkData(Connect,*pp, maxloadbytes)
      ;Print(Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+StrD(totalloadbytes*100/full_size,2)+"%")
      totalloadbytes + maxloadbytes
      *pp+maxloadbytes
              
      If totalloadbytes<full_size
        If totalloadbytes+maxloadbytes>full_size
          maxloadbytes = full_size-totalloadbytes
        EndIf
      EndIf
      i+1
      If totalloadbytes>=full_size
        isSending = #False
        quit = #True
        Break
      EndIf
    EndIf
  EndIf
  Delay (1)
Until quit
If Connect
  CloseNetworkConnection(Connect)
EndIf
FreeMemory(*Buffer)
UnlockMutex(getworkMutex)
ProcedureReturn err  
EndProcedure


Procedure sendWinKEtToHost(winkey$)
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, i, err, batchCRC32, get_work_sendbatch_string.s, quit=#False,  timeout
  Protected Connect, dis=1, pars_res, *Buffer, ReceivedBytes, answer_t$, pos, pos2, answer_f$, tempjson, get_work, Values, get_work_authorize_string.s, id_json_answer,msginit$, isAuthorized
  Shared settings(), getworkMutex
  LockMutex(getworkMutex)
  *Buffer = AllocateMemory(65536)
  msginit$ ="[GETWORK] "
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #login_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "Login")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))      
    SetJSONString(AddJSONElement(Values), settings("1")\name)     
    SetJSONString(AddJSONElement(Values), settings("1")\pass)
    get_work_authorize_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
  EndIf
  
  tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #sendkey_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "keysend")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))         
      SetJSONString(AddJSONElement(Values), winkey$)
      get_work_sendbatch_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    
  Repeat
    If dis=1
      isAuthorized =#False      
      Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
      If Not Connect
        
        While Not Connect And timeout<5
          timeout+1
          Debug "try conect to getwork"
          Delay(1000)
          Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
        Wend
        If Not Connect
          ;cant` connect
           Connect = 0
            err=2
            quit = #True
            Break
        EndIf
      EndIf   
      dis=0
      SendQuestion(Connect,get_work_authorize_string) 
      
    EndIf
  
  If Connect
    Select NetworkClientEvent(Connect) 
        Case #PB_NetworkEvent_Data     
        ReceivedBytes = ReceiveNetworkData(Connect, *Buffer, 65536) 
        If ReceivedBytes>0
          answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Ascii)  
          Debug answer_t$
          pos=FindString(answer_t$, "{")
          While pos                
            pos2=FindString(answer_t$, "}",pos+1)
            If pos2            
              answer_f$=Mid(answer_t$,pos,pos2-pos+1)            
              answer_f$ = RTrim(answer_f$,"}")
              answer_f$ = LTrim(answer_f$,"{")                   
              answer_f$ = "{"+answer_f$+"}"
              Debug">>"+answer_f$        
              pars_res=ParseJSON(#PB_Any, answer_f$)                    
              If pars_res
                id_json_answer=Val(LCase(cutHex(getElem(pars_res,"id",0))))
                If id_json_answer
                  Select id_json_answer                            
                    Case #login_id
                      If Not Val(getElem(pars_res,"result",0))
                        If LCase(getElem(pars_res,"error",0))="invalid_login"
                          Sprint(msginit$+">>Invalid Login<<",#colorRed)
                          Delay(1000)
                          dis=1
                          isAuthorized =#False
                        ElseIf LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        EndIf
                      Else
                        Sprint(msginit$+"Authorized", #colorBrown)  
                        isAuthorized =#True
                        SendQuestion(Connect,get_work_sendbatch_string)
                        
                        
                      EndIf
                      
                    Case #sendkey_id
                      If Not Val(getElem(pars_res,"result",0))
                        Sprint(msginit$+">>"+getElem(pars_res,"result",0)+"<<",#colorRed)
                        Delay(1000)
                        dis=1
                        isAuthorized =#False
                      Else
                        Sprint(msginit$+"Key was send to host", #colorBrown)                         
                        quit = #True
                        Break
                      EndIf
                  EndSelect
                EndIf
                If IsJSON(pars_res)
                  FreeJSON(pars_res)
                EndIf 
              Else
                    Sprint(msginit$+"Unknown json",#colorred)
              EndIf
              answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
              pos=FindString(answer_t$, "{")
            Else
              pos=0
            EndIf
          Wend
        EndIf
        
      Case #PB_NetworkEvent_Disconnect
        Debug "getwork disconnected"
        Connect = 0
        err=1
        quit = #True
    EndSelect
    
    
  EndIf
  Delay (1)
Until quit
If Connect
  CloseNetworkConnection(Connect)
EndIf
FreeMemory(*Buffer)
UnlockMutex(getworkMutex)
ProcedureReturn err  
EndProcedure


Procedure GetJobHost()
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, i, err, batchCRC32, get_work_sendbatch_string.s, quit=#False,  timeout
  Protected Connect, dis=1, pars_res, *Buffer, ReceivedBytes, answer_t$, pos, pos2, answer_f$, tempjson, get_work, Values, get_work_authorize_string.s, id_json_answer,msginit$, isAuthorized
  Shared settings() 
  
  *Buffer = AllocateMemory(65536)
  msginit$ ="[GETWORK] "
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #login_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "Login")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))      
    SetJSONString(AddJSONElement(Values), settings("1")\name)     
    SetJSONString(AddJSONElement(Values), settings("1")\pass)
    get_work_authorize_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
  EndIf
  
  tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #sendgetjob_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "getwork")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))
      SetJSONNull(AddJSONElement(Values))
      get_work_sendbatch_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    
  Repeat
    If dis=1
      isAuthorized =#False      
      Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
      If Not Connect
        
        While Not Connect And timeout<5
          timeout+1
          Debug "try conect to getwork"
          Delay(1000)
          Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
        Wend
        If Not Connect
          ;cant` connect
           Connect = 0
            err=2
            quit = #True
            Break
        EndIf
      EndIf   
      dis=0
      SendQuestion(Connect,get_work_authorize_string) 
      
    EndIf
  
  If Connect
    Select NetworkClientEvent(Connect) 
        Case #PB_NetworkEvent_Data     
        ReceivedBytes = ReceiveNetworkData(Connect, *Buffer, 65536) 
        If ReceivedBytes>0
          answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Ascii)  
          Debug answer_t$
          pos=FindString(answer_t$, "{")
          While pos                
            pos2=FindString(answer_t$, "}",pos+1)
            If pos2            
              answer_f$=Mid(answer_t$,pos,pos2-pos+1)            
              answer_f$ = RTrim(answer_f$,"}")
              answer_f$ = LTrim(answer_f$,"{")                   
              answer_f$ = "{"+answer_f$+"}"
              Debug">>"+answer_f$        
              pars_res=ParseJSON(#PB_Any, answer_f$)                    
              If pars_res
                id_json_answer=Val(LCase(cutHex(getElem(pars_res,"id",0))))
                If id_json_answer
                  Select id_json_answer 
                    Case #login_id
                      If Not Val(getElem(pars_res,"result",0))
                        If LCase(getElem(pars_res,"error",0))="invalid_login"
                          Sprint(msginit$+">>Invalid Login<<",#colorRed)
                          Delay(1000)
                          dis=1
                          isAuthorized =#False
                        ElseIf LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        EndIf
                      Else
                        Sprint(msginit$+"Authorized", #colorBrown)  
                        isAuthorized =#True
                        SendQuestion(Connect,get_work_sendbatch_string)
                        
                        
                      EndIf
                  EndSelect
                Else                 
                      Debug"*****"
                      If getElem(pars_res,"error",0)=""
                        Sprint(msginit$+">>Got Job from host<<",#colorBrown)
                        settings("1")\getworkDP = Val(getElem(pars_res,"result",0))
                        settings("1")\getworkPub = getElem(pars_res,"result",1)
                        settings("1")\getworkrangeBegin= getElem(pars_res,"result",2)
                        settings("1")\getworkrangeEnd.s= getElem(pars_res,"result",3)
                        If settings("1")\wi = 0
                          settings("1")\jobsaveinterval= Val(getElem(pars_res,"result",4))
                        EndIf
                        settings("1")\htpow= Val(getElem(pars_res,"result",5))
                        Sprint(msginit$+"DP    ["+Str(settings("1")\getworkDP)+"]",#colorBrown)
                        Sprint(msginit$+"KEY   ["+settings("1")\getworkPub+"]",#colorBrown)
                        Sprint(msginit$+"BEGIN ["+settings("1")\getworkrangeBegin+"]",#colorBrown)
                        Sprint(msginit$+"END   ["+settings("1")\getworkrangeEnd+"]",#colorBrown)
                        If settings("1")\wi = 0
                          Sprint(msginit$+"WI    ["+settings("1")\jobsaveinterval+"]",#colorBrown)
                        Else
                          Sprint(msginit$+"WI    ["+getElem(pars_res,"result",4)+"=>"+settings("1")\jobsaveinterval+"]",#colorBrown)
                        EndIf
                        Sprint(msginit$+"HTPOW ["+Str(settings("1")\htpow)+"]",#colorBrown)
                        quit = #True
                        Break
                      Else
                        Sprint(msginit$+getElem(pars_res,"error",0), #colorRed)
                        err=1
                        quit = #True
                        Break
                      EndIf
                EndIf
                If IsJSON(pars_res)
                  FreeJSON(pars_res)
                EndIf 
              Else
                    Sprint(msginit$+" unknown json",#colorred)
              EndIf
              answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
              pos=FindString(answer_t$, "{")
            Else
              pos=0
            EndIf
          Wend
        EndIf
        
      Case #PB_NetworkEvent_Disconnect
        Debug "getwork disconnected"
        Connect = 0
        err=1
        quit = #True
    EndSelect
    
    
  EndIf
  Delay (1)
Until quit
If Connect
  CloseNetworkConnection(Connect)
EndIf
FreeMemory(*Buffer)
ProcedureReturn err  
EndProcedure

Procedure.s GetJobFile(FileName.s)
  Protected Directory$, name$, NewList filelist$()
  Directory$ = GetCurrentDirectory()
  If ExamineDirectory(0, Directory$, "*.*")  
    While NextDirectoryEntry(0)
      If DirectoryEntryType(0) = #PB_DirectoryEntry_File        
        If FindString( DirectoryEntryName(0), FileName)         
          AddElement(filelist$())
          filelist$() = GetFilePart(DirectoryEntryName(0))
        EndIf
      EndIf      
    Wend
    FinishDirectory(0)
  EndIf  
  ResetList(filelist$())
  While NextElement(filelist$())
    ;prerfer to file without wait_
    If Left(filelist$(), 5)<>"wait_"
      name$ = filelist$()
      Break
    EndIf
  Wend
  If name$="" And ListSize(filelist$())
    FirstElement(filelist$()) 
    name$ = filelist$()
  EndIf
  FreeList(filelist$())
 ProcedureReturn name$
EndProcedure 

Procedure runKangaroo(*kangaroo.kangarooStructure)
  Protected  procname$="[SOLVER]", Output$, err$, Outputcuted$, err, dead$, totaldelemiter
  Protected string_win$=LCase("Priv:")
  Protected *Buffer, a$, pos
  Shared settings(), winkeylist(), IsFindKey, CompilerKangaroo
  CompilerKangaroo = RunProgram(settings("1")\programmname ,*kangaroo\params$,"",#PB_Program_Open)  
  If CompilerKangaroo
    SPrint(procname$+"["+settings("1")\programmname+"] programm running..",#colorYellow)
    SPrint(procname$+"params ["+*kangaroo\params$+"]",#colorYellow)
    While ProgramRunning(CompilerKangaroo)
      err$ = ReadProgramError(CompilerKangaroo)
      If err$
        SPrint (procname$+"Error: "+err$,#colorRed)
      EndIf      
      Delay(10)      
    Wend
    SPrint(procname$+"["+settings("1")\programmname+"] programm finished",#colorYellow)
  Else
    SPrint(procname$+"Can't found ["+settings("1")\programmname+"] programm",#colorred)  
  EndIf
  ;check result file
  If FileSize(settings("1")\resultfile)<0
    
  Else
    If OpenFile(#RESULTFILE, settings("1")\resultfile)
      ;Pub:  03100611C54DFEF604163B8358F7B7FAC13CE478E02CB224AE16D45526B25D9D4D 
      ;Priv: 0xF7051F27B09112D4 
      a$ = ReadString(#RESULTFILE , #PB_Ascii)
      ;PrintN("File->"+a$+"<--")
      If FindString(a$, "Pub:",1,#PB_String_NoCase)            
        
      Else
        SPrint(procname$+"Missing string [Pub:]",#colorred)        
      EndIf
      a$ = ReadString(#RESULTFILE , #PB_Ascii)
      pos=FindString(a$, "Priv: ",1,#PB_String_NoCase) 
      If  pos          
        a$=cutHex(Mid(a$, pos+6))
        AddElement(winkeylist())
        winkeylist() = a$
      Else
        SPrint(procname$+"Missing string [Priv:]",#colorred)                 
      EndIf
      CloseFile(#RESULTFILE) 
    Else
      SPrint(procname$+"Can`t open file ["+settings("1")\resultfile,#colorred)         
    EndIf
  EndIf
  If ListSize(winkeylist())>0
    ForEach winkeylist()
      err = sendWinKEtToHost(winkeylist())
      While err<>0
        Select err
          Case 1
            Sprint(procname$+"Server disconnected",#colorRed)
          Case 2
            Sprint(procname$+"Server not connected",#colorRed)
          Case 3
            Sprint(procname$+"Key already founded by host",#colorRed)          
            Break
        EndSelect
        Delay(5000)
        err = sendWinKEtToHost(winkeylist())
      Wend
    Next winkeylist()  
    IsFindKey = #True
    DeleteFile(settings("1")\resultfile,#PB_FileSystem_Force)    
  EndIf
  CompilerKangaroo=0
EndProcedure

Define CurrentWsplitFile$
;--Start
;Client app for Etarkangaroo.
;Require Etarkangaroo in the same folder.
;Usage:
;-wi		  [optional] timer interval for autosaving ht/kangaroos, without setting, the value from the server is used
;-min		  [optional] the minimum size (in MB) of a working file to send to the server
;-name		[optional] name of client
;-pool		[required] IP adress of server:port, default 127.0.0.1:8000
;-grid		[Etarkangaroo settings]
;-d		    [Etarkangaroo settings]
;Example:
;EtarkangarooClient.exe -pool 127.0.0.1:8000 -d 0 -grid 44,64 -wi 300 -wmerge -min 10



InitNetwork()
OpenConsole()

settings("1")\name = ComputerName()
settings("1")\pass = "x"
settings("1")\file$ = "test1"
settings("1")\programmname = "Etarkangaroo.exe"
settings("1")\host = "127.0.0.1"
settings("1")\port = 8000
settings("1")\minimumFileSizeMB = #MB * 1

settings("1")\jobsaveinterval = 300
settings("1")\resultfile = "result"
Define full_size.q,*MemoryBuffer, computeCRC32file, computeCRC32memory, err, curfilemodified, prefilemodified, starttime, pidresult$, msginit$="[MAIN] "
Define kangarooparams.kangarooStructure

Sprint(msginit$+"App started",#colorDefault)

If Not FileExists(settings("1")\programmname)
  Sprint("File "+settings("1")\programmname+" doesn`t exist",#colorRed)
  End
EndIf

pidresult$ = getsnapshot(settings("1")\programmname)
If pidresult$=""
    Sprint(msginit$+settings("1")\programmname+" is not running..", #colorDefault)
     
Else
    Sprint(msginit$+settings("1")\programmname+" is already running..", #colorDefault)
    Sprint(msginit$+"Close it.", #colorDefault)
    If Not shutdownRunningMiners(settings("1")\programmname)
      Sprint("Can`t kill process ["+msginit$+settings("1")\programmname+"]",#colorRed)
      Delay(2000)
      End
    EndIf
    Delay(2000)
EndIf


kangarooparams\params$ = getprogparam()


If settings("1")\wi 
  settings("1")\jobsaveinterval= settings("1")\wi
EndIf
                        
Select GetJobHost()
  Case 1
    Sprint(msginit$+"Server disconnected",#colorRed)
    End
  Case 2
    Sprint(msginit$+"Server not connected",#colorRed)
    End
  Case 3
    Sprint(msginit$+"Key already founded by host",#colorRed)
    End
EndSelect
            

If kangarooparams\params$=""
  kangarooparams\params$ = ""
EndIf

kangarooparams\params$ + "-ht "+Str(settings("1")\htpow)+" "
kangarooparams\params$ + "-dp "+Str(settings("1")\getworkDP)+" "
kangarooparams\params$ + "-o "+settings("1")\resultfile+" "
kangarooparams\params$ + "-kf kangaroowork"+" "
kangarooparams\params$ + "-wf "+settings("1")\file$+" "
kangarooparams\params$ + "-wi "+Str(settings("1")\jobsaveinterval)+" "
kangarooparams\params$ + "-wsplit"+" "
kangarooparams\params$ + "-rb "+settings("1")\getworkrangeBegin+" "
kangarooparams\params$ + "-re "+settings("1")\getworkrangeEnd+" "
kangarooparams\params$ + "-pub "+settings("1")\getworkPub+" "




;check if working files exist and send to server

While FileSize(GetJobFile(settings("1")\file$)) > settings("1")\minimumFileSizeMB
  
  CurrentWsplitFile$ = GetJobFile(settings("1")\file$)
  Sprint(msginit$+"Have unsended file "+CurrentWsplitFile$, #colorDefault)
  
  full_size = FileSize(CurrentWsplitFile$)
  If full_size>0
    computeCRC32file = CRC32FileFingerprint(CurrentWsplitFile$)
    *MemoryBuffer=AllocateMemory(full_size)
    If *MemoryBuffer
      If Not ReadFileToMemory(*MemoryBuffer, CurrentWsplitFile$, full_size)
        computeCRC32memory = CRC32Fingerprint(*MemoryBuffer, full_size)
        If LCase(Hex(computeCRC32file))= LCase(Hex(computeCRC32memory))
          ;Sprint(msginit$+"File "+CurrentWsplitFile$+" was load, modified ["+FormatDate("%hh:%ii:%ss %yyyy/%mm/%dd", curfilemodified)+"], CRC32="+Hex(computeCRC32file)+", size:"+Str(full_size), #colorDefault)
          
          Repeat
            starttime=ElapsedMilliseconds()
            err = sendfiletohost(*MemoryBuffer,full_size, computeCRC32file)
            If Not err
              ;Print(Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+"100%"+Chr(13))
              Sprint(msginit$+"File "+CurrentWsplitFile$+" sended in "+Str((ElapsedMilliseconds()-starttime)/1000)+" s", #colorBrown)
              prefilemodified = curfilemodified
              If FileSize(CurrentWsplitFile$)>=0
                DeleteFile(CurrentWsplitFile$)
              EndIf
              If FileSize(GetJobFile(settings("1")\file$))>0
                Sprint(msginit$+"Await 10s", #colorDefault)
                Delay(10*1000)
              EndIf
            Else
              Select err
                Case 1
                  Sprint(msginit$+"Server disconnected, try after 30s",#colorRed)                  
                  Delay(30*1000)
                Case 2
                  Sprint(msginit$+"Server not connected, try after 30s",#colorRed)                  
                  Delay(30*1000)
                Case 3
                  Sprint(msginit$+"Key already founded by host",#colorRed)
                  IsFindKey=#True
                  If ProgramRunning(CompilerKangaroo)
                    KillProgram(CompilerKangaroo)
                  EndIf                  
              EndSelect
            EndIf
          Until err=0 Or err = 3
          If IsFindKey=#True
             Break
          EndIf
        Else
          Sprint(msginit$+"CRC32 file: "+LCase(Hex(computeCRC32file))+" not equil to CRC32 memory: "+LCase(Hex(computeCRC32memory)),#colorRed)
        EndIf
      Else
        Sprint(msginit$+"Loadinf file "+CurrentWsplitFile$+" failed",#colorRed)
      EndIf
    Else
      Sprint(msginit$+"Can`t allocate memory buffer "+Str(full_size)+" bytes",#colorRed)
    EndIf
  Else
    Sprint(msginit$+"File ["+CurrentWsplitFile$+"] Not exist",#colorRed)
  EndIf
  
Wend

CreateThread(@runKangaroo(), @kangarooparams)

Repeat
  If FileSize(GetJobFile(settings("1")\file$))<0
    ;Sprint(msginit$+"File "+settings("1")\file$+" is not exist yet", #colorDefault)
    Debug "File is not exist yet"
    While  FileSize(GetJobFile(settings("1")\file$))<0
      If IsFindKey=#True
        Break
      EndIf
      Delay(1000)
    Wend
    ;Sprint(msginit$+"File "+GetJobFile(settings("1")\file$)+" already created", #colorDefault)
  EndIf
  CurrentWsplitFile$ = GetJobFile(settings("1")\file$)
  If FileSize(CurrentWsplitFile$)<= settings("1")\minimumFileSizeMB
    Debug "File is in process"
    While  FileSize(CurrentWsplitFile$)<= settings("1")\minimumFileSizeMB
      If IsFindKey=#True
        Break
      EndIf
      Delay(1000)
    Wend
    ;Sprint(msginit$+"File "+CurrentWsplitFile$+" ready for reading", #colorDefault)
  EndIf
curfilemodified = GetFileDate(CurrentWsplitFile$, #PB_Date_Modified)
;If prefilemodified=0
  ;prefilemodified = curfilemodified
  ;Sprint(msginit$+"File "+settings("1")\file$+" init modified ["+FormatDate("%hh:%ii:%ss %yyyy/%mm/%dd", curfilemodified)+"]", #colorDefault)
;EndIf
If curfilemodified<>prefilemodified
  ;Sprint(msginit$+"["+CurrentWsplitFile$+"] Await 10s", #colorDefault)
  Delay(10*1000)
  curfilemodified = GetFileDate(CurrentWsplitFile$, #PB_Date_Modified)
  full_size = FileSize(CurrentWsplitFile$)
  If full_size>0
    computeCRC32file = CRC32FileFingerprint(CurrentWsplitFile$)
    *MemoryBuffer=AllocateMemory(full_size)
    If *MemoryBuffer
      If Not ReadFileToMemory(*MemoryBuffer, CurrentWsplitFile$, full_size)
        computeCRC32memory = CRC32Fingerprint(*MemoryBuffer, full_size)
        If LCase(Hex(computeCRC32file))= LCase(Hex(computeCRC32memory))
          ;Sprint(msginit$+"File "+CurrentWsplitFile$+" was load, modified ["+FormatDate("%hh:%ii:%ss %yyyy/%mm/%dd", curfilemodified)+"], CRC32="+Hex(computeCRC32file)+", size:"+Str(full_size), #colorDefault)
          
          ;Repeat
            starttime=ElapsedMilliseconds()
            err = sendfiletohost(*MemoryBuffer,full_size, computeCRC32file)
            If Not err
              ;Print(Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+"100%"+Chr(13))
              PrintN("")
              Sprint(msginit$+"File "+CurrentWsplitFile$+" sended in "+Str((ElapsedMilliseconds()-starttime)/1000)+" s", #colorBrown)
              prefilemodified = curfilemodified
              If FileSize(CurrentWsplitFile$)>=0
                DeleteFile(CurrentWsplitFile$)
              EndIf
            Else
              Select err
                Case 1
                  PrintN("")
                  Sprint(msginit$+"Server disconnected, try after 30s",#colorRed)
                  If Left(CurrentWsplitFile$, 5)<>"wait_"
                    RenameFile(CurrentWsplitFile$,"wait_"+Str(Date())+"_"+CurrentWsplitFile$)
                  EndIf
                  Delay(30*1000)
                Case 2
                  PrintN("")
                  Sprint(msginit$+"Server not connected, try after 30s",#colorRed) 
                  If Left(CurrentWsplitFile$, 5)<>"wait_"
                    RenameFile(CurrentWsplitFile$,"wait_"+Str(Date())+"_"+CurrentWsplitFile$)
                  EndIf
                  Delay(30*1000)
                Case 3
                  PrintN("")
                  Sprint(msginit$+"Key already founded by host",#colorRed)
                  IsFindKey=#True
                  If ProgramRunning(CompilerKangaroo)
                    KillProgram(CompilerKangaroo)
                  EndIf                  
              EndSelect
            EndIf
          ;Until err=0 Or err = 3
          If IsFindKey=#True
             Break
          EndIf
        Else
          PrintN("")
          Sprint(msginit$+"CRC32 file: "+LCase(Hex(computeCRC32file))+" not equil to CRC32 memory: "+LCase(Hex(computeCRC32memory)),#colorRed)
        EndIf
      Else
        PrintN("")
        Sprint(msginit$+"Loadinf file "+CurrentWsplitFile$+" failed",#colorRed)
      EndIf
    Else
      PrintN("")
      Sprint(msginit$+"Can`t allocate memory buffer "+Str(full_size)+" bytes",#colorRed)
    EndIf
  Else
    PrintN("")
    Sprint(msginit$+"File ["+CurrentWsplitFile$+"] Not exist",#colorRed)
  EndIf    
  
  If *MemoryBuffer
    FreeMemory(*MemoryBuffer)
  EndIf
Else
  Debug "File did not changed["+FormatDate("Y=%yyyy, M= %mm, D=%dd %hh:%ii:%ss", curfilemodified)+"]"
EndIf

If IsFindKey=#True
  Break
EndIf
Delay(1000)
ForEver


End

; IDE Options = PureBasic 5.31 (Windows - x64)
; ExecutableFormat = Console
; CursorPosition = 1098
; FirstLine = 1088
; Folding = ----
; EnableThread
; EnableXP
; Executable = EtarkangarooClient.exe
; DisableDebugger