EnableExplicit 
IncludeFile "lib\Curve64.pb"
Structure ClientInfoStructure  
  authorization.i  
  time.i
  ClientId.i
  ClientIP.i
  name.s  
  quit.i
  isSend.i
  FileCRC32.s
  Filesize.i
  FileUploaded.i
  FileName.s
EndStructure

Structure ServerStructure
  Quit.i
  Map Client.ClientInfoStructure()
EndStructure

Structure SocketStructure
  id.i
  ClientId.i
  req.s  
EndStructure

Structure CollideStructure 
  pos1.i
  hashhex1.s
  distancehex1.s
  pos2.i
  hashhex2.s
  distancehex2.s
EndStructure  

Structure comparsationStructure
  pos.i
  direction.i
EndStructure

Structure HashTableResultStructure   
 size.l
 *contentpointer
 fileoffset.i
EndStructure

Structure settingsStructure
  host.s    
  port.i 
  saveworkfilename.s
  programmname.s
  getworkrangeBegin.s
  getworkrangeEnd.s
  getworkPub.s
  getworkDP.i
  getworkjobsaveinterval.i
  tg_botid.s
  tg_myid.s
  htpow.i
  checkDP.i
EndStructure

Structure jobsterStructure
  count.i
  firsttime.i
  lasttime.i
EndStructure

Structure JobSturucture
  beginNumber.i
  totalchecks.i
  POSinWildArray.i
  procname$
  sourcefile$
EndStructure

  
#MB=1048576
#GB=1073741824

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

#HashTablesz=4;4B counter items
#Pointersz=8
#HashTableSizeHash = 8
#HashTableSizeItems = 40

#TAME =0
#WILD =1

#HEADERSIZE=172
#HEAD="FA6A8002"
Enumeration
  #Server
  #File
  #LOGFILE
  #dirtemp
  #binfile
EndEnumeration
#FileCheck=777
#File1=778
#File2=779
#FileT=780

#align_size=128


Define stratumwork_g
Define Server.ServerStructure
Define NewMap settings.settingsStructure()
Define NewMap socketreq.SocketStructure()
Define NewList  filejob.s()
Define NewMap TG_Hook.s(), hookenabled_g=0
Define MutexConsole, MutexClientMap, socketreqMutex, FileMutex, LogMutex, SomeMutex
Define IsFindKey=#False
Define get_work_send_string.s
Define NewMap jobster.jobsterStructure(), NewMap job.JobSturucture()
Define totallaunched, totalcheckscompleted
Define BitRange, *RangeBegin, *RangeEnd

Define *CurveP, *CurveGX, *CurveGY, *Curveqn
*CurveP = Curve::m_getCurveValues()
*CurveGX = *CurveP+32
*CurveGY = *CurveP+64
*Curveqn = *CurveP+96

Define *GTable

MutexConsole = CreateMutex()
MutexClientMap = CreateMutex()
socketreqMutex = CreateMutex()
FileMutex = CreateMutex()
LogMutex = CreateMutex()
SomeMutex = CreateMutex()

Procedure exit(a$)
  
  PrintN(a$)  
  ;Input()
  CloseConsole()  
  End
EndProcedure

Procedure SPrint(text$, cl)
 Shared MutexConsole
  LockMutex(MutexConsole)
  ConsoleColor(cl,0)
  Debug FormatDate("%hh:%ii:%ss ", Date())+" "+text$
  PrintN(FormatDate("%hh:%ii:%ss ", Date())+" "+text$)  
  ConsoleColor(#colorDefault,0)
  UnlockMutex(MutexConsole)
EndProcedure

Procedure ValueL(*a)
  !mov rbx,[p.p_a]   
  !mov eax,[rbx]  
ProcedureReturn
EndProcedure

Procedure.s commpressed2uncomressedPub(ha$)
  Protected y_parity, ruc$, x$, a$, *a, *res
  Shared *CurveP
  *a = AllocateMemory(64)  
  *res=*a + 32  
  
  y_parity = Val(Left(ha$,2))-2
  x$ = Right(ha$,Len(ha$)-2)
  
  a$=RSet(x$, 64,"0")
  Curve::m_sethex32(*a, @a$)  
  Curve::m_YfromX64(*res,*a, *CurveP)  
  
  If PeekB(*res)&1<>y_parity
    Curve::m_subModX64(*res,*CurveP,*res,*CurveP)
  EndIf
  
  ruc$ = Curve::m_gethex32(*res)
  
  FreeMemory(*a)
  ProcedureReturn x$+ruc$

EndProcedure

Procedure.s uncomressed2commpressedPub(ha$)
  Protected Str1.s, Str2.s, x$,y$,ru$,rc$
  ha$=LCase(ha$)
  If Left(ha$,2)="04" And Len(ha$)=130
    ha$=Right(ha$,Len(ha$)-2)
  EndIf
  Str1=Left(ha$,64)
  Str2=Right(ha$,64)
  Debug Str1
  Debug Str2
  
  x$=PeekS(@Str1,-1,#PB_Ascii)
  x$=RSet(x$,64,"0")
  y$=PeekS(@Str2,-1,#PB_Ascii)
  y$=RSet(y$,64,"0")
  ru$="04"+x$+y$
  If FindString("13579bdf",Right(y$,1))>0
    rc$="03"+x$
  Else
    rc$="02"+x$
  EndIf
  
  ProcedureReturn rc$

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

#INTERNET_DEFAULT_HTTP_PORT = 80
#INTERNET_DEFAULT_HTTPS_PORT = 443

#INTERNET_OPEN_TYPE_DIRECT = 1
#INTERNET_OPEN_TYPE_PROXY = 3

#INTERNET_SERVICE_HTTP = 3

#INTERNET_FLAG_PRAGMA_NOCACHE =           $00000100
#INTERNET_FLAG_IGNORE_CERT_CN_INVALID =   $00001000
#INTERNET_FLAG_IGNORE_CERT_DATE_INVALID = $00002000
#INTERNET_FLAG_NO_COOKIES =               $00080000
#INTERNET_FLAG_NO_AUTO_REDIRECT =         $00200000
#INTERNET_FLAG_SECURE =                   $00800000
#INTERNET_FLAG_DONT_CACHE =               $04000000


#HTTP_ADDREQ_FLAG_ADD =     $20000000
#HTTP_ADDREQ_FLAG_REPLACE = $80000000


#ERROR_INTERNET_INVALID_CA = 12045

#INTERNET_OPTION_SECURITY_FLAGS = 31

#SECURITY_FLAG_IGNORE_UNKNOWN_CA = $00000100

Procedure.s do_request(host.s,port, page.s="", post_data.s="",  cookie.s="", is_secure.i=#False, user_agent.s="", referer.s="", proxy.s="", timeout.l=1000, redirect.i=#True)
 
  Protected.l flags, bytes_read, dwFlags, dwBuffLen
  Protected.i open_handle, connect_handle, request_handle, send_handle, Ok, access_type, LastError
  Protected.s verb, headers, buffer, result
 
  
  If proxy = "" : access_type =  #INTERNET_OPEN_TYPE_DIRECT : Else : access_type = #INTERNET_OPEN_TYPE_PROXY : EndIf
  If user_agent = "" : user_agent = "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)" : EndIf
  open_handle = InternetOpen_(user_agent, access_type, "", "", 0)
 
  InternetSetOption_(open_handle, 2, timeout, 4)
 
  flags = #INTERNET_FLAG_PRAGMA_NOCACHE
  flags | #INTERNET_FLAG_NO_COOKIES
  flags | #INTERNET_FLAG_DONT_CACHE
 
  If is_secure
    port = #INTERNET_DEFAULT_HTTPS_PORT
   
    flags | #INTERNET_FLAG_SECURE
    flags | #INTERNET_FLAG_IGNORE_CERT_CN_INVALID
    flags | #INTERNET_FLAG_IGNORE_CERT_DATE_INVALID
  Else
    If port=0
      port = #INTERNET_DEFAULT_HTTP_PORT
    EndIf
  EndIf
 
  
  connect_handle = InternetConnect_(open_handle, host, port, "", "", #INTERNET_SERVICE_HTTP, 0, 0)
  
 
  If Not redirect : flags | #INTERNET_FLAG_NO_AUTO_REDIRECT : EndIf
  If post_data = "" : verb = "GET" : Else : verb = "POST" : EndIf
  
  If page = "" : page = "/" : EndIf
  
  request_handle = HttpOpenRequest_(connect_handle, verb, page, "", referer, #Null, flags, 0)
 
  
  If verb = "POST"    
    headers = "Content-Type: application/json; charset=UTF-8" + #CRLF$
    HttpAddRequestHeaders_(request_handle, headers, Len(headers), #HTTP_ADDREQ_FLAG_ADD | #HTTP_ADDREQ_FLAG_REPLACE)
  EndIf
  If cookie <> ""
    headers = "Cookie: "+ cookie + #CRLF$
    HttpAddRequestHeaders_(request_handle, headers, Len(headers), #HTTP_ADDREQ_FLAG_ADD | #HTTP_ADDREQ_FLAG_REPLACE)
  EndIf
 
 
  Ok = 0
  Repeat
    send_handle = HttpSendRequest_(request_handle, #Null, 0, post_data, Len(post_data))
   
    If send_handle = 0
      LastError = GetLastError_()
     
      Debug ( "[doRerquest] Error " + Str(LastError))
     
      If LastError = #ERROR_INTERNET_INVALID_CA
       
        dwBuffLen = SizeOf(dwFlags)
       
        InternetQueryOption_(request_handle, #INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, @dwBuffLen)
       
        dwFlags | #SECURITY_FLAG_IGNORE_UNKNOWN_CA
        InternetSetOption_(request_handle, #INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, SizeOf(dwFlags))
        Ok + 1
      Else
        Ok = 2
      EndIf
    Else
      Ok = 2
    EndIf
  Until Ok = 2
 
 
  buffer = Space(1024)
  Repeat
    InternetReadFile_(request_handle, @buffer, 1024, @bytes_read)
    result + Left(buffer, bytes_read)
    buffer = Space(1024)
  Until bytes_read = 0
 
  InternetCloseHandle_(open_handle)
  InternetCloseHandle_(connect_handle)
  InternetCloseHandle_(request_handle)
  InternetCloseHandle_(send_handle)
 
  ProcedureReturn result
 
EndProcedure

Procedure makehook(text$)
 Protected answer$, resultanswer, pars_res_send,page$, answerdo$
 Shared settings()       
  Repeat        
    page$ =settings("1")\tg_botid+"/sendMessage?chat_id="+settings("1")\tg_myid+"&text="+text$          
    answerdo$ = do_request("api.telegram.org",0,page$, "","",#True,"","","",2000)
    answer$ = PeekS(@answerdo$, -1, #PB_Ascii)    
    pars_res_send = ParseJSON(#PB_Any, answer$)
    If pars_res_send                      
            If getElem(pars_res_send,"ok",0)
              resultanswer=1
              Sprint("<TELEGRAMM> HOOK WAS MADED",#colorbrightmagenta)
            Else
              Sprint("<TELEGRAMM> Can`t send data to server. Try again after 10s.."+#CRLF$+"BUF["+answer$+"]", #colorred)
              Delay(10000)
            EndIf
            FreeJSON(pars_res_send)
          Else
            Sprint("<TELEGRAMM> Server: can`t parse data. Try again after 10s.."+#CRLF$+"BUF["+answer$+"]", #colorred)
            Delay(10000)
          EndIf         
  Until resultanswer
EndProcedure

Procedure Hooktimer2s(i)
  Shared TG_Hook()
  
  Sprint("Telegramm hooker started",#colorCyan)
  
!ll_Hooktimer2slabel:
While MapSize(TG_Hook())
  ForEach TG_Hook()
    makehook(TG_Hook())
    DeleteMapElement(TG_Hook())
    Delay(1000)
  Next
Wend


Delay(1000*i)  

!jmp ll_Hooktimer2slabel
EndProcedure

Procedure getprogparam()
  Protected parametrscount, datares$, i
  Shared  settings()
  parametrscount=CountProgramParameters()
  
  i=0
  While i<parametrscount  
    Select LCase(ProgramParameter(i))
      Case "-port" 
        Debug "found -port"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\port = Val(datares$)
          Sprint( "-port "+Str(settings("1")\port),#colordefault)
        EndIf  
        
      Case "-checkdp" 
        Debug "found -checkdp"             
        settings("1")\checkDP = 1
        Sprint( "-checkdp "+Str(settings("1")\htpow),#colordefault)
        
      Case "-ht" 
        Debug "found -htpow"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\htpow = Val(datares$)
          Sprint( "-ht "+Str(settings("1")\htpow),#colordefault)
        EndIf  
        
      Case "-dp"
        Debug "found -dp"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\getworkDP = Val(datares$)
          Sprint( "-dp "+Str(settings("1")\getworkDP),#colordefault)
        EndIf
        
      Case "-wi"
        Debug "found -wi"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\getworkjobsaveinterval = Val(datares$)
          Sprint( "-wi "+Str(settings("1")\getworkjobsaveinterval),#colordefault)
        EndIf
        
      Case "-beginrange"
        Debug "found -beginrange"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\getworkrangeBegin = datares$
          Sprint( "-beginrange "+settings("1")\getworkrangeBegin,#colordefault)
        EndIf
        
      Case "-endrange"
        Debug "found -endrange"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\getworkrangeEnd = datares$
          Sprint( "-endrange "+settings("1")\getworkrangeEnd,#colordefault)
        EndIf 
        
      Case "-pub"
        Debug "found -pub"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\getworkPub = datares$
          Sprint( "-pub "+settings("1")\getworkPub,#colordefault)
        EndIf
        
      Case "-tgbotid"
        Debug "found -tgbotid"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\tg_botid = datares$
          Sprint( "-tgbotid "+settings("1")\tg_botid,#colordefault)
        EndIf 
        
      Case "-tgmyid"
        Debug "found -tgmyid"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\tg_myid = datares$
          Sprint( "-tgmyid "+settings("1")\tg_myid,#colordefault)
        EndIf  
        
    EndSelect
    i+1 
  Wend
EndProcedure

Procedure Socket_ClientDisconnect(ClientID.i, *Server.ServerStructure) 
  Protected procname$="[SERVER] "
  If FindMapElement(*Server\Client(), Str(ClientID))   
    If *Server\Client(Str(ClientID))\isSend=#True And *Server\Client(Str(ClientID))\FileUploaded<*Server\Client(Str(ClientID))\Filesize
      ;delete part of file
      DeleteFile(*Server\Client(Str(ClientID))\FileName)
      Debug "File "+*Server\Client(Str(ClientID))\FileName+" was deleted"
      Sprint(procname$ + "PROCESS FILE => ["+*Server\Client(Str(ClientID))\FileName+"] DELETED", #colorDefault)
    EndIf
    DeleteMapElement(*Server\Client(), Str(ClientID))   
    ;SPrint("TOTAL CLIENTS: " + Str(MapSize(*Server\Client())),#colorMagenta) 
  
EndIf
EndProcedure

 Procedure.s cutHex(a$)
  a$=Trim(UCase(a$)) 
  If Left(a$,2)="0X" 
    a$=Mid(a$,3,Len(a$)-2)
  EndIf 
  If Len(a$)=1
    a$="0"+a$
  EndIf
ProcedureReturn LCase(a$)
EndProcedure

Procedure m_check_equilX8(*s,*t)
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]    
  
  !mov rax,[rsi]
  !mov rbx,[rdi]
 
  !cmp rax,rbx
  !jne llm_check_equil_exit_noteqil8
   
  !mov rax,1
  !jmp llm_check_equil_exit8  
  
  !llm_check_equil_exit_noteqil8:
  !mov rax,0
  !llm_check_equil_exit8:
ProcedureReturn  
EndProcedure

Procedure.s m_gethex8(*bin)  
  Protected *sertemp=AllocateMemory(16, #PB_Memory_NoClear)
  Protected res$  
  ;************************************************************************
  ;Convert bytes in LITTLE indian format to HEX string in BIG indian format
  ;************************************************************************ 
  Curve::m_serializeX64(*bin,0,*sertemp,2)  
  res$=PeekS(*sertemp,16, #PB_Ascii)
  FreeMemory(*sertemp)
ProcedureReturn res$
EndProcedure

Procedure ReturnIdentify(*pointer) 
  ProcedureReturn Curve::m_Ecc_TestBitX64(*pointer, 255)
EndProcedure

Procedure check_LME32bit(*s,*t)
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]
    
  !mov eax,[rsi]
  !cmp eax,[rdi]
  !jb llm_LME32bit_exit_less
  !ja llm_LME32bit_exit_more  
   
  !xor eax,eax
  !jmp llm_LME32bit_exit  
  
  !llm_LME32bit_exit_more:
  !mov eax,2
  !jmp llm_LME32bit_exit  
  
  !llm_LME32bit_exit_less:
  !mov eax,1
  !llm_LME32bit_exit:
ProcedureReturn  
EndProcedure
Procedure check_LME64bit(*s,*t)
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]
    
  !mov rax,[rsi]
  !cmp rax,[rdi]
  !jb llm_LME64bit_exit_less
  !ja llm_LME64bit_exit_more  
   
  !xor rax,rax
  !jmp llm_LME64bit_exit  
  
  !llm_LME64bit_exit_more:
  !mov rax,2
  !jmp llm_LME64bit_exit  
  
  !llm_LME64bit_exit_less:
  !mov rax,1
  !llm_LME64bit_exit:
ProcedureReturn  
EndProcedure

Procedure findInHashTable64bit(*findvalue, *arr, beginrange, endrange, *res.comparsationStructure)
  Protected temp_beginrange, temp_endrange, rescmp,   exit.b, center
  
  temp_beginrange = beginrange
  temp_endrange = endrange

  While (endrange-beginrange)>=0
    If beginrange=endrange
      If endrange<=temp_endrange
        ;0 - s = t, 1- s < t, 2- s > t
        rescmp = check_LME64bit(*findvalue,*arr + beginrange * #HashTableSizeItems)
        ;Debug "cmp "+get64bithash(*findvalue)+" - "+get64bithash(*arr + beginrange * #HashTableSizeItems)+" = "+Str(rescmp)
        If rescmp=2;more
          *res\pos=-1
          *res\direction=endrange+1
          exit=1
          Break
        ElseIf rescmp=1;less
          If endrange>0
            *res\pos=-1
            *res\direction=endrange
            exit=1
            Break
          Else
            *res\pos=-1
            *res\direction=0
            exit=1
            Break
          EndIf
        Else;equil
          *res\pos=beginrange
          *res\direction=0
          exit=1
          Break
        EndIf
      Else
        Debug("Unknown exeptions")        
      EndIf
    EndIf
    center=(endrange-beginrange)/2+beginrange    
    rescmp = check_LME64bit(*findvalue,*arr + center * #HashTableSizeItems)
    ;Debug "cmp "+get64bithash(*findvalue)+" - "+get64bithash(*arr + beginrange * #HashTableSizeItems)+" = "+Str(rescmp)
    If rescmp=2;more
      If (center+1)<=endrange:
        beginrange=center+1
      Else
        beginrange=endrange
      EndIf
    ElseIf rescmp=1;less
      If (center-1)>=beginrange:
        endrange=center-1
      Else
        endrange=beginrange
      EndIf
    Else;equil
      *res\pos=center
      *res\direction=0
      exit=1
      Break
    EndIf
  Wend
  If exit=0
    If beginrange=temp_endrange:
        *res\pos=-1
        *res\direction=1 
    Else
        *res\pos=-1
        *res\direction=-1
    EndIf
  EndIf
EndProcedure

Procedure findInHashTable64bitSimple(*findvalue, *arr, beginrange, endrange, *res.comparsationStructure)
  Protected temp_beginrange, temp_endrange, rescmp,   exit.b, center
  
  *res\pos=-1
  While endrange>=beginrange   
    center= beginrange+(endrange-beginrange)/2
    rescmp = check_LME64bit(*findvalue,*arr + center * #HashTableSizeItems)   
    If rescmp=2;more
      beginrange=center+1
    ElseIf rescmp=1;less
      endrange=center-1
    Else ;equil
      *res\pos=center
      Break
    EndIf     
  Wend 
EndProcedure

Procedure sortHashTable64bit(*arr, totalines, *colissionMap.CollideStructure)
  Protected err, i, rescmp,*temp, *INShash,pos, res.comparsationStructure
  *temp=AllocateMemory(#HashTableSizeItems)
  
  
  pos = 0
  While pos<totalines-1 And err=0
      *INShash = *arr+(pos+1) * #HashTableSizeItems
      findInHashTable64bit(*INShash, *arr, 0, pos, @res.comparsationStructure)
      ;Debug "pos:"+Str(pos)
      ;Debug get64bithash(*INShash)
      ;Debug "res\pos:"+Str(res\pos)+"res\dir:"+Str(res\direction)
      If res\pos=-1
        ;that mean that value is Not found in range
        If res\direction>pos
          pos=res\direction
          CopyMemory(*INShash, *arr + pos * #HashTableSizeItems, #HashTableSizeItems)       
        Else
          ;move block forward
          ;PrintN("move block")
          pos+1
          CopyMemory(*INShash, *temp, #HashTableSizeItems)
          CopyMemory(*arr + res\direction * #HashTableSizeItems, *arr + res\direction * #HashTableSizeItems + #HashTableSizeItems, (pos-res\direction) * #HashTableSizeItems)
          CopyMemory(*temp, *arr + res\direction * #HashTableSizeItems, #HashTableSizeItems)        
        EndIf
      Else
        err=1
        ;PrintN("["+Str(pos+1)+"] Value exist!!!>"+Curve::m_gethex32(*INShash+#HashTableSizeHash))
        ;PrintN("["+Str(res\pos)+"] Value exist!!!>"+Curve::m_gethex32(*arr + res\pos * #HashTableSizeItems+#HashTableSizeHash))
        *colissionMap\pos1 = pos+1
        *colissionMap\hashhex1 = m_gethex8(*INShash)
        *colissionMap\distancehex1 = Curve::m_gethex32(*INShash+#HashTableSizeHash)
        
        *colissionMap\pos2 = res\pos
        *colissionMap\hashhex2 = m_gethex8(*arr + res\pos * #HashTableSizeItems)
        *colissionMap\distancehex2 = Curve::m_gethex32(*arr + res\pos * #HashTableSizeItems+#HashTableSizeHash)
        
       Break
      EndIf
      ;For i =0 To totalines-1
        ;Debug ("["+Str(i)+"] "+get64bithash(*arr + i * #HashTableSizeItems))  
      ;Next i
  Wend  
  
  FreeMemory(*temp)
 ProcedureReturn err
EndProcedure 

Procedure.s getElapsedTime(timeS)
  Protected dd,hh,mm,ss, timeStemp, a$
  timeStemp = timeS
  dd = timeStemp / 86400
  timeStemp - dd * 86400
  hh = timeStemp / 3600
  timeStemp - hh * 3600
  mm = timeStemp / 60
  ss = timeStemp - mm * 60
  If dd
    a$ = Str(dd)+"d "+Str(hh)+"h "+Str(mm)+"m "+Str(ss)+"s"
  Else
    If hh
      a$ = Str(hh)+"h "+Str(mm)+"m "+Str(ss)+"s"
    Else
      If mm
        a$ = Str(mm)+"m "+Str(ss)+"s"
      Else
        a$ = Str(ss)+"s"
      EndIf
    EndIf
  EndIf
ProcedureReturn a$
EndProcedure

Procedure writeDataToFile(fileID, *buffFrom, sz, *tempbuffer, isFinal = 0, isResetStatic = 0)
  Static bufusedsize
  If isResetStatic = 1
    bufusedsize = 0
  Else
    If isFinal = 0
      If bufusedsize + sz < 100*#MB
        CopyMemory(*buffFrom, *tempbuffer + bufusedsize, sz)
        bufusedsize + sz
      Else
        If WriteData(fileID, *tempbuffer, bufusedsize)<>bufusedsize
          ;error during saving
          sz = 0
        Else 
          bufusedsize = 0
          If bufusedsize + sz < 100*#MB
            CopyMemory(*buffFrom, *tempbuffer, sz)
            bufusedsize + sz
          Else
            exit("Amount of data bigger then temporary buffer size")
          EndIf
        EndIf
      EndIf
    Else
      ;final, save what we have
      If bufusedsize
        If WriteData(fileID, *tempbuffer, bufusedsize)<>bufusedsize
          ;error during saving
          sz = 0        
        EndIf
      EndIf
      bufusedsize = 0
    EndIf
  EndIf
  
ProcedureReturn sz
EndProcedure

Procedure closeAllMergeFiles()
  CloseFile(#File1)
  CloseFile(#File2)
  CloseFile(#FileT)
EndProcedure

Procedure.s mergeHTFilesNew(filename1$, filename2$, filenameTarget$)

  Protected procname$ = "[MERGER INTERNAL] "
  Protected *MemoryBuffer0, *MemoryBuffer1, *MemoryBuffer2, *headerbuff1,  *headerbuff2, batchsize, i, hashcurrent, hash1, hash2, sz1, sz2, szT, sz1_2, lengthFile1, lengthFile2, pos1, pos2, _res
  Protected dpcount1, dpcount2, dpcountT, endfile, identify1, identify2, CollisionFlag, dead, isOk, copypos, *tamedistance, *wilddistance
  Protected collision.CollideStructure, starttime
  Protected *temp = AllocateMemory(480), *x = *temp + 32, *y = *temp + 64, *wilddisttemp = *temp + 96
  Protected *RangeB_l = *temp + 128, *ShiftedRangeEhalf_l = *temp + 160,  *ShiftedFindPub_X_l = *temp + 192, *ShiftedFindPub_Y_l = *temp + 224
  Protected *FindPub_X_l = *temp + 256,  *FindPub_Y_l = *temp + 288, *RangeE_l = *temp + 320, *PubRangeB_X_l = *temp + 352, *PubRangeB_Y_l = *temp + 384
  Protected *PubRangeB_Y_neg_l = *temp + 416, *ShiftedRangeE_l = *temp + 448, Output$="", deadcount1, timecount1, deadcount2, timecount2, dpsize1, *tempFilebuffer, filesizeTargetFile
  Protected filesizeTargetFile$
  Shared  IsFindKey, *CurveGX, *CurveGY ,*CurveP, *Curveqn
  
starttime = ElapsedMilliseconds()  
batchsize = 30*#MB

*tempFilebuffer = AllocateMemory(100*#MB)
If Not *tempFilebuffer
  exit("Can`t allocate memory")
EndIf
  ;reset file writer
  ;writeDataToFile(#FileT, 0, 0, 0, 0, 1)
  
If FileSize(filename1$)<#HEADERSIZE
  exit("File "+filename1$+" does not exist or too small")
EndIf
If FileSize(filename2$)<#HEADERSIZE
  exit("File "+filename2$+" does not exist or too small")
EndIf

*headerbuff1 = AllocateMemory(#HEADERSIZE * 2)
*headerbuff2 = *headerbuff1 + #HEADERSIZE

*MemoryBuffer0 = AllocateMemory(batchsize*3+64)
If Not *MemoryBuffer0
  exit("Can`t allocate memory")
EndIf

*MemoryBuffer1 = *MemoryBuffer0 + 64

*MemoryBuffer2 = *MemoryBuffer1 + batchsize*2


If Not OpenFile(#File1, filename1$)
  exit("Can`t open "+filename1$)
EndIf
If Not OpenFile(#File2, filename2$)
  CloseFile(#File1)
  exit("Can`t open "+filename2$)
EndIf


ReadData(#File1, *headerbuff1, #HEADERSIZE)  
ReadData(#File2, *headerbuff2, #HEADERSIZE) 
If  Not CompareMemory(*headerbuff1, *headerbuff2 ,148)  
  CloseFile(#File1)
  CloseFile(#File2)
  exit("Header data is not same in files")
EndIf
If Hex(ValueL(*headerbuff1))<>#HEAD
  CloseFile(#File1)
  CloseFile(#File2)
  exit("Wrong header format")
EndIf

If Not CreateFile(#FileT, filenameTarget$+".temp" )  
  CloseFile(#File1)
  CloseFile(#File2)
  exit("Can`t creat "+filenameTarget$+".temp")     
EndIf

dpsize1=Valuel(*headerbuff1 + 8)
SPrint(procname$+"DP size: "+Str(Valuel(*headerbuff1 + 8)),#colorDarkgrey)
SPrint(procname$+"HT size: "+Str(PeekI(*headerbuff1 + 140)),#colorDarkgrey)
If Curve::m_check_nonzeroX64(*headerbuff1 + 12)
  SPrint(procname$+"RB: "+LTrim(Curve::m_gethex32(*headerbuff1 + 12),"0"),#colorDarkgrey)
Else
  SPrint(procname$+"RB: 0",#colorDarkgrey)
EndIf
If Curve::m_check_nonzeroX64(*headerbuff1 + 44)
  SPrint(procname$+"RE: "+LTrim(Curve::m_gethex32(*headerbuff1 + 44),"0"),#colorDarkgrey)
Else
  SPrint(procname$+"RE: 0",#colorDarkgrey)
EndIf
SPrint(procname$+"PUB: "+uncomressed2commpressedPub(Curve::m_gethex32(*headerbuff1 + 76)+Curve::m_gethex32(*headerbuff1 + 108)),#colorDarkgrey)

dpcount1 = PeekI(*headerbuff1 + 148)
deadcount1 = PeekI(*headerbuff1 + 156)
timecount1 = PeekI(*headerbuff1 + 164)
SPrint(procname$+"DP count1  : "+Str(dpcount1),#colorDarkgrey)
SPrint(procname$+"Dead count1: "+Str(deadcount1),#colorDarkgrey)
SPrint(procname$+"Time count1: "+getElapsedTime(timecount1),#colorDarkgrey)

dpcount2 = PeekI(*headerbuff2 + 148)
deadcount2 = PeekI(*headerbuff2 + 156)
timecount2 = PeekI(*headerbuff2 + 164)
SPrint(procname$+"DP count2  : "+Str(dpcount2),#colorDarkgrey)
SPrint(procname$+"Dead count2: "+Str(deadcount2),#colorDarkgrey)
SPrint(procname$+"Time count2: "+getElapsedTime(timecount2),#colorDarkgrey)

CopyMemory(*headerbuff1 + 12, *RangeB_l, 32)
CopyMemory(*headerbuff1 + 44, *RangeE_l, 32)

CopyMemory(*headerbuff1 + 76, *FindPub_X_l, 32)
CopyMemory(*headerbuff1 + 108, *FindPub_Y_l, 32)

CopyMemory(*FindPub_X_l, *ShiftedFindPub_X_l, 32)
CopyMemory(*FindPub_Y_l, *ShiftedFindPub_Y_l, 32)
If Curve::m_check_nonzeroX64(*RangeB_l)
  
  ;if begining range is not zero, substruct range from findpub
  Curve::m_PTMULX64(*PubRangeB_X_l, *PubRangeB_Y_l, *CurveGX, *CurveGY, *RangeB_l, *CurveP)
  Curve::m_subModX64(*PubRangeB_Y_neg_l, *CurveP, *PubRangeB_Y_l, *CurveP)
  Curve::m_ADDPTX64(*ShiftedFindPub_X_l, *ShiftedFindPub_Y_l, *ShiftedFindPub_X_l, *ShiftedFindPub_Y_l, *PubRangeB_X_l, *PubRangeB_Y_neg_l, *CurveP)
  ;SPrint(procname$+"Shifted Find X:"+Curve::m_gethex32(*ShiftedFindPub_X_l),#colorDarkgrey)
  ;SPrint(procname$+"Shifted Find Y:"+Curve::m_gethex32(*ShiftedFindPub_Y_l),#colorDarkgrey)
EndIf

Curve::m_subX64(*ShiftedRangeE_l,*RangeE_l,*RangeB_l)

CopyMemory(*ShiftedRangeE_l, *ShiftedRangeEhalf_l, 32)
Curve::m_shrX64(*ShiftedRangeEhalf_l)
;SPrint(procname$+ "Shifted Range half     :"+Curve::m_gethex32(*ShiftedRangeEhalf_l),#colorDarkgrey)

If writeDataToFile(#FileT, *headerbuff1, #HEADERSIZE, *tempFilebuffer, 0, 0)<>#HEADERSIZE
  closeAllMergeFiles()
  exit("Error during writing file")
EndIf
      
      
lengthFile1 = Lof(#File1)
lengthFile2 = Lof(#File2)

pos1 = #HEADERSIZE-1
pos2 = #HEADERSIZE-1

If pos1 + 8<lengthFile1
  If ReadData(#File1, *MemoryBuffer1, 8)<>8
    closeAllMergeFiles()
    exit("Error during reading file")
  EndIf
  hash1 = ValueL(*MemoryBuffer1)
  sz1 =   ValueL(*MemoryBuffer1 + 4)
  pos1+8  
  If pos1 + sz1 * #HashTableSizeItems < lengthFile1    
    If ReadData(#File1, *MemoryBuffer1 + 8, sz1 * #HashTableSizeItems) <> sz1 * #HashTableSizeItems   
      closeAllMergeFiles()
      exit("Error during reading file")
    EndIf 
    pos1 + sz1 * #HashTableSizeItems 
  Else
    closeAllMergeFiles()
    exit("Unexpected end of file")
  EndIf
Else
  closeAllMergeFiles()
  exit("File empty")
EndIf

If pos2 + 8<lengthFile2
  If ReadData(#File2, *MemoryBuffer2, 8)<>8
    closeAllMergeFiles()
    exit("Error during reading file")
  EndIf
  hash2 = ValueL(*MemoryBuffer2)
  sz2 =   ValueL(*MemoryBuffer2 + 4)
  pos2+8  
  If pos2 + sz2 * #HashTableSizeItems<lengthFile2    
    If ReadData(#File2, *MemoryBuffer2 + 8, sz2 * #HashTableSizeItems) <> sz2 * #HashTableSizeItems   
      closeAllMergeFiles()
      exit("Error during reading file")
    EndIf 
    pos2 + sz2 * #HashTableSizeItems 
  Else
    closeAllMergeFiles()
    exit("Unexpected end of file")
  EndIf
Else
  closeAllMergeFiles()
  exit("File empty")
EndIf

endfile=0


While endfile=0 And CollisionFlag=0
  
  _res = check_LME32bit(*MemoryBuffer1, *MemoryBuffer2)
  If _res=0 
    
    ;ht1 = ht2 
    sz1_2 = sz1 + sz2
    CopyMemory(*MemoryBuffer2 + 8, *MemoryBuffer1 + 8 + sz1 * #HashTableSizeItems, sz2 * #HashTableSizeItems)
    isOk = 0
    
    
    Repeat
      PokeL(*MemoryBuffer1 + 4, sz1_2 )
      If sortHashTable64bit(*MemoryBuffer1 + 8, sz1_2, @collision)
        ;PrintN("size:"+Str(sz1)+"size:"+Str(sz2)+"size:"+Str(sz1_2))
        ;For i = 0 To sz1_2-1
          ;PrintN(Curve::m_gethex32(*MemoryBuffer1 + 8 + i*#HashTableSizeItems + #HashTableSizeHash))
        ;Next i
    
        
        
        Curve::m_sethex32(*MemoryBuffer0, @collision\distancehex1)
        Curve::m_sethex32(*MemoryBuffer0 + 32, @collision\distancehex2)
        identify1 = ReturnIdentify(*MemoryBuffer0)
        identify2 = ReturnIdentify(*MemoryBuffer0+32)
        
        ;PrintN("["+Str(collision\pos1)+"] Hash1: "+collision\hashhex1+" dist1:" + collision\distancehex1 + " ["+Str(identify1)+"]")
        ;PrintN("["+Str(collision\pos2)+"] Hash2: "+collision\hashhex2+" dist2:" + collision\distancehex2 + " ["+Str(identify2)+"]")
        
        
        If identify1<>identify2
          If identify1 = #TAME
            *tamedistance = *MemoryBuffer0
            *wilddistance = *MemoryBuffer0 + 32            
          Else
            *tamedistance = *MemoryBuffer0 + 32
            *wilddistance = *MemoryBuffer0
          EndIf
          ;PrintN("")
          
          ;PrintN("Tame dist: "+Curve::m_gethex32(*tamedistance))
          ;PrintN("Wild dist: "+Curve::m_gethex32(*wilddistance))
          
          CopyMemory(*wilddistance, *wilddisttemp, 32)
          ;reset bit #WILD
          !mov rsi,[p.p_wilddisttemp]
          !mov eax,[rsi+28]
          !and eax,0x7fffffff
          !mov [rsi+28],eax    
          Curve::m_addModX64(*temp,*tamedistance,*ShiftedRangeEhalf_l, *Curveqn)
          Curve::m_subModX64(*temp,*temp,*wilddisttemp, *Curveqn)
          Curve::m_PTMULX64(*x, *y, *CurveGX, *CurveGY, *temp ,*CurveP)
          ;PrintN("X:"+Curve::m_gethex32(*x))
          ;PrintN("Y:"+Curve::m_gethex32(*y))
          ;PrintN("Shifted Find X:"+Curve::m_gethex32(*ShiftedFindPub_X_l))
          ;PrintN("Shifted Find Y:"+Curve::m_gethex32(*ShiftedFindPub_Y_l))
          ;PrintN("Shifted Key: "+Curve::m_gethex32(*temp))
          If Curve::m_check_nonzeroX64(*RangeB_l)
            Curve::m_addModX64(*temp,*temp,*RangeB_l, *Curveqn)
            Curve::m_PTMULX64(*x, *y, *CurveGX, *CurveGY, *temp ,*CurveP)   
          EndIf
          ;PrintN("Fin X:"+Curve::m_gethex32(*x))
          ;PrintN("Fin Y:"+Curve::m_gethex32(*y))
          ;PrintN("Priv: "+Curve::m_gethex32(*temp))
          If Curve::m_check_equilX64(*x,*FindPub_X_l) And Curve::m_check_equilX64(*y,*FindPub_Y_l)
            PrintN("")
            Sprint(procname$+"Pub: "+uncomressed2commpressedPub(Curve::m_gethex32(*x)+Curve::m_gethex32(*y)),#colorWhite)    
            Sprint(procname$+" Priv: 0x"+LTrim(Curve::m_gethex32(*temp),"0"),#colorWhite)           
            Output$ = " Priv: 0x"+LTrim(Curve::m_gethex32(*temp),"0")            
            
            CollisionFlag=1 
            isOk = 1
            ;PrintN("Collision found")
            IsFindKey=#True
            PrintN("")
            SPrint(procname$+"Merging aborted", #colorDarkgrey)
          Else
            Sprint("Tame dist: "+Curve::m_gethex32(*tamedistance), #colorRed)
            Sprint("Wild dist: "+Curve::m_gethex32(*wilddistance), #colorRed)
            closeAllMergeFiles()
            exit("False collision")
          EndIf
        Else
          ;colission in the same herdz   
          copypos = collision\pos2
          If collision\pos1<copypos
            copypos = collision\pos1
          EndIf
          sz1_2 - 1
          dead + 1          
          CopyMemory(*MemoryBuffer1 + 8 + (copypos+1) * #HashTableSizeItems, *MemoryBuffer1 + 8 + copypos * #HashTableSizeItems, (sz1_2 - copypos) * #HashTableSizeItems)
          
          
          
        EndIf          
      Else
        isOk = 1
      EndIf
    Until isOk = 1     
    If CollisionFlag=0
      If writeDataToFile(#FileT, *MemoryBuffer1, sz1_2 * #HashTableSizeItems + 8, *tempFilebuffer, 0, 0)<>sz1_2 * #HashTableSizeItems + 8
        closeAllMergeFiles()
        exit("Error during writing file")
      EndIf
      dpcountT + sz1_2
      
      If pos1 + 8<lengthFile1
        If ReadData(#File1, *MemoryBuffer1, 8)<>8
          closeAllMergeFiles()
          exit("Error during reading file")
        EndIf
        hash1 = ValueL(*MemoryBuffer1)
        sz1 =   ValueL(*MemoryBuffer1 + 4)
        pos1+8  
        If pos1 + sz1 * #HashTableSizeItems<lengthFile1          
          If ReadData(#File1, *MemoryBuffer1 + 8, sz1 * #HashTableSizeItems) <> sz1 * #HashTableSizeItems   
            closeAllMergeFiles()
            exit("Error during reading file")
          EndIf 
          pos1 + sz1 * #HashTableSizeItems 
        Else
          closeAllMergeFiles()
          exit("Unexpected end of file1_1 "+Hex(hash2))
        EndIf
      Else
        endfile = endfile | 1        
      EndIf
      
      If pos2 + 8<lengthFile2
        If ReadData(#File2, *MemoryBuffer2, 8)<>8
          closeAllMergeFiles()
          exit("Error during reading file")
        EndIf
        hash2 = ValueL(*MemoryBuffer2)
        sz2 =   ValueL(*MemoryBuffer2 + 4)
        pos2+8  
        If pos2 + sz2 * #HashTableSizeItems<lengthFile2          
          If ReadData(#File2, *MemoryBuffer2 + 8, sz2 * #HashTableSizeItems) <> sz2 * #HashTableSizeItems   
            closeAllMergeFiles()
            exit("Error during reading file")
          EndIf 
          pos2 + sz2 * #HashTableSizeItems 
        Else
          closeAllMergeFiles()
          exit("Unexpected end of file2_1 "+Hex(hash2))
        EndIf
      Else
        endfile = endfile | 2    
      EndIf
    EndIf
  Else
    ;ht1 < ht2 
    If _res = 1
      If writeDataToFile(#FileT, *MemoryBuffer1, sz1 * #HashTableSizeItems + 8, *tempFilebuffer, 0, 0)<>sz1 * #HashTableSizeItems + 8
        closeAllMergeFiles()
        exit("Error during writing file")
      EndIf
      dpcountT + sz1
      
      If pos1 + 8<lengthFile1
        If ReadData(#File1, *MemoryBuffer1, 8)<>8
          closeAllMergeFiles()
          exit("Error during reading file")
        EndIf
        hash1 = ValueL(*MemoryBuffer1)
        sz1 =   ValueL(*MemoryBuffer1 + 4)
        pos1+8  
        If pos1 + sz1 * #HashTableSizeItems<lengthFile1          
          If ReadData(#File1, *MemoryBuffer1 + 8, sz1 * #HashTableSizeItems) <> sz1 * #HashTableSizeItems   
            closeAllMergeFiles()
            exit("Error during reading file")
          EndIf 
          pos1 + sz1 * #HashTableSizeItems 
        Else
          closeAllMergeFiles()
          exit("Unexpected end of file1_2 "+Hex(hash1))
        EndIf
      Else
        endfile = endfile | 1     
      EndIf
      
    Else
      ;ht1 > ht2 
      If writeDataToFile(#FileT, *MemoryBuffer2, sz2 * #HashTableSizeItems + 8, *tempFilebuffer, 0, 0)<>sz2 * #HashTableSizeItems + 8
        closeAllMergeFiles()
        exit("Error during writing file")
      EndIf
      dpcountT + sz2
      
      If pos2 + 8<lengthFile2
        If ReadData(#File2, *MemoryBuffer2, 8)<>8
          closeAllMergeFiles()
          exit("Error during reading file")
        EndIf
        hash2 = ValueL(*MemoryBuffer2)
        sz2 =   ValueL(*MemoryBuffer2 + 4)
        pos2+8  
        If pos2 + sz2 * #HashTableSizeItems<lengthFile2          
          If ReadData(#File2, *MemoryBuffer2 + 8, sz2 * #HashTableSizeItems) <> sz2 * #HashTableSizeItems   
            closeAllMergeFiles()
            exit("Error during reading file")
          EndIf 
          pos2 + sz2 * #HashTableSizeItems 
        Else
          closeAllMergeFiles()
          exit("Unexpected end of file2_2 "+Hex(hash2))
        EndIf
      Else
        endfile = endfile | 2   
      EndIf
    EndIf
  EndIf 

Wend      
  
 
  
If  endfile  
  If endfile=1
    ;first file end, copy rest of second file to target
    Repeat
      If writeDataToFile(#FileT, *MemoryBuffer2, sz2 * #HashTableSizeItems + 8, *tempFilebuffer, 0, 0)<>sz2 * #HashTableSizeItems + 8
        closeAllMergeFiles()
        exit("Error during writing file")
      EndIf
      dpcountT + sz2
      
      If pos2 + 8<lengthFile2
        If ReadData(#File2, *MemoryBuffer2, 8)<>8
          closeAllMergeFiles()
          exit("Error during reading file")
        EndIf
        hash2 = ValueL(*MemoryBuffer2)
        sz2 =   ValueL(*MemoryBuffer2 + 4)
        pos2+8  
        If pos2 + sz2 * #HashTableSizeItems<lengthFile2          
          If ReadData(#File2, *MemoryBuffer2 + 8, sz2 * #HashTableSizeItems) <> sz2 * #HashTableSizeItems   
            closeAllMergeFiles()
            exit("Error during reading file")
          EndIf 
          pos2 + sz2 * #HashTableSizeItems 
        Else
          closeAllMergeFiles()
          exit("Unexpected end of file")
        EndIf
      Else        
        Break ;end of file
      EndIf
    ForEver
  Else
    If endfile=2
      ;second file end, copy rest of first file to target
      Repeat
        If writeDataToFile(#FileT, *MemoryBuffer1, sz1 * #HashTableSizeItems + 8, *tempFilebuffer, 0, 0)<>sz1 * #HashTableSizeItems + 8
          closeAllMergeFiles()
          exit("Error during writing file")
        EndIf
        dpcountT + sz1
        
        If pos1 + 8<lengthFile1
          If ReadData(#File1, *MemoryBuffer1, 8)<>8
            closeAllMergeFiles()
            exit("Error during reading file")
          EndIf
          hash1 = ValueL(*MemoryBuffer1)
          sz1 =   ValueL(*MemoryBuffer1 + 4)
          pos1+8  
          If pos1 + sz1 * #HashTableSizeItems<lengthFile1          
            If ReadData(#File1, *MemoryBuffer1 + 8, sz1 * #HashTableSizeItems) <> sz1 * #HashTableSizeItems   
              closeAllMergeFiles()
              exit("Error during reading file")
            EndIf 
            pos1 + sz1 * #HashTableSizeItems 
          Else
            closeAllMergeFiles()
            exit("Unexpected end of file")
          EndIf
        Else
          endfile=1
          Break
        EndIf
      ForEver
    EndIf
  EndIf
EndIf

;FINAL, write rest bytes from buffer to file
writeDataToFile(#FileT, 0, 0, *tempFilebuffer, 1, 0)

;set new DPcount
FileSeek(#FileT, 148)
WriteInteger(#FileT, dpcountT)
WriteInteger(#FileT, deadcount1 + deadcount2 )
WriteInteger(#FileT, timecount1 + timecount2)

closeAllMergeFiles()

FreeMemory(*tempFilebuffer)
FreeMemory(*MemoryBuffer0)
FreeMemory(*headerbuff1)
FreeMemory(*temp)
If CollisionFlag=0
  If dpcountT<>(dpcount1 + dpcount2 - dead)
    PrintN("")
    exit("Expected DPs:"+Str(dpcount1 + dpcount2 - dead)+" but saved:"+Str(dpcountT))
    DeleteFile(filenameTarget$+".temp" ,#PB_FileSystem_Force)
  Else
    filesizeTargetFile = FileSize(filenameTarget$+".temp")
    If filesizeTargetFile>#GB
      filesizeTargetFile$ = "["+StrD(FileSize(filenameTarget$+".temp")/#GB,2)+"Gb]"
    Else
      filesizeTargetFile$ = "["+StrD(FileSize(filenameTarget$+".temp")/#MB,2)+"Mb]"
    EndIf
    SPrint(procname$+"Saved DPs: "+Str(dpcountT)+" 2^"+StrD(Log(dpcountT)/Log(2),2) + filesizeTargetFile$ + " in "+Str((ElapsedMilliseconds()-starttime)/1000)+"s",#colorDarkgrey)
    SPrint(procname$+"Skiped DPs during merging: "+Str(dead),#colorDarkgrey)
    SPrint(procname$+"Total dead: "+Str(deadcount1 + deadcount2),#colorDarkgrey)
    SPrint(procname$+"Avg Speed ~"+StrD( Pow(2, (dpsize1 + Log(dpcountT)/Log(2)) - Log(timecount1 + timecount2)/Log(2) - 20) ,2)+"Mkeys/s",#colorDarkgrey)
    SPrint(procname$+"Total time: "+getElapsedTime(timecount1 + timecount2),#colorDarkgrey)
    If FileSize(filenameTarget$)>0
      DeleteFile(filenameTarget$ ,#PB_FileSystem_Force)
    EndIf
    RenameFile(filenameTarget$+".temp", filenameTarget$)
  EndIf
Else
  DeleteFile(filenameTarget$+".temp" ,#PB_FileSystem_Force)
EndIf
ProcedureReturn Output$
EndProcedure

Procedure.i SendQuestionstratum(con_id,string$)
 Protected err
 err=0
 
  If con_id
    SendNetworkString(con_id,string$+#LF$,#PB_Ascii)
    Debug "send to miner :"+Str(con_id)+">"+string$
  EndIf
 ProcedureReturn err  
EndProcedure

Procedure AnaliseRequest(reqid)
  Protected answer_f$, metod_json_answer$, id_json_answer, pars_res, name$, pass$, tempjson, get_work, get_resfalse.s, batchdata$, batchLength, batchCRC32$, computeCRC32, filename$
  Protected ClientID, ClientIP, key$
  Protected procname$ = "[SERVER] ", Error$
  
  Shared MutexClientMap,  settings(),  socketreq(), socketreqMutex, Server.ServerStructure, FileMutex, IsFindKey, LogMutex, get_work_send_string, hookenabled_g, TG_Hook()
  
  Debug( "Thread AnaliseRequest #"+Str(reqid)+" started") 
  
  ClientID = socketreq(Str(reqid))\ClientId
  ClientIP = Server\Client(Str(ClientID))\ClientIP
  answer_f$ = socketreq(Str(reqid))\req
   If Server\Client(Str(ClientID))\authorization
     procname$ + Str(ClientID)+"\"+IPString(ClientIP)+" ["+Server\Client(Str(ClientID))\name+"]"
  EndIf
   
  pars_res=ParseJSON(#PB_Any, answer_f$) 
  If pars_res 
    metod_json_answer$ = LCase(getElem(pars_res,"method",0))
    id_json_answer = Val(LCase(getElem(pars_res,"id",0)))
                  
    Select metod_json_answer$
      Case "login"         
        If Not Server\Client(Str(ClientID))\authorization
            If IsFindKey=#False
              name$=LCase(getElem(pars_res,"params",0))
              pass$ = getElem(pars_res,"params",1)
                                                          
              If Len(name$)>45
                name$=Left(name$,45)
              EndIf
              If name$         
                LockMutex(MutexClientMap)                  
                Server\Client(Str(ClientID))\name = name$           
                Server\Client(Str(ClientID))\authorization = #True    
                UnlockMutex(MutexClientMap) 
                
                Sprint(procname$+Str(ClientID)+"\"+IPString(ClientIP)+" AUTORIZED AS ["+name$ + "]",#colorMagenta)
                tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))
                  SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
                  SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), #True) 
                  SetJSONNull(AddJSONMember(get_work, "error"))
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(ClientID,get_resfalse)
                EndIf
              
              Else 
                Error$ = "Invalid_login"
              EndIf
          
              
            Else
              Error$ = "keyfounded"  
            EndIf 
        Else
           Error$ = "Already_authorized" 
        EndIf
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
        
       
      Case "filetansfer"        
        If Server\Client(Str(ClientID))\authorization
          If IsFindKey=#False
            If Server\Client(Str(ClientID))\isSend = #False
              LockMutex(FileMutex)              
              Server\Client(Str(ClientID))\FileName = Hex(Date())+Hex(ClientID)+".part"
              Server\Client(Str(ClientID))\Filesize = Val(getElem(pars_res,"params",0))
              Server\Client(Str(ClientID))\FileCRC32 = LCase(cutHex(getElem(pars_res,"params",1)))
              Server\Client(Str(ClientID))\isSend = #True            
              UnlockMutex(FileMutex)
              Sprint(procname$+"PROCESS FILE => ["+Server\Client(Str(ClientID))\FileName+"][SIZE:"+Str(Server\Client(Str(ClientID))\Filesize)+"][CRC32:"+Server\Client(Str(ClientID))\FileCRC32+"]", #colorDefault)
              
              
                tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))
                  SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
                  SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), #True) 
                  SetJSONNull(AddJSONMember(get_work, "error"))
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(ClientID,get_resfalse)
                EndIf
            Else
              Error$ = "Send process already started"      
            EndIf
          Else
              Error$ = "keyfounded"  
          EndIf
        Else
          Error$ = "Unauthorized"                              
        EndIf                           
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
      Case "keysend"        
        If Server\Client(Str(ClientID))\authorization
          key$ = getElem(pars_res,"params",0)
          SPrint (procname$+"*** "+key$+" ***"+Chr(13),#colorGreen)
          LockMutex(LogMutex)
          If  OpenFile(#LOGFILE, FormatDate("%dd_%mm-%hh_%ii_%ss ", Date())+"_key_log.txt",#PB_File_Append )
             WriteStringN(#LOGFILE,FormatDate("%dd/%mm/%hh:%ii:%ss:", Date())+key$,#PB_UTF8)
             FlushFileBuffers(#LOGFILE)
             CloseFile(#LOGFILE)
           EndIf
           UnlockMutex(LogMutex)
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #True) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf
          IsFindKey=#True
          
          If hookenabled_g
            TG_Hook(Str(Date())) = "Key found!!!"
          EndIf
                  
        Else
          Error$ = "Unauthorized"                              
        EndIf                           
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
        
      Case "getwork"        
        If Server\Client(Str(ClientID))\authorization
          SendQuestionstratum(ClientID,get_work_send_string)
          Sprint(procname$+"=> REQUEST JOB",#colorMagenta)
        Else
          Error$ = "Unauthorized"                              
        EndIf                           
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
    EndSelect   
    
    If IsJSON(pars_res)
      FreeJSON(pars_res) 
    EndIf
  EndIf
  
  LockMutex(socketreqMutex)
    DeleteMapElement(socketreq(), Str(reqid))
  UnlockMutex(socketreqMutex)
  Debug( "Thread AnaliseRequest #"+Str(reqid)+" ended") 
    
EndProcedure

Procedure Socket_Server(*Server.ServerStructure)
  
  Protected.i SEvent, ClientID, i, ClientIP,  ReceivedBytes, procname$="[SERVER] "
  Protected answer_t$, answer_f$, pos, pos2, pars_res, metod_json_answer$, id_json_answer, get_work, get_restrue.s, resultArr, ArrayValue, resultArr_1, mining_set_difficulty.s, filename$, computeCRC32file
  Protected full_size
  Protected  get_true.s, get_resfalse.s, poolid, socket, answer_ff$, ShareID, curmill
  Protected name$, worker$, posname,  a$, pass$
  Protected NewMap templist(), socketreqN
  Protected *Buffer, counter, tempjson, extr$, mining_subscribe_answer.s, flagidpool

  Shared MutexClientMap, settings(),socketreq(), socketreqMutex, stratumwork_g, FileMutex, jobster()
  
  SPrint(procname$ + "started And listen port:"+Str(settings("1")\port),#colorGreen)
  
  *Buffer = AllocateMemory(65536)
  If *Buffer
    Repeat
      stratumwork_g = 0
      ;-delete unatorized users
      counter+1
      If counter&$1ff=0 
      
        LockMutex(MutexClientMap)        
          ForEach *Server\Client()        
            If *Server\Client()\time And Not*Server\Client()\authorization 
              ; if connected but not autorized more than 2s
              If Date() - *Server\Client()\time > 2              
                  templist(Str(*Server\Client()\ClientID))=*Server\Client()\quit        
              EndIf 
            EndIf
          Next
        UnlockMutex(MutexClientMap)       
       
        ForEach templist()
          If templist()=#False
            If Val(MapKey(templist()))>0
              CloseNetworkConnection(Val(MapKey(templist())))
            EndIf
            Debug( "CLIENT: "+MapKey(templist()) + " TIME OUT. KICK...")
          Else
            Debug( "CLIENT: "+MapKey(templist()) + " DISCONNECT...")
          EndIf
          LockMutex(MutexClientMap)
            Socket_ClientDisconnect(Val(MapKey(templist())), *Server)
          UnlockMutex(MutexClientMap)
        Next
        ClearMap(templist())
      EndIf
      
      SEvent = NetworkServerEvent(#Server)
      If SEvent 
        ClientID = EventClient()
        ClientIP = GetClientIP(ClientID)
        Select SEvent
          Case #PB_NetworkEvent_Connect
            
              LockMutex(MutexClientMap)
                *Server\Client(Str(ClientID))\ClientId = ClientID
                *Server\Client(Str(ClientID))\ClientIP = ClientIP                                     
                *Server\Client(Str(ClientID))\time = Date()
                *Server\Client(Str(ClientID))\authorization = #False
                *Server\Client(Str(ClientID))\quit = #False
                
              UnlockMutex(MutexClientMap)
                
              ;SPrint(procname$ + "CLIENT: "+Str(ClientID)+" IP: "+IPString(*Server\Client(Str(ClientID))\ClientIP)+" HAS CONNECTED !",#colorMagenta)
              ;SPrint(procname$ + "TOTAL CLIENTS: " + Str(MapSize(*Server\Client())),#colorMagenta)
              
            Case #PB_NetworkEvent_Data
              *Server\Client(Str(ClientID))\time = Date()
              ;FillMemory(*Buffer, 10000)
              ReceivedBytes = ReceiveNetworkData(ClientID, *Buffer, 65536)            
              If ReceivedBytes>=0
                Debug "recieved "+Str(ReceivedBytes)+" bytes"
                If *Server\Client(Str(ClientID))\authorization And *Server\Client(Str(ClientID))\isSend=#True And *Server\Client(Str(ClientID))\FileUploaded<*Server\Client(Str(ClientID))\Filesize
                  filename$ =  *Server\Client(Str(ClientID))\FileName+"temp"                  
                  If OpenFile(#File, filename$ ,#PB_File_Append)
                    WriteData(#File, *Buffer, ReceivedBytes)
                    CloseFile(#File)
                  EndIf
                  *Server\Client(Str(ClientID))\FileUploaded + ReceivedBytes
                  If *Server\Client(Str(ClientID))\FileUploaded>=*Server\Client(Str(ClientID))\Filesize
                    full_size = FileSize(filename$)                    
                    computeCRC32file = CRC32FileFingerprint(filename$)                   
                    If LCase(Hex(computeCRC32file)) = *Server\Client(Str(ClientID))\FileCRC32
                      *Server\Client(Str(ClientID))\isSend=#False
                      
                      Sprint(procname$ + "RECIEVED NEW FILE ["+*Server\Client(Str(ClientID))\FileName+"][SIZE:"+Str(full_size)+"][CRC32:"+LCase(Hex(computeCRC32file))+"] from ["+*Server\Client(Str(ClientID))\name+"]",#colorWhite)
                      Sprint(procname$ + "APPEND  JOB  FILE ["+*Server\Client(Str(ClientID))\FileName+"]",#colorWhite)
                      RenameFile(filename$, *Server\Client(Str(ClientID))\FileName)
                      jobster(*Server\Client(Str(ClientID))\name)\count + 1
                      If jobster(*Server\Client(Str(ClientID))\name)\firsttime = 0
                        jobster(*Server\Client(Str(ClientID))\name)\firsttime = Date()
                      EndIf
                      jobster(*Server\Client(Str(ClientID))\name)\lasttime = Date()
                      
                    Else
                      Sprint(procname$ + "INVALID CRC32 FILE > NEED:"+*Server\Client(Str(ClientID))\FileCRC32+", GOT:"+LCase(Hex(computeCRC32file)),#colorRed)
                      DeleteFile(Filename$)
                      Debug "File "+Filename$+" was deleted"
                    EndIf
                  EndIf
                Else
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
                      
                      If MapSize(socketreq())=0
                        socketreqN = 0
                      EndIf
                      LockMutex(socketreqMutex)
                      socketreq(Str(socketreqN))\id = socketreqN
                      socketreq(Str(socketreqN))\ClientId = ClientID
                      socketreq(Str(socketreqN))\req = answer_f$    
                      UnlockMutex(socketreqMutex)
                      CreateThread(@AnaliseRequest(),socketreqN)
                      
                      
                     
                      socketreqN+1
                        
                      answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
                      pos=FindString(answer_t$, "{")
                    Else
                     pos=0
                    EndIf
                  Wend  
                EndIf
                           
                
             EndIf
          Case #PB_NetworkEvent_Disconnect
          ;SPrint( "CLIENT: "+Str(ClientID) + " IP:"+IPString(*Server\Client(Str(ClientID))\ClientIP)+" DISCONNECT (CLIENT SIDE)",#colorMagenta)
          LockMutex(MutexClientMap)
              *Server\Client(Str(ClientID))\quit = #True
              Socket_ClientDisconnect(ClientID, *Server)
          UnlockMutex(MutexClientMap)
              
      EndSelect
    EndIf
    Delay(1)    
    Until *Server\Quit   
    FreeMemory(*Buffer)
    FreeMap(templist())
  Else
    SPrint(procname$ + "Can't allocate buffer",#colorred)
  EndIf  
EndProcedure


Procedure checkSourceFile(sourcefile$, isCheckDp=0)
  Protected  err, procname$="[CHEKER]", P_totalItem, pos1, cnt, part, np, endfile, lengthFile1, hash1, sz1, j, *phash, *pdist, P_tame, P_wild, res.comparsationStructure, starttime
  Protected *MemoryBuffer, *ShiftedFindPub_X_l, *ShiftedFindPub_Y_l, *PubRangeB_X_l, *PubRangeB_Y_l, *PubRangeB_Y_neg_l, *ShiftedRangeE_l, *ShiftedRangeEhalf_l
  Protected *help_X_l, *help_Y_l, *help_Y_neg_l, *ZeroShiftedFindPub_X_l, *ZeroShiftedFindPub_Y_l, *FullR_X_l, *FullR_Y_l, *FullR_Y_neg_l, *testX, *testY, i, *testNeg_Y
  Protected  *pubdisX, *pubdisY, *tempdist, a$, *priv, *privX, *privY, DPerr
  Shared settings(), *GTable, *CurveP, LogMutex, hookenabled_g, TG_Hook()
  
  *MemoryBuffer=AllocateMemory(100*#MB)
  *ShiftedFindPub_X_l= AllocateMemory(32)
  *ShiftedFindPub_Y_l= AllocateMemory(32)
  *PubRangeB_X_l= AllocateMemory(32)
  *PubRangeB_Y_l= AllocateMemory(32)
  *PubRangeB_Y_neg_l= AllocateMemory(32)
  *ShiftedRangeE_l= AllocateMemory(32)
  *ShiftedRangeEhalf_l= AllocateMemory(32)
  *help_X_l= AllocateMemory(32)
  *help_Y_l= AllocateMemory(32)
  *help_Y_neg_l= AllocateMemory(32)
  *ZeroShiftedFindPub_X_l= AllocateMemory(32)
  *ZeroShiftedFindPub_Y_l= AllocateMemory(32) 
  *FullR_X_l = AllocateMemory(32)
  *FullR_Y_l = AllocateMemory(32)
  *FullR_Y_neg_l = AllocateMemory(32)
  *pubdisX = AllocateMemory(288) 
  *pubdisY = *pubdisX + 32
  *tempdist = *pubdisX + 64
  *testX = *pubdisX + 96
  *testY = *pubdisX + 128
  *testNeg_Y = *pubdisX + 160
  *priv = *pubdisX + 192
  *privX = *pubdisX + 224
  *privX = *pubdisX + 256
  
    
  If FileSize(sourcefile$)>#HEADERSIZE
    If  OpenFile(#FileCheck, sourcefile$)
      lengthFile1 = Lof(#FileCheck)    
      If ReadData(#FileCheck, *MemoryBuffer, #HEADERSIZE)=#HEADERSIZE        
        If Valuel(*MemoryBuffer + 8)<>settings("1")\getworkDP
          SPrint(procname$+" File ["+sourcefile$+"] Mismath DP size",#colorRed)
          err=1
        EndIf
        If PeekI(*MemoryBuffer + 140)<>settings("1")\htpow
          SPrint(procname$+" File ["+sourcefile$+"] Mismath HT size",#colorRed)
          err=1
        EndIf
        If LCase(RSet(settings("1")\getworkrangeBegin, 64,"0"))<>LCase(Curve::m_gethex32(*MemoryBuffer + 12))
          SPrint(procname$+" File ["+sourcefile$+"] Mismath range begin",#colorRed)
          err=1
        EndIf
        If LCase(RSet(settings("1")\getworkrangeEnd, 64,"0"))<>LCase(Curve::m_gethex32(*MemoryBuffer + 44))
          SPrint(procname$+" File ["+sourcefile$+"] Mismath range end",#colorRed)
          err=1
        EndIf
        If LCase(settings("1")\getworkPub)<>LCase(uncomressed2commpressedPub(Curve::m_gethex32(*MemoryBuffer + 76)+Curve::m_gethex32(*MemoryBuffer + 108)))
          SPrint(procname$+" File ["+sourcefile$+"] Mismath pub",#colorRed)
          err=1
        EndIf
        If Not err And isCheckDp=1
          ;CHECK DPS
          starttime=Date()
          SPrint(procname$+" File ["+sourcefile$+"] Check "+Str(PeekI(*MemoryBuffer+148))+"DPs..",#colorDefault)
          
          CopyMemory(*MemoryBuffer + 76, *ShiftedFindPub_X_l, 32)
          CopyMemory(*MemoryBuffer + 108, *ShiftedFindPub_Y_l, 32)
          
                    
          If Curve::m_check_nonzeroX64(*MemoryBuffer + 12)
            
            ;if begining range is not zero, substruct range from findpub
            
            Curve::ComputePublicKey(*PubRangeB_X_l, *PubRangeB_Y_l, *GTable,  *MemoryBuffer + 12)  
            
            Curve::m_subModX64(*PubRangeB_Y_neg_l, *CurveP, *PubRangeB_Y_l, *CurveP)
            Curve::m_ADDPTX64(*ShiftedFindPub_X_l, *ShiftedFindPub_Y_l, *ShiftedFindPub_X_l, *ShiftedFindPub_Y_l, *PubRangeB_X_l, *PubRangeB_Y_neg_l, *CurveP)
            ;PrintN("Shifted Find X:"+Curve::m_gethex32(*ShiftedFindPub_X_l))
            ;PrintN("Shifted Find Y:"+Curve::m_gethex32(*ShiftedFindPub_Y_l))
            
          EndIf
          
          Curve::m_subX64(*ShiftedRangeE_l,*MemoryBuffer + 44,*MemoryBuffer + 12)
          ;PrintN("Shifted Range     :"+Curve::m_gethex32(*ShiftedRangeE_l))
          
                    
          ;pub of rangeend      
          Curve::ComputePublicKey(*FullR_X_l, *FullR_Y_l, *GTable,  *ShiftedRangeE_l)
          Curve::m_subModX64(*FullR_Y_neg_l, *CurveP, *FullR_Y_l, *CurveP)
          
          
          CopyMemory(*ShiftedRangeE_l, *ShiftedRangeEhalf_l, 32)
          Curve::m_shrX64(*ShiftedRangeEhalf_l)
          ;PrintN( "Shifted Range half:"+Curve::m_gethex32(*ShiftedRangeEhalf_l))
          
          ;substruct half of range from shifted pubkey
          ;pub of range half 
          Curve::ComputePublicKey(*help_X_l, *help_Y_l, *GTable,  *ShiftedRangeEhalf_l)      
          Curve::m_subModX64(*help_Y_neg_l, *CurveP, *help_Y_l, *CurveP)
          
          Curve::m_ADDPTX64(*ZeroShiftedFindPub_X_l, *ZeroShiftedFindPub_Y_l, *ShiftedFindPub_X_l, *ShiftedFindPub_Y_l, *help_X_l, *help_Y_neg_l, *CurveP)
          ;PrintN("Zero Shifted Find X:"+Curve::m_gethex32(*ZeroShiftedFindPub_X_l))
          ;PrintN("Zero Shifted Find Y:"+Curve::m_gethex32(*ZeroShiftedFindPub_Y_l))
          
          
          ;READ HASHTABLE
          P_totalItem=0
          pos1 = #HEADERSIZE-1
          
          endfile=0   
          cnt=0
          part=PeekI(*MemoryBuffer+148)/100
          np = 0
      
          While endfile=0
            If pos1 + 8<lengthFile1
              If ReadData(#FileCheck, *MemoryBuffer, 8)<>8
                err=1
                Sprint(procname$+" File ["+sourcefile$+"] Error during reading file", #colorRed)
                CloseFile(#FileCheck)
                Break
              EndIf
              hash1 = ValueL(*MemoryBuffer)
              sz1 =   ValueL(*MemoryBuffer + 4)
              pos1+8  
              If pos1 + sz1 * #HashTableSizeItems < lengthFile1    
                If ReadData(#FileCheck, *MemoryBuffer + 8, sz1 * #HashTableSizeItems) <> sz1 * #HashTableSizeItems   
                  err=1
                  Sprint(procname$+" File ["+sourcefile$+"] Error during reading file", #colorRed)
                  CloseFile(#FileCheck)  
                  Break
                EndIf 
                
                For j=0 To sz1-1  
                  *phash = *MemoryBuffer + j * #HashTableSizeItems + 8
                  *pdist = *phash + #HashTableSizeHash            
                  
                  If Curve::m_Ecc_TestBitX64(*pdist, 255) = #TAME
                    P_tame+1  
                    ;checkDPTame(*phash,*pdist)
                   
                    Curve::ComputePublicKey(*pubdisX, *pubdisY, *GTable,  *pdist)  
                    
                    If m_check_equilX8(*pubdisX + 16,*phash)<>1                      
                      ;PrintN( "invalid")
                      Sprint(procname$+" File ["+sourcefile$+"] TCheck x>"+m_gethex8(*phash), #colorRed)
                      Sprint(procname$+" File ["+sourcefile$+"] TCalc  x>"+m_gethex8(*pubdisX + 16), #colorRed)
                      Sprint(procname$+" File ["+sourcefile$+"] TDist>"+Curve::m_gethex32(*pdist), #colorRed)
                      DPerr = 1
                    Else
                      ;PrintN( "correct")
                    EndIf                    
                  Else           
                    P_wild+1    
                    ;checkDPWild(*phash,*pdist)
                    CopyMemory(*pdist, *tempdist, 32)
                    Curve::m_ResetBitX64(*tempdist,255)
                    
                    Curve::ComputePublicKey(*pubdisX, *pubdisY, *GTable,  *tempdist)  
                    Curve::m_ADDPTX64(*pubdisX,*pubdisY,*ZeroShiftedFindPub_X_l,*ZeroShiftedFindPub_Y_l,*pubdisX,*pubdisY,*CurveP)
                    
                    If m_check_equilX8(*pubdisX + 16,*phash)<>1                      
                      ;PrintN( "invalid")
                      Sprint(procname$+" File ["+sourcefile$+"] WCheck x>"+m_gethex8(*phash), #colorRed)
                      Sprint(procname$+" File ["+sourcefile$+"] WCalc  x>"+m_gethex8(*pubdisX + 16), #colorRed)
                      Sprint(procname$+" File ["+sourcefile$+"] WDist>"+Curve::m_gethex32(*tempdist), #colorRed)
                      DPerr = 1
                    Else
                      ;PrintN( "correct")
                    EndIf                    
                  EndIf
                 
                Next j
                P_totalItem + sz1    
                pos1 + sz1 * #HashTableSizeItems 
              Else
                err=1
                Sprint(procname$+" File ["+sourcefile$+"] Unexpected end of file", #colorRed)
                CloseFile(#FileCheck) 
                Break
              EndIf
            Else
              endfile = 1
            EndIf
            
            If P_totalItem>cnt
              np = (P_totalItem-cnt)/part
              If np                
                ConsoleTitle(StrD(P_totalItem / (part * 100)*100,0)+"%")                
                cnt + np*part
              EndIf
            EndIf 
          Wend 
          ConsoleTitle("")
          SPrint(procname$+" File ["+sourcefile$+"] Done in "+getElapsedTime(Date()-starttime),#colorDefault)          
        EndIf
      Else
        SPrint(procname$+" File ["+sourcefile$+"] Error during read",#colorRed)
        err=1
      EndIf
      CloseFile(#FileCheck)
    Else
      SPrint(procname$+" File ["+sourcefile$+"] Can`t open",#colorRed)
      err=1
    EndIf
    
  Else
    err=1
  EndIf
  If DPerr Or err
    If hookenabled_g
      TG_Hook(Str(Date())) = "..error in ["+sourcefile$+"]"
    EndIf
  EndIf
  If Not err
    SPrint(procname$+" File ["+sourcefile$+"] ..Ok",#colorDefault)
  Else
    SPrint(procname$+" File ["+sourcefile$+"] ..Failed",#colorRed)    
  EndIf
 
  FreeMemory(*pubdisX)
  FreeMemory(*MemoryBuffer)
  FreeMemory(*ShiftedFindPub_X_l)
  FreeMemory(*ShiftedFindPub_Y_l)
  FreeMemory(*PubRangeB_X_l)
  FreeMemory(*PubRangeB_Y_l)
  FreeMemory(*PubRangeB_Y_neg_l)
  FreeMemory(*ShiftedRangeE_l)
  FreeMemory(*ShiftedRangeEhalf_l)
  FreeMemory(*help_X_l)
  FreeMemory(*help_Y_l)
  FreeMemory(*help_Y_neg_l)
  FreeMemory(*ZeroShiftedFindPub_X_l)
  FreeMemory(*ZeroShiftedFindPub_Y_l)
  FreeMemory(*FullR_X_l)
  FreeMemory(*FullR_Y_l)
  FreeMemory(*FullR_Y_neg_l)
ProcedureReturn err
EndProcedure

Procedure mergerInternal(i)
  Protected Directory$, procname$ = "[MERGER] ", err, Compiler, params$, sourcefile1$, sourcefile2$, destinationfile$, isMainsaveExist=#False, Output$, readyjob
  Protected string_win$=LCase("Priv:"), _res
  Protected NewList Myfilejob.s(), NewList win.s()
  Shared  FileMutex, settings(), IsFindKey, LogMutex, hookenabled_g, TG_Hook()
  SPrint(procname$+"started",#colorYellow)
  Directory$ = GetCurrentDirectory()
  Repeat 
    readyjob=#False   
    If ExamineDirectory(0, Directory$, "*.part")  
      While NextDirectoryEntry(0)
        If DirectoryEntryType(0) = #PB_DirectoryEntry_File 
          _res=0
          ResetList(Myfilejob())
          While NextElement(Myfilejob())
            If Myfilejob()=GetFilePart(DirectoryEntryName(0))
              _res=1
            EndIf
          Wend
          If _res=0
            If Not checkSourceFile(GetFilePart(DirectoryEntryName(0)), settings("1")\checkDP)
              AddElement(Myfilejob())
              Myfilejob()=GetFilePart(DirectoryEntryName(0))
            Else
              
              DeleteFile(GetFilePart(DirectoryEntryName(0)),#PB_FileSystem_Force)
            EndIf
          EndIf
            
        EndIf 
        If FileSize(settings("1")\saveworkfilename)>=0 And ListSize(Myfilejob())>=1
          Break
        EndIf
        If FileSize(settings("1")\saveworkfilename)<0 And ListSize(Myfilejob())>=2 
          Break
        EndIf
      Wend
      FinishDirectory(0)
    EndIf 
    If ListSize(Myfilejob())>=1
      If FileSize(settings("1")\saveworkfilename)>=0
        Debug procname$+" mainsave file exist"
        isMainsaveExist = #True        
        readyjob = #True
      Else        
        Debug procname$+" mainsave file not exist yet"
        isMainsaveExist = #False
        If ListSize(Myfilejob())>=2          
          readyjob = #True
        Else
          ;ClearList(Myfilejob()) 
        EndIf
      EndIf
      
    EndIf    
    err=0
    If readyjob = #True
      While Not err And ListSize(Myfilejob())
        ;continue work
        If isMainsaveExist=#True
          ;mainsave file exist
          FirstElement(Myfilejob())
          sourcefile1$ = Myfilejob()
          sourcefile2$ = settings("1")\saveworkfilename
          destinationfile$ = settings("1")\saveworkfilename             
        Else
          ;mainsavefile is not exist yet
          FirstElement(Myfilejob())
          sourcefile1$ = Myfilejob()
          NextElement(Myfilejob())            
          sourcefile2$ = Myfilejob()            
          destinationfile$ = settings("1")\saveworkfilename            
        EndIf
        
          params$ = "-wm"+" "+sourcefile1$+" "+sourcefile2$+" "+destinationfile$ 
          SPrint(procname$+"MERGE ["+sourcefile1$+"]+["+sourcefile2$+"]>["+destinationfile$+"]",#colorYellow)
          Output$ = mergeHTFilesNew(sourcefile1$, sourcefile2$, destinationfile$)
          If IsFindKey=#True
            SPrint (procname$+"*** "+Output$+" ***"+Chr(13),#colorGreen)
            LockMutex(LogMutex)
            If  OpenFile(#LOGFILE, FormatDate("%dd_%mm-%hh_%ii_%ss ", Date())+"_key_merge_log.txt",#PB_File_Append )
              WriteStringN(#LOGFILE,FormatDate("%dd/%mm/%hh:%ii:%ss:", Date())+Output$,#PB_UTF8)
              FlushFileBuffers(#LOGFILE)
              CloseFile(#LOGFILE)
            EndIf
            UnlockMutex(LogMutex)
            If hookenabled_g
              TG_Hook(Str(Date())) = "Key found!!!"
            EndIf
            Break
          EndIf
          
          Delay(50)
          
        If Not err
          FirstElement(Myfilejob())
          Debug procname$+" delete file:"+Myfilejob()
          
          If Not DeleteFile(Myfilejob())
            err=1
          EndIf
          DeleteElement(Myfilejob(),1)          
          If isMainsaveExist=#False
            FirstElement(Myfilejob())
            Debug procname$+" delete file:"+Myfilejob()
            
            If Not DeleteFile(Myfilejob())
              err=1
            EndIf
            DeleteElement(Myfilejob(),1) 
          EndIf
        EndIf
        If Not err
          SPrint(procname$+"JOB DONE",#colorYellow)
        EndIf
      Wend
    EndIf
    Delay(3000)
  Until IsFindKey=#True
  SPrint(procname$+"finished",#colorYellow)
EndProcedure

Procedure clearAllFiles()
  Protected Directory$
  Directory$ = GetCurrentDirectory()
  DeleteDirectory(Directory$,"*.part")
EndProcedure



Procedure calculateBitRange(*rb, *re)
  Protected wholebitinrange, *bufferResult = AllocateMemory(32)
  
  Curve::m_subX64(*bufferResult,*re,*rb)  
  wholebitinrange=0
  While Curve::m_check_nonzeroX64(*bufferResult)
    Curve::m_shrX64(*bufferResult)
    wholebitinrange+1
  Wend
  FreeMemory (*bufferResult)
  ProcedureReturn wholebitinrange
EndProcedure


;Server app for Etarkangaroo.
;To manage workfiles on server side use EtarkangarooServer.
;This is standalone app that does not require other programs..
;Server app sets search key, search ranges, size of DP and other things for client and merge work files to single work file.
;The use of a server app allows you to work more flexibly with working files.
;Usage:
;-port      [optional] port on which clients are listening, default 8000
;-checkdp 	[optional] this flag alow check every DP from clients
;-ht		    [optional] hashtable size 2^, default value 25
;-dp		    [required] number of trailing zeros distinguished point
;-wi		    [optional] timer interval (in seconds) for autosaving ht/kangaroos on client side, can be overwritten by client app, default 7200
;-beginrange[required] range start from
;-endrange	[required] end range
;-pub		    [required] set single uncompressed/compressed pubkey for searching
;Example:
;EtarkangarooServer.exe -checkdp -dp 16 -wi 180 -beginrange 80000000000000000000 -endrange FFFFFFFFFFFFFFFFFFFF -pub 037E1238F7B1CE757DF94FAA9A2EB261BF0AEB9F84DBF81212104E78931C2A19DC

*GTable = AllocateMemory(524288)
*RangeBegin = AllocateMemory(32)
*RangeEnd = AllocateMemory(32)

Curve::GTableGen(*GTable)

InitNetwork()
OpenConsole()
settings("1")\port=8000
settings("1")\saveworkfilename="savework"
settings("1")\programmname="serverapp.exe"
settings("1")\getworkjobsaveinterval=7200
settings("1")\htpow = 25
settings("1")\checkDP = 0; check all DPs in sended work file DISABLED

Define StratServ, Thread, tempjson, get_work, Values, uncompressedpub$, a$

getprogparam()

If settings("1")\getworkrangeBegin="" Or settings("1")\getworkrangeEnd="" Or settings("1")\getworkPub="" Or settings("1")\getworkDP=0
  Sprint("Check you params!", #colorred)
  End
EndIf 


  ;check if it uncompressed
  If Len(cuthex(settings("1")\getworkPub))=130 And Left(cuthex(settings("1")\getworkPub),2)="04"    
    settings("1")\getworkPub = uncomressed2commpressedPub(Right(cuthex(settings("1")\getworkPub), 128))
  Else  
    ;check if it compressed
    If Len(settings("1")\getworkPub)=66 And ( Left(settings("1")\getworkPub,2)="03" Or Left(settings("1")\getworkPub,2)="02")
      ;ok
    Else
      Sprint("Invalid Public Key (-pb) length!!!", #colorred)
      exit("")
    EndIf
  EndIf
  
  ;check if pub is on curve
  uncompressedpub$ = commpressed2uncomressedPub(settings("1")\getworkPub)
  a$=Left(cutHex(uncompressedpub$),64)
  Curve::m_sethex32(*RangeBegin, @a$ )
  a$=Right(cutHex(uncompressedpub$),64)
  Curve::m_sethex32(*RangeEnd, @a$)

  If Not Curve::m_isOnCurve(*RangeBegin,*RangeEnd)
    Sprint("Public key is not lie on curve", #colorred)
    exit("")
  EndIf
  
  
Curve::m_sethex32(*RangeBegin, @settings("1")\getworkrangeBegin)
Curve::m_sethex32(*RangeEnd, @settings("1")\getworkrangeEnd)

If Curve::m_check_less_more_equilX64(*RangeBegin, *RangeEnd)<>1
  Sprint("End range shoud be more then begin range", #colorred)
  exit("")
EndIf

If Curve::m_check_nonzeroX64(*RangeBegin)=0
  Sprint("Begin range can`t be zero", #colorred)
  exit("")
EndIf

BitRange = calculateBitRange(*RangeBegin, *RangeEnd)

tempjson = CreateJSON(#PB_Any)
If tempjson   
  get_work = SetJSONObject(JSONValue(tempjson))   
  SetJSONInteger(AddJSONMember(get_work, "id"), 0)       
  Values =SetJSONArray(AddJSONMember(get_work, "result"))
  SetJSONInteger(AddJSONElement(Values), settings("1")\getworkDP)
  SetJSONString(AddJSONElement(Values), settings("1")\getworkPub)
  SetJSONString(AddJSONElement(Values), settings("1")\getworkrangeBegin)
  SetJSONString(AddJSONElement(Values), settings("1")\getworkrangeEnd)
  SetJSONInteger(AddJSONElement(Values), settings("1")\getworkjobsaveinterval)
  SetJSONInteger(AddJSONElement(Values), settings("1")\htpow)
  SetJSONNull(AddJSONMember(get_work, "error"))
  get_work_send_string=ComposeJSON(tempjson)
  FreeJSON(tempjson)
EndIf
          
Sprint("DP    ["+Str(settings("1")\getworkDP)+"]",#colorBrown)
Sprint("KEY   ["+settings("1")\getworkPub+"]",#colorBrown)
Sprint("BEGIN ["+settings("1")\getworkrangeBegin+"]",#colorBrown)
Sprint("END   ["+settings("1")\getworkrangeEnd+"] 2^"+Str(BitRange),#colorBrown)
Sprint("WI    ["+settings("1")\getworkjobsaveinterval+"]",#colorBrown)
Sprint("HTPOW ["+Str(settings("1")\htpow)+"]",#colorBrown)

If settings("1")\checkDP =1
  Sprint("[MAIN  ] Check Dps enabled",#colorDefault)
Else
  Sprint("[MAIN  ] Check Dps disabled",#colorDefault)
EndIf

;check if exist main work file
If FileSize(settings("1")\saveworkfilename)>0
  Sprint("[MAIN  ] Work file ["+settings("1")\saveworkfilename+"] exist",#colorDefault)
  If checkSourceFile(settings("1")\saveworkfilename, 0)
    exit("")
  EndIf
EndIf

If settings("1")\port
  StratServ = CreateNetworkServer(#Server,settings("1")\port,#PB_Network_TCP)
  If StratServ=0 
    Sprint("Can't create the SSserver ("+Str(settings("1")\port)+" in use ?).", #colorred) 
    Delay(2000)        
    End  
  EndIf
  Thread = CreateThread(@Socket_Server(), @Server)
Else
  End
EndIf


CreateThread(@mergerInternal(), 0)


If settings("1")\tg_botid And settings("1")\tg_myid And CreateThread(@Hooktimer2s(), 5)  
  hookenabled_g=1
  Sprint("Telegramm hook enabled", #colorYellow)  
  TG_Hook(Str(Date())) = "Key server started at port:"+Str(settings("1")\port)
  
EndIf

Define KeyPressed$, String$, totalreset
Repeat 
  Delay(100)
      KeyPressed$ = Inkey()
      If KeyPressed$ <> ""        
        Select RawKey()
          Case 13 ;enter
            Print(">")
            
            String$ = Input()
            
          String$=RemoveString(String$, Chr(13),#PB_String_NoCase  )          
          If String$="s"
            ForEach (jobster())
              Sprint("["+MapKey(jobster())+"]["+FormatDate("%dd/%mm/%hh:%ii:%ss:", jobster()\firsttime)+"]["+FormatDate("%dd/%mm/%hh:%ii:%ss:", jobster()\lasttime)+"]["+jobster()\count+"]", #colorYellow)
            Next
          EndIf
          If String$="r"
            totalreset=0
            ForEach (jobster())
              jobster()\count = 0
              totalreset+1
            Next
            Sprint("Total "+Str(totalreset)+" worker counter was reset", #colorYellow)
          EndIf
        EndSelect        
      EndIf
  
ForEver

; IDE Options = PureBasic 5.31 (Windows - x64)
; ExecutableFormat = Console
; CursorPosition = 57
; FirstLine = 54
; Folding = -----+
; EnableThread
; EnableXP
; Executable = EtarkangarooServer.exe