*          DATA SET CTSECXML   AT LEVEL 002 AS OF 09/06/18                      
*PHASE KXTRACTA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DDXML                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE SORTER                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DDTRAN                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CALLOFF                                                                
*&&      SET   NOP=N                                                            
         IEABRCX DEFINE                                                         
         IEABRCX DISABLE                                                        
         TITLE 'CTFILE TEST CTRECXML'                                           
*************************************************************                   
*        COPY CTFILE TO FLAT FILE IN XML                    *                   
*************************************************************                   
*                                                                               
CTREPXML CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CTXML*,WORK=A(WORKC),CLEAR=YES                     
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RC,SAVERC                                                        
         ST    RD,SAVERD                                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         J     START                                                            
*                                                                               
$$DATA   LOCTR ,                   DATA LOCATION CLOSE TO RB                    
*                                                                               
*************************************************************                   
* RB = BASE1 LITERALS AND CONSTANTS                         *                   
* RA = RESERVED                                             *                   
* R9 = LOCAL BASE                                           *                   
* CONSTANTS & LITERALS DEFINE LATER USING $$DATA LOCTR      *                   
*************************************************************                   
*                                                                               
$$CODE   LOCTR ,                   CODE AFTER DATA                              
*                                                                               
START    L     R1,=A(IO-WORKD)     ADDRESS OUT OF RANGE WORK                    
         AR    R1,RC                                                            
         ST    R1,AIO                                                           
         L     R1,=A(IO2-WORKD)                                                 
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         L     R1,=A(MYXMLBLK-WORKD)                                            
         AR    R1,RC                                                            
         ST    R1,AXMLBLK                                                       
*                                                                               
         LARL  R1,XMLOUT           REGULAR FORMATTED OUTPUT                     
         ST    R1,AXMLOUT                                                       
         LARL  R1,XMLOUTL          LONG CONTINUOUS OUTPUT                       
         ST    R1,AXMLOUTL                                                      
         L     R1,=A(CFACSTRT)                                                  
         ST    R1,ACOMFACS                                                      
*                                                                               
MAIN     BRAS  RE,INIT             OPEN FILES ECT                               
*                                                                               
         CLC   DATATYPE,=CL10'SYSADV'                                           
         JNE   MAIN005                                                          
*                                                                               
         BRAS  RE,SYSADV                                                        
         J     MAIN990                                                          
*                                                                               
MAIN005  DS    0H                                                               
         CLC   DATATYPE,=CL10'SYSPROG'                                          
         JNE   MAIN010                                                          
*                                                                               
         GOTO1 =V(DDXML),DMCB,(C'I',AXMLOUT),AXMLOUTL,AXMLBLK                   
*                                                                               
         BRAS  RE,SYSPROG                                                       
         J     MAIN990                                                          
*                                                                               
MAIN010  DS    0H                                                               
         CLC   DATATYPE,=CL10'SECXML'                                           
         JNE   *+2                                                              
*                                                                               
         GOTO1 =V(DDXML),DMCB,(C'I',AXMLOUT),0,AXMLBLK                          
         LARL  RF,XML001           MAINFRAMESECURITY                            
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
* XML SECURITY EXTRACT HERE                                                     
*                                                                               
         LARL  RF,XML105           agylistlist  AGY/USERID                      
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         BRAS  RE,PUTUSER                                                       
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
         LARL  RF,XML104           userlistlist LIST/USERIDS                    
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         BRAS  RE,PUTLIST                                                       
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
         LARL  RF,XML050           securitylist AG/USER/PERSON                  
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         BRAS  RE,PUTSORT                                                       
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
* PERSON LIST                                                                   
*                                                                               
         BRAS  RE,BLDSEC        BUILD TABLE OF AGY-SECAGY RELATIONSHIPS         
         XC    LASTAGY,LASTAGY                                                  
         LARL  RF,XML002           <personlist>                                 
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         MVI   CTINIT,C'N'                                                      
         XC    SELOW,SELOW                                                      
         XC    SEHIGH,SEHIGH                                                    
         MVI   SELOW,C'F'                                                       
         MVI   SEHIGH,C'F'                                                      
         MVI   SELOW+1,X'04'                                                    
         MVI   SEHIGH+1,X'04'                                                   
         MVI   SEHIGH+2,X'FF'                                                   
*                                                                               
MAIN020  BRAS  RE,GETSE            READ FILE                                    
         JNE   MAIN029                                                          
*                                                                               
         L     R2,AIO                                                           
         USING SAPEREC,R2          READ RECORDS                                 
*&&US*&& CLC   =C'#N',SAPEAGY      suppress #N in US                            
*&&UK*&& CLC   =C'#E',SAPEAGY      suppress #E in UK                            
         JE    MAIN020                                                          
         DROP  R2                                                               
*                                                                               
         BRAS  RE,PUTPE            PUT NEW RECORDS                              
         J     MAIN020                                                          
*                                                                               
MAIN029  DS    0H                                                               
* </secagy>                                                                     
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
* </personlist>                                                                 
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
         LARL  RF,XML201           <agyofficelists>                             
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         BRAS  RE,PUTOFLST                                                      
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
         BRAS  RE,PUTSCAGY         LIST OF AGYS BY SECAGY                       
*                                                                               
         GOTO1 =V(DDXML),DMCB,(C'A',0),0,AXMLBLK                                
         MVI   CTINIT,C'N'         RE-INIT                                      
*                                                                               
EXIT99   L     RD,SAVERD           RESTORE RD POSITION                          
         J     MAIN990                                                          
*                                                                               
* GENERATE UID/AGY/SECAGY/SYSTEM/ADV TABLE HERE                                 
*                                                                               
MAIN090  DS    0H                                                               
*                                                                               
MAIN990  BRAS  RE,CLOSE            CLOSE FILES ECT                              
*                                                                               
         MVC   P(20),=CL20'CTSECXML DONE'                                       
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
*                                                                               
XBASE    XBASE                     PROG EXIT                                    
*                                                                               
NEQXIT   DS    0X                                                               
EXITN    LTR   RB,RB               EXIT CC=NEQ                                  
         J     EXIT                                                             
EQXIT    DS    0X                                                               
EXITY    CR    RB,RB               EXIT CC=EQU                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISATION / OPEN FILES                        *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         ZAP   COUNTER,=P'1'                                                    
         MVC   DATADISP,=AL2(28)   FOR CTFILE                                   
*                                                                               
         L     R1,=V(SSB)                                                       
         OI    SSOFLAG1-SSOOFF(R1),SSOFXCPY   TURN OFF SSB COPY                 
         MVI   SSOXTND-SSOOFF(R1),X'FF'       EXTENDED OFFLINE SSB              
         MVI   SSODSPAC-SSOOFF(R1),C'A'       DEFAULT IS DSPACE=A               
*                                                                               
         MVC   DUB,=CL8'T00A38'   OFFICER                                       
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   AOFFICER,DMCB+4                                                  
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(2,TODAY)                                  
*                                                                               
         MVI   TRANFLAG,C'N'                                                    
         MVI   ADVLKUP,C'N'                                                     
         MVI   SHOWBAD,C'N'                                                     
         MVI   SHOINACT,C'N'                                                    
         MVC   DATATYPE,=CL10'SECXML' DEFAULT                                   
*                                                                               
         LA    R3,WORK                                                          
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'*',0(R3)                                                      
         JE    INIT010                                                          
         CLC   =C'/*',0(R3)                                                     
         JE    INIT020                                                          
*                                                                               
         CLC   =C'DSPACE=',0(R3)                                                
         JNE   INIT010A                                                         
*                                                                               
         ICM   RF,15,=V(SSB)                                                    
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         J     INIT010                                                          
*                                                                               
INIT010A DS    0H                                                               
         CLC   =C'DDSIO=',0(R3)                                                 
         JNE   INIT010B                                                         
         L     RF,=V(DDSIO)        SET UP DDSIO OVERRIDE                        
         MVC   0(8,RF),6(R3)                                                    
         J     INIT010                                                          
*                                                                               
INIT010B DS    0H                                                               
         CLC   =C'TRANSLATE=',0(R3)                                             
         JNE   INIT010C                                                         
         MVC   TRANFLAG,10(R3)                                                  
         J     INIT010                                                          
*                                                                               
INIT010C DS    0H                                                               
         CLC   =C'DATATYPE=',0(R3)                                              
         JNE   INIT010D                                                         
         MVC   DATATYPE,9(R3)                                                   
         J     INIT010                                                          
*                                                                               
INIT010D DS    0H                                                               
         CLC   =C'ADVLOOKUP=',0(R3)                                             
         JNE   INIT010E                                                         
         MVC   ADVLKUP,10(R3)                                                   
         J     INIT010                                                          
*                                                                               
INIT010E DS    0H                                                               
         CLC   =C'SHOWBAD=',0(R3)                                               
         JNE   INIT010F                                                         
         MVC   SHOWBAD,8(R3)                                                    
         J     INIT010                                                          
*                                                                               
INIT010F DS    0H                                                               
         CLC   =C'AGENCY=',0(R3)                                                
         JNE   INIT010G                                                         
*                                                                               
         LA    R3,7(R3)                                                         
         LA    RF,AGYFILTS                                                      
         LHI   R0,AGYFILTNQ                                                     
                                                                                
INIT010F1 CLI  0(RF),X'00'         FIND FIRST EMPTY SLOT IN TABLE               
         JNH   INIT010F2                                                        
         AHI   RF,2                                                             
         JCT   R0,INIT010F1                                                     
                                                                                
INIT010F2 MVC   0(2,RF),0(R3)      COPY TO FILTERS TABLE                        
         AHI   R3,2                                                             
         AHI   RF,2                                                             
         CLI   0(R3),C' '          END OF STRING OF AGENCY CODES                
         JNH   INIT010             READ NEXT CONTROL CARD                       
         CLI   0(R3),C','                                                       
         JNE   *+2                                                              
         AHI   R3,1                SKIP COMMA                                   
         JCT   R0,INIT010F2                                                     
         DC    H'0'                AGENCY TABLE FULL                            
*                                                                               
INIT010G DS    0H                                                               
         CLC   =C'INACTIVE=',0(R3)                                              
         JNE   INIT010X                                                         
         MVC   SHOINACT,9(R3)                                                   
         J     INIT010                                                          
*                                                                               
INIT010X DS    0H                                                               
         MVC   P(30),0(R3)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=CL30'ERROR: INVALID CONTROL CARD'                         
         GOTO1 =V(PRINTER)                                                      
         J     *+2                                                              
*                                                                               
         IEABRCX ENABLE                                                         
INIT020  OPEN  (TAPEOUT,OUTPUT)                                                 
         OPEN  (TAPEOUTL,OUTPUT)                                                
         IEABRCX DISABLE                                                        
*                                                                               
         MVI   CTINIT,C'N'                                                      
*                                                                               
*                                                                               
INIT040  L     R1,=V(UTL)          OPEN CTRL FILES                              
         MVI   4(R1),X'0A'                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,CONTROL,CTFILES,IO                       
         CLI   8(R1),0                                                          
         J     *+6                                                              
         DC    H'0'                                                             
*                                                                               
* OBTAIN STORAGE AND INITIALIZE BINSEARCH PARAMETERS                            
*                                                                               
* ALWAYS SET UP AGENCY - SECURITY AGENCY TABLE                                  
SECTAB_MAX_ENTRIES EQU 1500                                                     
SECTAB_ENTRY_SIZE  EQU 5       AGENCY(2), SECURITY AGENCY(2), FLAG(1)           
*                              FLAG: X'80' = INACTIVE                           
*                                                                               
         XC    BSPARS1,BSPARS1 INITIALIZE BINSRCH PARAMETERS                    
         ICM   R0,15,=AL4(SECTAB_MAX_ENTRIES*SECTAB_ENTRY_SIZE)                 
         GOTO1 =A(GETMEM),DMCB,(R0),BSP1_2                                      
*                                                                               
         MVI   BSP1_4+3,SECTAB_ENTRY_SIZE                                       
         MVI   BSP1_5+3,2      P5 = L'KEY, AGENCY(2)                            
         MVC   BSP1_6+2(2),=AL2(SECTAB_MAX_ENTRIES)                             
*                                                                               
* ALWAYS SET UP SECURITY AGENCY - AGENCY TABLE                                  
* BINSRCH PARAMS IDENTICAL TO AGY-SECAGY, EXCEPT TABLE ADDRESS                  
         MVC   BSPARS2,BSPARS1 INITIALIZE BINSRCH PARAMETERS                    
         ICM   R0,15,=AL4(SECTAB_MAX_ENTRIES*SECTAB_ENTRY_SIZE)                 
         GOTO1 =A(GETMEM),DMCB,(R0),BSP2_2                                      
*                                                                               
         CLC   DATATYPE,=CL10'SYSADV'                                           
         JE    *+14                                                             
         CLC   DATATYPE,=CL10'SYSPROG'                                          
         JNE   INIT990                                                          
*                                                                               
* SET UP UID-SYSTEMS-PROGRAMS TABLE                                             
PROGTAB_ENTRY_LENGTH EQU PROGTABDLQ ID(10),AGY(2) 6X(SE(1),PROGS(32))           
PROGTAB_MAX_ENTRIES EQU 65535                                                   
*                                                                               
         XC    BSPARS3,BSPARS3 INITIALIZE BINSRCH PARAMETERS                    
         ICM   R0,15,=AL4(PROGTAB_ENTRY_LENGTH*PROGTAB_MAX_ENTRIES)             
         GOTO1 =A(GETMEM),DMCB,(R0),BSP3_2                                      
*                                                                               
         MVI   BSP3_4+3,PROGTAB_ENTRY_LENGTH                                    
         MVI   BSP3_5+3,L'PTID P5 = L'KEY, UID(10)                              
         MVC   BSP3_6+2(2),=AL2(PROGTAB_MAX_ENTRIES)                            
*                                                                               
* SET UP PROGRAM NAMES TABLE                                                    
PNAMTAB_ENTRY_LENGTH EQU 9     SYSTEM(1), PROGRAM(1), PROGRAM NAME(7)           
PNAMTAB_MAX_ENTRIES EQU 300                                                     
         XC    BSPARS4,BSPARS4 INITIALIZE BINSRCH PARAMETERS                    
         ICM   R0,15,=AL4(PNAMTAB_ENTRY_LENGTH*PNAMTAB_MAX_ENTRIES)             
         GOTO1 =A(GETMEM),DMCB,(R0),BSP4_2                                      
*                                                                               
         MVI   BSP4_4+3,PNAMTAB_ENTRY_LENGTH                                    
         MVI   BSP4_5+3,2      P5 = L'KEY, SYSTEM(1), PROGRAM(1)                
         MVC   BSP4_6+2(2),=AL2(PNAMTAB_MAX_ENTRIES)                            
*                                                                               
* SET UP PID'S USER IDS TABLE                                                   
PIDUID_ENTRY_LENGTH EQU 10    UID(10)                                           
PIDUID_MAX_ENTRIES EQU 65535                                                    
         XC    BSPARS5,BSPARS5 INITIALIZE BINSRCH PARAMETERS                    
         ICM   R0,15,=AL4(PIDUID_ENTRY_LENGTH*PIDUID_MAX_ENTRIES)               
         GOTO1 =A(GETMEM),DMCB,(R0),BSP5_2                                      
*                                                                               
         MVI   BSP5_4+3,PIDUID_ENTRY_LENGTH                                     
         MVI   BSP5_5+3,PIDUID_ENTRY_LENGTH P5 = L'KEY, UID(10)                 
         MVC   BSP5_6+2(2),=AL2(PIDUID_MAX_ENTRIES)                             
*                                                                               
*&&US                                                                           
         GOTO1 =V(DMFATABS),DMCB                                                
         MVC   DMTABS(DMTABSLQ),0(R1) A(SYSTAB,FILTAB,SYSTABS,FACIDTAB)         
*&&                                                                             
*&&UK                                                                           
         GOTO1 =V(DMOD000),DMCB,(0,2),(1,0)                                     
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         AHI   R1,-16              BACK UP TO SYSTABS (DMFILESUK)               
         ST    R1,DMTABS+8                                                      
*                                                                               
         LARL  R1,FACIDTAB                                                      
         ST    R1,DMTABS+12                                                     
*&&                                                                             
*                                                                               
INIT990  J     EXIT                                                             
*                                                                               
*                                                                               
XML001   DC    CL32'mainframesecurity'                                          
XML002   DC    CL32'personlist'                                                 
XML003   DC    CL32'secagy'                                                     
XML004   DC    CL32'per'                                                        
XML005   DC    CL32'perid'                                                      
XML006   DC    CL32'paswd'                                                      
XML007   DC    CL32'fstnm'                                                      
XML008   DC    CL32'midnm'                                                      
XML009   DC    CL32'lstnm'                                                      
XML010   DC    CL32'secadmin'                                                   
XML011   DC    CL32'offcd'                                                      
XML012   DC    CL32'offcdnm'                                                    
XML013   DC    CL32'deptcd'                                                     
XML014   DC    CL32'deptnm'                                                     
XML015   DC    CL32'secgrp'                                                     
XML016   DC    CL32'secgrpnm'                                                   
XML017   DC    CL32'tsappgrp'                                                   
XML018   DC    CL32'tsappgrpnm'                                                 
XML019   DC    CL32'email'                                                      
XML020   DC    CL32'addr'                                                       
XML021   DC    CL32'city'                                                       
XML022   DC    CL32'zip'                                                        
XML023   DC    CL32'ctry'                                                       
XML024   DC    CL32'tel'                                                        
XML025   DC    CL32'telex'                                                      
XML026   DC    CL32'fax'                                                        
XML027   DC    CL32'state'                                                      
XML028   DC    CL32'secgrpnum'                                                  
*                                                                               
XML029   DC    CL32'title'                                                      
XML030   DC    CL32'staffcode'                                                  
XML031   DC    CL32'hiredate'                                                   
XML032   DC    CL32'termdate'                                                   
XML033   DC    CL32'effdate'                                                    
XML034   DC    CL32'actdate'                                                    
XML035   DC    CL32'acttime'                                                    
*                                                                               
XML050   DC    CL32'securitylist'                                               
*                                                                               
XML100   DC    CL32'usrlist'                                                    
XML101   DC    CL32'agylst'                                                     
XML102   DC    CL32'id'                                                         
XML103   DC    CL32'companyid'                                                  
XML104   DC    CL32'userlistlist'                                               
XML105   DC    CL32'agylistlist'                                                
*                                                                               
XML106   DC    CL32'limit'                                                      
XML107   DC    CL32'office1'                                                    
XML108   DC    CL32'office2'                                                    
XML109   DC    CL32'officelist'                                                 
XML110   DC    CL32'client'                                                     
XML111   DC    CL32'clientgroup'                                                
XML112   DC    CL32'market'                                                     
*                                                                               
* NB: tags 113-116 must be in sequence, they're for the UK                      
* limit access                                                                  
XML113   DC    CL32'creativeagency'                                             
XML114   DC    CL32'buyingagency'                                               
XML115   DC    CL32'limitaccess1'                                               
XML116   DC    CL32'limitaccess2'                                               
*                                                                               
XML117   DC    CL32'limitlist'                                                  
XML118   DC    CL32'clientlimitaccesslist'                                      
XML119   DC    CL32'minusclient'                                                
XML120   DC    CL32'useridnum'                                                  
XML121   DC    CL32'peridnum'                                                   
*                                                                               
XML201   DC    CL32'agyofficelists'                                             
XML202   DC    CL32'agency'                                                     
XML203   DC    CL32'system'                                                     
XML204   DC    CL32'officelist'                                                 
*                                                                               
XML210   DC    CL32'alphalist'                                                  
*                                                                               
*                                                                               
*                                                                               
XML_SYSPROG DC CL32'sysprog'                                                    
XML_PERSON  DC CL32'person'                                                     
XML_PID     DC CL32'pid'                                                        
XML_SECAGY  DC CL32'secagy'                                                     
XML_USERID  DC CL32'userid'                                                     
XML_ID      DC CL32'id'                                                         
XML_AGENCY  DC CL32'agency'                                                     
XML_SYSTEM  DC CL32'system'                                                     
XML_SYSNAME DC CL32'sysname'                                                    
XML_ADV     DC CL32'adv'                                                        
XML_PROGRAM DC CL32'program'                                                    
XML_PROGNAM DC CL32'progname'                                                   
XML_PROGNUM DC CL32'prog'                                                       
XML_PROGTAB DC CL32'progtab'                                                    
*                                                                               
*                                                                               
*************************************************************                   
*        PUT USER DATA TO SORT                              *                   
*************************************************************                   
         SPACE 1                                                                
PUTUSER  NTR1                                                                   
         L     R8,AXMLBLK                                                       
         USING XMLBLKD,R8                                                       
*                                                                               
         LARL  R2,SUCARD1                                                       
         LARL  R3,SUCARD2                                                       
         GOTO1 =V(SORTER),DMCB,(R2),(R3)                                        
*                                                                               
         MVI   CTINIT,C'N'                                                      
         XC    SELOW,SELOW                                                      
         XC    SEHIGH,SEHIGH                                                    
         MVI   SELOW,C'I'                                                       
         MVC   SELOW+23(2),=X'0001'                                             
         MVI   SEHIGH,C'I'                                                      
         MVC   SEHIGH+23(2),=X'FFFF'                                            
*                                                                               
PUTUS    BRAS  RE,GETSE                                                         
         JNE   PUTUS990                                                         
         XC    WORK,WORK                                                        
*                                                                               
         USING CTIREC,R2                                                        
         L     R2,AIO                                                           
*                                                                               
         MVC   WORK+12(2),CTIKNUM-CTIREC(R2) BINARY ID                          
*                                                                               
         LA    R3,CTIDATA          ELEMENTS - LOOK FOR USERID AGY               
         XC    DUB,DUB                                                          
*                                                                               
PUTUS20  CLI   0(R3),0             EOR                                          
         JE    PUTUS50                                                          
         CLI   0(R3),CTDSCELQ      USER                                         
         JE    PUTUS25                                                          
         CLI   0(R3),CTAGYELQ      AGY                                          
         JE    PUTUS26                                                          
         CLI   0(R3),CTSYSELQ      SYSTEM AUTHORIZATION ELEMENT                 
         JE    PUTUS27                                                          
*                                                                               
         J     PUTUS30             NEXT ELEMENT                                 
*                                                                               
PUTUS25  MVC   WORK+2(10),2(R3)    SAVE USERID                                  
         J     PUTUS30                                                          
*                                                                               
PUTUS26  MVC   WORK(2),2(R3)       SAVE AGY                                     
         J     PUTUS30                                                          
*                                                                               
PUTUS27  DS    0H                                                               
         OC    8(4,R3),8(R3)       LIMIT ACCESS - ANYTHING?                     
         JZ    PUTUS30                                                          
         LARL  RF,PUTUSSYS         LIST OF SUPPORTED SYSTEMS                    
*                                                                               
PUTUS28  CLI   0(RF),X'FF'                                                      
         JE    PUTUS30             SYSTEM NOT SUPPORTED, SKIP                   
         CLC   2(1,R3),0(RF)                                                    
         JE    *+12                                                             
         AHI   RF,1                                                             
         J     PUTUS28                                                          
*                                                                               
         LA    RF,WORK+14          LIST OF LIMIT ACCESS ELEMENTS                
         LHI   R0,L'PUTUSSYS       NUMBER OF SLOTS=SYSTEMS SUPPORTED            
         OC    0(5,RF),0(RF)       SLOT EMPTY?                                  
         JZ    *+16                                                             
         AHI   RF,5                                                             
         BRCT  R0,*-14                                                          
         J     PUTUS30                                                          
*                                                                               
         MVC   0(1,RF),2(R3)       SYSTEM                                       
         MVC   1(4,RF),8(R3)       LIMIT ACCESS                                 
         J     PUTUS30                                                          
*                                                                               
PUTUS30  XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTUS50                                                          
         AR    R3,R0               BUMP                                         
         J     PUTUS20                                                          
*                                                                               
PUTUS50  OC    WORK(2),WORK                                                     
         JZ    PUTUS                                                            
         OC    WORK+2(10),WORK+2                                                
         JZ    PUTUS                                                            
*                                                                               
*&&US*&& CLC   =C'#N',WORK         suppress #N in US                            
*&&UK*&& CLC   =C'#E',WORK         suppress #E in UK                            
         JE    PUTUS                                                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,PUT,WORK SUCARD1,2                               
*                                                                               
         J     PUTUS                                                            
*                                                                               
PUTUS990 MVI   BYTE,0              indicate <agylst> tag not open               
         XC    WORK,WORK                                                        
         XC    SAVEAGY,SAVEAGY                                                  
*                                                                               
PUTUS991 GOTO1 =V(SORTER),DMCB,GET                                              
         ICM   R3,15,4(R1)                                                      
         JZ    PUTUS995                                                         
*                                                                               
* make sure ID record keyed off 10-character alphabetical ID exists             
*                                                                               
         L     R2,AIO                                                           
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,2(R3)        USER ID FROM SORT RECORD                     
         MVC   CTSAVKEY,CTIKEY                                                  
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         TM    8(R1),X'80'                                                      
         JO    PUTUS991            ID RECORD NOT FOUND                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         CLC   CTSAVKEY,CTIKEY                                                  
         JNE   PUTUS991            ID RECORD NOT FOUND                          
*                                                                               
         CLC   SAVEAGY,0(R3)       SAME AGENCY?                                 
         JE    PUTUS994                                                         
*                                                                               
         MVC   SAVEAGY,0(R3)                                                    
*                                                                               
         CLI   BYTE,0              <agylst> tag open?                           
         JE    PUTUS992            no - don't close it                          
* </agylst>                                                                     
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
         XC    WORK,WORK                                                        
*                                                                               
PUTUS992 GOTO1 ,WORK,=C'alpha',(2,0(R3)),(255,0)    <agylst> attribute          
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML101           <agylst>                                     
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
         MVI   BYTE,1              indicate <agylst> tag open                   
*                                                                               
PUTUS994 DS    0H                                                               
         GOTO1 ,WORK,=C'alpha',(10,2(R3)),(255,0)    <id> attribute             
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML102           <id>                                         
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
*                                                                               
         XC    WORK,WORK                                                        
         XR    RF,RF                                                            
         ICM   RF,3,12(R3)                                                      
         EDITR (RF),(5,WORK),ZERO=NOBLANK,ALIGN=LEFT,WRK=WORK1                  
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML120           <useridnum>                                  
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
         LA    R3,14(R3)           LIMIT ACCESS                                 
         LHI   R4,L'PUTUSSYS       max systems                                  
PUTUS994A OC   0(5,R3),0(R3)       anything in this slot?                       
         JZ    PUTUS994X                                                        
*                                                                               
         MVC   GETSYSNO,0(R3)                                                   
         BRAS  RE,GETSYSN                                                       
* set attributes of <LIMIT> tag                                                 
         GOTO1 ,WORK,=C'system',(7,GETSYSNA),(255,0)                            
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML106           <limit>                                      
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
*&&UK                                                                           
         CLI   0(R3),X'04'         MEDIA SYSTEM?                                
         JNE   PUTUS994B5                                                       
*                                                                               
         CLC   =X'FFFF',1(R3)      L=?                                          
         JNE   PUTUS994B2                                                       
*                                                                               
         LARL  RF,XML117           <limitlist>                                  
         MVC   WORK(2),=C'L='                                                   
         MVC   WORK+2(2),3(R3)                                                  
         J     PUTUS994M                                                        
*                                                                               
PUTUS994B2 DS  0H                                                               
         LARL  RF,XML113           <CREATIVEAGENCY>                             
         EDITR (B1,1(R3)),(3,WORK),ZERO=NOBLANK,ALIGN=LEFT                      
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(3,WORK),AXMLBLK                      
*                                                                               
         LARL  RF,XML114           <BUYINGAGENCY>                               
         EDITR (B1,2(R3)),(3,WORK),ZERO=NOBLANK,ALIGN=LEFT                      
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(3,WORK),AXMLBLK                      
*                                                                               
         LARL  RF,XML115           <LIMITACCESS1>                               
         EDITR (B1,3(R3)),(3,WORK),ZERO=NOBLANK,ALIGN=LEFT                      
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(3,WORK),AXMLBLK                      
*                                                                               
         LARL  RF,XML116           <LIMITACCESS2>                               
         EDITR (B1,4(R3)),(3,WORK),ZERO=NOBLANK,ALIGN=LEFT                      
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(3,WORK),AXMLBLK                      
*                                                                               
         J     PUTUS994N                                                        
*                                                                               
PUTUS994B5 DS  0H                                                               
         CLI   0(R3),X'06'         ACC SYSTEM?                                  
         JNE   *+2                 SYSTEM UNKNOWN                               
*                                                                               
         LARL  RF,XML109           <OFFICELIST>                                 
         CLI   1(R3),C'$'          OFFICE LIST?                                 
         JNE   *+14                                                             
         MVC   WORK(1),2(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
         CLI   1(R3),C'*'          1/2 CHAR OFFICE?                             
         JNE   PUTUS994B7          unknown limit access type                    
*                                                                               
         CLI   2(R3),C'*'          2-CHAR OFFICE?                               
         JNE   PUTUS994B6                                                       
         LARL  RF,XML108           <OFFICE2>                                    
         MVC   WORK(2),3(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
PUTUS994B6 DS  0H                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         MVC   WORK(1),2(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
PUTUS994B7 DS  0H                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         MVC   WORK(3),=C'???'                                                  
         J     PUTUS994M                                                        
*&&                                                                             
*                                                                               
*&&US                                                                           
         LARL  RF,XML109           <OFFICELIST>                                 
         CLI   1(R3),C'$'          OFFICE LIST?                                 
         JNE   PUTUS994B9                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),2(R3)                                                    
         BRAS  RE,SETWLEN                                                       
         J     PUTUS994M1                                                       
*                                                                               
PUTUS994B9 DS  0H                                                               
         LARL  RF,XML112           <MARKET>                                     
         CLI   1(R3),C'+'          MARKET?                                      
         JNE   *+14                                                             
         MVC   WORK(3),2(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
         CLI   1(R3),C'*'          office/client group?                         
         JNE   PUTUS994K                                                        
*                                                                               
         CLI   0(R3),6             ACC?                                         
         JNE   PUTUS994E                                                        
*                                                                               
         CLI   2(R3),C'*'          2-CHAR OFFICE?                               
         JNE   PUTUS994C                                                        
         LARL  RF,XML108           <OFFICE2>                                    
         MVC   WORK(2),3(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
PUTUS994C DS   0H                                                               
         CLI   3(R3),C' '          client group?                                
         JNH   PUTUS994D                                                        
         LARL  RF,XML111           <clientgroup>                                
         MVC   WORK(3),2(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
PUTUS994D DS   0H                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         MVC   WORK(1),2(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
PUTUS994E DS   0H                                                               
         LARL  RF,XML111           <clientgroup>                                
         CLI   3(R3),C' '          client group?                                
         JNH   *+14                                                             
         MVC   WORK(3),2(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
         LA    RF,WORK1                                                         
         USING OFFICED,RF                                                       
         XC    WORK1,WORK1                                                      
*                                                                               
         CLI   0(R3),2             SPOT                                         
         JE    *+12                                                             
         CLI   0(R3),13            SPOT TRAFFIC                                 
         JNE   *+8                                                              
         MVI   OFCSYS,C'S'                                                      
*                                                                               
         CLI   0(R3),3             NET                                          
         JNE   *+8                                                              
         MVI   OFCSYS,C'N'                                                      
*                                                                               
         CLI   0(R3),4             PRINT                                        
         JNE   *+8                                                              
         MVI   OFCSYS,C'P'                                                      
*                                                                               
         MVC   OFCAGY,SAVEAGY            AGENCY ALPHA                           
         MVC   OFCOFC,2(R3)              1 BYTE OFFICE                          
         DROP  RF                                                               
*                                                                               
         GOTO1 AOFFICER,DMCB,(C'2',WORK1),ACOMFACS                              
*                                                                               
         TM    OFCINDS-OFFICED+WORK1,OFCINOLA+OFCIOINV                          
         JZ    PUTUS994F                                                        
*                                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         MVC   WORK(1),2(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
PUTUS994F DS   0H                                                               
         LARL  RF,XML108           <OFFICE2>                                    
         MVC   WORK(2),OFCOFC2-OFFICED+WORK1                                    
         J     PUTUS994M                                                        
*                                                                               
PUTUS994K DS   0H                                                               
         CLI   0(R3),4             PRINT                                        
         JNE   *+14                                                             
         MVC   WORK(3),1(R3)                                                    
         J     PUTUS994M                                                        
*                                                                               
         GOTO1 =V(CLUNPK),DMCB,1(R3),WORK                                       
         LARL  RF,XML110            client                                      
         J     PUTUS994M                                                        
*&&                                                                             
PUTUS994M DS   0H                                                               
         MVI   WLEN,4                                                           
*                                                                               
PUTUS994M1 DS  0H                                                               
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
PUTUS994N DS   0H                                                               
* </limit>                                                                      
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
PUTUS994P DS   0H                                                               
         AHI   R3,5                                                             
         J     PUTUS994A                                                        
*                                                                               
PUTUS994X DS   0H                                                               
* </id>                                                                         
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
         J     PUTUS991                                                         
*                                                                               
PUTUS995 DS    0H                                                               
* </agylist>                                                                    
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,END                                              
*                                                                               
         J     EXIT                                                             
*                                                                               
* SYSTEMS SUPPORTED - SPOT, NET, PRINT, ACC, SPOT TRAFFIC                       
PUTUSSYS DC    X'020304060D'                                                    
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*  AGUSERIDXXXX                                                                 
*                                                                               
SUCARD1  DC    C'SORT FIELDS=(1,12,BI,A) '                                      
* REC CARD:                                                                     
* 2 AGY POWERCODE                                                               
* 10 USER ID                                                                    
* 2  USER ID number                                                             
* 25 LIST OF 5-BYTE LIMIT ACCESS ELEMENTS (1 SYSTEM, 4 LIMIT ACCESS)            
*                                                                               
SUCARD2  DC    C'RECORD TYPE=F,LENGTH=39 '                                      
         DROP  R8                                                               
         EJECT                                                                  
*************************************************************                   
*        PUT L= USER DATA                                   *                   
*************************************************************                   
         SPACE 1                                                                
PUTLIST  NTR1                                                                   
         L     R8,AXMLBLK                                                       
         USING XMLBLKD,R8                                                       
*                                                                               
         MVI   CTINIT,C'N'                                                      
         XC    SELOW,SELOW                                                      
         XC    SEHIGH,SEHIGH                                                    
         MVI   SELOW,C'W'                                                       
         MVI   SELOW+17,C'I'                                                    
         MVI   SEHIGH,C'W'                                                      
         MVI   SEHIGH+17,C'J'                                                   
PUTLIS   BRAS  RE,GETSE                                                         
         JNE   PUTLS90                                                          
         XC    WORK,WORK                                                        
*                                                                               
         USING CTWREC,R2                                                        
         L     R2,AIO                                                           
         MVC   DUB,18(R2)                                                       
         CLI   DUB,C' '                                                         
         JL    PUTLIS                                                           
*                                                                               
         LA    RF,=C'list'         list=                                        
         MVI   WORK,4                                                           
         STCM  RF,7,WORK+1                                                      
         LA    RF,DUB              id                                           
         MVI   WORK+4,8                                                         
         STCM  RF,7,WORK+5                                                      
         MVI   WORK+8,255                                                       
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
*                                                                               
         MVI   BYTE,1                                                           
         LARL  RF,XML100           usrlist                                      
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
*                                                                               
         LA    R3,CTWDATA          ELEMENTS - LOOK FOR USERID AGY               
         XC    DUB,DUB                                                          
*                                                                               
PUTLS20  CLI   0(R3),0             EOR                                          
         JE    PUTLS50                                                          
*                                                                               
         CLI   0(R3),CTLSTELQ      LIST ELEMENTS                                
         JNE   PUTLS25                                                          
*                                                                               
         LARL  RF,XML102           USERID                                       
         XC    WORK,WORK                                                        
         MVC   WORK(10),3(R3)                                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(10,WORK),AXMLBLK                     
         J     PUTLS30             NEXT ELEMENT                                 
*                                                                               
PUTLS25  DS    0H                                                               
         CLI   0(R3),CTLINELQ      LIST INCLUDE ELEMENT                         
         JNE   PUTLS30                                                          
*                                                                               
         LARL  RF,XML102           USERID                                       
         XC    WORK,WORK                                                        
         MVC   WORK(2),=C'L='                                                   
         MVC   WORK+2(6),2(R3)                                                  
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(8,WORK),AXMLBLK                      
         J     PUTLS30             NEXT ELEMENT                                 
*                                                                               
PUTLS30  XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTLS50                                                          
         AR    R3,R0               BUMP                                         
         J     PUTLS20                                                          
*                                                                               
PUTLS50  GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
         J     PUTLIS                                                           
*                                                                               
PUTLS90  J     EXIT                                                             
*                                                                               
PUTLSRTX J     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*************************************************************                   
*        PUT PASSWORD USER DATA TO SORT                     *                   
*************************************************************                   
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         L     R8,AXMLBLK                                                       
         USING XMLBLKD,R8                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,SCARD1,SCARD2                                    
*                                                                               
         MVI   CTINIT,C'N'                                                      
         XC    SELOW,SELOW                                                      
         XC    SEHIGH,SEHIGH                                                    
         MVI   SELOW,SAPETYPQ      C'F', PERSON RECORD                          
         MVI   SELOW+1,SAPESUBQ    X'04' SUBTYPE                                
         MVC   SEHIGH(2),SELOW                                                  
         MVI   SEHIGH+2,X'FF'                                                   
*                                                                               
PUTSO00  BRAS  RE,GETSE                                                         
         JNE   PUTSO990                                                         
*                                                                               
         MVC   CTSAVKEY,CTOLDKEY   SAVE CURRENT KEY                             
         XC    SAVEPW,SAVEPW                                                    
*                                                                               
         L     R2,AIO                                                           
         USING SAPEREC,R2                                                       
         LA    R3,SAPEDATA                                                      
*                                                                               
PUTSO02  CLI   0(R3),0             EOR                                          
         JE    PUTSO09                                                          
*                                                                               
         CLI   0(R3),SAPWDELQ      PASSWORD ELEMENT                             
         JNE   *+14                                                             
         MVC   SAVEPW,SAPWDNUM-SAPWDD(R3)                                       
         J     PUTSO04                                                          
*                                                                               
* 10/18/2017 tzih                                                               
* per Neil Shelford: we will extract all records, including                     
* terminated ones.  java side will decide what to do with them                  
*                                                                               
*        CLI   0(R3),SAPERELQ      PERSONNEL DETAILS ELEMENT                    
*        JNE   PUTSO04                                                          
*                                                                               
*        CLC   SAPERDTE-SAPERD(2,R3),=X'0000'                                   
*        JE    PUTSO09             NO TERMINATION DATE                          
*        CLC   TODAY,SAPERDTE-SAPERD(R3)                                        
*        JH    PUTSO50             TERMINATED, GET NEXT PERSON RECORD           
*        J     PUTSO09                                                          
*                                                                               
PUTSO04  XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTSO09                                                          
         AR    R3,R0               BUMP                                         
         J     PUTSO02                                                          
*                                                                               
PUTSO09  DS    0H                                                               
         OC    SAVEPW,SAVEPW                                                    
         JZ    PUTSO50             GET NEXT PERSON RECORD                       
*                                                                               
         MVI   CTINIT,C'N'                                                      
         XC    SELOW,SELOW                                                      
         XC    SEHIGH,SEHIGH                                                    
         LA    R2,SELOW                                                         
         USING CT0REC,R2                                                        
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,CTSAVKEY+SAPEAGY-SAPEREC                                 
         MVC   CT0KNUM,SAVEPW                                                   
         MVC   SEHIGH,SELOW                                                     
         LA    R2,SEHIGH                                                        
         MVI   CT0KNUM-1,X'01'                                                  
*        MVC   CT0KNUM,=X'FFFF'                                                 
*                                                                               
         BRAS  RE,GETSE                                                         
         JNE   PUTSO50             GET NEXT PERSON RECORD                       
*                                                                               
         USING CT0REC,R2                                                        
         L     R2,AIO                                                           
*                                                                               
         CLC   SELOW,CT0KEY                                                     
         JNE   PUTSO50             GET NEXT PERSON RECORD                       
*                                                                               
         OC    CT0KEYS(20),CT0KEYS                                              
         JNZ   PUTSO50             GET NEXT PERSON RECORD                       
*                                                                               
         LA    R3,CT0DATA          ELEMENTS - LOOK FOR PERSON                   
         XC    DUB,DUB                                                          
*                                                                               
PUTSO20  CLI   0(R3),0             EOR                                          
         JE    PUTSO29                                                          
         CLI   0(R3),SAPALELQ      PERSONID                                     
         JNE   PUTSO25                                                          
*                                                                               
         MVC   DUB,2(R3)           SAVE PERSON ID IN DUB                        
         J     PUTSO29                                                          
*                                                                               
PUTSO25  XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTSO29                                                          
         AR    R3,R0               BUMP                                         
         J     PUTSO20                                                          
*                                                                               
PUTSO29  LA    R3,CT0DATA          ELEMENTS                                     
         XC    BYTE,BYTE                                                        
         OC    DUB,DUB             ANY PERSON ELEMENT                           
         JZ    PUTSO50             GET NEXT PERSON RECORD                       
*                                                                               
PUTSO30  CLI   0(R3),0             EOR                                          
         JE    PUTSO39                                                          
*                                                                               
         CLI   0(R3),CTIDELQ       USER ID                                      
         JNE   PUTSO35                                                          
         CLI   1(R3),X'0C'         NO NEW STYLE ELEMENTS                        
         JNE   PUTSO32                                                          
         CLC   2(10,R3),SPACES     IGNORE ALL SPACES                            
         JE    PUTSO35                                                          
*                                                                               
* 3 agy(or "all"), 2 secagy, 10 uid, 8 pid                                      
*                                                                               
         MVI   BYTE,1                                                           
         MVC   WORK+0(2),1(R2)     agency alpha                                 
         MVC   WORK+3(2),1(R2)     security agency alpha                        
         MVC   WORK+5(10),2(R3)    USER ID                                      
         MVC   WORK+15(8),DUB      saved PID                                    
*                                                                               
         OC    WORK+5(2),WORK+5    HO 2 BYTES=0, THEN - LIST(L=)                
         JNZ   *+10                                                             
         MVC   WORK+5(2),=C'L='                                                 
*                                                                               
* "ALL" IDs means this PID has access to all IDs under that                     
* security agency.  So, agency alpha is set to "ALL" as well                    
         CLC   =C'ALL ',WORK+5      "ALL" in UID Field?                         
         JNE   *+10                                                             
         MVC   WORK+0(3),=C'ALL'                                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,PUT,WORK                                         
         J     PUTSO35                                                          
*                                                                               
PUTSO32  DS    0H                                                               
         CLI   1(R3),X'06'                                                      
         JNE   PUTSO35                                                          
         CLC   =X'0001',2(R3)      AGENCY?                                      
         JNE   PUTSO35                                                          
*                                                                               
         MVI   BYTE,1                                                           
         MVC   WORK+0(2),4(R3)     AGENCY FROM ID(X'20') ELEM                   
         MVC   WORK+3(2),1(R2)     SECURITY AGENCY FROM RECORD KEY              
         MVC   WORK+5(10),=CL10'ALL'                                            
         MVC   WORK+15(8),DUB                                                   
         GOTO1 =V(SORTER),DMCB,PUT,WORK                                         
*                                                                               
PUTSO35  XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTSO39                                                          
         AR    R3,R0               BUMP                                         
         J     PUTSO30                                                          
*                                                                               
* END OF PERSONAL AUTH (C'0') RECORD                                            
*                                                                               
PUTSO39  CLI   BYTE,0              ANY USERID FOUND                             
         JNE   PUTSO50             NEXT PERSON RECORD                           
*                                                                               
         MVC   WORK+0(3),=C'ALL'                                                
         MVC   WORK+3(2),1(R2)                                                  
         MVC   WORK+5(10),=CL10'ALL'                                            
         MVC   WORK+15(8),DUB                                                   
         GOTO1 =V(SORTER),DMCB,PUT,WORK                                         
*                                                                               
PUTSO50  DS    0H                  RESTORE PERSON RECORD READ SEQUENCE          
         MVI   CTINIT,C'N'                                                      
         MVC   SELOW,CTSAVKEY      SAVED PERSON RECORD (C'F'X40') KEY           
         MVC   SEHIGH,SELOW                                                     
         MVC   SEHIGH+SAPEDEF-SAPEKEY(2),=X'FFFF'                               
*                                                                               
         BRAS  RE,GETSE                                                         
         JE    *+6                                                              
         DC    H'0'                SEQUENCE NOT RESTORED, VERY SAD              
*                                                                               
         USING CT0REC,R2                                                        
         L     R2,AIO                                                           
         CLC   SELOW,CT0KEY                                                     
         JE    *+6                                                              
         DC    H'0'                SEQUENCE NOT RESTORED, VERY SAD              
*                                                                               
         MVC   SEHIGH(2),SELOW                                                  
         MVI   SEHIGH+2,X'FF'                                                   
         J     PUTSO00             GET NEXT PERSON RECORD                       
*                                                                               
* ALL RECORDS READ IN AND PUT TO SORT HERE                                      
* RETRIEVE THEM, AND WRITE TO XML FILE                                          
*                                                                               
PUTSO990 DS    0H                                                               
         MVI   BYTE,0                                                           
PUTSO991 GOTO1 =V(SORTER),DMCB,GET                                              
         ICM   R3,15,4(R1)                                                      
         JZ    PUTSO995                                                         
*                                                                               
*&&US*&& CLC   =C'#N',0(R3)        suppress #N in US                            
*&&UK*&& CLC   =C'#E',0(R3)        suppress #E in UK                            
         JE    PUTSO991                                                         
*&&US*&& CLC   =C'#N',3(R3)        suppress #N in US                            
*&&UK*&& CLC   =C'#E',3(R3)        suppress #E in UK                            
         JE    PUTSO991                                                         
*                                                                               
         CLC   WORK(23),0(R3)                                                   
         JE    PUTSO991                                                         
*                                                                               
         CLC   =C'L=',5(R3)                                                     
         JE    PUTSO991Z                                                        
         CLC   =C'ALL',5(R3)                                                    
         JE    PUTSO991Z                                                        
*                                                                               
* Person records can have dead user IDs on them                                 
* read hi for user ID, to make sure it is valid                                 
*                                                                               
         L     R2,AIO                                                           
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,5(R3)        USER ID FROM SORT RECORD                     
         MVC   CTSAVKEY,CTIKEY                                                  
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         TM    8(R1),X'80'                                                      
         JO    PUTSO991            ID RECORD NOT FOUND                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         CLC   CTSAVKEY,CTIKEY                                                  
         JNE   PUTSO991            ID RECORD NOT FOUND                          
*                                                                               
PUTSO991Z DS   0H                                                               
         CLC   WORK(15),0(R3)                                                   
         JE    PUTSO994                                                         
*                                                                               
         CLI   BYTE,0                                                           
         JE    PUTSO992                                                         
*                                                                               
         GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
         XC    WORK,WORK                                                        
*                                                                               
PUTSO992 DS    0H                                                               
         MVI   WORK,5                                                           
         LA    RF,=C'alpha'        ALPHA=AG                                     
         STCM  RF,7,WORK+1                                                      
         MVI   WORK+4,2                                                         
         LA    RF,0(R3)            ag                                           
         STCM  RF,7,WORK+5                                                      
*                                                                               
         CLC   =C'ALL',0(R3)                                                    
         JNE   *+8                                                              
         MVI   WORK+4,3                                                         
*                                                                               
         MVI   WORK+8,8                                                         
         LA    RF,=C'secalpha'     SECALPHA                                     
         STCM  RF,7,WORK+9                                                      
         MVI   WORK+12,2                                                        
         LA    RF,3(R3)            ag                                           
         STCM  RF,7,WORK+13                                                     
*                                                                               
         MVI   WORK+16,9                                                        
         LA    RF,=C'companyid'    companyid                                    
         STCM  RF,7,WORK+17                                                     
         LA    RF,5(R3)            userid                                       
         MVI   WORK+20,10                                                       
         STCM  RF,7,WORK+21                                                     
*                                                                               
         MVI   WORK+24,255         end of attribute list                        
*                                                                               
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
*                                                                               
         MVI   BYTE,1                                                           
         LARL  RF,XML103           companyid                                    
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
*                                                                               
PUTSO994 LARL  RF,XML005           PERSONID                                     
         XC    WORK,WORK                                                        
         MVC   WORK(8),15(R3)                                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(10,WORK),AXMLBLK                     
         MVC   WORK(23),0(R3)                                                   
         J     PUTSO991                                                         
*                                                                               
PUTSO995 GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
         J     EXIT                                                             
*                                                                               
PUTSORTX J     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*************************************************************                   
*        XML OUTPUT EXIT ROUTINE                            *                   
*************************************************************                   
         SPACE 1                                                                
XMLOUT   NTR1                                                                   
         LR    R2,R1                                                            
         USING XMLBLKD,R2                                                       
*                                                                               
         LARL  RB,CTREPXML         RE-ESTABLISH RB                              
         L     RC,SAVERC           RE-ESTABLISH RC                              
*                                                                               
         LAY   R4,XMLIOL           IO AREA                                      
*                                                                               
         CLI   TRANFLAG,C'Y'       TRANSLATING TO ASCII?                        
         JNE   XMLOUT10                                                         
*                                                                               
         LA    R3,DDTRANP          DDTRAN PARM BLOCK                            
         USING DDTRAND,R3                                                       
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,XMLIOL         INPUT RECORD LENGTH                          
         SHI   RF,4                MINUS 4 VR RECLEN X'0000' BYTES              
         STCM  RF,15,DDTRLEN                                                    
         LA    RF,XMLIOA                                                        
         STCM  RF,15,DDTRAIN                                                    
         LA    RF,WRKIOA                                                        
         STCM  RF,15,DDTRAOUT                                                   
*&&UK*&& MVI   DDTRCTRY,CTRYUKQ    UK                                           
*&&US*&& MVI   DDTRCTRY,CTRYUSAQ   US                                           
         OI    DDTROPT,DDTRO_CRLFQ WE WANT CR,LF                                
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(DDTRAN),DMCB,DDTRANP                                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,XMLIOL                                                      
         AHI   RF,2                PLUS CR, LF                                  
         STCM  RF,3,WRKIOL                                                      
         LAY   R4,WRKIOL                                                        
*                                                                               
XMLOUT10 DS    0H                                                               
         AP    COUNTER,=P'1'                                                    
         PUT   TAPEOUT,(R4)        JUST PUT TO TAPEOUT                          
*                                                                               
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        XML OUTPUT EXIT ROUTINE 2                          *                   
*************************************************************                   
         SPACE 1                                                                
XMLOUTL  NTR1                                                                   
         LR    R2,R1                                                            
         USING XMLBLKD,R2                                                       
*                                                                               
         LARL  RB,CTREPXML         RE-ESTABLISH RB                              
         L     RC,SAVERC           RE-ESTABLISH RC                              
*                                                                               
         CLC   XMLLEN(2),=X'0004'                                               
         JL    *+2                                                              
         CLC   XMLLEN(2),=X'0FA0'                                               
         JH    *+2                                                              
         PUT   TAPEOUTL,XMLLEN     PUT TO TAPEOUTL                              
*                                                                               
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES                                        *                   
*************************************************************                   
         SPACE 1                                                                
CLOSE    NTR1                                                                   
*                                                                               
         IEABRCX ENABLE                                                         
         CLOSE TAPEOUT             CLOSE TAPES AND SORT                         
         CLOSE TAPEOUTL                                                         
         IEABRCX DISABLE                                                        
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,CONTROL,0,IO                             
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
         CLC   DATATYPE,=CL10'SYSADV'                                           
         JE    CLOSE10                                                          
         CLC   DATATYPE,=CL10'SYSPROG'                                          
         JNE   EXITY                                                            
*                                                                               
CLOSE10  DS    0H                                                               
         ICM   R0,15,=AL4(SECTAB_MAX_ENTRIES*SECTAB_ENTRY_SIZE)                 
         L     R1,BSP1_2                                                        
*                                                                               
         ST    RB,FULL                                                          
         BASR  RB,0                                                             
         PUSH  USING                                                            
         USING *,RB                                                             
         STORAGE RELEASE,LENGTH=(R0),ADDR=(R1),COND=NO                          
         POP   USING                                                            
         L     RB,FULL                                                          
*                                                                               
         J     EXITY                                                            
*                                                                               
************************************************************                    
*        READ NEXT RECORD FROM CTFILE                      *                    
************************************************************                    
         SPACE 1                                                                
GETSE    NTR1                                                                   
*                                                                               
         CLI   CTINIT,C'N'                                                      
         JNE   GETNXSE1                                                         
*                                                                               
GETSEDMP L     R2,AIO                                                           
         USING CTIREC,R2           READ RECORDS                                 
         XC    CTIKEY,CTIKEY                                                    
         MVC   CTIKEY,SELOW                                                     
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         TM    8(R1),X'80'                                                      
         JO    TAPEEND                                                          
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   CTOLDKEY,CTIKEY                                                  
         MVI   CTINIT,C'Y'                                                      
         CLC   CTOLDKEY,SEHIGH                                                  
         JL    GETNXSEX                                                         
         J     TAPEEND                                                          
*                                                                               
GETNXSE1 L     R2,AIO                                                           
         TM    READFLAG,X'80'      CHECK NEED TO RESTORE SEQ                    
         JO    GETNXSE2                                                         
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,CTIKEY,CTIKEY,IOWORK              
         TM    8(R1),X'80'                                                      
         JO    TAPEEND                                                          
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   CTOLDKEY,CTIKEY                                                  
         CLC   CTOLDKEY,SEHIGH                                                  
         JL    GETNXSEX                                                         
         J     TAPEEND                                                          
*                                                                               
GETNXSE2 MVI   READFLAG,0                                                       
         MVC   CTIKEY,CTOLDKEY                                                  
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         MVC   CTOLDKEY,CTIKEY                                                  
         CLC   CTOLDKEY,SEHIGH                                                  
         JL    GETNXSEX                                                         
         J     TAPEEND                                                          
*                                                                               
GETNXSEX J     EXITY                                                            
*                                                                               
TAPEEND  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* PUT TO TAPEOUT                                                      *         
***********************************************************************         
         SPACE 1                                                                
PUTSE    NTR1                                                                   
         L     R8,AXMLBLK                                                       
         USING XMLBLKD,R8                                                       
*                                                                               
         L     R2,AIO                                                           
         USING SAPEREC,R2          READ RECORDS                                 
         CLC   SAPEAGY,LASTAGY                                                  
         JE    PUTSE10                                                          
*                                                                               
         LARL  RF,XML050           securitylist                                 
         OC    LASTAGY,LASTAGY                                                  
         JZ    PUTSE01                                                          
*                                                                               
         GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
*                                                                               
PUTSE01  MVC   LASTAGY,SAPEAGY     Save current security agyagy                 
*                                                                               
         BRAS  RE,BUILDOFF         Build office code table                      
*                                                                               
         LA    RF,=C'alpha'        alpha=ag                                     
         MVI   WORK,5                                                           
         STCM  RF,7,WORK+1                                                      
         MVI   WORK+4,2                                                         
         LA    RF,LASTAGY                                                       
         STCM  RF,7,WORK+5                                                      
         MVI   WORK+8,255                                                       
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
*                                                                               
         LARL  RF,XML003           securityagency                               
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
*                                                                               
PUTSE10  XC    WORK,WORK                                                        
         MVC   WORK(8),SAPEPID                                                  
         LARL  RF,XML005           personalid                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(8,WORK),AXMLBLK                      
*                                                                               
PUTSE99  EQU   *   GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                   
         MVC   SAPEDEF,=X'FFFF'                                                 
         MVC   CTOLDKEY,0(R2)                                                   
         OI    READFLAG,X'80'      RESTORE SEQ                                  
*                                                                               
PUTSEX   J     EXITY                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PUT TO TAPEOUT                                                      *         
***********************************************************************         
         SPACE 1                                                                
PUTPE    NTR1                                                                   
         MVI   OPENFLAG,X'00'                                                   
         XC    LASTSYS,LASTSYS                                                  
*                                                                               
         L     R8,AXMLBLK                                                       
         USING XMLBLKD,R8                                                       
*                                                                               
         L     R2,AIO                                                           
         USING SAPEREC,R2          READ RECORDS                                 
*                                                                               
         CLI   SHOINACT,C'Y'       SHOWING INACTIVE AGENCIES?                   
         JE    PUTPE001            YES - DON'T BOTHER CHECKING                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),SAPEAGY     GET AGENCY FROM TABLE                        
         GOTO1 =A(BINS_GET),DMCB,AGY_SEC_TAB,WORK                               
         JNE   *+2                                                              
*                                                                               
         L     RF,DMCB+4                                                        
         SAM31                                                                  
         TM    4(RF),X'80'         AGENCY INACTIVE?                             
         JO    EQXIT               SKIP THIS RECORD                             
         SAM24                                                                  
*                                                                               
PUTPE001 DS    0H                                                               
         CLC   SAPEAGY,LASTAGY                                                  
         JE    PUTPE05                                                          
*                                                                               
         LARL  RF,XML003           Secagy                                       
         OC    LASTAGY,LASTAGY                                                  
         JZ    PUTPE01                                                          
*                                                                               
         GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
*                                                                               
PUTPE01  MVC   LASTAGY,SAPEAGY     Save current security agyagy                 
*                                                                               
         BRAS  RE,BUILDOFF         Build office code table                      
*                                                                               
         LA    RF,=C'alpha'        alpha=ag                                     
         MVI   WORK,5                                                           
         STCM  RF,7,WORK+1                                                      
         MVI   WORK+4,2                                                         
         LA    RF,LASTAGY                                                       
         STCM  RF,7,WORK+5                                                      
         MVI   WORK+8,255                                                       
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
*                                                                               
         LARL  RF,XML003           <secagy>                                     
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
*                                                                               
PUTPE05  DS    0H                                                               
*                                                                               
* 10/18/2017 tzih                                                               
* per Neil Shelford: we will extract all records, including                     
* terminated ones.  java side will decide what to do with them                  
*                                                                               
*UTPE05  LA    R3,SAPEDATA                                                      
*UTPE05A CLI   0(R3),SAPERELQ      PERSON DETAIL                                
*        JNE   PUTPE05B                                                         
*        CLC   SAPERDTE-SAPERD(2,R3),=X'0000'                                   
*        JE    PUTPE10                                                          
*        CLC   TODAY,SAPERDTE-SAPERD(R3)                                        
*        JH    PUTPE99B                                                         
*        JNH   PUTPE10                                                          
*                                                                               
*UTPE05B XR    R0,R0               NEXT ELEMENT                                 
*        ICM   R0,1,1(R3)                                                       
*        JZ    PUTPE10                                                          
*        AR    R3,R0               BUMP                                         
*        J     PUTPE05A                                                         
*                                                                               
PUTPE10  LARL  RF,XML004           <per>                                        
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(8),SAPEPID                                                  
         MVC   SAVEPER,SAPEPID                                                  
         LARL  RF,XML005           <perid>                                      
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(8,WORK),AXMLBLK                      
*                                                                               
         MVC   WORK(2),=X'FFFF'                                                 
         XC    WORK(2),SAPEDEF                                                  
         GOTO1 =V(DATCON),DMCB,(2,WORK),(21,WORK+2)                             
         LARL  RF,XML033           <effdate>                                    
*&&US*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(10,WORK+2),AXMLBLK                   
*&&UK*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(9,WORK+2),AXMLBLK                    
*                                                                               
         LA    R3,SAPEDATA         ELEMENTS                                     
*                                                                               
PUTPE20  CLI   0(R3),0             EOR                                          
         JE    PUTPE99                                                          
         CLI   0(R3),SAPWDELQ      PWD                                          
         JE    PUTPE50                                                          
         CLI   0(R3),SANAMELQ      NAME                                         
         JE    PUTPE60                                                          
         CLI   0(R3),SAPERELQ      PERSON DETAIL                                
         JE    PUTPE70                                                          
         CLI   0(R3),SAAGCELQ      SECURITY GROUP                               
         JE    PUTPE90                                                          
         CLI   0(R3),SAAPCELQ      TIMESHEET APPROVER                           
         JE    PUTPEA0                                                          
         CLI   0(R3),SAPEEELQ      EMAIL                                        
         JE    PUTPEB0                                                          
         CLI   0(R3),SAADRELQ      ADDRESS                                      
         JE    PUTPEC0                                                          
         CLI   0(R3),GACTELQ       ACTIVITY,X'FE'                               
         JE    PUTPED0                                                          
*                                                                               
PUTPE25  XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTPE99                                                          
         AR    R3,R0               BUMP                                         
         J     PUTPE20                                                          
*                                                                               
         USING SAPWDD,R3                                                        
PUTPE50  DS    0H                                                               
         XC    WORK,WORK                                                        
         XR    RF,RF                                                            
         ICM   RF,3,SAPWDNUM                                                    
         EDITR (RF),(6,WORK),ZERO=NOBLANK,ALIGN=LEFT,WRK=WORK1                  
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML121           <peridnum>                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
         MVC   SAVEPW,SAPWDNUM                                                  
*        XC    WORK,WORK                                                        
*        MVC   WORK(L'SAPWDCOD),SAPWDCOD                                        
*        LARL  RF,XML006           password                                     
*        GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(10,WORK),AXMLBLK                     
         J     PUTPE25                                                          
*                                                                               
         USING SANAMD,R3                                                        
PUTPE60  MVC   BYTE,SANAMIND                                                    
         LA    R4,SANAMES                                                       
*                                                                               
PUTPE61  TM    BYTE,SANAMIFN                                                    
         JZ    PUTPE62                                                          
         LARL  RF,XML007           firstname                                    
         MVC   BYTE1,0(R4)                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(20),1(R4)                                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(BYTE1,WORK),AXMLBLK                  
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
*                                                                               
PUTPE62  TM    BYTE,SANAMIMN                                                    
         JZ    PUTPE63                                                          
         LARL  RF,XML008           middlename                                   
         MVC   BYTE1,0(R4)                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(20),1(R4)                                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(BYTE1,WORK),AXMLBLK                  
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
*                                                                               
PUTPE63  TM    BYTE,SANAMILN                                                    
         JZ    PUTPE99                                                          
         LARL  RF,XML009           lastname                                     
         MVC   BYTE1,0(R4)                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(20),1(R4)                                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(BYTE1,WORK),AXMLBLK                  
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         J     PUTPE25                                                          
*                                                                               
         USING SAPERD,R3                                                        
PUTPE70  LARL  RF,XML010           securityadmin                                
         XC    WORK,WORK                                                        
         MVI   WORK,C'N'                                                        
         TM    SAPEROVF,X'80'      Y if X'80'                                   
         JNO   *+8                                                              
         MVI   WORK,C'Y'                                                        
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(1,WORK),AXMLBLK                      
*                                                                               
         LARL  RF,XML011           officecode                                   
         XC    WORK,WORK                                                        
         MVC   WORK(2),SAPEROFF                                                 
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(2,WORK),AXMLBLK                      
*                                                                               
         LARL  R1,OFFTAB                                                        
PUTPE71  CLC   0(2,R1),WORK                                                     
         JE    PUTPE72                                                          
         LA    R1,32(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         JNE   PUTPE71                                                          
         LA    R1,=C'XXUnknown!                      '                          
*                                                                               
PUTPE72  XC    WORK,WORK                                                        
         MVC   WORK(30),2(R1)                                                   
         LARL  RF,XML012           officecodename                               
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(30,WORK),AXMLBLK                     
*                                                                               
         LARL  RF,XML013           departmentcode                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),SAPERDID                                                 
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(3,WORK),AXMLBLK                      
*                                                                               
         LARL  R1,DEPTAB                                                        
PUTPE81  CLC   0(5,R1),SAPEROFF    OFFICE AND DEPT CHECK                        
         JE    PUTPE82                                                          
         LA    R1,35(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         JNE   PUTPE81                                                          
         LA    R1,=C'XXXXXUnknown!                      '                       
*                                                                               
PUTPE82  XC    WORK,WORK                                                        
         MVC   WORK(30),5(R1)                                                   
         LARL  RF,XML014           departmentname                               
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(30,WORK),AXMLBLK                     
*                                                                               
         LLC   RF,SAPERLN                                                       
         AHI   RF,-(SAPERLNQ)                                                   
         JZ    PUTPE84                                                          
*                                                                               
         XC    WORK,WORK                                                        
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   WORK(0),SAPERTIT                                                 
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML029           <title>                                      
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
PUTPE84  DS    0H                  extract staff code                           
         CLC   SAPERSTA,SPACES                                                  
         JNH   PUTPE86                                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'SAPERSTA),SAPERSTA                                        
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML030           <staffcode>                                  
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
PUTPE86  DS    0H                  hire date                                    
         CLC   SAPERDHI,SPACES                                                  
         JNH   PUTPE87                                                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,SAPERDHI),(21,WORK)                           
         LARL  RF,XML031           <hiredate>                                   
*&&US*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(10,WORK),AXMLBLK                     
*&&UK*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(9,WORK),AXMLBLK                      
*                                                                               
PUTPE87  DS    0H                  termination date                             
         CLC   SAPERDTE,=X'0000'                                                
         JNH   PUTPE25             get next element                             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,SAPERDTE),(21,WORK)                           
         LARL  RF,XML032           <termdate>                                   
*&&US*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(10,WORK),AXMLBLK                     
*&&UK*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(9,WORK),AXMLBLK                      
*                                                                               
         J     PUTPE25             get next element                             
*                                                                               
         USING SAAGCD,R3                                                        
PUTPE90  DS    0H                                                               
         XC    WORK,WORK                                                        
         XR    RF,RF                                                            
         ICM   RF,3,SAAGCNUM                                                    
         JZ    PUTPE25                                                          
         EDITR (RF),(5,WORK),ZERO=NOBLANK,ALIGN=LEFT,WRK=WORK1                  
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML028           <secgrpnum>                                  
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
         LARL  RF,XML015           <secgrp>                                     
         MVC   WORK(8),SAAGCCOD                                                 
         OC    WORK(8),WORK        IGNORE A ZERO ENTRY                          
         JZ    PUTPE25                                                          
*                                                                               
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(8,WORK),AXMLBLK                      
*                                                                               
         LARL  R1,SECTAB                                                        
PUTPE91  CLC   0(8,R1),WORK                                                     
         JE    PUTPE92                                                          
         LA    R1,38(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         JNE   PUTPE91                                                          
         LA    R1,=C'XXXXXXXXUnknown!                      '                    
*                                                                               
PUTPE92  XC    WORK,WORK                                                        
         MVC   WORK(30),8(R1)                                                   
         LARL  RF,XML016           securitygroupname                            
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(30,WORK),AXMLBLK                     
*                                                                               
         J     PUTPE25                                                          
*                                                                               
         USING SAAPCD,R3                                                        
PUTPEA0  LARL  RF,XML017           tsapprovergroup                              
         XC    WORK,WORK                                                        
         MVC   WORK(8),SAAPCCOD                                                 
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(3,WORK),AXMLBLK                      
*                                                                               
         LARL  R1,TSHTAB                                                        
         CLI   0(R1),X'FF'                                                      
         JE    PUTPEA1X                                                         
PUTPEA1  CLC   0(8,R1),WORK                                                     
         JE    PUTPEA2                                                          
         LA    R1,38(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         JNE   PUTPEA1                                                          
PUTPEA1X LA    R1,=C'XXXXXXXXUnknown!                      '                    
*                                                                               
PUTPEA2  XC    WORK,WORK                                                        
         MVC   WORK(30),8(R1)                                                   
         LARL  RF,XML018           tsapprovergroupname                          
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(30,WORK),AXMLBLK                     
*                                                                               
         J     PUTPE25                                                          
*                                                                               
         USING SAPEED,R3                                                        
PUTPEB0  LLC   R1,1(R3)                                                         
         AHI   R1,-(SAPEELNQ+1)                                                 
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   WORK(0),SAPEEID                                                  
         LARL  RF,XML019           EMAIL                                        
         AHI   R1,1                                                             
         LR    R0,R1                                                            
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),((R0),WORK),AXMLBLK                   
         J     PUTPE25                                                          
*                                                                               
         USING SAADRD,R3                                                        
         DC    H'0'                                                             
PUTPEC0  CLI   SAADRTYP,SAADLINQ  X'0N'=ADDRESS LINE N (1-3)                    
         JE    PUTPEC1                                                          
         CLI   SAADRTYP,SAADLINQ+1                                              
         JE    PUTPEC1                                                          
         CLI   SAADRTYP,SAADLINQ+2                                              
         JE    PUTPEC1                                                          
         CLI   SAADRTYP,SAADCITQ  X'04'=CITYESS LINE N (1-3)                    
         JE    PUTPEC2                                                          
         CLI   SAADRTYP,SAADCODQ  X'05'=CODE/STATENZIP (1-3)                    
         JE    PUTPEC3                                                          
         CLI   SAADRTYP,SAADCTRQ  X'06'=COUNTRY CODE (2(BYTES)                  
         JE    PUTPEC4                                                          
*&&US*&& CLI   SAADRTYP,SAADSTEQ  X'07'=STATE                                   
*&&US*&& JE    PUTPEC8                                                          
         CLI   SAADRTYP,SAADPHOQ  X'1N'=TELEPHONEINE(1 OR-2)                    
         JE    PUTPEC5                                                          
         CLI   SAADRTYP,SAADTLXQ  X'20'=TELEXSS LINE N (1-3)                    
         JE    PUTPEC6                                                          
         CLI   SAADRTYP,SAADFAXQ  X'30'=FAXRESS LINE N (1-3)                    
         JE    PUTPEC7                                                          
         J     PUTPE25                                                          
*                                                                               
PUTPEC1  LARL  RF,XML020                                                        
         J     PUTPEC9                                                          
PUTPEC2  LARL  RF,XML021                                                        
         J     PUTPEC9                                                          
PUTPEC3  LARL  RF,XML022                                                        
         J     PUTPEC9                                                          
PUTPEC4  LARL  RF,XML023                                                        
         J     PUTPEC9                                                          
PUTPEC5  LARL  RF,XML024                                                        
         J     PUTPEC9                                                          
PUTPEC6  LARL  RF,XML025                                                        
         J     PUTPEC9                                                          
PUTPEC7  LARL  RF,XML026                                                        
         J     PUTPEC9                                                          
PUTPEC8  LARL  RF,XML027                                                        
         J     PUTPEC9                                                          
*                                                                               
PUTPEC9  LLC   R0,SAADRDLN                                                      
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),((R0),SAADRDAT),AXMLBLK               
         J     PUTPE25                                                          
*                                                                               
*                                                                               
*                                                                               
         USING GACTELD,R3                                                       
PUTPED0  DS    0H                                                               
         OC    GACTCDT,GACTCDT                                                  
         JZ    PUTPED02                                                         
         GOTO1 =V(DATCON),DMCB,(3,GACTCDT),(21,WORK)                            
         LARL  RF,XML034           <actdate>                                    
*&&US*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(10,WORK),AXMLBLK                     
*&&UK*&& GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(9,WORK),AXMLBLK                      
*                                                                               
PUTPED02 DS    0H                                                               
         XR    R0,R0                                                            
         ICM   R0,7,GACTTIM                                                     
         JZ    PUTPE25                                                          
         BRAS  RE,PRTTBIN                                                       
         LARL  RF,XML035           <ACTTIME>                                    
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(11,WORK),AXMLBLK                     
         J     PUTPE25                                                          
*                                                                               
*                                                                               
*                                                                               
PUTPE99  DS    0H                                                               
         MVC   SAPEDEF,=X'FFFF'                                                 
         MVC   CTOLDKEY,0(R2)                                                   
         OI    READFLAG,X'80'      RESTORE SEQ                                  
*                                                                               
         MVC   CTSAVKEY,CTOLDKEY   Save current keys for return                 
         MVC   CTSAVLO,SELOW                                                    
         MVC   CTSAVHI,SEHIGH                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         USING SA0REC,R2                                                        
*                                                                               
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,LASTAGY                                                  
         MVC   SA0KNUM,SAVEPW                                                   
*                                                                               
         MVI   CTINIT,C'N'                                                      
         MVC   SELOW,SA0KEY                                                     
         MVC   SEHIGH,SA0KEY                                                    
         MVC   SEHIGH+SA0KNUM-SA0REC(2),=X'FFFF'                                
*                                                                               
         BRAS  RE,GETSE            READ FILE                                    
         JNE   PUTPE300                                                         
*                                                                               
         MVI   SECFLAG,X'00'                                                    
         LA    R3,SA0DATA                                                       
*                                                                               
* READ FOR E1(SACLAEL) ELEMENTS.                                                
* IF PRESENT, THEN CLIENTS ON X'21' ELEMENTS SHOULD BE IGNORED                  
PUTPE101 CLI   0(R3),X'00'         EOR                                          
         JE    PUTPE104                                                         
         CLI   0(R3),SACLAELQ      x'E1'                                        
         JNE   PUTPE102                                                         
         OI    SECFLAG,SECFE1Q     INDICATE E1'S ARE PRESENT                    
         J     PUTPE104                                                         
*                                                                               
PUTPE102 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTPE104            EOR                                          
         AR    R3,R0                                                            
         J     PUTPE101                                                         
*                                                                               
PUTPE104 LA    R3,SA0DATA                                                       
PUTPE105 CLI   0(R3),0                                                          
         JE    PUTPE300            eor                                          
*                                                                               
         XC    AXMLTAG,AXMLTAG                                                  
         XC    WORK,WORK                                                        
* for client list                                                               
         XC    ACLTCOD,ACLTCOD                                                  
         XC    AENDCLST,AENDCLST                                                
* for uk limit access                                                           
         XC    ALSTCOD,ALSTCOD                                                  
         XC    ANXTTAG,ANXTTAG                                                  
         XC    AENDLST,AENDLST                                                  
*                                                                               
         CLI   0(R3),SASYSELQ      x'21'                                        
         JE    PUTPE120                                                         
         CLI   0(R3),SACLAELQ      x'E1'                                        
         JE    PUTPE121                                                         
*                                                                               
PUTPE110 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTPE300            eor                                          
         AR    R3,R0                                                            
         J     PUTPE105                                                         
*                                                                               
PUTPE120 DS    0H                  x'21', sys auth element                      
         USING SASYSD,R3                                                        
         OC    SASYSLMT,SASYSLMT                                                
         JZ    PUTPE110                                                         
         MVC   SAVESYS,SASYSNUM                                                 
         J     PUTPE122                                                         
         DROP  R3                                                               
*                                                                               
PUTPE121 DS    0H                  x'E1', limit access element                  
         USING SACLAD,R3                                                        
         OC    SACLACOD,SACLACOD                                                
         JZ    PUTPE110                                                         
         MVC   SAVESYS,SACLASYS                                                 
         DROP  R3                                                               
*                                                                               
PUTPE122 DS    0H                                                               
*                                                                               
* SEE IF WE EVEN NEED TO EXTRACT THIS SYSTEM                                    
*                                                                               
         LARL  RF,PUTUSSYS         LIST OF SUPPORTED SYSTEMS                    
PUTPE125 CLI   0(RF),X'FF'                                                      
         JE    PUTPE110            SYSTEM NOT SUPPORTED, SKIP                   
         CLC   SAVESYS,0(RF)                                                    
         JE    *+12                                                             
         AHI   RF,1                                                             
         J     PUTPE125                                                         
*                                                                               
* READ ACCESS RECORD, MAKE SURE AGENCY HAS ACCESS TO THE SYSTEM                 
* THAT IS ON THE LIMIT ACCESS ELEMENT                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK             PARAMETERS FOR READACC                       
         MVC   WORK(2),SA0KAGY                                                  
         MVC   WORK+2(1),SAVESYS                                                
         BRAS  RE,READACC                                                       
         JE    PUTPE127            ALL OK                                       
*                                                                               
         MVC   P(2),SA0KAGY                                                     
         MVC   P+3(8),SAVEPER                                                   
         MVC   P+12(30),=CL30'21 ELEM, NO SYS ACCESS'                           
         GOTO1 =V(PRINTER)                                                      
         J     PUTPE110                                                         
*                                                                               
* PROCESS THE LIMIT ACCESS VALUE                                                
*                                                                               
PUTPE127 DS    0H                                                               
         CLI   0(R3),SASYSELQ      X'21'?                                       
         JNE   PUTPE185                                                         
*                                                                               
         USING SASYSD,R3                                                        
         XC    WORK,WORK                                                        
*                                                                               
*&&UK                                                                           
         CLI   SASYSNUM,X'04'      MEDIA SYSTEM?                                
         JNE   PUTPE135                                                         
*                                                                               
         CLC   =X'FFFF',SASYSLMT   L=?                                          
         JNE   PUTPE131                                                         
*                                                                               
         LARL  RF,XML117           <limitlist>                                  
         ST    RF,AXMLTAG                                                       
         MVC   WORK(2),=C'L='                                                   
         MVC   WORK+2(2),SASYSLMT+2                                             
         BRAS  RE,SETWLEN                                                       
         J     PUTPE200                                                         
*                                                                               
PUTPE131 DS    0H                                                               
         LA    RF,SASYSLMT         1ST UK LIMIT CATEGORY                        
         ST    RF,ALSTCOD                                                       
         AHI   RF,4                                                             
         ST    RF,AENDLST                                                       
         LARL  RF,XML113           1ST OF THE 4 UK LIMIT TAGS                   
         ST    RF,ANXTTAG                                                       
*                                                                               
PUTPE132 DS    0H                                                               
         L     RF,ALSTCOD                                                       
         LLC   RF,0(RF)                                                         
         EDITR (RF),(3,WORK),ZERO=NOBLANK,ALIGN=LEFT,WRK=WORK1                  
         BRAS  RE,SETWLEN                                                       
         MVC   AXMLTAG,ANXTTAG                                                  
         J     PUTPE200                                                         
*                                                                               
PUTPE135 DS    0H                                                               
         CLI   SASYSNUM,X'06'      ACC SYSTEM?                                  
         JNE   *+2                 SYSTEM UNKNOWN                               
*                                                                               
         CLI   SASYSLMT,C'$'       OFFICE LIST?                                 
         JNE   PUTPE136                                                         
*                                                                               
         LARL  RF,XML109           <OFFICELIST>                                 
         ST    RF,AXMLTAG                                                       
         MVC   WORK(1),SASYSLMT+1                                               
         MVI   WLEN,1                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE136 DS    0H                                                               
         CLI   SASYSLMT,C'*'       1/2 CHAR OFFICE?                             
         JNE   PUTPE138            UNKNOWN LIMIT ACCESS TYPE                    
*                                                                               
         CLI   SASYSLMT+1,C'*'     2-CHAR OFFICE?                               
         JNE   PUTPE137                                                         
*                                                                               
         LARL  RF,XML108           <OFFICE2>                                    
         ST    RF,AXMLTAG                                                       
         MVC   WORK(2),SASYSLMT+2                                               
         MVI   WLEN,2                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE137 DS    0H                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         ST    RF,AXMLTAG                                                       
         MVC   WORK(1),SASYSLMT+1                                               
         MVI   WLEN,1                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE138 DS    0H                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         ST    RF,AXMLTAG                                                       
         MVC   WORK(3),=C'???'                                                  
         MVI   WLEN,3                                                           
         J     PUTPE200                                                         
*&&                                                                             
*                                                                               
*&&US                                                                           
         CLI   SASYSLMT,C'$'       OFFICE LIST?                                 
         JNE   PUTPE140                                                         
*                                                                               
         LARL  RF,XML109           <OFFICELIST>                                 
         ST    RF,AXMLTAG                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(2),SASYSLMT+1                                               
         BRAS  RE,SETWLEN                                                       
         J     PUTPE200                                                         
*                                                                               
PUTPE140 DS    0H                                                               
         CLI   SASYSLMT,C'+'          MARKET?                                   
         JNE   PUTPE142                                                         
         MVC   WORK(3),SASYSLMT+1                                               
         MVI   WLEN,3                                                           
         LARL  RF,XML112           <MARKET>                                     
         ST    RF,AXMLTAG                                                       
         J     PUTPE200                                                         
*                                                                               
PUTPE142 DS    0H                                                               
         CLI   SASYSLMT,C'*'       office/client group?                         
         JNE   PUTPE180                                                         
*                                                                               
         CLI   SASYSNUM,X'06'      ACC?                                         
         JNE   PUTPE160                                                         
*                                                                               
* ACC SYSTEM HERE, DON'T CALL OFFICER                                           
*                                                                               
         CLI   SASYSLMT+1,C'*'     2-CHAR OFFICE?                               
         JNE   PUTPE145                                                         
         LARL  RF,XML108           <OFFICE2>                                    
         ST    RF,AXMLTAG                                                       
         MVC   WORK(2),SASYSLMT+2                                               
         MVI   WLEN,2                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE145 DS    0H                                                               
         CLI   SASYSLMT+2,C' '     client group?                                
         JNH   PUTPE150                                                         
         LARL  RF,XML111           <clientgroup>                                
         ST    RF,AXMLTAG                                                       
         MVC   WORK(3),SASYSLMT+1                                               
         MVI   WLEN,3                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE150 DS    0H                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         ST    RF,AXMLTAG                                                       
         MVC   WORK(1),SASYSLMT+1                                               
         MVI   WLEN,1                                                           
         J     PUTPE200                                                         
*                                                                               
* NON-ACC SYSTEM, MUST CALL OFFICER                                             
*                                                                               
PUTPE160 DS    0H                                                               
         CLI   SASYSLMT+2,C' '          client group?                           
         JNH   PUTPE165                                                         
         MVC   WORK(3),SASYSLMT+1                                               
         MVI   WLEN,3                                                           
         LARL  RF,XML111           <clientgroup>                                
         ST    RF,AXMLTAG                                                       
         J     PUTPE200                                                         
*                                                                               
PUTPE165 DS    0H                                                               
         LA    RF,WORK1                                                         
         USING OFFICED,RF                                                       
         XC    WORK1,WORK1                                                      
*                                                                               
         CLI   SASYSNUM,2          SPOT                                         
         JE    *+12                                                             
         CLI   SASYSNUM,13         SPOT TRAFFIC                                 
         JNE   *+8                                                              
         MVI   OFCSYS,C'S'                                                      
*                                                                               
         CLI   SASYSNUM,3          NET                                          
         JNE   *+8                                                              
         MVI   OFCSYS,C'N'                                                      
*                                                                               
         CLI   SASYSNUM,4          PRINT                                        
         JNE   *+8                                                              
         MVI   OFCSYS,C'P'                                                      
*                                                                               
         MVC   OFCAGY,LASTAGY      AGENCY ALPHA                                 
         MVC   OFCOFC,SASYSLMT+1   1 BYTE OFFICE                                
         DROP  RF                                                               
*                                                                               
         GOTO1 AOFFICER,DMCB,(C'2',WORK1),ACOMFACS                              
         MVI   WLEN,2                                                           
*                                                                               
         TM    OFCINDS-OFFICED+WORK1,OFCINOLA+OFCIOINV                          
         JZ    PUTPE170                                                         
*                                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         ST    RF,AXMLTAG                                                       
         MVC   WORK(1),SASYSLMT+1                                               
         MVI   WLEN,1                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE170 DS    0H                                                               
         LARL  RF,XML108           <OFFICE2>                                    
         ST    RF,AXMLTAG                                                       
         MVC   WORK(2),OFCOFC2-OFFICED+WORK1                                    
         MVI   WLEN,2                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE180 DS    0H                  CLIENT HERE                                  
         TM    SECFLAG,SECFE1Q     IGNORING CLIENTS ON X'21'S?                  
         JO    PUTPE110                                                         
*                                                                               
         LARL  RF,XML110           <CLIENT>                                     
         ST    RF,AXMLTAG                                                       
         XC    WORK,WORK                                                        
*                                                                               
         CLI   SASYSNUM,X'04'      DON'T UNPACK FOR PRINT                       
         JNE   PUTPE181                                                         
         MVC   WORK(L'SASYSLMT),SASYSLMT                                        
         MVI   WLEN,3                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE181 DS    0H                                                               
         GOTO1 =V(CLUNPK),DMCB,SASYSLMT,WORK                                    
         MVI   WLEN,3                                                           
         J     PUTPE200                                                         
         DROP  R3                                                               
*&&                                                                             
*                                                                               
* X'E1', LIMIT ACCESS ELEM HERE                                                 
*                                                                               
PUTPE185 DS    0H                                                               
         XC    WORK,WORK                                                        
         USING SACLAD,R3                                                        
*&&UK                                                                           
         CLI   SACLACOD,X'00'                                                   
         JNE   PUTPE186                                                         
         LARL  RF,XML118           <CLIENTLIMITACCESSLIST>                      
         ST    RF,AXMLTAG                                                       
         MVC   WORK(2),=C'L='                                                   
         MVC   WORK+2(L'SACLALAL),SACLALAL                                      
         BRAS  RE,SETWLEN                                                       
         J     PUTPE200                                                         
*                                                                               
* SINGLE CLIENT, OR MULTIPLE CLIENTS ON X'E1' HERE                              
*                                                                               
PUTPE186 DS    0H                                                               
         LA    RF,SACLACOD                                                      
         ST    RF,ACLTCOD          A(NEXT CLIENT CODE)                          
         LLC   RF,SACLALN                                                       
         AR    RF,R3                                                            
         ST    RF,AENDCLST         A(END OF CLIENT CODE LIST)                   
*                                                                               
PUTPE186L DS   0H                  CLIENT LIST LOOP                             
         L     RF,ACLTCOD          NEXT CLIENT TO PROCESS                       
         MVC   SVCLTCOD,0(RF)                                                   
*                                                                               
         TM    SVCLTCOD,X'80'      MINUS CLIENT?                                
         JO    PUTPE187                                                         
         LARL  RF,XML119           <MINUSCLIENT>                                
         ST    RF,AXMLTAG                                                       
         MVC   WORK(L'SVCLTCOD),SVCLTCOD                                        
         OI    WORK,X'80'                                                       
         BRAS  RE,SETWLEN                                                       
         J     PUTPE200                                                         
*                                                                               
PUTPE187 DS    0H                                                               
         CLI   SVCLTCOD+1,X'40'    5-BYTE CLIENT?                               
         JH    PUTPE188            NO                                           
         LARL  RF,XML110           <CLIENT>                                     
         ST    RF,AXMLTAG                                                       
         MVC   WORK(1),SVCLTCOD                                                 
         EDITR (B2,SVCLTCOD+1),(4,WORK+1),WRK=WORK1                             
         MVI   WLEN,5                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE188 DS    0H                                                               
         LARL  RF,XML110           <CLIENT>                                     
         ST    RF,AXMLTAG                                                       
         MVC   WORK(L'SVCLTCOD),SVCLTCOD                                        
         J     PUTPE200                                                         
*&&                                                                             
*                                                                               
*&&US                                                                           
         LARL  RF,XML110            client                                      
         ST    RF,AXMLTAG                                                       
         XC    WORK,WORK                                                        
*                                                                               
         CLI   SACLASYS,X'04'      DON'T UNPACK FOR PRINT                       
         JNE   PUTPE190                                                         
         MVC   WORK(L'SACLACOD),SACLACOD                                        
         MVI   WLEN,3                                                           
         J     PUTPE200                                                         
*                                                                               
PUTPE190 DS    0H                                                               
         GOTO1 =V(CLUNPK),DMCB,SACLACOD,WORK                                    
         MVI   WLEN,3                                                           
         DROP  R3                                                               
*&&                                                                             
*                                                                               
* AT THIS POINT                                                                 
* AXMLTAG HAS THE ADDRESS OF AN XML TAG                                         
* SAVESYS HAS THE SYSTEM                                                        
* WORK HAS THE VALUE                                                            
* WLEN HAS THE VALUE'S LENGTH                                                   
*                                                                               
PUTPE200 DS    0H                                                               
         CLC   LASTSYS,SAVESYS     SYSTEM CHANGED?                              
         JE    PUTPE204            NO - GO DISPLAY LIMIT ACCESS                 
*                                                                               
* NEW SYSTEM HERE                                                               
*                                                                               
         MVC   LASTSYS,SAVESYS                                                  
         TM    OPENFLAG,OPLIMQ     <limit> ALREADY OPEN?                        
         JZ    PUTPE202            NO - DON'T CLOSE IT!                         
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK  </SYSTEM>                     
*                                                                               
PUTPE202 DS    0H                                                               
* OPEN <system> TAG FOR THE NEW SYSTEM                                          
* OBTAIN SYSTEM NAME FROM SYSTEM NUMBER                                         
*                                                                               
         MVC   GETSYSNO,SAVESYS                                                 
         BRAS  RE,GETSYSN                                                       
*                                                                               
* SET THE <LIMIT> TAG ATTRIBUTE                                                 
         XC    WORK1,WORK1                                                      
         GOTO1 ,WORK1,=C'system',(7,GETSYSNA),(255,0)                           
         LA    RF,WORK1                                                         
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML106           <limit>                                      
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
         OI    OPENFLAG,OPLIMQ INDICATE WE HAVE <SYSTEM> TAG OPEN               
*                                                                               
PUTPE204 DS    0H                                                               
         GOTO1 =V(DDXML),DMCB,(C'P',AXMLTAG),(WLEN,WORK),AXMLBLK                
*                                                                               
*&&US                                                                           
         J     PUTPE110            GET NEXT ELEMENT                             
*&&                                                                             
*                                                                               
*&&UK                                                                           
         OC    ACLTCOD,ACLTCOD     PROCESSING CLIENTS?                          
         JZ    PUTPE210                                                         
*                                                                               
* YES!! DON'T READ NEXT ELEMENT, LOOP BACK TO CLIENT LOGIC                      
         L     RF,ACLTCOD          CLIENT CODE JUST PROCESSED                   
         AHI   RF,3                ADVANCE TO NEXT CLIENT                       
         ST    RF,ACLTCOD          SAVE IT                                      
*                                                                               
         C     RF,AENDCLST         REACHED END OF LIST OF CLIENTS?              
         JL    PUTPE186L           NO - PROCESS THE NEXT CLIENT CODE            
         J     PUTPE110            YES, GET NEXT ELEMENT                        
*                                                                               
PUTPE210 DS    0H                                                               
         OC    ALSTCOD,ALSTCOD     PROCESSING UK 4-BYTE LIMIT ACCESS?           
         JZ    PUTPE110            NO - GET NEXT ELEMENT                        
*                                                                               
         L     RF,ANXTTAG                                                       
         AHI   RF,L'XML113         TAGS ARE 32 CHARS LONG                       
         ST    RF,ANXTTAG                                                       
*                                                                               
         L     RF,ALSTCOD                                                       
         AHI   RF,1                ADVANCE TO NEXT LIMIT CATEGORY               
         ST    RF,ALSTCOD                                                       
*                                                                               
         C     RF,AENDLST                                                       
         JL    PUTPE132                                                         
         J     PUTPE110            END OF LIST - GET NEXT ELEMENT               
*                                                                               
*&&                                                                             
*                                                                               
PUTPE300 DS    0H                                                               
         DROP  R2                                                               
*                                                                               
         TM    OPENFLAG,OPLIMQ                                                  
         JZ    PUTPE310                                                         
* </limit>                                                                      
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
PUTPE310 DS    0H                                                               
* </PER>                                                                        
         GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
*                                                                               
         MVC   CTOLDKEY,CTSAVKEY                                                
         MVC   SELOW,CTSAVLO                                                    
         MVC   SEHIGH,CTSAVHI                                                   
         OI    READFLAG,X'80'      RESTORE SEQ                                  
*                                                                               
PUTPEX   J     EXITY                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
PUTPE99B DS    0H                                                               
         USING SAPEREC,R2          READ RECORDS                                 
         MVC   SAPEDEF,=X'FFFF'                                                 
         MVC   CTOLDKEY,0(R2)                                                   
         OI    READFLAG,X'80'      RESTORE SEQ                                  
         J     EXITY                                                            
*                                                                               
*********************************************************************           
*        BUILD OFFICE TABLE                                         *           
*********************************************************************           
*                                                                               
BUILDOFF NTR1                                                                   
*                                                                               
         MVC   CTSAVKEY,CTOLDKEY   Save current keys for return                 
         MVC   CTSAVLO,SELOW                                                    
         MVC   CTSAVHI,SEHIGH                                                   
*                                                                               
         XC    SELOW(25),SELOW                                                  
         XC    SEHIGH(25),SEHIGH                                                
*                                                                               
         MVC   SELOW(2),=X'C602'                                                
         MVC   SELOW+13(2),LASTAGY                                              
         MVC   SEHIGH(2),=X'C602'                                               
         MVC   SEHIGH+13(2),LASTAGY                                             
         LLC   R1,SEHIGH+14                                                     
         LA    R1,1(R1)                                                         
         STC   R1,SEHIGH+14                                                     
         MVI   CTINIT,C'N'                                                      
*                                                                               
         LARL  R4,OFFTAB           FILL OFFICE TABLE                            
         USING SAOFREC,R2                                                       
BUILDONX BRAS  RE,GETSE                                                         
         JNE   BUILDO05                                                         
         L     R2,AIO                                                           
         MVC   0(2,R4),SAOFOID                                                  
         LA    R3,SAOFDATA         ELEMENTS                                     
         USING SAOFFD,R3                                                        
*                                                                               
BUILDO02 CLI   0(R3),0             EOR                                          
         JE    BUILDONX                                                         
         CLI   0(R3),SAOFFELQ      OFFICE ELEMENT                               
         JE    BUILDO04                                                         
*                                                                               
         XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    BUILDONX                                                         
         AR    R3,R0               BUMP                                         
         J     BUILDO02                                                         
*                                                                               
BUILDO04 LLC   R1,1(R3)                                                         
         AHI   R1,-(SAOFFLNQ+1)                                                 
         MVC   2(30,R4),SPACES                                                  
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   2(0,R4),SAOFFNAM                                                 
         LA    R4,32(R4)           NEXT                                         
         LARL  R1,OFFTABX                                                       
         CR    R4,R1                                                            
         JL    BUILDONX            READ NEXT                                    
         DC    H'0'                                                             
*                                                                               
BUILDO05 MVI   0(R4),X'FF'         EOT                                          
         LARL  R1,OFFTAB                                                        
         SR    R4,R1                                                            
         XR    RE,RE                                                            
         LR    RF,R4                                                            
         D     RE,=F'38'                                                        
         LARL  R4,OFFTAB                                                        
         AHI   R4,-32                                                           
         EDIT  (RF),(6,26(R4))                                                  
*                                                                               
*********************************************************************           
*        BUILD DEPT TABLE                                           *           
*********************************************************************           
*                                                                               
         MVC   SELOW(2),=X'C603'                                                
         MVC   SEHIGH(2),=X'C603'                                               
         MVI   CTINIT,C'N'                                                      
*                                                                               
         LARL  R4,DEPTAB           FILL DEPT TABLE                              
         USING SADPREC,R2                                                       
BUILDDNX BRAS  RE,GETSE                                                         
         JNE   BUILDD05                                                         
         L     R2,AIO                                                           
         MVC   0(2,R4),SADPOID                                                  
         MVC   2(3,R4),SADPDID                                                  
         LA    R3,SADPDATA         ELEMENTS                                     
         USING SADPTD,R3                                                        
*                                                                               
BUILDD02 CLI   0(R3),0             EOR                                          
         JE    BUILDDNX                                                         
         CLI   0(R3),SADPTELQ      DEPT ELEMENT                                 
         JE    BUILDD04                                                         
*                                                                               
         XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    BUILDDNX                                                         
         AR    R3,R0               BUMP                                         
         J     BUILDD02                                                         
*                                                                               
BUILDD04 LLC   R1,1(R3)                                                         
         AHI   R1,-(SADPTLNQ+1)                                                 
         MVC   5(30,R4),SPACES                                                  
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   5(0,R4),SADPTNAM                                                 
         LA    R4,35(R4)                                                        
         LARL  R1,DEPTABX                                                       
         CR    R4,R1                                                            
         JL    BUILDDNX                                                         
*                                                                               
BUILDD05 MVI   0(R4),X'FF'                                                      
         LARL  R1,DEPTAB                                                        
         SR    R4,R1                                                            
         XR    RE,RE                                                            
         LR    RF,R4                                                            
         D     RE,=F'38'                                                        
         LARL  R4,DEPTAB                                                        
         AHI   R4,-32                                                           
         EDIT  (RF),(6,26(R4))                                                  
*                                                                               
*********************************************************************           
*        BUILD SECURITY TABLE                                       *           
*********************************************************************           
*                                                                               
         MVC   SELOW(2),=X'C605'                                                
         MVC   SEHIGH(2),=X'C605'                                               
         MVI   CTINIT,C'N'                                                      
*                                                                               
         LARL  R4,SECTAB                                                        
         USING SAAGREC,R2                                                       
BUILDSNX BRAS  RE,GETSE                                                         
         JNE   BUILDS05                                                         
         L     R2,AIO                                                           
         MVC   0(8,R4),SAAGAGR                                                  
         LA    R3,SAAGDATA         ELEMENTS                                     
         USING SAAGND,R3                                                        
*                                                                               
BUILDS02 CLI   0(R3),0             EOR                                          
         JE    BUILDSNX                                                         
         CLI   0(R3),SAAGNELQ      AGRP ELEMENT                                 
         JE    BUILDS04                                                         
*                                                                               
         XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    BUILDSNX                                                         
         AR    R3,R0               BUMP                                         
         J     BUILDS02                                                         
*                                                                               
BUILDS04 LLC   R1,1(R3)                                                         
         AHI   R1,-(SAAGNLNQ+1)                                                 
         MVC   8(30,R4),SPACES                                                  
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   8(0,R4),SAAGNNAM                                                 
         LA    R4,38(R4)                                                        
         LARL  R1,SECTABX                                                       
         CR    R4,R1                                                            
         JL    BUILDSNX                                                         
         DC    H'0'                                                             
*                                                                               
BUILDS05 MVI   0(R4),X'FF'                                                      
         LARL  R1,SECTAB                                                        
         SR    R4,R1                                                            
         XR    RE,RE                                                            
         LR    RF,R4                                                            
         D     RE,=F'38'                                                        
         LARL  R4,SECTAB                                                        
         AHI   R4,-32                                                           
         EDIT  (RF),(6,26(R4))                                                  
*                                                                               
*********************************************************************           
*        BUILD TIMESHEET TABLE                                      *           
*********************************************************************           
*                                                                               
         LARL  R4,TSHTAB                                                        
*                                                                               
         MVC   SELOW(2),=X'C620'                                                
         MVC   SEHIGH(2),=X'C620'                                               
         MVI   CTINIT,C'N'                                                      
*                                                                               
         LARL  R4,TSHTAB                                                        
*                                                                               
         USING SAAPREC,R2                                                       
BUILDTNX BRAS  RE,GETSE                                                         
         JNE   BUILDT05                                                         
         L     R2,AIO                                                           
         MVC   0(8,R4),SAAPAGR                                                  
         LA    R3,SAAPDATA         ELEMENTS                                     
         USING SAAGND,R3                                                        
*                                                                               
BUILDT02 CLI   0(R3),0             EOR                                          
         JE    BUILDTNX                                                         
         CLI   0(R3),SAAPGELQ      AGRP ELEMENT                                 
         JE    BUILDT04                                                         
*                                                                               
         XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    BUILDTNX                                                         
         AR    R3,R0               BUMP                                         
         J     BUILDT02                                                         
*                                                                               
BUILDT04 LLC   R1,1(R3)                                                         
         AHI   R1,-(SAAPGLNQ+1)                                                 
         MVC   8(30,R4),SPACES                                                  
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   8(0,R4),SAAGNNAM                                                 
         LA    R4,38(R4)                                                        
         LARL  R1,TSHTABX                                                       
         CR    R4,R1                                                            
         JL    BUILDTNX                                                         
         DC    H'0'                                                             
*                                                                               
BUILDT05 MVI   0(R4),X'FF'                                                      
         LARL  R1,TSHTAB                                                        
         SR    R4,R1                                                            
         XR    RE,RE                                                            
         LR    RF,R4                                                            
         D     RE,=F'38'                                                        
         LARL  R4,TSHTAB                                                        
         AHI   R4,-32                                                           
         EDIT  (RF),(6,26(R4))                                                  
*                                                                               
*********************************************************************           
*        END OF TABLE BUILD                                         *           
*********************************************************************           
*                                                                               
BUILDXXX MVC   CTOLDKEY,CTSAVKEY   RESTORE and REREAD                           
         MVC   SELOW(25),CTSAVLO                                                
         MVC   SEHIGH(25),CTSAVHI                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTOLDKEY,AIO,IOWORK               
*                                                                               
BUILDOFX J     EXITY                                                            
*                                                                               
*                                                                               
PUTOFLST NTR1                                                                   
         L     R8,AXMLBLK                                                       
         USING XMLBLKD,R8                                                       
*                                                                               
         LARL  R2,SCARD3                                                        
         LARL  R3,SCARD4                                                        
         GOTO1 =V(SORTER),DMCB,(R2),(R3)                                        
*                                                                               
         MVI   CTINIT,C'N'                                                      
         XC    SELOW,SELOW                                                      
         XC    SEHIGH,SEHIGH                                                    
         MVI   SELOW,C'U'                                                       
         MVI   SEHIGH,C'U'                                                      
         MVI   SEHIGH+11,X'FF'                                                  
*                                                                               
PUTOF10  BRAS  RE,GETSE                                                         
         JNE   PUTOF90                                                          
         XC    WORK,WORK                                                        
*                                                                               
         USING CTUREC,R2                                                        
         L     R2,AIO                                                           
*                                                                               
         CLI   CTUKPROG,C'$'                                                    
         JE    *+12                                                             
         CLI   CTUKPROG+1,C'$'                                                  
         JNE   PUTOF10                                                          
*                                                                               
         CLI   CTUKAGY,X'40'                                                    
         JL    PUTOF10                                                          
         OC    CTUKMED(L'CTUKMED+L'CTUKCLT+L'CTUKBCT),CTUKMED                   
         JNZ   PUTOF10                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         MVC   WORK(2),CTUKAGY                                                  
         BRAS  RE,READACC                                                       
         JE    PUTOF15                                                          
*                                                                               
         MVC   P(2),CTUKAGY                                                     
         MVC   P+3(30),=CL30'USER PROFILE W/O ACCESS RECORD'                    
         GOTO1 =V(PRINTER)                                                      
         J     PUTOF10                                                          
*                                                                               
PUTOF15  DS    0H                                                               
         MVC   WORK(2),CTUKAGY                                                  
         MVC   WORK+2(1),CTUKSYS                                                
*                                                                               
         MVC   WORK+3(1),CTUKPROG+2                                             
         CLI   CTUKPROG,C'$'                                                    
         JNE   *+10                                                             
         MVC   WORK+3(2),CTUKPROG+1                                             
*                                                                               
         LA    R3,CTUDATA          ELEMENTS - LOOK FOR USERID AGY               
         XC    DUB,DUB                                                          
*                                                                               
PUTOF20  CLI   0(R3),0             EOR                                          
         JE    PUTOF10                                                          
         CLI   0(R3),CTPVELQ       PROFILE VALUE ELEMENT (X'72')?               
         JE    PUTOF25                                                          
*&&US                                                                           
         CLI   0(R3),CTOFELQ       MEDIA OFFICE LIST ELEMENT (X'75')?           
         JE    PUTOF30                                                          
*&&                                                                             
*                                                                               
         XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTOF10                                                          
         AR    R3,R0               BUMP                                         
         J     PUTOF20                                                          
*                                                                               
PUTOF25  DS    0H                  CTPVELQ (X'72') HERE                         
         MVI   WORK+5,X'01'        1-CHAR OFFICE LIST                           
         MVC   WORK+6(16),5(R3)                                                 
         J     PUTOF50                                                          
*                                                                               
*&&US                                                                           
PUTOF30  DS    0H                  CTOFELQ (X'75') HERE                         
         MVI   WORK+5,X'02'        2-CHAR OFFICE LIST                           
         LLC   RF,1(R3)                                                         
         SHI   RF,CTOFOFFS-CTOFEL  NUMBER OFOFFICE CODES IN LIST                
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   WORK+6(0),CTOFOFFS-CTOFD(R3)                                     
         J     PUTOF50                                                          
*&&                                                                             
*                                                                               
PUTOF50  OC    WORK(2),WORK                                                     
         JZ    PUTOF10                                                          
* 2 Agency                                                                      
* 1 System                                                                      
* 2 Office List code                                                            
* 1 Office list code length                                                     
         GOTO1 =V(SORTER),DMCB,PUT,WORK                                         
         J     PUTOF10                                                          
*                                                                               
PUTOF90  DS    0H                                                               
         MVI   OPENFLAG,X'00'                                                   
         XC    WORK,WORK                                                        
         XC    SAVEAGY,SAVEAGY                                                  
         XC    SAVESYS,SAVESYS                                                  
         XC    SAVEOFL,SAVEOFL                                                  
*                                                                               
         XC    SVOFLKEY,SVOFLKEY                                                
*                                                                               
PUTOF100 GOTO1 =V(SORTER),DMCB,GET                                              
         ICM   R3,15,4(R1)                                                      
         JZ    PUTOF900                                                         
*                                                                               
         OC    SVOFLKEY,SVOFLKEY   NEED TO SKIP A,B,C LIST PORTIONS             
         JZ    PUTOF110            NO, PROCEED AS USUAL                         
*                                                                               
         CLC   SVOFLKEY,0(R3)      SAME AGY,SYS,OFFLIST?                        
         JE    PUTOF100            SKIP A,B,C PARTS OF OFFLIST                  
         XC    SVOFLKEY,SVOFLKEY                                                
*                                                                               
PUTOF110 DS    0H                                                               
         CLI   5(R3),X'02'         2-CHAR MEDIA OFFICE HERE?                    
         JNE   *+10                                                             
         MVC   SVOFLKEY,0(R3)                                                   
*                                                                               
         CLC   SAVEAGY,0(R3)       same agency?                                 
         JE    PUTOF120            yes, proceed to system                       
*                                                                               
* new agency here                                                               
         MVC   SAVEAGY,0(R3)                                                    
         XC    SAVESYS,SAVESYS                                                  
         XC    SAVEOFL,SAVEOFL                                                  
*                                                                               
         MVI   BYTE,X'FF'                                                       
         BRAS  RE,CLOSETAG         close all open tags                          
*                                                                               
         NI    OPENFLAG,X'FF'-OPSYSQ-OPLSTQ                                     
*                                                                               
* <agency> open new agency tag                                                  
         GOTO1 ,WORK,=C'alpha',(2,0(R3)),(255,0)    <agency> attribute          
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML202           <agency>                                     
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
         OI    OPENFLAG,OPAGYQ     indicate <agency> tag open                   
*                                                                               
PUTOF120 DS    0H                                                               
         CLC   SAVESYS,2(R3)       same system?                                 
         JE    PUTOF140            proceed to office list                       
*                                                                               
* new system here                                                               
*                                                                               
         L     RF,=A(SYSLST)                                                    
         AHI   RF,6                                                             
PUTOF130 CLI   0(RF),0                                                          
         JNE   *+14                                                             
         XC    SAVESYS,SAVESYS     system not in table                          
         J     PUTOF100                                                         
*                                                                               
         CLC   12(1,RF),2(R3)                                                   
         JE    *+12                                                             
         AHI   RF,L'SYSLST                                                      
         J     PUTOF130                                                         
*                                                                               
         MVC   SAVESYS,2(R3)                                                    
         XC    SAVEOFL,SAVEOFL                                                  
*                                                                               
         MVI   BYTE,X'FF'-OPAGYQ   CLOSE ALL, EXCEPT <agency>                   
         BRAS  RE,CLOSETAG                                                      
*                                                                               
         NI    OPENFLAG,X'FF'-OPLSTQ                                            
*                                                                               
         GOTO1 ,WORK,=C'alpha',(7,2(RF)),(255,0) <system> attribute             
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML203           <limit>                                      
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
         OI    OPENFLAG,OPSYSQ     indicate <system> tag open                   
*                                                                               
PUTOF140 DS    0H                                                               
         CLC   SAVEOFL,3(R3)       same office list?                            
         JE    PUTOF160            proceed to individual offices                
*                                                                               
* new office list here                                                          
         MVC   SAVEOFL,3(R3)                                                    
*                                                                               
         MVI   BYTE,X'FF'-OPAGYQ-OPSYSQ ALL, EXCEPT <agency>, <system>          
         BRAS  RE,CLOSETAG                                                      
*                                                                               
*&&UK                                                                           
         GOTO1 ,WORK,=C'alpha',(1,3(R3)),(255,0) <officelist> attribute         
*&&                                                                             
*                                                                               
*&&US                                                                           
         XC    OFFBLK,OFFBLK                                                    
OFB      USING OFFICED,OFFBLK                                                   
*                                                                               
         MVC   OFB.OFCSYS,2(R3)                                                 
         MVC   OFB.OFCAGY,0(R3)                                                 
         MVC   OFB.OFCMOL,3(R3)                                                 
         OI    OFB.OFCINDS,OFCIMOLC        OFFICE LIST CONVERSION               
*                                                                               
         GOTO1 AOFFICER,DMCB,(C'2',OFFBLK),(0,ACOMFACS)                         
         JNE   *+2                                                              
*                                                                               
         XC    WORK1,WORK1                                                      
         MVC   WORK1(2),OFB.OFCMOL2                                             
         MVC   WLEN,5(R3)          1- OR 2- CHAR OFFLIST NAME LENGTH            
*                                                                               
* <OFFICELIST> ATTRIBUTE                                                        
         GOTO1 ,WORK,=C'alpha',(WLEN,WORK1),(255,0)                             
*&&                                                                             
*                                                                               
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML204           <officelist>                                 
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
         OI    OPENFLAG,OPLSTQ     indicate <system> tag open                   
*                                                                               
PUTOF160 DS    0H                                                               
         LA    R4,6(R3)            list of offices                              
*&&UK*&& LHI   R5,16                                                            
*&&US*&& LHI   R5,L'CTOFOFFS                                                    
*                                                                               
PUTOF180 DS    0H                                                               
         CLI   0(R4),X'00'                                                      
         JE    PUTOF190                                                         
         CLI   0(R4),C'0'                                                       
         JE    PUTOF190                                                         
*                                                                               
*&&US                                                                           
         XC    WORK1,WORK1                                                      
         LA    RF,WORK1                                                         
         USING OFFICED,RF                                                       
         MVC   OFCSYS,2(R3)                                                     
         MVC   OFCAGY,0(R3)        AGENCY ALPHA                                 
         MVC   OFCOFC,0(R4)        1 BYTE OFFICE                                
         DROP  RF                                                               
         GOTO1 AOFFICER,DMCB,(C'2',WORK1),ACOMFACS                              
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         MVC   WORK(1),0(R4)                                                    
         MVI   WLEN,1                                                           
*                                                                               
         TM    OFCINDS-OFFICED+WORK1,OFCINOLA+OFCIOINV                          
         JNZ   PUTOF185                                                         
         LARL  RF,XML108           <OFFICE2>                                    
         MVC   WORK(2),OFCOFC2-OFFICED+WORK1                                    
         MVI   WLEN,2                                                           
*&&                                                                             
* 04/05/2016                                                                    
* looks like offlist/office records are not being used on production            
* systems.  Doesn't look like the UK uses OFFICER either                        
* for now will print office characters the way they're stored                   
* on profile records                                                            
*                                                                               
*&&UK                                                                           
         CLI   0(R4),C'*'                                                       
         JE    PUTOF190                                                         
         CLI   0(R4),X'00'                                                      
         JE    PUTOF190                                                         
*                                                                               
         XC    WORK,WORK                                                        
         LARL  RF,XML107           <OFFICE1>                                    
         MVC   WORK(1),0(R4)                                                    
         MVI   WLEN,1                                                           
*&&                                                                             
*&&DO                                                                           
         L     R2,AIO                                                           
         USING COCREC,R2                                                        
*                                                                               
         XC    COCKEY,COCKEY                                                    
         MVI   COCKTYP,COCKTYPQ                                                 
         MVI   COCKSUB,COCKSUBQ                                                 
         MVC   COCKAGY,0(R3)                                                    
         MVC   COCKHEX,0(R4)                                                    
*                                                                               
         MVC   CTSAVKEY,COCKEY                                                  
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,COCKEY,COCKEY,IOWORK              
         CLC   COCKEY,CTSAVKEY                                                  
         JNE   PUTOF190                                                         
*                                                                               
         LA    R2,COCDATA                                                       
         MVC   WORK(2),COCCODE-COCELEM(R2)                                      
*                                                                               
         LARL  RF,XML107           <OFFICE1>                                    
         MVI   WLEN,1                                                           
         CLI   WORK+1,C' '                                                      
         JNH   PUTOF185                                                         
         LARL  RF,XML108           <OFFICE2>                                    
         MVI   WLEN,2                                                           
*                                                                               
*&&                                                                             
*                                                                               
PUTOF185 DS    0H                                                               
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
PUTOF190 DS    0H                                                               
         AHI   R4,1                                                             
         BRCT  R5,PUTOF180                                                      
*                                                                               
         J     PUTOF100                                                         
*                                                                               
PUTOF900 DS    0H                                                               
         MVI   BYTE,X'FF'                                                       
         BRAS  RE,CLOSETAG         close all open tags                          
         GOTO1 =V(SORTER),DMCB,END                                              
         J     EXIT                                                             
         DROP  R8                                                               
*                                                                               
*                                                                               
*                                                                               
CLOSETAG NTR1                                                                   
         MVC   BYTE1,OPENFLAG                                                   
         NC    BYTE1,BYTE                                                       
         LHI   R0,8                                                             
*                                                                               
CLOSET10 TM    BYTE1,X'01'                                                      
         JZ    CLOSET20                                                         
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
CLOSET20 LLC   RF,BYTE1                                                         
         SRL   RF,1                                                             
         STC   RF,BYTE1                                                         
         BRCT  R0,CLOSET10                                                      
         J     EXITY                                                            
*                                                                               
* R1 EXPECTED TO ADDRESS READACC BLOCK:                                         
* 2 BYTE ALPHA AENCY, 1 BYTE SYSTEM NUMBER                                      
*                                                                               
READACC  NTR1                                                                   
         LR    R4,R1               SAVE PARAMETERS                              
         MVI   READACCE,X'00'      CC=EQ EXIT                                   
*                                                                               
         L     R2,AIO                                                           
         MVC   READACCK,0(R2)      SAVE CURRENT KEY                             
*                                                                               
         L     R2,AIO2                                                          
         USING CT5REC,R2                                                        
*                                                                               
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ      C'5'                                       
         MVC   CT5KALPH,0(R4)        AGENCY                                     
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CT5KEY,CT5KEY,IOWORK              
         TM    8(R1),X'80'                                                      
         JO    READANO                                                          
         CLI   8(R1),0                                                          
         JNE   READANO                                                          
*                                                                               
         CLI   CT5KTYP,CT5KTYPQ                                                 
         JNE   READANO             NO ACCESS RECORD FOR THIS AGENCY             
         CLC   CT5KALPH,0(R4)                                                   
         JNE   READANO             NO ACCESS RECORD FOR THIS AGENCY             
*                                                                               
         CLI   2(R4),0             SYSTEM NUMBER PASSED?                        
         JE    READACCX                                                         
*                                                                               
         LA    R6,CT5DATA                                                       
*                                                                               
READA50  DS    0H                                                               
         CLI   0(R6),0                                                          
         JE    READANO                                                          
*                                                                               
         CLI   0(R6),CTSYSELQ                                                   
         JNE   READA80                                                          
*                                                                               
         USING CTSYSD,R6                                                        
         CLC   CTSYSNUM,2(R4)                                                   
         JE    READACCX                                                         
*                                                                               
READA80  XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         JZ    READANO                                                          
         AR    R6,R0                                                            
         J     READA50                                                          
*                                                                               
READANO  MVI   READACCE,X'FF'                                                   
*                                                                               
READACCX DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,READACCK,AIO,IOWORK               
         CLI   READACCE,X'00'                                                   
         JE    EXITY                                                            
         J     EXITN                                                            
*                                                                               
*                                                                               
* ON INPUT GETSYSNO EXPECTED TO HAVE SYSTEM NUMBER                              
* ON EXIT (RF) AND GETSYSNA WILL CONTAIN ADDRESS OF SYSTEM NAME                 
GETSYSN  L     RF,=A(SYSLST)                                                    
         AHI   RF,6                                                             
*                                                                               
         USING SYSLSTD,RF                                                       
*                                                                               
GETSYSNL CLI   SYSLNUM,0                                                        
         JE    *+2                                                              
*                                                                               
         CLC   SYSLNUM,GETSYSNO                                                 
         JE    GETSYSNX                                                         
         AHI   RF,SYSLLEN                                                       
         J     GETSYSNL                                                         
*                                                                               
GETSYSNX LA    RF,SYSLNAME                                                      
         ST    RF,GETSYSNA                                                      
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
* FIND LENGTH OF DATA IN WORK, AND SAVE THE VALUE IN WLEN                       
SETWLEN  NTR1                                                                   
         CLI   WORK,C' '                                                        
         JH    *+12                                                             
         MVI   WLEN,1              DEFAULT FOR EMPTY STRINGS                    
         J     EXIT                                                             
*                                                                               
         LA    RE,WORK                                                          
         LA    RF,WORK+L'WORK-1                                                 
*                                                                               
SETWLEN1 CLI   0(RF),C' '                                                       
         JH    SETWLX                                                           
*                                                                               
         BCTR  RF,0                                                             
         CR    RE,RF                                                            
         JH    *+2                 WENT PAST THE START OF WORK                  
         J     SETWLEN1                                                         
*                                                                               
SETWLX   SR    RF,RE                                                            
         AHI   RF,1                                                             
         STC   RF,WLEN                                                          
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
PUTSCAGY NTR1                                                                   
         L     R8,AXMLBLK                                                       
         USING XMLBLKD,R8                                                       
*                                                                               
         XC    LASTAGY,LASTAGY                                                  
*                                                                               
         LARL  R2,SCARD5                                                        
         LARL  R3,SCARD6                                                        
         GOTO1 =V(SORTER),DMCB,(R2),(R3)                                        
*                                                                               
         LARL  RF,XML210           <alphalist>                                  
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         MVI   CTINIT,C'N'                                                      
         XC    SELOW,SELOW                                                      
         XC    SEHIGH,SEHIGH                                                    
         MVI   SELOW,CT5KTYPQ                                                   
         MVI   SEHIGH,CT5KTYPQ                                                  
         MVI   SEHIGH+1,X'FF'                                                   
*                                                                               
PUTSCA10 BRAS  RE,GETSE                                                         
         JNE   PUTSCA50                                                         
*                                                                               
         USING CT5REC,R2                                                        
         L     R2,AIO                                                           
*                                                                               
* DEFAULT CASE: NO SECAGY                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(L'CT5KALPH),CT5KALPH AGY ALPHA                              
         MVC   WORK+2(L'CT5KALPH),CT5KALPH AGY ALPHA                            
*                                                                               
         LA    R3,CT5DATA          ELEMENTS - LOOK FOR USERID AGY               
*                                                                               
PUTSCA20 CLI   0(R3),0             EOR                                          
         JE    PUTSCA45            SECAGY ELEM NOT FOUND - DEFAULT CASE         
*                                                                               
         CLI   0(R3),CTSEAELQ      security agency alpha id element             
         JE    PUTSCA40                                                         
*                                                                               
PUTSCA30 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)                                                       
         JZ    PUTSCA45            SECAGY ELEM NOT FOUND - DEFAULT CASE         
         AR    R3,R0               BUMP                                         
         J     PUTSCA20                                                         
*                                                                               
PUTSCA40 DS    0H                                                               
         USING CTSEAD,R3                                                        
         MVC   WORK(L'CTSEAAID),CTSEAAID SECURITY AGENCY ALPHA                  
         DROP  R3                                                               
*                                                                               
PUTSCA45 DS    0H                                                               
*&&US*&& CLC   =C'#N',WORK         suppress #N in US                            
*&&UK*&& CLC   =C'#E',WORK         suppress #E in UK                            
         JE    PUTSCA10            NEXT RECORD                                  
*&&US*&& CLC   =C'#N',WORK+2       suppress #N in US                            
*&&UK*&& CLC   =C'#E',WORK+2       suppress #E in UK                            
         JE    PUTSCA10            NEXT RECORD                                  
*                                                                               
         GOTO1 =V(SORTER),DMCB,PUT,WORK                                         
         J     PUTSCA10            NEXT RECORD                                  
*                                                                               
* ALL ACCESS RECORDS READ IN                                                    
*                                                                               
PUTSCA50 DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,GET                                              
         ICM   R3,15,4(R1)                                                      
         JZ    PUTSCA90                                                         
*                                                                               
         CLC   LASTAGY,0(R3)                                                    
         JE    PUTSCA60                                                         
*                                                                               
         OC    LASTAGY,LASTAGY     have <secagy> tag open?                      
         JZ    PUTSCA55            no, don't close it                           
*                                                                               
         LARL  RF,XML003           close tag: </secagy>                         
         GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
*                                                                               
PUTSCA55 DS    0H                                                               
* SAVE LAST SECURITY AGENCY ALPHA AND INDICATE <SECAGY> TAG IS OPEN             
         MVC   LASTAGY,0(R3)                                                    
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 ,WORK,=C'alpha',(2,0(R3)),(255,0) <secagy> attribute             
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML003           <secagy>                                     
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
*                                                                               
PUTSCA60 DS    0H                                                               
         XC    WORK,WORK                                                        
         GOTO1 ,WORK,=C'alpha',(2,2(R3)),(255,0) <agency> attribute             
         LA    RF,WORK                                                          
         ST    RF,XMLATTRT         SET ATTRIBUTES POINTER                       
         LARL  RF,XML202           <agency>                                     
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),0,AXMLBLK                             
         XC    XMLATTRT,XMLATTRT   CLEAR ATTRIB POINTER                         
         J     PUTSCA50            next sorter record                           
*                                                                               
PUTSCA90 DS    0H                                                               
         LARL  RF,XML003           close tag: </secagy>                         
         GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
         LARL  RF,XML210           close tag: </alphalist>                      
         GOTO1 =V(DDXML),DMCB,(C'C',(RF)),0,AXMLBLK                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,END                                              
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
SYSADV   NTR1                                                                   
         BRAS  RE,BLDSEC        BUILD TABLE OF AGY-SECAGY RELATIONSHIPS         
*                                                                               
         XC    WORK1,WORK1         OUTPUT RECORD AREA                           
         MVC   WORK1(2),=AL2(SYSADVDLQ+4) VARIABLE-LENGTH RECORD                
*                                                                               
SYSADREC USING SYSADVD,WORK1+4                                                  
* INSERT DELIMITERS EVERYWHERE                                                  
         MVI   SYSADREC.SYSADUID+L'SYSADUID,SYSADSEPQ                           
         MVI   SYSADREC.SYSADSYS+L'SYSADSYS,SYSADSEPQ                           
         MVI   SYSADREC.SYSADAGY+L'SYSADAGY,SYSADSEPQ                           
         MVI   SYSADREC.SYSADSEC+L'SYSADSEC,SYSADSEPQ                           
         MVI   SYSADREC.SYSADADV+L'SYSADADV,SYSADSEPQ                           
*                                                                               
         L     R2,AIO                                                           
         USING CTIREC,R2           ID RECORD                                    
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVI   CTIKID,X'01'        START WITH ALPHA IDS, SKIP BIANARIES         
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         J     SYSADV11                                                         
*                                                                               
SYSADV10 GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,CTIKEY,CTIKEY,IOWORK              
SYSADV11 TM    8(R1),X'80'         EOF?                                         
         JO    SYSADVX                                                          
         CLI   CTIKTYP,CTIKTYPQ    ID RECORD?                                   
         JNE   SYSADVX             NO, WE'RE DONE                               
*                                                                               
         MVC   SYSADREC.SYSADUID,CTIKID                                         
*                                                                               
         LA    R6,CTIDATA                                                       
*                                                                               
SYSADV20 CLI   0(R6),0             EOR                                          
         JE    SYSADV10            PROCESS NEXT ID RECORD                       
*                                                                               
         CLI   0(R6),CTAGYELQ      X'06' ALPHA ID ELEMENT                       
         JE    SYSADV25                                                         
         CLI   0(R6),CTSYSELQ      X'21' SYSTEM AUTHORIZATION ELEMENT           
         JE    SYSADV30                                                         
*                                                                               
SYSADV21 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         JZ    SYSADV10            EOR, ALL ELEMENTS PROCESSED                  
         AR    R6,R0                                                            
         J     SYSADV20                                                         
*                                                                               
SYSADV25 DS    0H                                                               
         USING CTAGYD,R6           X'06' AGENCY ALPHA ID ELEMENT                
         MVC   SYSADREC.SYSADAGY,CTAGYID                                        
         MVC   WORK(2),CTAGYID                                                  
*                                                                               
         GOTO1 =A(BINS_GET),DMCB,AGY_SEC_TAB,WORK                               
         JNE   *+2                                                              
*                                                                               
         L     RF,DMCB+4                                                        
         SAM31                                                                  
         MVC   SYSADREC.SYSADSEC,0(RF)                                          
         SAM24                                                                  
         J     SYSADV21            PROCESS NEXT ELEMENT                         
*                                                                               
SYSADV30 DS    0H                                                               
         USING CTSYSD,R6           X'21' SYSTEM AUTHORIZATION ELEMENT           
         MVC   GETSYSNO,CTSYSNUM                                                
         BRAS  RE,GETSYSN                                                       
         MVC   SYSADREC.SYSADSYS,0(RF) RF POINTS TO SYSTEM NAME                 
*                                                                               
         CLI   ADVLKUP,C'Y'        ARE WE FORCING ADV LOOKUPS (FOR CSC)         
         JE    SYSADV35                                                         
* IF NOT FORCING, THEN TEST SYSTEMS ARE DETERMINED BY DSPACE VALUE              
         L     R1,=V(SSB)                                                       
         CLI   SSODSPAC-SSOOFF(R1),C'T'                                         
         JNE   *+14                                                             
         MVC   SYSADREC.SYSADADV,=CL4'TST'                                      
         J     SYSADV40                                                         
*                                                                               
         CLI   SSODSPAC-SSOOFF(R1),C'C'                                         
         JNE   *+14                                                             
         MVC   SYSADREC.SYSADADV,=CL4'CSC'                                      
         J     SYSADV40                                                         
*                                                                               
         CLI   SSODSPAC-SSOOFF(R1),C'Q'                                         
         JNE   *+14                                                             
         MVC   SYSADREC.SYSADADV,=CL4'FQA'                                      
         J     SYSADV40                                                         
*                                                                               
         CLI   SSODSPAC-SSOOFF(R1),C'R'                                         
         JNE   *+14                                                             
         MVC   SYSADREC.SYSADADV,=CL4'REP'                                      
         J     SYSADV40                                                         
*                                                                               
*&&UK                                                                           
         CLI   SSODSPAC-SSOOFF(R1),C'B'                                         
         JNE   *+14                                                             
         MVC   SYSADREC.SYSADADV,=CL4'BAR'                                      
         J     SYSADV40                                                         
*&&                                                                             
*                                                                               
* AT THIS POINT WE SHOULD BE ON THE PRODUCTION SYSTEM                           
         CLI   SSODSPAC-SSOOFF(R1),C'A' DOUBLE-CHECK DATASPACE VALUE            
         JNE   *+2                 SOMETHING WRONG                              
*                                                                               
* LOOK UP ADV SYSTEM FROM SYSTEM SE NUMBER ON THE 21 ELEMENT                    
SYSADV35 DS    0H                                                               
         L     R1,DMTABS+8         A(SYSTABS)                                   
         L     R1,12(R1)           A(SYSSTAB)                                   
         LLC   RF,CTSYSSE          SYSTEM'S SE NUMBER FROM 21 ELEMENT           
         LLC   RF,0(R1,RF)         INDEX INTO SYSSTAB, PICK UP ADV NUM          
*                                                                               
         CHI   RF,X'FF'            GLOBAL FILE?                                 
         JNE   *+14                                                             
         MVC   SYSADREC.SYSADADV,=CL4'ALL' ACCESSIBLE ON ALL ADVS               
         J     SYSADV40                                                         
*                                                                               
         CHI   RF,0                SE NUMBER PRESENT?                           
         JNE   *+12                YES - PROCESS IT                             
* NO SE NUMBER HERE.  IGNORE THESE, UNLESS SHOWBAD=Y                            
         CLI   SHOWBAD,C'N'        DISPLAYING BAD ADV ENTRIES?                  
         JE    SYSADV21            NO, SUPPRESS BAD SYSTEMS BY DEFAULT          
*                                                                               
         L     R1,DMTABS+12        A(FACIDTAB)                                  
         MHI   RF,L'FACITAB        INDEX INTO FACIDTAB                          
         AR    R1,RF                                                            
         MVC   SYSADREC.SYSADADV,(FACISN4-FACITABD)(R1)                         
*                                                                               
SYSADV40 DS    0H                                                               
         PUT   TAPEOUT,WORK1       PUT TO TAPEOUT                               
         J     SYSADV21            PROCESS NEXT ELEMENT                         
*                                                                               
SYSADVX  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* BUILD A TABLE OF AGENCY-SECAGY CODES                                          
BLDSEC   NTR1                                                                   
         L     R2,AIO                                                           
         USING CT5REC,R2           SYSTEM ACCESS RECORD                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CT5KEY,CT5KEY,IOWORK              
         J     BLDSEC11                                                         
*                                                                               
BLDSEC10 GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,CT5KEY,CT5KEY,IOWORK              
BLDSEC11 TM    8(R1),X'80'                                                      
         JO    BLDSECX                                                          
         CLI   CT5KTYP,CT5KTYPQ    STILL SYSTEM ACCESS RECORD?                  
         JNE   BLDSECX             NO, WE'RE DONE                               
*                                                                               
         XC    WORK,WORK           RECORD AREA FOR BINSRCH                      
         MVC   WORK(2),CT5KALPH                                                 
         MVC   WORK+2(2),CT5KALPH  DEFAULT - SECAGY SAME AS AGY                 
*                                                                               
         LA    R6,CT5DATA                                                       
*                                                                               
BLDSEC20 CLI   0(R6),0             EOR                                          
         JE    BLDSEC60            ADD DEFAULT ENTRY TO BINSRCH                 
         CLI   0(R6),CTSEAELQ      X'B8' SECURITY AGENCY ELEMENT                
         JE    BLDSEC30                                                         
         CLI   0(R6),CTAADELQ      X'B9' AGENCY ACCESS DETAILS                  
         JE    BLDSEC40                                                         
*                                                                               
         J     BLDSEC50            GET NEXT ELEMENT                             
*                                                                               
BLDSEC30 DS    0H                                                               
         USING CTSEAD,R6           SECURITY AGENCY ALPHA ID ELEMENT             
         MVC   WORK+2(2),CTSEAAID                                               
         J     BLDSEC50            GET NEXT ELEMENT                             
*                                                                               
BLDSEC40 DS    0H                                                               
         USING CTAADD,R6           AGENCY ACCESS DETAIL ELEMENT                 
         CLI   CTAACSTN,X'0A'      INACTIVE - SEE DDTEAMLST                     
         JNE   BLDSEC50                                                         
         OI    WORK+4,X'80'        SET INACTIVE FLAG IN AGY TABLE               
         J     BLDSEC50            GET NEXT ELEMENT                             
*                                                                               
BLDSEC50 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         JZ    BLDSEC60            EOR - ADD DEFAULT ENTRY TO BINSRCH           
         AR    R6,R0                                                            
         J     BLDSEC20                                                         
*                                                                               
BLDSEC60 DS    0H                  ADD AGY-SECAGY ENTRY TO BINSRCH              
         GOTO1 =A(BINS_ADD),DMCB,AGY_SEC_TAB,WORK                               
         J     BLDSEC10            NO - READ NEXT ACCESS RECORD                 
*                                                                               
BLDSECX  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
* BUILD A TABLE OF SECAGY-AGY RELATIONSHIPS                                     
BLDSEC2  NTR1                                                                   
         OC    BSP1_3,BSP1_3       AGY->SECAGY TABLE BETTER BE BUILT            
         JZ    *+2                                                              
*                                                                               
         L     R2,BSP1_2           A(AGY->SECAGY TABLE)                         
         L     R3,BSP1_3           NUMBER OF RECORDS THERE                      
*                                                                               
BLDS210  DS    0H                  ADD AGY-SECAGY ENTRY TO BINSRCH              
         XC    WORK,WORK                                                        
         SAM31                                                                  
         MVC   WORK(2),2(R2)       SECAGY                                       
         MVC   WORK+2(2),0(R2)     AGY                                          
* SECAGY->AGY IS NOT A 1-TO-1 RELATIONSHIP, SO LEAVE FLAG BLANK                 
         SAM24                                                                  
*                                                                               
         GOTO1 =A(BINS_ADD),DMCB,SEC_AGY_TAB,WORK                               
*                                                                               
         AHI   R2,4                                                             
         BRCT  R3,BLDS210                                                       
*                                                                               
BLDSEC2X DS    0H                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
SYSPROG  NTR1                                                                   
         BRAS  RE,BLDSEC        BUILD TABLE OF AGY-SECAGY RELATIONSHIPS         
         BRAS  RE,BLDSEC2       BUILD TABLE OF SECAGY-AGY RELATIONSHIPS         
         BRAS  RE,BLDPROG       BUILD TABLE OF UID-PROGRAMS                     
         BRAS  RE,BLDPNAM       BUILD TABLE OF PROGRAM NAMES                    
*                                                                               
         LARL  RF,XML_SYSPROG   <sysprog>                                       
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         L     R2,AIO                                                           
         USING SA0REC,R2           PERSON AUTH RECORD                           
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ    C'0'                                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,SA0KEY,SA0KEY,IOWORK              
         J     SYSPRG11                                                         
*                                                                               
SYSPRG10 GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,SA0KEY,SA0KEY,IOWORK              
SYSPRG11 TM    8(R1),X'80'                                                      
         JO    SYSPRGX                                                          
         CLI   SA0KTYP,SA0KTYPQ    STILL PERSON AUTH RECORD?                    
         JNE   SYSPRGX             NO, WE'RE DONE                               
*                                                                               
* FILTER BY AGENCY, IF NEEDED                                                   
*                                                                               
         CLI   AGYFILTS,C' '       FILTERING BY AGENCY?                         
         JNH   SYSPRG11A                                                        
         LA    R1,SA0KAGY                                                       
         BRAS  RE,FILTAGY                                                       
         JNE   SYSPRG10                                                         
*                                                                               
* FIND PERSONAL ID ELEMENT                                                      
*                                                                               
SYSPRG11A DS   0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,SAPALELQ     X'C3' PERSONAL ID ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    SYSPRG12                                                         
*                                                                               
* NO X'C3' ELEMENT HERE.                                                        
* WILL BE IGNORING THESE FOR NOW                                                
         J     SYSPRG10                                                         
*                                                                               
         ICM   RF,15,=V(SSB)                                                    
         CLI   SSODSPAC-SSOOFF(RF),C'T'                                         
         JE    SYSPRG10            IF DSPACE=T, SKIP THIS PID                   
         DC    H'0'                IF PRODUCTION - DIE                          
*                                                                               
SYSPRG12 DS    0H                                                               
         LARL  RF,XML_PERSON    OPEN <PERSON> TAG                               
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         USING SAPALD,R6           X'C3' PERSONAL ID ELEMENT                    
*                                                                               
         XC    WORK,WORK                                                        
         LLC   RF,SAPALLN          ELEMENT LENGTH                               
         AHI   RF,-3               MINUS ELCODE, LENGTH, BCTR FOR EX            
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   WORK(0),SAPALPID                                                 
         DROP  R6                                                               
*                                                                               
         AHI   RF,1                                                             
         STC   RF,WLEN                                                          
*                                                                               
* GENERATE <PID>                                                                
*                                                                               
         LARL  RF,XML_PID          <PID>                                        
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
* GENERATE <SECAGY>                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),SA0KAGY                                                  
         LARL  RF,XML_SECAGY       <SECAGY>                                     
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(2,WORK),AXMLBLK                      
*                                                                               
         BRAS  RE,READIDS          FIND ALL UIDS FOR THIS PID                   
*                                                                               
* NOW FIND ALL X'21' ELEMENTS, AND ADD SYSTEM-PROGRAM ACCESS                    
* INFORMATION TO THIS PID'S PROGTAB ENTRY IN WORK3                              
         XC    WORK3,WORK3         WORK AREA FOR PID'S PROGTABD ENTRY           
*                                                                               
         LA    R6,SA0DATA                                                       
*                                                                               
SYSPRG20 CLI   0(R6),0             EOR                                          
         JE    SYSPRG30                                                         
         CLI   0(R6),SASYSELQ      SYSTEM AUTHORIZATION ELEMENT (X'21')         
         JNE   SYSPRG25                                                         
*                                                                               
         USING SASYSD,R6           SYSTEM AUTHORIZATION ELEMENT (X'21')         
         GOTO1 =A(PROC21),DMCB,(R6),WORK3                                       
*                                                                               
SYSPRG25 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         JZ    SYSPRG30            EOR                                          
         AR    R6,R0                                                            
         J     SYSPRG20                                                         
*                                                                               
* EOR, ALL X'21' ELEMENTS PROCESSED                                             
*                                                                               
* NOW GO OVER ALL UIDS FOR THIS PID, AND CROSS-CHECK                            
* SYSTEM-PROGRAM ACCESS FOR EACH, PID VS UID                                    
SYSPRG30 DS    0H                                                               
         L     R6,BSP5_2           A(UID TABLE FOR THIS PID)                    
*                                                                               
SYSPRG40 DS    0H                                                               
         SAM31                                                                  
         OC    0(10,R6),0(R6)      EMPTY SLOT?                                  
         SAM24                                                                  
         JZ    SYSPRG90            ALL UIDS PROCESSED FOR THIS PID              
*                                                                               
         LARL  RF,XML_USERID    OPEN <USERID> TAG                               
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         XC    WORK,WORK                                                        
         SAM31                                                                  
         MVC   WORK(10),0(R6)                                                   
         SAM24                                                                  
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML_ID        <ID> TAG                                        
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
* FIND THIS UID'S AGENCY CODE                                                   
*                                                                               
         GOTO1 =A(BINS_GET),DMCB,UID_PGM_TAB,WORK                               
         JNE   *+2                                                              
         L     RF,DMCB+4                                                        
         SAM31                                                                  
         MVC   WORK1(PROGTABDLQ),0(RF) SAVE ENTIRE PROGTABD ENTRY               
         SAM24                                                                  
*                                                                               
* GENERATE <AGENCY>                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(2),WORK1+PTAGY-PROGTABD                                     
         LARL  RF,XML_AGENCY       <AGENCY>                                     
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(2,WORK),AXMLBLK                      
*                                                                               
* FOR EACH PROGTAB ENTRY WE MUST PROCESS ALL SYSTEMS                            
* WHICH SYSTEMS AND HOW MANY IS DETERMINED BY BLDPGMS TABLE                     
* R4 = SYSTEM COUNTER, USED FOR INDEXING INTO SYSTEM LIST                       
         XR    R4,R4                                                            
*                                                                               
SYSPRG50 DS    0H                                                               
* POINT TO THIS PID'S PROGTAB ENTRY                                             
         LA    R5,WORK3+PTSYSLST-PROGTABD                                       
* INDEX INTO THE SYSTEMS LIST                                                   
         LR    RF,R4                                                            
         MHI   RF,PTSYSLSTLQ                                                    
         AR    R5,RF               R5->PID'S SYSTEM-PROGRAM LIST                
         MVC   XL32,1(R5)                                                       
*                                                                               
* POINT TO CURRENT UID'S PROGTAB ENTRY                                          
         LA    R7,WORK1+PTSYSLST-PROGTABD                                       
* INDEX INTO THE SYSTEMS LIST                                                   
         AR    R7,RF               R7->UID'S SYSTEM-PROGRAM LIST                
*                                                                               
* "AND" PID'S AND UID'S PROGRAM ACCESS BYTES                                    
* TO GET THE RESULTING UID-PID ACCESS INFORMATION                               
*                                                                               
         NC    XL32,1(R7)                                                       
         JZ    SYSPRG70            ZERO = NO ACCESS, GO TO NEXT SYSTEM          
*                                                                               
* HAVE ACCESS TO AT LEAST SOME PROGRAMS IN A SYSTEM HERE                        
*                                                                               
         LARL  RF,XML_SYSTEM    OPEN <SYSTEM> TAG                               
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         LARL  RF,BLDPGMS                                                       
         LR    R0,R4                                                            
         MHI   R0,L'BLDPGMS                                                     
         AR    RF,R0                                                            
         MVC   GETSYSNO,0(RF)      SYSTEM NUMBER                                
         BRAS  RE,GETSYSN                                                       
* GENERATE <SYSNAME>                                                            
         XC    WORK,WORK                                                        
         MVC   WORK(7),0(RF)                                                    
         LARL  RF,XML_SYSNAME      <SYSNAME>                                    
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(7,WORK),AXMLBLK                      
*                                                                               
* GENERATE <ADV>                                                                
         LLC   RF,GETSYSNO                                                      
         GOTO1 =A(GETADV),DMCB,(RF)                                             
         L     RF,DMCB                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),0(RF)                                                    
         LARL  RF,XML_ADV          <ADV>                                        
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(7,WORK),AXMLBLK                      
*                                                                               
* LIST PROGRAM NAMES                                                            
*                                                                               
         LHI   R5,1                PROGRAM COUNTER                              
*                                                                               
SYSPRG60 DS    0H                                                               
         XC    WORK,WORK                                                        
         GOTO1 =A(SETBIT),DMCB,(R5),WORK                                        
         MVC   WORK+32(32),XL32    MAKE A COPY OF PGM ACCESS LIST               
         NC    WORK(32),WORK+32    TEST IF PROGRAM'S BIT IS ON                  
         JZ    SYSPRG65            NOT ON - DO NEXT SYSTEM                      
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),GETSYSNO                                                 
         STC   R5,WORK+1                                                        
         GOTO1 =A(BINS_GET),DMCB,PROGNAM_TAB,WORK                               
         JNE   SYSPRG65                                                         
*                                                                               
         L     RF,DMCB+4                                                        
         SAM31                                                                  
         MVC   WORK(7),2(RF)       PROGRAM NAME FROM BINSRCH TABLE              
         SAM24                                                                  
*                                                                               
* temp debug - testing io volume w/o progname                                   
*        LARL  RF,XML_PROGRAM   OPEN <PROGRAM> TAG                              
*        GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*        LARL  RF,XML_PROGNAM      <PROGNAME>                                   
*        GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(7,WORK),AXMLBLK                      
*                                                                               
         EDITR (R5),(3,WORK),ALIGN=LEFT,WRK=WORK+3                              
         XC    WORK+3(L'WORK-3),WORK+3                                          
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML_PROGNUM      <PROG>                                       
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
* temp debug - testing io volume w/o progname                                   
* CLOSE </PROGRAM> TAG                                                          
*        GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
* ADVANCE TO NEXT PROGRAM                                                       
SYSPRG65 DS    0H                                                               
         AHI   R5,1                                                             
         CHI   R5,32                                                            
         JNH   SYSPRG60                                                         
*                                                                               
* CLOSE </SYSTEM> TAG                                                           
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
* MOVE ON TO NEXT SYSTEM IN THE LIST                                            
SYSPRG70 DS    0H                                                               
         CHI   R4,BLDPGMNQ-1       NUMBER OF SUPPORTED SYSTEMS                  
         JNL   SYSPRG80                                                         
         AHI   R4,1                                                             
         J     SYSPRG50                                                         
*                                                                               
SYSPRG80 DS    0H                                                               
* CLOSE </USERID> TAG.                                                          
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
         AHI   R6,10               NEXT UID IN THE PID'S LIST                   
         J     SYSPRG40                                                         
*                                                                               
SYSPRG90 DS    0H                                                               
* CLOSE </PERSON> TAG.                                                          
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
         J     SYSPRG10            READ NEXT ACCESS RECORD                      
*                                                                               
SYSPRGX  DS    0H                                                               
* CLOSE </SYSPROG> TAG.  THIS SHOULD BE THE ONLY TAG STILL OPEN                 
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
         J     EXITY                                                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* BUILD A TABLE OF USERD-PROGRAM ENTRIES                                        
BLDPROG  NTR1                                                                   
         L     R2,AIO                                                           
         USING CTIREC,R2           ID RECORD                                    
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVI   CTIKID,C'A'         FIRST ALPHABETICAL ID                        
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         J     BLDPRG11                                                         
*                                                                               
BLDPRG10 GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,CTIKEY,CTIKEY,IOWORK              
BLDPRG11 TM    8(R1),X'80'                                                      
         JO    BLDPRGX                                                          
         CLI   CTIKTYP,CTIKTYPQ    STILL ID RECORD?                             
         JNE   BLDPRGX             NO, WE'RE DONE                               
*                                                                               
         LA    R3,WORK                                                          
         USING PROGTABD,R3                                                      
*                                                                               
         XC    WORK,WORK           RECORD AREA FOR BINSRCH                      
         MVC   PTID,CTIKID         ID FROM KEY                                  
*                                                                               
         LA    R6,CTIDATA                                                       
*                                                                               
BLDPRG20 CLI   0(R6),0             EOR                                          
         JE    BLDPRG60        EOR: ADD UID-SYS-PGM ENTRY TO BINSRCH            
*                                                                               
         CLI   0(R6),CTAGYELQ      X'06' AGENCY ELEMENT                         
         JE    BLDPRG29                                                         
         CLI   0(R6),CTIDOELQ      X'07' USER ID OPTIONS ELEMENT                
         JE    BLDPRG28                                                         
         CLI   0(R6),CTSYSELQ      X'21' SYSTEM AUTH. ELEMENT                   
         JE    BLDPRG30                                                         
*                                                                               
BLDPRG25 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         JZ    BLDPRG60           EOR: ADD UID-SYS-PGM ENTRY TO BINSRCH         
         AR    R6,R0                                                            
         J     BLDPRG20                                                         
*                                                                               
BLDPRG28 DS    0H                                                               
         USING CTIDOD,R6           X'07' USER ID OPTIONS ELEMENT                
         TM    CTIDOFL1,CTIDOFSN   USER ID IS A SYNONYM?                        
         JO    BLDPRG10            YES, IGNORE THESE                            
         J     BLDPRG25                                                         
         DROP  R6                                                               
*                                                                               
BLDPRG29 DS    0H                                                               
         USING CTAGYD,R6           X'06' AGENCY ELEMENT                         
         MVC   PTAGY,CTAGYID                                                    
         J     BLDPRG25                                                         
         DROP  R6                                                               
*                                                                               
BLDPRG30 DS    0H                                                               
         USING CTSYSD,R6           X'21' SYSTEM AUTH. ELEMENT                   
*                                                                               
         GOTO1 =A(FINDSYSL),DMCB,CTSYSNUM                                       
         JNE   BLDPRG25            UNSUPPORTED SYS - GET NEXT ELEMENT           
*                                                                               
         L     R4,0(R1)            SYSTEM SLOT NUMBER                           
         MHI   R4,PTSYSLSTLQ                                                    
         LA    R4,PTSYSLST(R4)                                                  
*                                                                               
         LLC   RE,CTSYSLEN         ELEMENT LENGTH                               
         LA    R5,0(RE,R6)         R5 = END OF ELEMENT                          
         MVC   0(1,R4),CTSYSSE     SE NUMBER FROM X'21' ELEMENT                 
         LA    R4,1(R4)            SYSTEM'S PROGRAM AREA (PT...PGS)             
*                                                                               
* FIRST CHECK DEFAULT ACCESS LEVEL                                              
         OC    0(32,R4),=32X'FF'   FF'S = DEFAULT IS ALL ACCESS                 
         MVI   BYTE,X'FF'                                                       
*                                                                               
         OC    CTSYSALL,CTSYSALL   ALL AUTH LEVEL                               
         JNZ   *+14                                                             
         XC    0(32,R4),0(R4)      ZERO = DEFAULT IS NO ACCESS                  
         MVI   BYTE,X'00'                                                       
*                                                                               
* NOW CHECK EXCEPTIONS                                                          
*                                                                               
         LA    R7,CTSYSPGM         LIST OF EXCEPTIONS                           
*                                                                               
BLDPRG50 DS    0H                                                               
         CR    R7,R5               WENT PAST END OF ELEMENT?                    
         JNL   BLDPRG25            ADD TO BINSRCH TABLE                         
*                                                                               
         LLC   R0,0(R7)            PROGRAM NUMBER                               
*                                                                               
         CLI   BYTE,X'FF'          DEFAULT ACCESS = ALL?                        
         JNE   BLDPRG55                                                         
*                                                                               
         OC    1(2,R7),1(R7)       IS PROGRAM'S LIMIT ACCESS ZERO?              
         JNZ   BLDPRG57            NO - DON'T RESET PROGRAM'S BIT               
         GOTO1 =A(RESETBIT),DMCB,(R0),0(R4)                                     
         J     BLDPRG57                                                         
*                                                                               
BLDPRG55 DS    0H                                                               
         OC    1(2,R7),1(R7)       IS PROGRAM'S LIMIT ACCESS NONZERO?           
         JZ    BLDPRG57            NO - DON'T SET PROGRAM'S BIT                 
         GOTO1 =A(SETBIT),DMCB,(R0),0(R4)                                       
*                                                                               
BLDPRG57 DS    0H                  NEXT EXCEPTION                               
         LA    R7,3(R7)                                                         
         J     BLDPRG50                                                         
*                                                                               
BLDPRG60 DS    0H                  ADD UID-SYS-PGM ENTRY TO BINSRCH             
         CLC   WORK+PTAGY-PROGTABD(L'PTAGY),SPACES                              
         JNH   BLDPRG10            IGNORE IF NO AGENCY                          
*                                                                               
         GOTO1 =A(BINS_ADD),DMCB,UID_PGM_TAB,WORK                               
         J     BLDPRG10                                                         
*                                                                               
BLDPRGX  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
* SETS N-TH BIT (STARTING WITH BIT 0) IN A 32-BYTE AREA                         
* P1 - 1-BYTE BINARY                                                            
* P2 - ADDRESS OF A 32-BYTE AREA, WHERE BIT IS TO BE SET                        
SETBIT   NTR1                                                                   
         L     RF,0(R1)            BIT NUMBER                                   
         L     R1,4(R1)            A(32-BYTE AREA)                              
*                                                                               
         LTR   RF,RF                                                            
         JZ    *+2                 MUST BE >0                                   
         LTR   R1,R1                                                            
         JZ    *+2                 ADDRESS MAY NOT BE EMPTY                     
*                                                                               
         XR    RE,RE                                                            
         LHI   R0,32               BITS IN A FULLWORD                           
         DR    RE,R0               RF=RESULT, RE=REMAINDER                      
*                                                                               
         MHI   RF,4                                                             
         AR    R1,RF                                                            
*                                                                               
         LHI   R2,1                                                             
         SLL   R2,31               HOB SET TO 1                                 
*                                                                               
         LTR   RE,RE                                                            
         JZ    *+12                                                             
         SRL   R2,1                                                             
         BRCT  RE,*-4                                                           
*                                                                               
         O     R2,0(R1)                                                         
         STCM  R2,15,0(R1)                                                      
*                                                                               
         J     EXITY                                                            
*                                                                               
*                                                                               
*                                                                               
RESETBIT NTR1                                                                   
         L     R2,0(R1)            BIT NUMBER                                   
         L     R3,4(R1)                                                         
         XC    XL32,XL32                                                        
         GOTO1 =A(SETBIT),DMCB,(R2),XL32                                        
         XC    0(32,R3),XL32                                                    
         J     EXITY                                                            
                                                                                
*                                                                               
*                                                                               
*                                                                               
* BUILD PROGRAM NAME TABLE AND GENERATE XML                                     
BLDPNAM  NTR1                                                                   
         LARL  RF,XML_PROGTAB   <PROGTAB>                                       
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         LARL  R2,BLDPGMS          LIST OF SUPPORTED PROGRAMS                   
*                                                                               
BLDPN01  DS    0H                                                               
         CLI   0(R2),X'FF'                                                      
         JE    BLDPNX                                                           
*                                                                               
         LARL  RF,XML_SYSTEM    <SYSTEM>                                        
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   GETSYSNO,0(R2)      SYSTEM NUMBER  FROM BLDPGMS TABLE            
         BRAS  RE,GETSYSN                                                       
*                                                                               
* GENERATE <SYSNAME>                                                            
         MVC   WORK(7),0(RF)                                                    
         LARL  RF,XML_SYSNAME      <SYSNAME>                                    
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(7,WORK),AXMLBLK                      
*                                                                               
         L     R3,4(R2) ADDRESS OF SYSTEM'S PROGRAM LIST IN FATABPGUS           
*                                                                               
BLDPN10  DS    0H                                                               
         XR    R0,R0                                                            
         L     R4,2(R3)            END OF PROGRAM TABLE                         
         LA    R3,6(R3)            SKIP ENTRY LENGT, TABLE END ADDRESS          
*                                                                               
         USING PGMLSTD,R3                                                       
*                                                                               
BLDPN20  DS    0H                                                               
         CR    R3,R4                                                            
         JNL   BLDPN30                                                          
*                                                                               
*&&UK                                                                           
         CLI   PGMCTRY,0           ONLY USE DEFAULT ENTRIES                     
         JNE   BLDPN25                                                          
*&&                                                                             
*                                                                               
         XC    WORK1,WORK1                                                      
         MVC   WORK1(1),0(R2)       SYSTEM                                      
         MVC   WORK1+1(1),PGMNUM                                                
         MVC   WORK1+2(L'PGMNAME),PGMNAME                                       
*                                                                               
         GOTO1 =A(BINS_ADD),DMCB,PROGNAM_TAB,WORK1                              
*                                                                               
         LARL  RF,XML_PROGRAM   OPEN <PROGRAM> TAG                              
         GOTO1 =V(DDXML),DMCB,(C'O',(RF)),0,AXMLBLK                             
*                                                                               
         EDITR (B1,PGMNUM),(3,WORK),ALIGN=LEFT,WRK=WORK+3                       
         XC    WORK+3(L'WORK-3),WORK+3                                          
         BRAS  RE,SETWLEN                                                       
         LARL  RF,XML_PROGNUM      <PROG>                                       
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(WLEN,WORK),AXMLBLK                   
*                                                                               
         LARL  RF,XML_PROGNAM      <PROGNAME>                                   
         GOTO1 =V(DDXML),DMCB,(C'P',(RF)),(7,PGMNAME),AXMLBLK                   
*                                                                               
* CLOSE </PROGRAM> TAG                                                          
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
* NEXT PROGRAM                                                                  
*                                                                               
BLDPN25  DS    0H                                                               
         LA    R3,(PGMLSTX-PGMLSTD)(R3)                                         
         J     BLDPN20                                                          
*                                                                               
* NEXT SYSTEM                                                                   
*                                                                               
BLDPN30  DS    0H                                                               
* CLOSE </SYSTEM> TAG                                                           
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
*                                                                               
         LA    R2,L'BLDPGMS(R2)                                                 
         J     BLDPN01                                                          
*                                                                               
BLDPNX   DS    0H                                                               
* CLOSE </PROGTAB> TAG                                                          
         GOTO1 =V(DDXML),DMCB,(C'C',0),0,AXMLBLK                                
         J     EQXIT                                                            
*                                                                               
*                                                                               
* LIST OF SUPPORTED SYSTEMS                                                     
* AL1(SYSTEM EQUATE), 3X'00' SPARE, A(PROG LIST IN FATABPGUS/UK)                
         DS    0F                                                               
BLDPGMS  DS    0XL8                                                             
*&&US                                                                           
         DC    AL1(SYSSPTQ,0,0,0),A(SPOTPGM)                                    
         DC    AL1(SYSNETQ,0,0,0),A(NETPGM)                                     
         DC    AL1(SYSPRTQ,0,0,0),A(PRNTPGM)                                    
         DC    AL1(SYSACCQ,0,0,0),A(ACCTPGM)                                    
         DC    AL1(SYSSTRQ,0,0,0),A(TRFPGM)                                     
         DC    AL1(SYSCONQ,0,0,0),A(CTLPGM)                                     
*&&                                                                             
*&&UK                                                                           
         DC    AL1(SYSMEDQ,0,0,0),A(MEDPGM)                                     
         DC    AL1(SYSACCQ,0,0,0),A(ACCPGM)                                     
         DC    AL1(SYSCONQ,0,0,0),A(CTLPGM)                                     
*&&                                                                             
BLDPGMNQ EQU   (*-BLDPGMS)/8                                                    
         DC    X'FF'                                                            
*                                                                               
*&&US                                                                           
       ++INCLUDE FATABPGUS                                                      
*&&                                                                             
*&&UK                                                                           
       ++INCLUDE FATABPGUK                                                      
*&&                                                                             
*                                                                               
*                                                                               
*                                                                               
* LOOK UP ADV SYSTEM FROM SYSTEM SE NUMBER ON THE 21 ELEMENT                    
* P1 LOB - SYSTEM SE NUMBER                                                     
* ON EXIT                                                                       
* P1 - ADDRESS OF THE 4-CHARACTER ADV NAME                                      
GETADV   NTR1                                                                   
         L     RF,=V(SSB)                                                       
*                                                                               
         CLI   SSODSPAC-SSOOFF(RF),C'A'                                         
         JE    GETADV05                                                         
*                                                                               
         CLI   SSODSPAC-SSOOFF(RF),C'T'                                         
         JNE   *+12                                                             
         LA    RE,=CL4'TST'                                                     
         J     GETADV04                                                         
*                                                                               
         CLI   SSODSPAC-SSOOFF(RF),C'C'                                         
         JNE   GETADV03                                                         
         CLI   ADVLKUP,C'Y'        ARE WE FORCING ADV LOOKUPS (FOR CSC)         
         JE    GETADV05                                                         
         LA    RE,=CL4'CSC'                                                     
         J     GETADV04                                                         
*                                                                               
GETADV03 DS    0H                                                               
         CLI   SSODSPAC-SSOOFF(RF),C'Q'                                         
         JNE   *+12                                                             
         LA    RE,=CL4'FQA'                                                     
         J     GETADV04                                                         
*                                                                               
         CLI   SSODSPAC-SSOOFF(RF),C'R'                                         
         JNE   *+12                                                             
         LA    RE,=CL4'REP'                                                     
         J     GETADV04                                                         
*                                                                               
*&&UK                                                                           
         CLI   SSODSPAC-SSOOFF(RF),C'B'                                         
         JNE   *+12                                                             
         LA    RE,=CL4'BAR'                                                     
         J     GETADV04                                                         
*&&                                                                             
*                                                                               
         DC    H'0'                UNKNOWN DSPACE VALUE                         
*                                                                               
GETADV04 ST    RE,0(R1)                                                         
         J     EQXIT                                                            
*                                                                               
* LOOK UP ADV SYSTEM FROM SYSTEM SE NUMBER ON THE 21 ELEMENT                    
*                                                                               
GETADV05 DS    0H                                                               
         LLC   RF,3(R1)            SE NUMBER FROM P1                            
         L     RE,DMTABS+8         A(SYSTABS)                                   
         L     RE,12(RE)           A(SYSSTAB)                                   
         LLC   RF,0(RE,RF)         INDEX INTO SYSSTAB, PICK UP ADV NUM          
*                                                                               
         CHI   RF,X'FF'            GLOBAL FILE?                                 
         JNE   GETADV10                                                         
         LA    RE,=CL4'ALL'        ACCESSIBLE ON ALL ADVS                       
         ST    RE,0(R1)                                                         
         J     EQXIT                                                            
*                                                                               
GETADV10 DS    0H                                                               
         CHI   RF,0                SE NUMBER PRESENT?                           
         JNE   GETADV20            YES - PROCESS IT                             
*                                                                               
* NO SE NUMBER HERE.                                                            
         LA    RE,=CL4'NONE'       ACCESSIBLE ON ALL ADVS                       
         ST    RE,0(R1)                                                         
         J     EQXIT                                                            
*                                                                               
GETADV20 DS    0H                                                               
         L     RE,DMTABS+12        A(FACIDTAB)                                  
         MHI   RF,L'FACITAB        INDEX INTO FACIDTAB                          
         LA    RE,(FACISN4-FACITABD)(RE,RF)                                     
         ST    RE,0(R1)                                                         
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
* GET LIST OF USER IDS GIVEN PERSON RECORD                                      
* ANY IDS FOUND WILL BE ADDED TO THE ID TABLE                                   
* P1 - A(PERSON RECORD)                                                         
READIDS  NTR1                                                                   
* INITIALIZE TABLE                                                              
         L     RE,BSP5_2           A(TABLE)                                     
         ICM   RF,15,=AL4(PIDUID_ENTRY_LENGTH*PIDUID_MAX_ENTRIES)               
         SAM31                                                                  
         XCEFL                                                                  
         SAM24                                                                  
*                                                                               
         XC    BSP5_3,BSP5_3       XC NUMBER OF ENTRIES                         
*                                                                               
* INDICATE WE HAVEN'T YET FOUND ANY ID ELEMENTS                                 
         LARL  RF,HAVEX20                                                       
         MVI   0(RF),C'N'          NO ID ELEMENTS FOUND                         
*                                                                               
         L     R6,AIO                                                           
         USING SA0REC,R6           PERSONAL AUTHORIZATION RECORD                
         LA    R6,SA0DATA                                                       
*                                                                               
READID20 CLI   0(R6),0             EOR                                          
         JE    READID80                                                         
         CLI   0(R6),CTIDELQ       X'20' ID ELEMENT                             
         JE    READID30                                                         
* NEXT ELEMENT                                                                  
READID25 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         JZ    READID80            EOR                                          
         AR    R6,R0                                                            
         J     READID20                                                         
*                                                                               
* HAVE X'20' ID ELEMENT HERE                                                    
*                                                                               
         USING CTIDD,R6            ID ELEMENT (X'20')                           
READID30 DS    0H                                                               
         TM    CTIDTYP,X'80'       NEW-STYLE ELEMENT?                           
         JO    READID60            NO                                           
*                                                                               
* HAVE NEW-STYLE ELEMENT HERE                                                   
*                                                                               
         CLC   CTID,=C'  '                                                      
         JNH   READID25            IGNORE EMPTY ELEMENTS                        
*                                                                               
* CASE 1: LIST                                                                  
         CLC   =X'0000',CTID                                                    
         JNE   READID40                                                         
*                                                                               
         GOTO1 =A(READLIST),DMCB,CTID+2                                         
         LARL  RF,HAVEX20                                                       
         MVI   0(RF),C'Y'          INDICATE WE FOUND ONE X'20'                  
*                                                                               
         J     READID25            LOOK FOR NEXT X'20'                          
*                                                                               
* CASE 2: AGENCY                                                                
READID40 DS    0H                                                               
         CLC   =X'0001',CTID                                                    
         JNE   READID50                                                         
*                                                                               
         USING PROGTABD,R2                                                      
         L     R2,BSP3_2           A(UID-SYS-PID TABLE)                         
         L     R0,BSP3_3           NUMBER OF ENTRIES                            
*                                                                               
READID42 DS    0H                                                               
         SAM31                                                                  
         CLC   PTAGY,CTID+2                                                     
         SAM24                                                                  
         JNE   READID44                                                         
*                                                                               
         GOTO1 =A(BINS_ADD),DMCB,PID_UID_TAB,(R2)                               
*                                                                               
READID44 DS    0H                                                               
         AHI   R2,PROGTABDLQ                                                    
         BRCT  R0,READID42                                                      
*                                                                               
         LARL  RF,HAVEX20                                                       
         MVI   0(RF),C'Y'          INDICATE WE FOUND ONE X'20'                  
*                                                                               
         J     READID25            LOOK FOR NEXT X'20'                          
                                                                                
* CASE 3: ID                                                                    
READID50 DS    0H                                                               
         GOTO1 =A(BINS_GET),DMCB,UID_PGM_TAB,CTID+1                             
         JNE   READID25            INVALID ID - IGNORE                          
*                                                                               
         GOTO1 =A(BINS_ADD),DMCB,PID_UID_TAB,CTID+1                             
*                                                                               
         LARL  RF,HAVEX20                                                       
         MVI   0(RF),C'Y'          INDICATE WE FOUND ONE X'20'                  
         J     READID25            LOOK FOR NEXT X'20'                          
*                                                                               
* HAVE OLD-STYLE ELEMENT HERE                                                   
*                                                                               
READID60 DS    0H                                                               
         CLI   CTID,C' '                                                        
         JNH   READID25            IGNORE BLANK ID ELEMENTS                     
*                                                                               
         CLC   =CL10'ALL',CTID                                                  
         JE    READID81                                                         
*                                                                               
         GOTO1 =A(BINS_GET),DMCB,UID_PGM_TAB,CTID                               
         JNE   READID25            INVALID ID - IGNORE                          
*                                                                               
         GOTO1 =A(BINS_ADD),DMCB,PID_UID_TAB,CTID                               
*                                                                               
         LARL  RF,HAVEX20                                                       
         MVI   0(RF),C'Y'          INDICATE WE FOUND ONE X'20'                  
         J     READID25            LOOK FOR NEXT X'20'                          
*                                                                               
* EOR HERE                                                                      
*                                                                               
READID80 DS    0H                                                               
         LARL  RF,HAVEX20                                                       
         CLI   0(RF),C'Y'                                                       
         JE    EQXIT                                                            
*                                                                               
* NO X'20' ELEMENTS FOUND HERE                                                  
* MUST READ ALL IDS FOR THIS SECURITY AGENCY                                    
*                                                                               
READID81 DS    0H                                                               
         USING PROGTABD,R2                                                      
         L     R2,BSP3_2           A(UID-SYS-PID TABLE)                         
         L     R0,BSP3_3           NUMBER OF ENTRIES                            
*                                                                               
READID82 DS    0H                                                               
         SAM31                                                                  
         MVC   WORK(2),PTAGY       UID'S AGENCY                                 
         SAM24                                                                  
*                                                                               
         GOTO1 =A(BINS_GET),DMCB,AGY_SEC_TAB,WORK                               
         JE    READID83                                                         
         ICM   RF,15,=V(SSB)                                                    
         CLI   SSODSPAC-SSOOFF(RF),C'T'                                         
         JE    READID84            IF DSPACE=T, IGNORE THIS ID                  
         DC    H'0'                IF PRODUCTION - DIED                         
*                                                                               
READID83 DS    0H                                                               
         L     RF,DMCB+4                                                        
         SAM31                                                                  
         MVC   WORK(2),0(RF)       SECURITY AGENCY                              
         SAM24                                                                  
*                                                                               
         L     RF,AIO                                                           
         CLC   WORK(2),SA0KAGY-SA0REC(RF) SAME AS OUR PID'S?                    
         JNE   READID84                                                         
*                                                                               
         GOTO1 =A(BINS_ADD),DMCB,PID_UID_TAB,(R2)                               
*                                                                               
READID84 DS    0H                                                               
         AHI   R2,PROGTABDLQ                                                    
         BRCT  R0,READID82                                                      
*                                                                               
         J     EQXIT                                                            
*                                                                               
HAVEX20 DS     C                                                                
*                                                                               
*                                                                               
*                                                                               
* READ SYSLIST RECORD                                                           
* P1 - A(LIST NAME)                                                             
READLIST NTR1  WORK=(R8,4)                                                      
         STCM  R1,15,0(R8)         PARAMS                                       
*                                                                               
         L     RF,AIO                                                           
         MVC   4(25,R8),0(RF)      SAVE CURRENT KEY                             
*                                                                               
         L     R2,AIO                                                           
         USING CTWREC,R2           SYSTEM LIST RECORD                           
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ    C'W'                                         
         MVI   CTWKREC,CTWKRUSR    C'I', USER ID LIST                           
*                                                                               
         L     RF,0(R8)            SAVED PARMS                                  
         L     RF,0(RF)            A(LIST ID)                                   
         MVC   CTWKID,0(RF)                                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTWKEY,CTWKEY,IOWORK              
         TM    8(R1),X'80'                                                      
         JO    READLSX                                                          
         CLI   CTWKTYP,CTWKTYPQ    STILL SYSTEM LIST RECORD?                    
         JNE   READLSX             NO, WE'RE DONE                               
         CLI   CTWKREC,CTWKRUSR    STILL UID-LIST RECORD?                       
         JNE   READLSX             NO, WE'RE DONE                               
*                                                                               
         LA    R6,CTWDATA                                                       
*                                                                               
READLS20 CLI   0(R6),0             EOR                                          
         JE    READLSX                                                          
         CLI   0(R6),CTLSTELQ      X'A4' LIST ELEMENT                           
         JE    READLS30            NO, GET NEXT ELEMENT                         
         CLI   0(R6),CTLINELQ      X'A5' LIST INCLUDE ELEMENT                   
         JE    READLS40            NO, GET NEXT ELEMENT                         
*                                                                               
READLS25 XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         JZ    READLSX                                                          
         AR    R6,R0                                                            
         J     READLS20                                                         
*                                                                               
READLS30 DS    0H                  X'A4' LIST ELEMENT                           
         USING CTLSTD,R6                                                        
         GOTO1 =A(BINS_GET),DMCB,UID_PGM_TAB,CTLSTDTA                           
         JNE   READLS25            INVALID ID - IGNORE                          
         GOTO1 =A(BINS_ADD),DMCB,PID_UID_TAB,CTLSTDTA                           
         J     READLS25                                                         
*                                                                               
READLS40 DS    0H                  X'A5' LIST INCLUDE ELEMENT                   
         USING CTLIND,R6                                                        
*                                                                               
         GOTO1 =A(READLIST),DMCB,CTLINC                                         
*                                                                               
         J     READLS25                                                         
*                                                                               
* RESTORE CALLER'S READ SEQUENCE                                                
*                                                                               
READLSX  DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,4(R8),AIO,IOWORK                  
         TM    8(R1),X'80'                                                      
         JO    *+2                 SEQUENCE RESTORE FAILED                      
         L     RE,AIO                                                           
         CLC   4(25,R8),0(RE)                                                   
         JE    EQXIT                                                            
         DC    H'0'                SEQUENCE RESTORE FAILED                      
*                                                                               
*                                                                               
*                                                                               
* ADDS AN ENTRY TO BINSRCH TABLE                                                
* P1 EXPECTED TO HAVE TABLE EQUATE                                              
* P2 EXPECTED TO ADDRESS THE RECORD TO BE ADDED                                 
BINS_ADD NTR1                                                                   
         LR    R2,R1               SAVE PARMS                                   
*                                                                               
         L     RF,0(R2)            BINSEARCH TABLE EQUATE                       
         MHI   RF,6*4              TIMES THE BSPARS LENGTH (6F)                 
         LA    R3,BSPARS                                                        
         AR    R3,RF               INDEX INTO BSPARS LIST                       
*                                                                               
         USING BSPARS,R3                                                        
*                                                                               
         L     R4,4(R2)            A(REC)                                       
*                                                                               
         MVI   BP4,1               ADD IF NOT FOUND                             
         SAM31                                                                  
         GOTO1 =V(BINSRCH),BSPARS,(R4)                                          
         SAM24                                                                  
         OC    1(3,R1),1(R1)       TEST TABLE FULL                              
         JNZ   EQXIT                                                            
         DC    H'0'                TABLE FULL                                   
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
* GETS AN ENTRY FROM BINSRCH TABLE                                              
* P1 EXPECTED TO HAVE TABLE EQUATE                                              
* P2 EXPECTED TO ADDRESS RECORD KEY                                             
*                                                                               
* ON EXIT P2 WILL ADDRESS THE RECORD IN BINSRCH TABLE                           
BINS_GET NTR1                                                                   
         LR    R2,R1               SAVE PARMS                                   
*                                                                               
         L     RF,0(R2)            BINSEARCH TABLE EQUATE                       
         MHI   RF,6*4              TIMES THE BSPARS LENGTH (6F)                 
         LA    R3,BSPARS                                                        
         AR    R3,RF               INDEX INTO BSPARS LIST                       
*                                                                               
         USING BSPARS,R3                                                        
*                                                                               
         L     R4,4(R2)            A(REC KEY)                                   
*                                                                               
         MVI   BP4,0               FIND A RECORD                                
         SAM31                                                                  
         GOTO1 =V(BINSRCH),BSPARS,(R4)                                          
         SAM24                                                                  
         TM    BP1,X'80'           TEST IF RECORD NOT FOUND                     
         JZ    *+14                                                             
         XC    4(4,R2),4(R2)       XC A(OUTPUT RECORD)                          
         J     NEQXIT                                                           
*                                                                               
         MVC   4(4,R2),BP1         STORE ADDRESS OF THE RECORD IN P2            
         J     EXITY                                                            
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
* OBTAIN 31-BIT STORAGE                                                         
* P1 - REQUESTED STORAGE SIZE                                                   
* P2 - A(AREA WHERE STORAGE ADDRESS WILL BE SAVED)                              
GETMEM   NTR1                                                                   
         LR    R2,R1                                                            
         L     R0,0(R2)            STORAGE SIZE FROM P1                         
*                                                                               
         ST    RB,FULL                                                          
         BASR  RB,0                                                             
         PUSH  USING                                                            
         USING *,RB                                                             
         STORAGE OBTAIN,LENGTH=(R0),LOC=31,COND=YES                             
         POP   USING                                                            
         L     RB,FULL                                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         L     R2,4(R2)            A(ADDRESS SAVE AREA)                         
         ST    R1,0(R2)                                                         
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
* PROCESS SYSTEM AUTHORIZATION ELEMENT (X'21', SASYSD/CTSYSD)                   
* P1 - A(SYS AUTH ELEMENT)                                                      
* P2 - A(WORK AREA, WHERE PROGTABD ENTRY WILL BE CREATED)                       
PROC21   NTR1                                                                   
         L     R6,0(R1)                                                         
         USING CTSYSD,R6                                                        
         L     R3,4(R1)                                                         
         USING PROGTABD,R3                                                      
*                                                                               
         GOTO1 =A(FINDSYSL),DMCB,CTSYSNUM                                       
         JNE   NEQXIT              UNSUPPORTED SYS                              
         L     R4,0(R1)            SYSTEM SLOT NUMBER                           
         MHI   R4,PTSYSLSTLQ                                                    
         LA    R4,PTSYSLST(R4)                                                  
*                                                                               
         LLC   RE,CTSYSLEN         ELEMENT LENGTH                               
         LA    R5,0(RE,R6)         R5 = END OF ELEMENT                          
         MVC   0(1,R4),CTSYSSE     SE NUMBER FROM X'21' ELEMENT                 
         LA    R4,1(R4)            SYSTEM'S PROGRAM AREA                        
*                                                                               
* FIRST CHECK DEFAULT ACCESS LEVEL                                              
         OC    0(32,R4),=32X'FF'   FF'S = DEFAULT IS ALL ACCESS                 
         MVI   BYTE,X'FF'                                                       
*                                                                               
         OC    CTSYSALL,CTSYSALL   ALL AUTH LEVEL                               
         JNZ   *+14                                                             
         XC    0(32,R4),0(R4)      ZERO = DEFAULT IS NO ACCESS                  
         MVI   BYTE,X'00'                                                       
*                                                                               
* NOW CHECK EXCEPTIONS                                                          
*                                                                               
         LA    R7,CTSYSPGM         LIST OF EXCEPTIONS                           
*                                                                               
PROC2150 DS    0H                                                               
         CR    R7,R5               WENT PAST END OF ELEMENT?                    
         JNL   PROC2160            ADD TO BINSRCH TABLE                         
*                                                                               
         LLC   R0,0(R7)            PROGRAM NUMBER                               
*                                                                               
         CLI   BYTE,X'FF'          DEFAULT ACCESS = ALL?                        
         JNE   PROC2155                                                         
*                                                                               
         OC    1(2,R7),1(R7)       IS PROGRAM'S LIMIT ACCESS ZERO?              
         JNZ   PROC2157            NO - DON'T RESET PROGRAM'S BIT               
         GOTO1 =A(RESETBIT),DMCB,(R0),0(R4)                                     
         J     PROC2157                                                         
*                                                                               
PROC2155 DS    0H                                                               
         OC    1(2,R7),1(R7)       IS PROGRAM'S LIMIT ACCESS NONZERO?           
         JZ    PROC2157            NO - DON'T SET PROGRAM'S BIT                 
         GOTO1 =A(SETBIT),DMCB,(R0),0(R4)                                       
*                                                                               
PROC2157 DS    0H                  NEXT EXCEPTION                               
         LA    R7,3(R7)                                                         
         J     PROC2150                                                         
*                                                                               
PROC2160 DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*                                                                               
*                                                                               
FILTAGY  NTR1                                                                   
         LA    R2,AGYFILTS                                                      
         LHI   R0,AGYFILTNQ                                                     
*                                                                               
FILTAGY1 CLI   0(R2),C' '                                                       
         JNH   NEQXIT              AGENCY NOT IN FILTERS TABLE                  
         CLC   0(2,R1),0(R2)       FOUND A MATCH ON AGENCY?                     
         JE    EQXIT               GOOD                                         
         AHI   R2,2                                                             
         BRCT  R0,FILTAGY1                                                      
         J     NEQXIT              AGENCY NOT IN FILTERS TABLE                  
*                                                                               
*                                                                               
*                                                                               
* FIND SYSTEM'S SLOT WITHIN PROGTAB ENTRY (PTSYSLST)                            
* P1 - A(SYSTEM EQUATE)                                                         
*                                                                               
* ON EXIT P1 WILL HAVE SYSTEM'S SLOT NUMBER                                     
* IN PROGTABD ENTRY.                                                            
* ORDER AND NUMBER OF SLOTS IS DETERMINED BY BLDPGMS TABLE                      
FINDSYSL NTR1                                                                   
         L     R2,0(R1)            SYSTEM EQUATE                                
         LLC   R2,0(R2)                                                         
         LARL  RF,BLDPGMS                                                       
         XR    RE,RE                                                            
*                                                                               
FINDSYS1 DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         JE    NEQXIT              PROGRAM NOT IN LIST                          
*                                                                               
         CLM   R2,1,0(RF)                                                       
         JE    FINDSYS2                                                         
*                                                                               
         AHI   RF,L'BLDPGMS                                                     
         AHI   RE,1                                                             
         J     FINDSYS1                                                         
*                                                                               
FINDSYS2 DS    0H                                                               
         ST    RE,0(R1)                                                         
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ROUTINE TO OUTPUT TIME AS CHARACTERS                                *         
* NTRY: R0       = TIME IN 0.01S UNITS                                *         
* EXIT: WORK(11) = TIME AS CHARACTERS                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTTBIN  NTR1                                                                   
         SRDL  R0,32                                                            
         D     R0,=F'360000'       GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVI   WORK+2,C':'                                                      
         SRDL  R0,32                                                            
         D     R0,=F'6000'         GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         MVC   WORK+5(6),SPACES                                                 
         MVI   WORK+5,C':'                                                      
         SRDL  R0,32                                                            
         D     R0,=F'100'         GIVES SECS IN R1/100THS IN R0                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         MVI   WORK+8,C'.'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+9(2),DUB                                                    
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
PROGTABD DSECT                                                                  
PTID     DS    CL10                UID                                          
PTAGY    DS    CL2                 AGENCY                                       
*                                                                               
PTSYSLST DS    0X                                                               
PTSPTSE  DS    X                   SYSTEM SE                                    
PTSPTPGS DS    XL32                SYSTEM PROGRAMS LIST                         
PTSYSLSTLQ EQU *-PTSYSLST                                                       
*                                                                               
         ORG   PTSYSLST                                                         
         DS    (BLDPGMNQ*PTSYSLSTLQ)X LIST OF SUPPORTED SYSTEMS                 
*                                                                               
PROGTABDLQ EQU *-PROGTABD                                                       
*                                                                               
*                                                                               
*                                                                               
CTREPXML CSECT                                                                  
TABLES   DS    0D                                                               
         DC    CL32'********OFFICE CODE TAB*********'                           
OFFTAB   DC    (32*2000)C' '                                                    
OFFTABX  DC    D'0'                                                             
         DC    CL32'********DEPT CODE TAB***********'                           
DEPTAB   DC    (35*2000)C' '                                                    
DEPTABX  DC    D'0'                                                             
         DC    CL32'********SEC GROUP TAB **********'                           
SECTAB   DC    (38*2000)C' '                                                    
SECTABX  DC    D'0'                                                             
         DC    CL32'********TIMESHEET TAB **********'                           
TSHTAB   DC    (38*8000)C' '                                                    
TSHTABX  DC    D'0'                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
$$DATA   LOCTR ,                   BACKUP TO RB LOCATION                        
SAVERC   DC    F'0'                                                             
*                                                                               
*                                                                               
*                                                                               
*************************************************************                   
*        DCBS                                               *                   
*************************************************************                   
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004                                              
*                                                                               
TAPEOUTL DCB   DDNAME=TAPEOUTL,DSORG=PS,MACRF=(PM),                    X        
               RECFM=VB,LRECL=4004                                              
*                                                                               
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
*                                                                               
*  AGUSERIDXXXXPERSONID                                                         
*                                                                               
         DS    0H                                                               
SCARD1   DC    C'SORT FIELDS=(1,23,BI,A) '                                      
         DS    0H                                                               
SCARD2   DC    C'RECORD TYPE=F,LENGTH=23 '                                      
*                                                                               
         DS    0H                                                               
SCARD3   DC    C'SORT FIELDS=(1,5,BI,A) '                                       
         DS    0H                                                               
SCARD4   DC    C'RECORD TYPE=F,LENGTH=186 '                                     
*                                                                               
* sort cards for <alphalist> table                                              
*                                                                               
         DS    0H                                                               
SCARD5   DC    C'SORT FIELDS=(1,4,BI,A) '                                       
         DS    0H                                                               
SCARD6   DC    C'RECORD TYPE=F,LENGTH=4 '                                       
*                                                                               
*                                                                               
ZEROS    DC    32X'00'                                                          
FFS      DC    32X'FF'                                                          
DOTS     DC    20C'.'                                                           
FILE     DC    C'C'                                                             
         SPACE 1                                                                
SELOW    DS    0CL25                                                            
         DC    C'F'                                                             
         DC    XL24'040000000000000000000000000000000000000000000000'           
SEHIGH   DS    0CL25                                                            
         DC    C'F'                                                             
         DC    XL24'04FF00000000000000000000000000000000000000000000'           
         SPACE 1                                                                
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMUNLK   DC    CL8'DMUNLK'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ADDREC   DC    CL8'ADDREC'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
*                                                                               
PUT      DC    CL8'PUT'                                                         
GET      DC    CL8'GET'                                                         
END      DC    CL8'END'                                                         
*                                                                               
CONTROL  DC    CL8'CONTROL'                                                     
GENDIR   DC    CL8'GENDIR'                                                      
GENFIL   DC    CL8'GENFIL'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
CTFILES  DC    CL8'NCTFILE '                                                    
*&&US                                                                           
         DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
*&&                                                                             
         DC    CL8'X       '                                                    
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
$$CODE   LOCTR ,                   BACK TO CODE LOCATION                        
         EJECT                                                                  
***********************************************************************         
* SSB                                                                 *         
***********************************************************************         
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
*                                                                               
       ++INCLUDE FASYSLST                                                       
       ++INCLUDE DDCOMFACSC                                                     
       ++INCLUDE FACIDTAB             TABLE RETURNED BY DMFATABS CALL           
*                                                                               
*                                                                               
*                                                                               
WORKC    CSECT                                                                  
         DS    (64*1024)X             WORKING STORAGE POOL                      
         DS    0D                                                               
         EJECT                                                                  
*                                                                               
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
AXMLTAG  DS    A                                                                
*                                                                               
ACLTCOD  DS    A                                                                
AENDCLST DS    A                                                                
*                                                                               
ALSTCOD  DS    A                                                                
ANXTTAG  DS    A                                                                
AENDLST  DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
SVCLTCOD DS    CL3                                                              
*                                                                               
READFLAG DS    X                                                                
CTINIT   DS    X                                                                
TRANFLAG DS    C                                                                
DATATYPE DS    CL10                                                             
ADVLKUP  DS    C             FORCE ADV SYSTEM LOOKUP WITH DSPACE!=A             
SHOWBAD  DS    C             SHOW ENTRIES WITH INVALID SE NUMBERS               
SHOINACT DS    C             SHOW INACTIVE AGENCIES                             
*                                                                               
AGYFILTNQ EQU 50                                                                
AGYFILTS DS    CL(2*AGYFILTNQ) AGENCY FILTERS (DATATYPE=SYSPROG ONLY)           
*                                                                               
SECFLAG  DS    X                                                                
SECFE1Q  EQU   X'01'               E1 (SACLAEL) ELEMENT PRESENT                 
*                                                                               
SVOFLKEY DS    XL4                 SAVED MEDIA OFFICE LIST SORT KEY             
*                                                                               
OPENFLAG DS    X                                                                
OPAGYQ   EQU   X'01'                                                            
OPSYSQ   EQU   X'02'                                                            
OPLSTQ   EQU   X'04'                                                            
OPLIMQ   EQU   X'08'                                                            
*                                                                               
CTOLDKEY DS    CL25                                                             
CTSAVKEY DS    CL25                                                             
CTSAVHI  DS    CL25                                                             
CTSAVLO  DS    CL25                                                             
READACCK DS    CL25                                                             
READACCE DS    X                                                                
*                                                                               
GEOLDKEY DS    CL32                                                             
*                                                                               
LASTAGY  DS    CL2                                                              
SAVEAGY  DS    CL2                                                              
SAVESYS  DS    C                                                                
LASTSYS  DS    C                                                                
SAVEOFL  DS    C                                                                
SAVEPW   DS    XL2                                                              
SAVEPER  DS    CL8                                                              
SAVELIM  DS    XL4                                                              
*                                                                               
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
WORK     DS    XL255                                                            
WORK1    DS    XL255                                                            
WORK3    DS    XL255                                                            
WLEN     DS    X                                                                
DA       DS    F                                                                
*                                                                               
COUNTER  DS    PL8                                                              
*                                                                               
*                                                                               
AGY_SEC_TAB EQU 0                  AGY-SECAGY TABLE                             
SEC_AGY_TAB EQU 1                  SECAGY-AGY TABLE                             
UID_PGM_TAB EQU 2                  UID-SYSTEM-PROGRAM NUMBER TABLE              
PROGNAM_TAB EQU 3                  SYSTEM -PROGRAM NUMBER - NAME TABLE          
PID_UID_TAB EQU 4                  TABLE OF USER IDS FOR CURRENT PID            
*                                                                               
*                                                                               
         DS    0F                  ALIGN ON A FULLWORD                          
BSPARS   DS    XL24                BINSRCH PARAMETERS                           
         ORG   BSPARS                                                           
BP1      DS    F                   A(RECORD)                                    
BP2      DS    F                   A(TABLE)                                     
BP3      DS    F                   NUMBER OF RECORDS                            
BP4      DS    F                   RECORD LENGTH                                
BP5      DS    F                   KEY LENGTH                                   
BP6      DS    F                   MAX ENTRIES                                  
*                                                                               
         ORG   BSPARS                                                           
*                                                                               
BSPARS1  DS    XL24                BINSRCH PARAMETERS FOR AGY-SECAGY            
         ORG   BSPARS1                                                          
BSP1_1   DS    F                                                                
BSP1_2   DS    F                                                                
BSP1_3   DS    F                                                                
BSP1_4   DS    F                                                                
BSP1_5   DS    F                                                                
BSP1_6   DS    F                                                                
*                                                                               
BSPARS2  DS    XL24               BINSRCH PARAMETERS SECAGY-AGY                 
         ORG   BSPARS2                                                          
BSP2_1   DS    F                                                                
BSP2_2   DS    F                                                                
BSP2_3   DS    F                                                                
BSP2_4   DS    F                                                                
BSP2_5   DS    F                                                                
BSP2_6   DS    F                                                                
*                                                                               
BSPARS3  DS    XL24              BINSRCH PARAMETERS UID-PROGRAMS                
         ORG   BSPARS3                                                          
BSP3_1   DS    F                                                                
BSP3_2   DS    F                                                                
BSP3_3   DS    F                                                                
BSP3_4   DS    F                                                                
BSP3_5   DS    F                                                                
BSP3_6   DS    F                                                                
*                                                                               
BSPARS4  DS    XL24              BINSRCH PARAMETERS PROGNAME TABLE              
         ORG   BSPARS4                                                          
BSP4_1   DS    F                                                                
BSP4_2   DS    F                                                                
BSP4_3   DS    F                                                                
BSP4_4   DS    F                                                                
BSP4_5   DS    F                                                                
BSP4_6   DS    F                                                                
*                                                                               
BSPARS5  DS    XL24               BINSRCH PARAMETERS ID TABLE                   
         ORG   BSPARS5                                                          
BSP5_1   DS    F                                                                
BSP5_2   DS    F                                                                
BSP5_3   DS    F                                                                
BSP5_4   DS    F                                                                
BSP5_5   DS    F                                                                
BSP5_6   DS    F                                                                
*                                                                               
GETSYSNO DS    X                   SYSTEM NUMBER FOR GETSYSN                    
GETSYSNA DS    A                   A(SYSTEM NAME)                               
*                                                                               
AIO      DS    A                                                                
AIO2     DS    A                                                                
AXMLBLK  DS    A                                                                
AXMLOUT  DS    A                                                                
AXMLOUTL DS    A                                                                
AOFFICER DS    A                                                                
ACOMFACS DS    A                                                                
DMTABS   DS    6A                  TABLES RETURNED BY DMFATABS                  
DMTABSLQ EQU   *-DMTABS                                                         
*                                                                               
XL32     DS    XL32                                                             
*                                                                               
TODAY    DS    H                                                                
PLINE    DS    CL200                                                            
DDTRANP  DS    XL(DDTRANDLQ)       DDTRAN PARAMETER BLOCK                       
*                                                                               
WRKIOL   DS    XL4                                                              
WRKIOA   DS    CL256                                                            
*                                                                               
OFFBLK   DS    XL(OFCLENQ)                                                      
*                                                                               
         DS    0F                                                               
IOWORK   DS    XL64                                                             
*                                                                               
IOL      DS    XL4                                                              
IO       DS    4096C                                                            
IOL2     DS    XL4                                                              
IO2      DS    4096C                                                            
MYXMLBLK DS    5000C                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FACIDTABD                                                      
*&&US                                                                           
       ++INCLUDE SPGENAGY                                                       
*&&                                                                             
*&&UK                                                                           
       ++INCLUDE SPGENAGYA                                                      
*&&                                                                             
* DSECT TO COVER SYSADV EXTRACT ENTRY LINE                                      
SYSADVD  DSECT                                                                  
SYSADSEPQ EQU  X'5E'               SEMICOLON                                    
SYSADUID DS    CL(L'CTIKID)        USER ID                                      
         DS    C                                                                
SYSADSYS DS    CL(L'SYSLNAME)      SYSTEM                                       
         DS    C                                                                
SYSADAGY DS    CL(L'AGYKAGY)       AGENCY CODE                                  
         DS    C                                                                
SYSADSEC DS    CL(L'AGYKAGY)       SECURITY AGENCY CODE                         
         DS    C                                                                
SYSADADV DS    CL(L'FACISN4)       ADV SYSTEM NAME                              
         DS    C                                                                
SYSADVDLQ EQU  *-SYSADVD                                                        
*                                                                               
*                                                                               
*                                                                               
         PRINT ON                                                               
       ++INCLUDE DDXMLD                                                         
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DMRCVREXT                                                      
       ++INCLUDE GEGENGEN                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDTRAND                                                        
       ++INCLUDE FACTRYEQUS                                                     
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE DDTEAMLST                                                      
*&&UK                                                                           
       ++INCLUDE CTGENOFC                                                       
*&&                                                                             
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTSECXML  09/06/18'                                      
         END                                                                    
