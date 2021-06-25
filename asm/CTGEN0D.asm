*          DATA SET CTGEN0D    AT LEVEL 005 AS OF 05/14/19                      
*PHASE TA0B0DB                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE NUMVAL                                                                 
         TITLE 'CTGEN0D  -  TERMINAL RECORD MAINTENANCE'                        
GEN0D    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEND**,RA,R9,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTTREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         MVI   TERMINFO,0                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     SETTWA                                                           
         B     EXIT                                                             
         B     VALREC                                                           
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF TERMINAL RECORD                          *         
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,C'T'                                                     
         GOTO1 AFVAL,TRMTERMH      VALIDATE TERMINAL                            
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM       NUMERIC-ASSUME EXISTING UTL NUM              
         BO    KEYV2                                                            
         CLI   FVIFLD,C'#'         #NNNN MEANS TERM OR PSWD NUM INPUT           
         BE    EIIF                                                             
         SR    R1,R1               READ AND SPACE FILL 8 CHR ID                 
         LA    RF,8                                                             
         IC    R1,FVILEN                                                        
         SR    RF,R1               VTAM IDS CAN BE < 8 CHRS                     
         BM    EIIF                                                             
         BZ    KEYV1                                                            
         LA    R1,FVIFLD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
KEYV1    MVC   CTTKTID(8),FVIFLD                                                
         B     KEYV6                                                            
*                                                                               
KEYV2    XC    APPARM(20),APPARM   VALIDATE TERMINAL NUM                        
         L     RF,ACOM                                                          
         GOTO1 CPROTOFF-COMFACSD(RF)                                            
         L     RF,ACOM                                                          
         L     RF,CTERMVAL-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,(X'00',FVIHDR)                                       
         SR    R3,R3                                                            
         ICM   R3,7,APPARM+5       ERROR IF NUMBER NOT IN UTL                   
         BZ    EIIF                                                             
         L     RF,ACOM                                                          
         GOTO1 CPROTON-COMFACSD(RF)                                             
         SR    RE,RE                                                            
         ICM   RE,7,APPARM+1       POINT TO RETURN TERMINAL ID                  
         MVC   CTTKTID(8),0(RE)                                                 
*                                                                               
KEYV6    XC    TRMTERM,TRMTERM     ECHO TERMINAL ID BACK TO TWA                 
         MVC   TRMTERM(8),CTTKTID                                               
         MVI   TRMTERMH+5,8                                                     
         OI    TRMTERMH+6,X'80'                                                 
*                                  NOTE NEED TO PATCH THIS OUT IF               
*                                  WANT TO CHANGE DDSX TERMINALS                
*                                  SEE EJORDDNY                                 
*&&US                                                                           
KEYV7    EQU   *                                                                
         CLI   APACTN,ACTCHA                                                    
         BNE   KEYV7X                                                           
         CLC   CTTKTID(4),=CL4'DDSX'                                            
         BE    EDDSX                                                            
KEYV7X   EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
**********************************************************************          
* VALIDATE SYSTEM                                                               
**********************************************************************          
KEYVA    MVI   SYSTEM,0            SET SYSTEM NOT INPUT                         
         LA    R1,TRMSYSTH                                                      
         GOTO1 AFVAL                                                            
         BNE   KEYVD                                                            
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         BCTR  R1,0                                                             
         L     R3,ASYSLST                                                       
         USING SYSLSTD,R3          R3=A(VALID SYSTEMS TABLE)                    
         LA    R3,6(R3)                                                         
         LA    R3,SYSLLEN(R3)      GO PAST SERVICE ENTRY                        
*                                                                               
KEYVC    CLI   SYSLNUM,0           END OF LIST                                  
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    *+12                                                             
         LA    R3,SYSLLEN(R3)                                                   
         B     KEYVC                                                            
         MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   TRMSYST(7),SYSLNAME DISPLAY FULL SYSTEM NAME                     
         OI    TRMSYSTH+6,X'80'                                                 
         DROP  R3                                                               
         EJECT                                                                  
* VALIDATE PASSWORD                                                             
*                                                                               
KEYVD    MVI   PASSWORD,0          SET PASSWORD NOT INPUT                       
         XC    SVPSWRD,SVPSWRD                                                  
         LA    R1,TRMPWRDH                                                      
         GOTO1 AFVAL               TEST IF PASSWORD INPUT                       
         BNE   KEYVE                                                            
*                                  READ MASTER TERMINAL REC                     
         GOTO1 AIO,IOREAD+IOCONFIL+IO1                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         L     R2,AIOAREA1                                                      
         TM    CTTSTAT,X'0D'       RECORD MUST NOT BE PASSIVE OR PRNTR          
         BNZ   EIIF                                                             
         LA    R2,IOKEY                                                         
         MVC   CTTKPASS,FVIFLD     MOVE PASSWORD TO KEY                         
         MVC   SVPSWRD,FVIFLD                                                   
         MVI   PASSWORD,1                                                       
         OI    TERMINFO,X'80'      SET PASSWORD SPECIFIED                       
         EJECT                                                                  
* VALIDATE THIS/LAST ACTIONS                                                    
*                                                                               
KEYVE    MVC   APRECKEY(L'CTTKEY),CTTKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BH    VALKEYD             NRF - CHECK IF DELETED                       
         L     R2,AIOAREA1                                                      
         ZIC   R1,TRMPWRDH+5       CHECK CORRECT RECORD                         
         LA    R1,CTTKPASS-CTTKEY(,R1)                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEYSAV(0),0(R2)                                                
         BNE   VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
VALKEYD  TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
*                                                                               
VALKEYY  CLI   APACTN,ACTCPY                                                    
         BNE   VALKEYY1                                                         
         OC    SAVKEY,SAVKEY                                                    
         BZ    VALKEYY2                                                         
         MVC   IOKEY,SAVKEY                                                     
         MVC   CPYKEY,SAVKEY                                                    
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BL    VALKEYX             I/O ERROR EXIT                               
         BE    VALKEYY2            COPY RECORD REREAD OK                        
         MVC   FVADDR,AINKHDR                                                   
         TM    IOERR,IOEDEL        RECORD IS DELETED/NOT FOUND                  
         BZ    *+20                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         MVC   SAVKEY,APRECKEY                                                  
         B     VALKEYX                                                          
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         MVC   SAVKEY,APRECKEY                                                  
         B     VALKEYX                                                          
VALKEYY1 CLI   APACTN,ACTMOVE                                                   
         BNE   VALKEYY2                                                         
         LA    R1,TRMTERMH                                                      
         ST    R1,FVADDR                                                        
         MVC   SAVRECK,APRECKEY                                                 
         MVC   SAVRECI,APRECID                                                  
         B     VALKEYY3                                                         
VALKEYY2 MVC   SAVKEY,APRECKEY                                                  
VALKEYY3 MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE TERMINAL RECORDS                           *         
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
         MVC   CTTKEY,APRECKEY                                                  
         XC    SVTRMEL,SVTRMEL                                                  
         MVI   SVSTAT,0                                                         
         MVI   TAPLFLAG,0                                                       
*                                                                               
         CLI   APACTN,ACTADD       CHECK IF ADD FUNCTION                        
         BE    VRADD                                                            
         CLI   APACTN,ACTCPY       CHECK IF COPY FUNCTION                       
         BE    VRCPY                                                            
*                                  IF SYSTEM CHANGED REDISPLAY                  
         CLC   SYSTEM,SAVSYS         RECORD BEFORE CHANGE/MOVE                  
         BNE   DISREC                                                           
         CLI   APACTN,ACTMOVE      CHECK IF MOVE FUNCTION                       
         BE    VRMOVE                                                           
         B     VRCHA               ELSE CHANGE FUNCTION                         
*                                  HERE FOR ADD                                 
VRADD    XC    CTTREC(256),CTTREC                                               
         MVC   CTTKEY,APRECKEY                                                  
         LA    R0,CTTDATA+1-CTTREC                                              
         STCM  R0,3,CTTLEN                                                      
         B     VRPID                                                            
*                                  HERE FOR COPY                                
VRCPY    NI    TWAMODE,X'FF'-TWAMDFR                                            
         MVC   SVSTAT,CTTSTAT      SAVE ORIG STATUS                             
         LA    R3,CTTDATA          AND STRIP DOWN RECORD                        
         SR    R4,R4                                                            
VRCPY10  CLI   0(R3),0                                                          
         BE    VRUPD                                                            
         CLI   0(R3),X'01'         DELETE ACTIVITY ELEMENT                      
         BE    VRCPY30                                                          
         CLI   0(R3),X'03'         DELETE PASSIVE POINTER                       
         BE    VRCPY30                                                          
         CLI   0(R3),CTPNXELQ      DELETE PRINTER NAME X-REF ELEMENT            
         BE    VRCPY30                                                          
         CLI   0(R3),CTPNDELQ      DELETE PRINTER NAME DESCRIPTION              
         BE    VRCPY30                                                          
         CLI   0(R3),CTPRTELQ      PROCESS PRINTERQ PRINTER ID ELEMENT          
         BE    VRCPY32                                                          
         CLI   0(R3),CTPRQELQ      PROCESS PRINTERQ ENTRY ELEMENTS              
         BE    VRCPY32                                                          
         CLI   0(R3),X'25'         EXTRACT TERMINAL INFO                        
         BE    VRCPY40                                                          
VRCPY20  IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     VRCPY10                                                          
*                                                                               
VRCPY30  SR    R0,R0                                                            
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',=C'CTFILE  '),((R0),CTTREC),0,0              
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCPY10                                                          
*                                                                               
VRCPY32  CLI   OPTPQC,C'N'                                                      
         BNE   VRCPY20                                                          
         B     VRCPY30                                                          
*                                                                               
         USING CTTRMD,R3                                                        
VRCPY40  TM    CTTRMDEV,X'80'                                                   
         BZ    *+8                                                              
         OI    TERMINFO,X'04'                                                   
         CLI   CTTRMDEV,X'82'                                                   
         BNE   *+8                                                              
         OI    TERMINFO,X'18'                                                   
         TM    TERMINFO,X'04'                                                   
         BNE   VRCPY20                                                          
         NI    TERMINFO,255-X'08'                                               
         TM    CTTRMAT1,X'02'                                                   
         BZ    VRCPY20                                                          
         OI    TERMINFO,X'08'                                                   
         B     VRCPY20                                                          
         DROP  R3                                                               
         EJECT                                                                  
*                                  CHANGE FUNCTION                              
*                                                                               
VRCHA    MVC   SVSTAT,CTTSTAT      SAVE ORIG STATUS                             
         LA    R3,CTTDATA          AND STRIP DOWN RECORD                        
         SR    R4,R4                                                            
VRCHA10  CLI   0(R3),0                                                          
         BE    VRCHA60                                                          
VRCHA20  CLI   0(R3),X'25'         SAVE TERMINAL DEFN ELEMENT                   
         BNE   VRCHA30                                                          
         MVC   SVTRMEL,0(R3)                                                    
         TM    TERMINFO,X'80'                                                   
         BZ    VRCHA40                                                          
         B     VRCHA50                                                          
VRCHA30  CLI   0(R3),X'01'                                                      
         BE    VRCHA40             DELETE ELEMENT                               
         CLI   0(R3),X'1F'                                                      
         BE    VRCHA40                                                          
         CLI   0(R3),X'20'                                                      
         BE    VRCHA40                                                          
         CLI   0(R3),CTTOUELQ                                                   
         BE    VRCHA40                                                          
         CLI   0(R3),X'24'                                                      
         BNE   VRCHA50                                                          
         TM    TERMINFO,X'80'                                                   
         BZ    VRCHA40                                                          
         B     VRCHA50                                                          
*                                                                               
VRCHA40  SR    R0,R0                                                            
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',=C'CTFILE  '),((R0),CTTREC),0,0              
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHA50  IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     VRCHA10                                                          
*                                  DELETE PROGRAM EXCEPTION ELEMENTS            
VRCHA60  GOTO1 VHEXOUT,APPARM,SYSTEM,APWORK,1,=C'TOG'                           
         MVI   APWORK,C'T'                                                      
         GOTO1 ,APPARM,(C'D',=C'CTFILE  '),(X'23',CTTREC),(2,APWORK)            
         CLI   SYSTEM,0                                                         
         BNE   *+10                                                             
         XC    APPARM+8(4),APPARM+8                                             
         GOTO1 VHELLO,APPARM       DELETE PROGRAM EXCEPTION ELEMENTS            
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         NI    CTTSTAT,255-X'02'                                                
         TM    TERMINFO,X'80'      DISPLAYING TERM NETWORK DATA                 
         BZ    VRPID               YES                                          
         TM    SVSTAT,X'04'        NO REMEMBER IF TERMINAL/PRINTER              
         BZ    VRPID                                                            
         OI    TERMINFO,X'04'                                                   
         B     VRPID                                                            
         EJECT                                                                  
*                                  MOVE FUNCTION                                
*                                                                               
VRMOVE   MVC   SVSTAT,CTTSTAT      SAVE ORIG STATUS                             
         MVC   IOKEY,SAVKEY        READ LAST RECORD INTO IOAREA2                
         GOTO1 AIO,IOCONFIL+IORD+IO2                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   IOKEY,APRECKEY                                                   
         L     R2,AIOAREA1                                                      
         LA    R3,CTTDATA          STRIP DOWN RECORD                            
         SR    R4,R4                                                            
VRMOV10  CLI   0(R3),0                                                          
         BE    VRMOV60                                                          
VRMOV20  CLI   0(R3),X'25'         SAVE TERMINAL DEFN ELEMENT                   
         BNE   VRMOV30                                                          
         MVC   SVTRMEL,0(R3)                                                    
         B     VRMOV40                                                          
VRMOV30  CLI   0(R3),X'01'                                                      
         BE    VRMOV50             DELETE ELEMENTS                              
         CLI   0(R3),X'1F'                                                      
         BE    VRMOV50                                                          
         CLI   0(R3),X'20'                                                      
         BE    VRMOV50                                                          
         CLI   0(R3),X'21'                                                      
         BE    VRMOV50                                                          
         CLI   0(R3),X'23'                                                      
         BE    VRMOV50                                                          
         CLI   0(R3),CTTOUELQ                                                   
         BE    VRMOV50                                                          
*                                                                               
VRMOV40  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRMOV10                                                          
*                                                                               
VRMOV50  SR    R0,R0                                                            
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',=C'CTFILE  '),((R0),CTTREC),0,0              
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRMOV10                                                          
*                                                                               
VRMOV60  EQU   *                   COPY ELEMENTS FROM IOAREA2                   
         NI    CTTSTAT,255-X'02'                                                
         L     R2,AIOAREA2                                                      
         LA    R3,CTTDATA          STRIP DOWN RECORD                            
         SR    R4,R4                                                            
VRMOV70  CLI   0(R3),0                                                          
         BE    VRMOV100                                                         
         CLI   0(R3),X'1F'                                                      
         BE    VRMOV90                                                          
         CLI   0(R3),X'20'                                                      
         BE    VRMOV90                                                          
         CLI   0(R3),X'21'                                                      
         BE    VRMOV90                                                          
         CLI   0(R3),X'23'                                                      
         BE    VRMOV90                                                          
         CLI   0(R3),CTTOUELQ                                                   
         BE    VRMOV90                                                          
*                                                                               
VRMOV80  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRMOV70                                                          
*                                                                               
VRMOV90  EQU   *                                                                
         L     R4,AIOAREA1                                                      
         GOTO1 VHELLO,APPARM,(C'P',=C'CTFILE '),(R4),(R3),=C'ADD=CODE'          
         CLI   12(R1),0                                                         
         BNE   ERTB                CHECK RECORD TOO BIG                         
         B     VRMOV80                                                          
*                                                                               
VRMOV100 EQU   *                                                                
         L     R2,AIOAREA1                                                      
         MVC   SAVKEY,APRECKEY     RSTORE LAST KEY SAVE                         
         TM    SVSTAT,X'04'                                                     
         BZ    VRUPD                                                            
         OI    TERMINFO,X'04'                                                   
         B     VRUPD                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINCIPAL ID                                               *         
***********************************************************************         
VRPID    XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         MVI   FLAG,0              SET FLAGS FOR WHAT WAS INPUT                 
         XC    SVAGYAP,SVAGYAP                                                  
         XC    SVAGYAI,SVAGYAI                                                  
         GOTO1 AFVAL,TRMPIDH                                                    
         BNE   VRPIDX              PRINCIPAL ID NOT INPUT                       
*                                                                               
         CLI   FVILEN,3                                                         
         BL    EFTS                                                             
         L     R3,AIOAREA2         READ PRINCIPLE ID RECORD                     
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FVIFLD                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         BAS   RE,GETAGID          GET AGENCY ALPHA ID AND TEST ID              
         MVC   SVAGYAP,AGYID                                                    
         MVI   FLAG,1              SET COMPATIBLE ID LIST BUILT                 
         MVC   IOKEY,APRECKEY                                                   
         DROP  R3                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTPIDD,R3                                                        
         MVI   CTPIDEL,CTPIDELQ    BUILD PRINCIPAL ID ELEMENT                   
         MVI   CTPIDLEN,X'0C'                                                   
         MVC   CTPID,FVIFLD                                                     
         GOTO1 AADDELN,CTTREC                                                   
         BNE   VALRECER                                                         
VRPIDX   EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD ID ELEMENTS                                      *         
***********************************************************************         
VRCID    LA    R1,TRMIDSH                                                       
         GOTO1 AFVAL                                                            
         BNE   VRCIDX                                                           
         MVI   TVIDFLAG,0                                                       
         CLI   FVILEN,3                                                         
         BNE   VRCID10                                                          
         CLC   FVIFLD(3),=C'ALL'                                                
         BNE   VRCID10                                                          
         TM    CUSTAT,CUSDDS       ALL ONLY VALID FOR DDS TERMS                 
         BZ    EIIF                                                             
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ                                                   
         MVI   CTIDLEN,X'0C'                                                    
         MVC   CTID,FVIFLD                                                      
         GOTO1 AADDELN,CTTREC                                                   
         BNE   VALRECER                                                         
         OI    TVIDFLAG,TVIDALLQ   VALID ID = 'ALL'                             
         DROP  R3                                                               
         B     VRCIDX                                                           
*                                                                               
VRCID10  LA    R8,TRMIDSH                                                       
         MVI   LINECNT,3                                                        
*                                                                               
VRCID20  TM    1(R8),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R8)                                                         
         AR    R8,R1                                                            
         LR    R1,R8                                                            
         GOTO1 AFVAL                                                            
         BNE   VRCID200                                                         
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK1)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRCID30  CLC   FVINDX,FLDCNT                                                    
         BH    VRCID200                                                         
         CLI   1(R4),0                                                          
         BNE   VRCID40                                                          
         CLI   0(R4),3             VALIDATE USER-ID                             
         BL    EFTS                                                             
         CLI   0(R4),10                                                         
         BH    EFTL                                                             
         BAS   RE,WILDCARD                                                      
         BE    VRCID120                                                         
         L     R3,AIOAREA2          SWITCH I/O AREAS                            
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY       BUILD ID KEY                                 
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R4)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         DROP  R3                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         MVC   CTID,12(R4)                                                      
         B     VRCID70                                                          
         DROP  R3                                                               
*                                                                               
VRCID40  CLI   0(R4),0             VALIDATE LIST-ID                             
         BE    EIIF                                                             
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'LIST'   CHECK FOR VALID KEYWORD                      
         BE    VRCID60                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'AGY'    CHECK FOR VALID KEYWORD 'AGY'                
         BNE   EIIF                                                             
         CLI   1(R4),2                                                          
         BNE   EIIF                MUST BE 2 CHARS                              
*                                                                               
         CLI   FLAG,0              FIRST LIST ENTRY?                            
         BE    EIIF                CAN'T BE THE FIRST ENTRY                     
*                                                                               
         MVC   KEYSAVE,IOKEY       SAVE THE ID KEY AROUND                       
*                                  AGY=  AGY-UID LIST                           
         L     R3,AIOAREA2         SWITCH I/O AREAS                             
         USING CT9BREC,R3                                                       
         XC    CT9BKEY,CT9BKEY     BUILD LIST KEY                               
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01                                                
         MVC   CT9BKAGY,22(R4)                                                  
         MVC   IOKEY(L'CT9BKEY),CT9BKEY                                         
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BL    EIIO                                                             
         CLC   CT9BKAGY,22(R4)     SAME AGENCY?                                 
         BNE   ERNF                NO - NOT FOUND                               
*                                                                               
VRCID43  LA    R1,CT9BDATA                                                      
VRCID44  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE X'02' ELEMENT                      
         CLI   0(R1),X'02'                                                      
         BE    VRCID45                                                          
         SR    RF,RF                                                            
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VRCID44                                                          
*                                                                               
VRCID45  MVC   APWORK(10),2(R1)    SAVE THIS USER ID                            
         BAS   RE,VALID            VALIDATE IT                                  
         BE    EIIF                                                             
*                                                                               
         GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
         CLC   CT9BKAGY,22(R4)     STILL THE SAME AGENCY?                       
         BE    VRCID43                                                          
         DROP  R3                                                               
*                                                                               
         MVC   IOKEY,KEYSAVE       RESTORE THE ID REC KEY                       
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD THE ELEMENT                            
         MVI   CTIDLEN,X'06'                                                    
         MVC   CTID(2),=X'0001'    AGY= SUBCODE                                 
         MVC   CTIDAGY(2),22(R4)                                                
         B     VRCID190                                                         
         DROP  R3                                                               
*                                                                               
VRCID60  CLI   1(R4),6                                                          
         BH    EFTL                                                             
         L     R3,AIOAREA2         SWITCH I/O AREAS                             
         USING CTWREC,R3                                                        
         XC    CTWKEY,CTWKEY       BUILD LIST KEY                               
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R4)                                                    
         MVC   IOKEY(L'CTWKEY),CTWKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         DROP  R3                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         XC    CTID(2),CTID                                                     
         MVC   CTID+2(8),22(R4)                                                 
*                                                                               
VRCID70  CLI   FLAG,0              FIRST LIST ENTRY                             
         BNE   VRCID80                                                          
         OC    CTID(2),CTID        MUST BE AN ID                                
         BZ    EIIF                                                             
         L     R3,AIOAREA2                                                      
         BAS   RE,GETAGID                                                       
         MVC   SVAGYAI,AGYID                                                    
         MVI   FLAG,1                                                           
         B     VRCID190                                                         
*                                                                               
VRCID80  MVC   APWORK(10),CTID     OTHER LIST ENTRIES MUST BE IN FIRST          
         DROP  R3                                                               
         OC    APWORK(2),APWORK    LIST ENTRY'S COMPATIBLE ID LIST              
         BZ    *+16                                                             
         BAS   RE,VALID                                                         
         BE    EIIF                                                             
         B     VRCID190                                                         
         L     R3,AIOAREA2                                                      
         USING CTWREC,R3                                                        
         LA    R3,CTWDATA                                                       
         SR    R1,R1                                                            
*                                                                               
VRCID90  CLI   0(R3),0             IF ENTRY IS A LIST DO FOR ALL                
         BE    VRCID190            ENTRIES IN LIST RECORD                       
         CLI   0(R3),X'A4'                                                      
         BNE   VRCID92                                                          
         MVC   APWORK(10),3(R3)                                                 
         BAS   RE,VALID                                                         
         BE    EIIF                                                             
VRCID92  EQU   *                                                                
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VRCID90                                                          
         DROP  R3                                                               
VRCID120 EQU   *                   PROCESS WILDCARD ID XX*                      
         CLI   FLAG,0              FIRST LIST ENTRY                             
         BE    EIIF                MUST BE A SINGLE USERID                      
         MVC   APWORK(10),12(R4)                                                
         BAS   RE,VALID                                                         
         BE    EIIF                                                             
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         MVC   CTID,12(R4)                                                      
         DROP  R3                                                               
*                                  ADD ID ELEMENT TO TERM REC                   
VRCID190 LA    R3,APELEM                                                        
         GOTO1 VHELLO,APPARM,(C'P',=C'CTFILE '),(R2),(R3),=C'ADD=CODE'          
         CLI   12(R1),0                                                         
         BNE   ERTB                CHECK RECORD TOO BIG                         
*                                                                               
         ZIC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRCID30                                                          
*                                                                               
VRCID200 ZIC   R1,0(R8)            BUMP TO NEXT TWA LINE                        
         AR    R8,R1                                                            
         ZIC   RF,LINECNT                                                       
         BCT   RF,*+8                                                           
         B     VRCIDX                                                           
         STC   RF,LINECNT                                                       
         B     VRCID20                                                          
*                                                                               
VRCIDX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD PROGRAM EXCEPTION ELEMENTS                       *         
***********************************************************************         
VRPEX    LA    R8,TRMPE1H                                                       
         MVI   LINECNT,2                                                        
*                                                                               
VRPEX10  TM    1(R8),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R8)                                                         
         AR    R8,R1                                                            
         LR    R1,R8                                                            
         GOTO1 AFVAL                                                            
         BNE   VRPEX50                                                          
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK1)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRPEX20  CLC   FVINDX,FLDCNT                                                    
         BH    VRPEX50                                                          
         CLI   0(R4),1             L'PART1                                      
         BL    EMIF                                                             
         CLI   0(R4),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   1(R4),1             L'PART2                                      
         BH    EFTL                                                             
         BL    EIIF                                                             
         CLI   0(R4),4             L'PART1                                      
         BNE   VRPEX30                                                          
         CLI   12(R4),C'T'         'T' MEANS ONLINE                             
         BNE   VRPEX30                                                          
         GOTO1 VHEXIN,APPARM,13(R4),APDUB,3,0                                   
         OC    APPARM+12(4),APPARM+12                                           
         BNZ   VRPEX40                                                          
*                                                                               
VRPEX30  CLI   SYSTEM,0            CAN'T INPUT PGMNAME IF NOT IN                
         BE    EIIF                SYSTEM MODE                                  
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         L     RF,=A(GETPRGX)      GET PROGRAM NUMBER                           
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'                                                    
         BE    EIIF                                                             
         MVC   APDUB(1),SYSTEM                                                  
         MVC   APDUB+1(1),PROGRAM                                               
         GOTO1 VHEXOUT,APPARM,APDUB,12(R4),2,=C'TOG'                            
         MVI   12(R4),C'T'         NAME CONVERTED TO TSPP                       
*                                                                               
VRPEX40  CLI   22(R4),C'A'         TEST LEVEL S/B A,B OR C                      
         BL    EIIF                                                             
         CLI   22(R4),C'C'                                                      
         BH    EIIF                                                             
         XC    APELEM,APELEM       BUILD PROGRAM EXCEPTION ELEMENT              
         LA    R3,APELEM                                                        
         USING CTPRGD,R3                                                        
         MVC   CTPRGEL(2),=X'2307'                                              
         MVC   CTPRGRAM,12(R4)                                                  
         MVC   CTPRGTST,22(R4)                                                  
         GOTO1 AADDELN,CTTREC                                                   
         BNE   VALRECER                                                         
*                                                                               
         ZIC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRPEX20                                                          
*                                                                               
VRPEX50  ZIC   R1,0(R8)            BUMP TO NEXT TWA LINE                        
         AR    R8,R1                                                            
         ZIC   RF,LINECNT                                                       
         BCT   RF,*+8                                                           
         B     VRPEXX                                                           
         STC   RF,LINECNT                                                       
         B     VRPEX10                                                          
VRPEXX   EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD A SYSTEM ELEMENT                                 *         
***********************************************************************         
VRSYS    LA    R1,TRMPA1H                                                       
         GOTO1 AFVAL                                                            
         BE    VRSYS10                                                          
         CLI   SYSTEM,0                                                         
         BE    VRSYS140                                                         
         B     EMIF                                                             
*                                                                               
VRSYS10  CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
         CLI   FVILEN,6                                                         
         BNE   VRSYS20                                                          
         CLC   FVIFLD(6),=C'DELETE'                                             
         BNE   VRSYS20                                                          
         CLI   APACTN,ACTADD       CANNOT DELETE EL IF ACTN=ADD                 
         BE    EIIF                                                             
         GOTO1 VHELLO,APPARM,(C'D',=C'CTFILE '),(X'21',(R2)),(1,SYSTEM)         
         B     VRSYS140                                                         
*                                                                               
VRSYS20  LA    R8,TRMPA1H                                                       
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTSYSD,R3                                                        
         MVC   CTSYSEL(2),=X'2110'                                              
         MVC   CTSYSNUM,SYSTEM                                                  
         MVC   CTSYSALL,=X'FFFF'   PRESET ALL & PROGRAM VALUES                  
         MVC   CTSYSPGM,=X'FFFF'                                                
         MVC   CTSYSPGM+2(126),CTSYSPGM                                         
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         MVI   LINECNT,2                                                        
*                                                                               
VRSYS30  TM    1(R8),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R8)                                                         
         AR    R8,R1                                                            
         LR    R1,R8                                                            
         GOTO1 AFVAL                                                            
         BNE   VRSYS90                                                          
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK1)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRSYS40  CLC   FVINDX,FLDCNT                                                    
         BH    VRSYS90                                                          
         CLI   0(R4),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   0(R4),1             L'PART1                                      
         BL    EFTS                                                             
         CLI   1(R4),4             L'PART2                                      
         BH    EFTL                                                             
         BE    VRSYS50                                                          
         CLI   1(R4),1             L'PART2                                      
         BNE   EIIF                                                             
         MVC   APDUB(2),=X'000F'   ONE CHR INPUT S/B Y OR N                     
         CLI   22(R4),C'Y'                                                      
         BE    VRSYS60                                                          
         MVC   APDUB(2),=X'0000'                                                
         CLI   22(R4),C'N'                                                      
         BE    VRSYS60                                                          
         B     EIIF                                                             
*                                                                               
VRSYS50  TM    3(R4),X'20'         IF NOT Y OR N MUST BE VALID HEX              
         BZ    EFNH                                                             
         GOTO1 VHEXIN,APPARM,22(R4),APDUB,4                                     
         OC    APPARM+12(4),APPARM+12  DOUBLE CHECK FOR VALID HEX               
         BZ    EFNH                                                             
*                                                                               
VRSYS60  CLI   0(R4),3                                                          
         BNE   VRSYS70                                                          
         CLC   12(3,R4),=C'ALL'                                                 
         BNE   VRSYS70                                                          
         CLC   CTSYSALL,=X'FFFF'   ALL VALUES ALREADY INPUT ?                   
         BNE   EDIF                                                             
         MVC   CTSYSALL,APDUB                                                   
         B     VRSYS80                                                          
*                                                                               
VRSYS70  L     RF,=A(GETPGAX)      GET PROGRAM NUMBER                           
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'                                                    
         BE    EIIF                                                             
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,CTSYSPGM(R1)     POINT TO PROGRAM AUTH                        
         CLC   0(2,R1),=X'FFFF'    PRGM VALUE PREVIOUSLY INPUT ?                
         BNE   EDIF                                                             
         MVC   0(2,R1),APDUB                                                    
*                                                                               
VRSYS80  ZIC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRSYS40                                                          
*                                                                               
VRSYS90  ZIC   R1,0(R8)            BUMP TO NEXT TWA LINE                        
         AR    R8,R1                                                            
         ZIC   RF,LINECNT                                                       
         BCT   RF,*+8                                                           
         B     VRSYS92                                                          
         STC   RF,LINECNT                                                       
         B     VRSYS30                                                          
*                                                                               
VRSYS92  CLC   CTSYSALL,=X'FFFF'   SET ALL VALUE TO N IF N/I                    
         BNE   *+10                                                             
         MVC   CTSYSALL,=X'0000'                                                
         LA    R1,CTSYSPGM         SET PRG VALUES TO ALL VALUE IF N/I           
         LA    RE,64                                                            
*                                                                               
VRSYS100 CLC   0(2,R1),=X'FFFF'                                                 
         BNE   *+10                                                             
         MVC   0(2,R1),CTSYSALL                                                 
         LA    R1,2(R1)                                                         
         BCT   RE,VRSYS100                                                      
         OI    CTSYSEL+5,X'80'                                                  
         CLC   CTSYSALL(128),CTSYSPGM                                           
         BE    VRSYS110                                                         
         L     RF,=A(CNVELM21)                                                  
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     VRSYS120                                                         
VRSYS110 MVI   CTSYSLEN,16         SET SHORT IF NO PRGM OVERRIDES               
VRSYS120 CLI   APACTN,ACTADD                                                    
         BE    VRSYS130                                                         
         GOTO1 VHELLO,APPARM,(C'D',=C'CTFILE '),(X'21',(R2)),(1,SYSTEM)         
VRSYS130 GOTO1 AADDELN,CTTREC      DELETE OLD AND ADD NEW SYSTEM EL             
         BNE   VALRECER                                                         
*                                                                               
VRSYS140 TM    TERMINFO,X'80'      TEST IF PASSWORD RECORD                      
         BZ    VRSYSX                                                           
         B     VRUPD                                                            
*                                                                               
VRSYSX   EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD VTAM APPLICATION ID ELEMENTS                     *         
***********************************************************************         
VRAPL    LA    R1,TRMTAPLH                                                      
         GOTO1 AFVAL                                                            
         BNE   VRAPLX                                                           
         CLI   FVILEN,3                                                         
         BNE   VRAPL1                                                           
         CLC   FVIFLD(3),=C'ALL'                                                
         BNE   VRAPL1                                                           
         TM    CUSTAT,CUSDDS        ALL ONLY VALID FOR DDS TERMS                
         BZ    EIIF                                                             
         OI    TAPLFLAG,TAPLFMUQ                                                
         LA    R3,APELEM                                                        
         USING CTAPLD,R3                                                        
         XC    APELEM,APELEM                                                    
         MVI   CTAPLEL,CTAPLELQ     BUILD APPLICATION ID ELEMENT                
         MVI   CTAPLLEN,X'0B'                                                   
         MVI   CTAPLFLG,0                                                       
         MVC   CTAPLID,FVIFLD                                                   
         GOTO1 AADDELN,CTTREC                                                   
         BNE   VALRECER                                                         
         B     VRAPLX                                                           
         DROP  R3                                                               
*                                                                               
VRAPL1   LA    R8,TRMTAPLH                                                      
         MVI   LINECNT,2                                                        
         MVI   TAPLCNT,0                                                        
*                                                                               
VRAPL2   TM    1(R8),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R8)                                                         
         AR    R8,R1                                                            
         LR    R1,R8                                                            
         GOTO1 AFVAL                                                            
         BNE   VRAPL8                                                           
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK3)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R4,BLOCK3           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRAPL4   CLC   FVINDX,FLDCNT       TEST LAST FIELD THIS LINE                    
         BH    VRAPL8                                                           
         CLI   1(R4),0                                                          
         BNE   EIIF                                                             
         CLI   0(R4),2             CHANGE TO 2 TO ALLOW "QA" APPL ID            
*        CLI   0(R4),3             VALIDATE APPLICATION ID                      
         BL    EFTS                                                             
         CLI   0(R4),8                                                          
         BH    EFTL                                                             
         CLC   12(3,R4),=CL3'REP'                                               
         BNE   *+8                                                              
         OI    TAPLFLAG,TAPLFREQ                                                
         CLC   12(3,R4),=CL3'SHU'                                               
         BE    VRAPL6                                                           
         SR    RF,RF                                                            
         IC    RF,TAPLCNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,TAPLCNT                                                       
         CLI   TAPLCNT,1                                                        
         BE    *+8                                                              
         OI    TAPLFLAG,TAPLFMUQ                                                
*                                                                               
VRAPL6   EQU   *                                                                
         LA    R3,APELEM                                                        
         USING CTAPLD,R3                                                        
         XC    APELEM,APELEM                                                    
         MVI   CTAPLEL,CTAPLELQ     BUILD APPLICATION ID ELEMENT                
         MVI   CTAPLLEN,X'0B'                                                   
         MVI   CTAPLFLG,0                                                       
         MVC   CTAPLID,12(R4)                                                   
         GOTO1 AADDELN,CTTREC                                                   
         BNE   VALRECER                                                         
         DROP  R3                                                               
*                                                                               
         ZIC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRAPL4                                                           
*                                                                               
VRAPL8   ZIC   R1,0(R8)            BUMP TO NEXT TWA LINE                        
         AR    R8,R1                                                            
         ZIC   RF,LINECNT                                                       
         BCT   RF,*+8                                                           
         B     VRAPLX                                                           
         STC   RF,LINECNT                                                       
         B     VRAPL2                                                           
VRAPLX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADV TIMEOUT                                                *         
***********************************************************************         
VRATOU   XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         GOTO1 AFVAL,TRMATOUH                                                   
         BNE   VRATOUX             ADV TIMEOUT NOT INPUT                        
         USING CTTOUD,R3                                                        
         MVI   CTTOUEL,CTTOUELQ    INITIALISE ELEMENT                           
         MVI   CTTOULEN,CTTOULNQ                                                
         GOTO1 =V(NUMVAL),APPARM,TRMATOU,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    VRATOUE1                                                         
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    VRATOUE1                                                         
         C     R1,=F'1275'                                                      
         BH    VRATOUE1                                                         
         STCM  R1,3,CTTOUADV                                                    
         GOTO1 AADDELN,CTTREC                                                   
         BNE   VALRECER                                                         
         B     VRATOUX                                                          
VRATOUE1 MVC   FVMSGNO,=AL2(CE#ADTOU)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                                                             
VRATOUX  EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
* VALIDATE AND BUILD A TERMINAL INFO ELEMENT                                    
*                                                                               
VRTRM    LA    R3,APELEM           R3=A(TERMINAL DEFN EL)                       
         USING CTTRMD,R3                                                        
         XC    APELEM,APELEM                                                    
         MVI   TDEVFLAG,0                                                       
         MVC   CTTRMEL(2),=X'2520'                                              
*                                                                               
VRDEV    LARL  R8,DEVTBL           VALIDATE DEVICE                              
         LA    R1,TRMDEVH                                                       
         GOTO1 AFVAL                                                            
         BNE   VRDEV2                                                           
         SR    R1,R1                                                            
         ZIC   R1,FVILEN                                                        
         BCTR  R1,0                                                             
VRDEV1   CLI   0(R8),X'00'         END OF LIST                                  
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R8)                                                  
         BE    *+12                                                             
         LA    R8,DEVTBLL(R8)                                                   
         B     VRDEV1                                                           
*                                                                               
VRDEV2   MVC   CTTRMDEV,8(R8)      SET DEVICE                                   
         MVC   TRMDEV(8),0(R8)     DISPLAY FULL DEVICE NAME                     
         OI    TRMDEVH+6,X'80'                                                  
*                                                                               
         CLI   CTTRMDEV,X'04'      WEB TERMINAL?                                
         BNE   VRDEV3                                                           
*&&US*&& CLC   TRMTERM(3),=C'WEB'  YES - MUST START WITH WEB IN US              
*&&UK*&& CLC   TRMTERM+7(1),=C'W'  YES - MUST END IN W FOR UK                   
         BNE   EIIF                                                             
         OI    TDEVFLAG,TDEVWEBQ   SET WEB TERMINAL                             
*                                                                               
VRDEV3   TM    CTTRMDEV,X'80'                                                   
         BZ    *+8                                                              
         OI    TERMINFO,X'04'      SET DEVICE IS A PRINTER                      
         CLI   CTTRMDEV,X'82'                                                   
         BNE   *+8                                                              
         OI    TERMINFO,X'18'      SET DEVICE IS SHUTTLE/AUTO                   
*                                                                               
VRTYP    LARL  R8,TYPTBL           VALIDATE TYPE                                
         LA    R1,TRMTYPH                                                       
         GOTO1 AFVAL                                                            
         BNE   VRTYP2                                                           
         SR    R1,R1                                                            
         ZIC   R1,FVILEN                                                        
         BCTR  R1,0                                                             
VRTYP1   CLI   0(R8),X'00'         END OF LIST                                  
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R8)                                                  
         BE    *+12                                                             
         LA    R8,TYPTBLL(R8)                                                   
         B     VRTYP1                                                           
VRTYP2   MVC   CTTRMTYP,8(R8)      SET TYPE                                     
         MVC   TRMTYP(8),0(R8)     DISPLAY FULL TYPE NAME                       
         OI    TRMTYPH+6,X'80'                                                  
*                                                                               
VRCTY    GOTO1 AFVAL,TRMCTRYH      VALIDATE COUNTRY                             
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         ZIC   R8,FVILEN                                                        
         SH    R8,=H'1'            R8=L'INPUT-1                                 
         BNM   VRCTY1                                                           
         B     VRCTY2              NO COUNTRY INPUT                             
VRCTY1   EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   CTRYSHR(0),FVIFLD   COMPARE ON SHORT NAME                        
         BE    VRCTY2                                                           
         BXLE  R1,RE,VRCTY1                                                     
         B     EIIF                                                             
VRCTY2   MVC   CTTRMCTY,CTRYCODE   SAVE COUNTRY CODE                            
         MVC   TRMCTRY(L'CTRYSHR),CTRYSHR                                       
         OI    TRMCTRYH+6,X'80'                                                 
VRCTYX   EQU   *                                                                
         DROP  R1                                                               
*                                                                               
*&&UK                                                                           
VRAGY    GOTO1 AFVAL,TRMAGYH       VALIDATE AGENCY ALPHA FOR UK ONLY            
         BNE   VRAGY0                                                           
         TM    FVIIND,FVITHIS      WAS IT INPUT THIS TIME                       
         BO    VRAGY1                                                           
*                                                                               
VRAGY0   MVC   TRMAGY,SVAGYAP      SET VALUE FROM PRINCIPAL ID                  
         OC    TRMAGY,TRMAGY                                                    
         BNZ   *+10                                                             
         MVC   TRMAGY,SVAGYAI      SET VALUE FROM FIRST USER ID                 
         OC    TRMAGY,TRMAGY                                                    
         BNZ   VRAGYX                                                           
         MVC   TRMAGY,=CL2' '                                                   
         B     VRAGYX                                                           
VRAGY1   CLI   FVILEN,2            MUST BE TWO CHR LONG                         
         BNE   EFTS                                                             
VRAGYX   MVC   CTTRMAGY,TRMAGY                                                  
         OI    TRMAGYH+6,X'80'                                                  
*&&                                                                             
*&&US                                                                           
VRAGY    GOTO1 AFVAL,TRMAGYH       VALIDATE AGENCY ALPHA FOR US ONLY            
         BE    VRAG010                                                          
*                                  REMOVE THIS TEMPORARILY                      
*        CLI   APACTN,ACTADD       MUST BE INPUT IF NOT ADD ACTION              
*        BNE   EMIF                                                             
*                                                                               
         MVC   TRMAGY,=CL2' '                                                   
         B     VRAGYX              MUST BE INPUT IF CHANGE ACTION               
VRAG010  CLI   FVILEN,2            MUST BE TWO CHR LONG                         
         BNE   EFTS                                                             
         L     R1,AIOAREA2         READ AGENCY ACCESS RECORD                    
         USING CT5REC,R1           VALIDATE AGENCY ALPHA EXISTS                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
VRAGYX   MVC   CTTRMAGY,TRMAGY                                                  
         OI    TRMAGYH+6,X'80'                                                  
*&&                                                                             
*                                                                               
VROFC    GOTO1 AFVAL,TRMOFCH       VALIDATE OFFICE CODE                         
         CLI   FVILEN,1                                                         
         BH    VROFC1                                                           
         BNL   *+16                                                             
         MVI   TRMOFC,C' '                                                      
         MVI   CTTRMOFC,C' '       DEFAULT IS SPACE                             
         B     VROFCX                                                           
         MVC   CTTRMOFC,FVIFLD     INPUT IS SINGLE CHR                          
         CLI   FVIFLD,C'A'                                                      
         BNL   VROFCX                                                           
         CLI   FVIFLD,C'*'                                                      
         BNE   EIIF                                                             
         OI    CTTRMFL1,X'01'      TREAT * AS *1                                
         B     VROFCX                                                           
VROFC1   CLI   FVIFLD,C'*'         ALLOW *1 OR *2 FOR DDS LEVELS                
         BNE   EIIF                                                             
         MVI   CTTRMOFC,C'*'       SET DDS OFFICE CHR                           
         CLI   FVIFLD+1,C'1'                                                    
         BNE   *+12                                                             
         OI    CTTRMFL1,X'01'      SET DDS LEVEL 1 FLAG                         
         B     VROFCX                                                           
         CLI   FVIFLD+1,C'2'                                                    
         BNE   *+12                                                             
         OI    CTTRMFL1,X'02'      SET DDS LEVEL 2 FLAG                         
         B     VROFCX                                                           
         B     EIIF                                                             
VROFCX   OI    TRMOFCH+6,X'80'                                                  
*                                                                               
         TM    TVIDFLAG,TVIDALLQ   VALID ID = 'ALL'?                            
         BNO   VRVIDOK             NO - OKAY                                    
         TM    CTTRMFL1,X'01'+X'02'  DDS OFFICE?                                
         BNZ   VRVIDOK               YES - OKAY                                 
         TM    TDEVFLAG,TDEVWEBQ   WEB TERMINAL?                                
         BO    VRVIDOK               YES - OKAY                                 
*                                                                               
         L     R1,ASYSFACS                                                      
         L     R1,VSSB-SYSFACD(R1)                                              
         TM    SSBSYSFL-SSBD(R1),X'80'   TEST SYSTEMS?                          
         BZ    VRVIDBAD                  NO - INVALID INPUT                     
*                                                                               
         CLC   =C'DDQA',CTTKTID    DDQA???? TERMINAL IS OKAY                    
         BE    VRVIDOK                                                          
         CLC   =C'DDTNQ',CTTKTID   DDTNQ??? TERMINAL IS OKAY                    
         BE    VRVIDOK                                                          
*                                                                               
VRVIDBAD EQU   *                                                                
         LA    R1,TRMIDSH                                                       
         ST    R1,FVADDR                                                        
         B     EIIF                                                             
VRVIDOK  EQU   *                                                                
*                                                                               
VRNDE    LARL  R8,NODETBL          VALIDATE NODE                                
         LR    R0,R8                                                            
         GOTO1 AFVAL,TRMNODEH                                                   
         BNE   VRNDE2                                                           
         CLI   FVILEN,3                                                         
         BNE   EIIF                                                             
         LA    R8,4(R8)                                                         
VRNDE1   CLI   0(R8),C' '          END OF LIST                                  
         BE    EIIF                                                             
         CLC   FVIFLD(3),0(R8)                                                  
         BE    VRNDE2                                                           
         LA    R8,4(R8)                                                         
         B     VRNDE1                                                           
*                                                                               
VRNDE2   SR    R8,R0               GET INDEX INTO NODE LIST                     
         SRL   R8,2                                                             
         STC   R8,CTTRMNDE         SET NODE NUMBER                              
*                                                                               
VRLNE    GOTO1 AFVAL,TRMLINEH      VALIDATE LINE ID                             
         BNE   VRLNE1                                                           
         CLI   FVILEN,4                                                         
         BL    EIIF                                                             
VRLNE1   MVC   CTTRMLNE,FVIFLD                                                  
*                                                                               
VRCD     GOTO1 AFVAL,TRMCUDVH      VALIDATE CONTROL UNIT                        
         BNE   VRCD1               AND DEVICE ADDRESS                           
         CLI   FVILEN,4                                                         
         BNE   EIIF                                                             
         GOTO1 VHEXIN,APPARM,FVIFLD,CTTRMCU,2                                   
         OC    APPARM+12(4),APPARM+12  EXIT IF INVALID HEX                      
         BZ    EFNH                                                             
         GOTO1 VHEXIN,APPARM,FVIFLD+2,CTTRMDV,2                                 
         OC    APPARM+12(4),APPARM+12  EXIT IF INVALID HEX                      
         BZ    EFNH                                                             
VRCD1    EQU   *                                                                
         SPACE 2                                                                
VRTATL1  LA    R8,TRMATB1H         TERMINAL ATTRIBUTE LIST                      
         MVI   LINECNT,2                                                        
*                                                                               
VRTATL2  TM    1(R8),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R8)                                                         
         AR    R8,R1                                                            
         LR    R1,R8                                                            
         GOTO1 AFVAL                                                            
         BNE   VRTATL5                                                          
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK4)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R4,BLOCK4           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRTATL3  CLC   FVINDX,FLDCNT       TEST LAST FIELD THIS LINE                    
         BH    VRTATL5                                                          
         CLI   0(R4),8             MAX PART 1 LEN                               
         BH    EFTL                                                             
         CLI   0(R4),3             MIN PART 1 LEN                               
         BL    EFTS                                                             
         CLI   1(R4),1             MIN PART 2 LEN                               
         BL    EFTS                                                             
         B     VRAT1               GO CHECK KEYWORD=VALUE FIELD                 
*                                                                               
VRTATL4  ZIC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRTATL3                                                          
*                                                                               
VRTATL5  ZIC   R1,0(R8)            BUMP TO NEXT TWA LINE                        
         AR    R8,R1                                                            
         ZIC   RF,LINECNT                                                       
         BCT   RF,*+8                                                           
         B     VRTATLX                                                          
         STC   RF,LINECNT                                                       
         B     VRTATL2                                                          
*                                                                               
VRTATLX  MVI   FVINDX,0                                                         
         GOTO1 AADDELN,CTTREC      ADD TERMINAL DEFINITION EL                   
         BNE   VALRECER                                                         
         B     VRUPD                                                            
         SPACE 2                                                                
VRAT1    CLC   12(3,R4),=C'AT1='   AT1=ABCDEFGH                                 
         BNE   VRAT1X                                                           
         BAS   RE,VRALAT           VALIDATE TERMINAL ATTRIBUTES                 
         BNE   EIIF                                                             
         MVC   CTTRMAT1,APDUB                                                   
         TM    TERMINFO,X'04'      TEST IF TERMINAL IS A PRINTER                
         BZ    VRTATL4             NO                                           
         NI    TERMINFO,255-X'08'                                               
         TM    CTTRMAT1,X'02'      TEST IF AUTO MODE AT1=G                      
         BZ    VRTATL4             NO                                           
         OI    TERMINFO,X'08'      SET PRINTER AUTO BIT                         
         B     VRTATL4                                                          
VRAT1X   EQU   *                                                                
*                                                                               
VRAT2    CLC   12(3,R4),=C'AT2='   AT2=ABCDEFGH                                 
         BNE   VRAT2X                                                           
         BAS   RE,VRALAT           VALIDATE TERMINAL ATTRIBUTES                 
         BNE   EIIF                                                             
         MVC   CTTRMAT2,APDUB                                                   
         B     VRTATL4                                                          
VRAT2X   EQU   *                                                                
*                                                                               
VRAT3    CLC   12(3,R4),=C'AT3='   AT3=ABCDEFGH                                 
         BNE   VRAT3X                                                           
         BAS   RE,VRALAT           VALIDATE TERMINAL ATTRIBUTES                 
         BNE   EIIF                                                             
         MVC   CTTRMAT3,APDUB                                                   
         B     VRTATL4                                                          
VRAT3X   EQU   *                                                                
*                                                                               
VRESC    CLC   12(3,R4),=C'ESC='    ESCAPE SEQUENCE NUMBER                      
         BNE   VRESCX                                                           
         TM    3(R4),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R4)                                                         
         CH    R0,=H'1'                                                         
         BL    EIIF                                                             
         CH    R0,=H'255'                                                       
         BH    EIIF                                                             
         STCM  R0,1,CTTRMESC                                                    
         B     VRTATL4                                                          
VRESCX   EQU   *                                                                
*                                                                               
VRLTY    CLC   12(3,R4),=C'LTYPE=' LINE TYPE                                    
         BNE   VRLTYX                                                           
         CLI   1(R4),1                                                          
         BNE   EIIF                                                             
         MVC   APDUB(1),22(R4)                                                  
         CLI   APDUB,C'B'          B=BSC                                        
         BE    VRLTY1                                                           
         CLI   APDUB,C'L'          L=LOCAL                                      
         BE    VRLTY1                                                           
         CLI   APDUB,C'S'          S=SDLC                                       
         BE    VRLTY1                                                           
         CLI   APDUB,C'T'          T=TWX                                        
         BE    VRLTY1                                                           
         B     EIIF                                                             
VRLTY1   MVC   CTTRMLTY,APDUB                                                   
         B     VRTATL4                                                          
VRLTYX   EQU   *                                                                
*                                                                               
VRLSP    CLC   12(3,R4),=C'LSPEED=' LINE SPEED IN BAUD                          
         BE    *+14                                                             
         CLC   12(3,R4),=C'BAUD='                                               
         BNE   VRLSPX                                                           
         TM    3(R4),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R4)                                                         
         CH    R0,=H'200'                                                       
         BL    EIIF                                                             
         C     R0,=F'65536'                                                     
         BH    EIIF                                                             
         SRL   R0,3                 CONVERT BAUD TO CPS                         
         STCM  R0,3,CTTRMLSP                                                    
         B     VRTATL4                                                          
VRLSPX   EQU   *                                                                
*                                                                               
VRPSP    CLC   12(3,R4),=C'PSPEED=' PRINTER SPEED                               
         BNE   VRPSPX                                                           
         TM    3(R4),X'80'                                                      
         BZ    EIIF                                                             
         ICM   R0,15,8(R4)                                                      
         BZ    EIIF                                                             
         CH    R0,=H'2000'                                                      
         BH    EIIF                                                             
         TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         STCM  R0,3,CTTRMPSP                                                    
         B     VRTATL4                                                          
VRPSPX   EQU   *                                                                
*                                                                               
VRPST    CLC   12(3,R4),=C'PTYPE=' PRINTER SPEED TYPE                           
         BNE   VRPSTX                                                           
         CLI   1(R4),1                                                          
         BNE   EIIF                                                             
         MVC   APDUB(1),22(R4)                                                  
         CLI   APDUB,C'B'          B=BIDIRECTIONAL                              
         BE    VRPST1                                                           
         CLI   APDUB,C'C'          C=CHARACTER                                  
         BE    VRPST1                                                           
         CLI   APDUB,C'L'          L=LINE                                       
         BE    VRPST1                                                           
         CLI   APDUB,C'P'          P=PAGE                                       
         BE    VRPST1                                                           
         B     EIIF                                                             
VRPST1   TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         MVC   CTTRMPST,APDUB                                                   
         B     VRTATL4                                                          
VRPSTX   EQU   *                                                                
*                                                                               
VRPRQ    CLC   12(3,R4),=C'PQUEUE=' PRINTER QUEUE NUMBER OF ENTRIES             
         BNE   VRPRQX                                                           
         TM    3(R4),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R4)                                                         
         CH    R0,=H'250'          NEWER PQ MAXIMUM                             
         BH    EIIF                                                             
         CH    R0,=H'99'                                                        
         BNL   *+8                                                              
         LH    R0,=H'99'           DEFAULT MINIMUM                              
         TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         STCM  R0,1,CTTRMPRQ                                                    
         B     VRTATL4                                                          
VRPRQX   EQU   *                                                                
*                                                                               
VRPBS    CLC   12(3,R4),=C'PBUFF=' PRINTER BUFFER SIZE                          
         BNE   VRPBSX                                                           
         TM    3(R4),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R4)                                                         
         CH    R0,=H'256'                                                       
         BL    EIIF                                                             
         CH    R0,=H'4096'                                                      
         BH    EIIF                                                             
         TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         STCM  R0,3,CTTRMPBS                                                    
         B     VRTATL4                                                          
VRPBSX   EQU   *                                                                
*                                                                               
         B     EIIF                INVALID TERMINAL KEYWORD FIELD               
         SPACE 2                                                                
VRALAT   XC    APDUB,APDUB         VALIDATE ATTRIBUTE LIST                      
         CLI   1(R4),8                                                          
         BH    VRALATER                                                         
         ZIC   R0,1(R4)            R0=NUM OF ATTRIBUTES CHRS                    
         LA    R1,22(R4)           R1=A(NEXT ATTRIBUTE CHR)                     
VRALAT1  CLI   0(R1),C'*'                                                       
         BE    VRALAT2             * IS NOP VALUE                               
         CLI   0(R1),C'A'                                                       
         BL    VRALATER                                                         
         CLI   0(R1),C'H'                                                       
         BH    VRALATER                                                         
         MVC   APDUB+7(1),0(R1)    CONVERT A-H TO 1-8                           
         NI    APDUB+7,X'0F'                                                    
         LH    RF,APDUB+6                                                       
         LA    RF,ATTBITS-1(RF)                                                 
         OC    APDUB(1),0(RF)                                                   
         B     VRALAT2                                                          
VRALATER LA    R0,255              SET ERROR VALUE                              
         B     *+12                                                             
VRALAT2  LA    R1,1(R1)                                                         
         BCT   R0,VRALAT1                                                       
         LTR   R0,R0                                                            
         BR    RE                  EXIT WITH CC NEQ IF ERROR                    
ATTBITS  DC    X'8040201008040201'                                              
         DROP  R3                                                               
         EJECT                                                                  
* I/O HANDLING FOR ADD/CHANGE                                                   
*                                                                               
VRUPD    GOTO1 ASETACN,CTTREC                                                   
         BNE   VALRECER                                                         
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         TM    TERMINFO,X'18'                                                   
         BNO   VRUPD10                                                          
         TM    TAPLFLAG,TAPLFMUQ                                                
         BZ    VRUPD10                                                          
         LA    R1,TRMTAPLH         SET CURSOR                                   
         ST    R1,FVADDR                                                        
         TM    TAPLFLAG,TAPLFREQ                                                
         BO    EEAP                                                             
         CLI   SAVPROM,0                                                        
         BNE   VRUPD10                                                          
         MVI   SAVPROM,X'FF'                                                    
         B     EWAP                                                             
*                                                                               
VRUPD10  EQU   *                                                                
         MVI   SAVPROM,0                                                        
         TM    TERMINFO,X'04'      TEST IF PRINTER DEVICE                       
         BO    CHGPRT                                                           
*                                                                               
CHGTRM   CLI   APACTN,ACTCHA       CHANGE/MOVE TERMINAL RECORD                  
         BE    *+12                                                             
         CLI   APACTN,ACTMOVE                                                   
         BNE   ADDTRM                                                           
         TM    SVSTAT,X'04'        WAS IT A TERMINAL BEFORE                     
         BZ    CHGTRM1                                                          
         MVC   DELKEY,APRECKEY     NO DELETE OLD PRINTER PASSIVE                
         MVI   DELKEY+6,C'P'                                                    
         L     RF,=A(DLTREC)       DELETE UNWANTED PASSIVE                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
CHGTRM1  NI    CTTSTAT,255-X'0C'   SET RECORD IS A TERMINAL REC                 
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    VALRECX                                                          
         DC    H'00'                                                            
*                                                                               
ADDTRM   DS    0H                                                               
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    VALRECX                                                          
         DC    H'00'                                                            
*                                                                               
CHGPRT   CLI   APACTN,ACTCHA       CHANGE PRINTER RECORD                        
         BE    *+12                                                             
         CLI   APACTN,ACTMOVE                                                   
         BNE   ADDPRT                                                           
         TM    SVSTAT,X'04'        WAS IT A PRINTER BEFORE                      
         BO    CHGPRT1             YES                                          
*                                                                               
CHGPRT1  OI    CTTSTAT,X'04'       SET IS A PRINTER                             
         NI    CTTSTAT,255-X'08'                                                
         TM    TERMINFO,X'08'                                                   
         BZ    *+8                                                              
         OI    CTTSTAT,X'08'       SET PRINTER IN AUTO MODE                     
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
CHGPRT2  MVI   CTTKTID-1,C'P'      UPDATE PASSIVE PRINTER RECORD                
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         L     R2,AIOAREA2         SEARCH FOR EXISTING 'P' RECORD               
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'                                                            
         L     R2,AIOAREA1         RESTORE CURRENT RECORD POINTER               
         TM    SVSTAT,X'04'        CHANGE FROM PRINT OR TERM TYPE ?             
         BO    CHGPRT3                                                          
         TM    IOERR,X'10'        'P' RECORD NOT FOUND                          
         BO    CHGPRT4                                                          
         TM    IOERR,X'02'        'P' RECORD FOUND DELETED                      
         BO    CHGPRT5                                                          
         DC    H'0'                                                             
*                                                                               
CHGPRT3  CLI   IOERR,0             HERE IF PREVIOUSLY PRINTER TYPE              
         BE    *+6                                                              
         DC    H'00'                                                            
CHGPRT5  LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         B     CHGPRT6                                                          
CHGPRT4  LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
CHGPRT6  BE    VALRECX                                                          
         DC    H'00'                                                            
         SPACE 2                                                                
ADDPRT   OI    CTTSTAT,X'04'       ADD/COPY PRINTER RECORD                      
         NI    CTTSTAT,255-X'08'                                                
         TM    TERMINFO,X'08'                                                   
         BZ    *+8                                                              
         OI    CTTSTAT,X'08'       SET PRINTER IN AUTO MODE                     
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
ADDPRT2  MVI   CTTKTID-1,C'P'      CREATE PRINTER PASSIVE                       
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   APACTN,ACTCPY                                                    
         BNE   VALRECX                                                          
         CLI   OPTPQC,C'N'                                                      
         BE    VALRECX                                                          
         L     RF,=A(CPYPQP)       COPY PRINTERQ PAGE RECORDS                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     VALRECX                                                          
         SPACE 2                                                                
*                                                                               
VALRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   APACTN,ACTMOVE                                                   
         BE    DISREC                                                           
         B     EXIT                                                             
*                                                                               
VALRECER B     EXIT                                                             
         EJECT                                                                  
* ROUTINE CHECK ID IS IN COMPATIBLE ID LIST OF MASTER ID                        
*                                                                               
VALID    L     RF,AGIDBUF         A(EXTENDED GETID BUFFER)                      
*                                                                               
VALID2   CLC   0(10,RF),APWORK                                                  
         BE    VALIDX                                                           
*                                 CHECK FOR WILD CARD IN CID LIST               
         LA    R1,9(RF)                                                         
         LA    R0,10                                                            
*                                                                               
VALID3   EQU   *                                                                
         CLI   0(R1),C'*'                                                       
         BE    VALID4                                                           
         CLI   0(R1),C' '                                                       
         BNE   VALID5                                                           
         BCTR  R1,0                                                             
         BCT   R0,VALID3                                                        
         B     VALID5                                                           
*                                                                               
VALID4   EQU   *                                                                
         BCTR  R0,0                                                             
         LR    R1,R0              CHECK FOR WILD CARD MATCH                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),APWORK                                                   
         BE    VALIDX                                                           
*                                                                               
VALID5   LA    RF,12(RF)                                                        
         CLI   0(RF),X'FF'                                                      
         BNE   VALID2                                                           
*                                                                               
VALIDX   CLI   0(RF),X'FF'        CC=EQ IF NOT IN LIST                          
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO SET AGENCY ALPHA ID FROM ID RECORD AT R3                           
* AND BUILD COMPATIBLE ID LIST                                                  
*                                                                               
GETAGID  ST    RE,RETURN                                                        
         SR    R0,R0                                                            
         LA    RF,CTIDATA-CTIREC(R3)                                            
GETAGID1 CLI   0(RF),0                                                          
         BNE   *+12                                                             
         LA    RF,=X'06040000'     POINT TO DUMMY ELEMENT                       
         B     GETAGID2                                                         
         CLI   0(RF),X'06'         TEST AGENCY ALPHA ID EL                      
         BE    GETAGID2                                                         
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETAGID1                                                         
GETAGID2 MVC   AGYID,2(RF)          SAVE AGENCY ALPHA ID                        
*                                                                               
*                                                                               
         L     RF,ASYSFACS          GET V(GETIDS) T00AFA                        
         L     RF,VCALLOV-SYSFACD(RF)                                           
*&&UK*&& GOTO1 (RF),APPARM,0,X'D9000AF9',0                                      
*&&US*&& GOTO1 (RF),APPARM,0,X'D9000AFA',0                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
*                                                                               
         L     R1,ATIA                                                          
         AHI   R1,-TENK             BACK UP FOR A LITTLE MORE ROOM              
         LR    R0,R1                                                            
         SR    R0,RD                                                            
         C     R0,=A(EIGHTYK)       WE NEED TO HAVE AT LEAST 60K LEFT           
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    R1,AGIDBUF           SAVE ADDRESS OF GETIDS BUFFER               
*                                                                               
         GOTO1 (RF),APPARM,(C'C',(R3)),AGIDBUF,(C'W',VDMGR)                     
         CLI   0(R1),0                                                          
         BE    EIIF                                                             
         CLI   0(R1),X'FF'                                                      
         BE    ERNF                                                             
         L     RE,RETURN                                                        
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO CHECK FOR WILD CARD STYLE USERID IN COMPATIBLE ID LIST             
* R4=A(SCANNER CONTROL BLOCK)                                                   
* IF WILDCARD (I.E. STRING ENDS WITH '*') RETURN IN LIDSAVE                     
* AND RETURN CC .EQ.                                                            
*                                                                               
WILDCARD NTR1                                                                   
         ZIC   RF,0(R4)                                                         
         LA    R1,11(RF,R4)                                                     
WCAR010  CLI   0(R1),C'*'                                                       
         BE    WCAR020                                                          
         CLI   0(R1),C' '                                                       
         BE    WCAR012                                                          
         B     WCARNO                                                           
WCAR012  BCTR  R1,0                                                             
         BCT   RF,WCAR010                                                       
         B     WCARNO                                                           
WCAR020  EQU   *                                                                
         BCTR  RF,0                                                             
         STC   RF,WILDCLEN                                                      
         B     WCAROK                                                           
WCAROK   SR    RC,RC                                                            
WCARNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A TERMINAL RECORD                                 *         
***********************************************************************         
DELREC   EQU   *                                                                
         L     R2,AIOAREA1                                                      
         MVC   SAVKEY,CTTKEY                                                    
*                                                                               
DELPN    EQU   *                   DELETE PRINTER NAME RECORDS                  
         TM    CTTSTAT,X'04'                                                    
         BZ    DEL010                                                           
         L     RF,=A(DELPNM)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   DELRECX                                                          
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO1                                         
*                                                                               
DEL010   EQU   *                                                                
         GOTO1 ASETACN,CTTREC                                                   
         BNE   DELRECX                                                          
         OI    CTTSTAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  DELETE PASSWORD REC'S OF SAME TERM           
DELTPX   MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         MVC   KEYSAVE,IOKEY                                                    
         GOTO1 AIO,IORDUPD+IOCONFIL+IO1                                         
         GOTO1 AIO,IOSQUP+IOCONFIL+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CTTKTID,KEYSAVE+CTTKTID-CTTREC     SAME TERMINAL ID?             
         BE    DEL010                                                           
         MVC   CTTKEY,SAVKEY       RESTORE THE VERY 1ST TERM ID REC             
*                                                                               
DELPP    MVI   CTTKTID-1,C'P'      DELETE PASSIVE PRINTER RECORD                
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO1                                         
         BE    DELPP1                                                           
         CLI   IOERR,X'10'         NOT FOUND IS OK                              
         BE    DELRECX                                                          
         CLI   IOERR,X'02'         ALREADY DELETED IS OK                        
         BE    DELRECX                                                          
         DC    H'0'                                                             
DELPP1   GOTO1 ASETACN,CTTREC                                                   
         BNE   DELRECX                                                          
         OI    CTTSTAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,=A(DELPQP)       DELETE PRINTERQ PAGE RECORDS                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
DELRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED TERMINAL RECORD GROUP                  *         
***********************************************************************         
RESREC   EQU   *                                                                
         L     R2,AIOAREA1                                                      
         MVC   SAVKEY,CTTKEY                                                    
*                                                                               
RESPN    EQU   *                   RESTORE PRINTER NAME RECORDS                 
         TM    CTTSTAT,X'04'                                                    
         BZ    RES010                                                           
         L     RF,=A(RESPNM)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   RESRECX                                                          
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO1                                         
*                                                                               
RES010   EQU   *                                                                
         GOTO1 ASETACN,CTTREC                                                   
         BNE   RESRECX                                                          
         NI    CTTSTAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  RESTORE PASSWORD REC'S OF SAME TERM          
RESTPX   MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         MVC   KEYSAVE,IOKEY                                                    
         GOTO1 AIO,IORDUP+IOCONFIL+IO1                                          
         GOTO1 AIO,IOSQUPD+IOCONFIL+IO1                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   CTTKTID,KEYSAVE+CTTKTID-CTTREC     SAME TERMINAL ID?             
         BE    RES010                                                           
         MVC   CTTKEY,SAVKEY       RESTORE THE VERY 1ST TERM ID REC             
*                                                                               
RESPP    MVI   CTTKTID-1,C'P'      RESTORE PASSIVE PRINTER RECORD               
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO1                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF                                                     
         BO    RESRECX                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETACN,CTTREC                                                   
         BNE   RESRECX                                                          
         NI    CTTSTAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,=A(RESPQP)       RESTORE PRINTERQ PAGE RECORDS                
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
RESRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF TERMINAL RECORD                           *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         MVC   TRMTERM,CTTKTID                                                  
         MVC   TRMPWRD,CTTKPASS                                                 
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY TERMINAL RECORD                                  *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         TWAXC TRMPIDH                                                          
         XC    XXCNT(XXCNTL),XXCNT ZERO ALL COUNTERS                            
         CLI   SYSTEM,0                                                         
         BE    DISP1                                                            
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
DISP1    XC    ASYSEL,ASYSEL       SET A(SYSTEM ELEMENT)                        
         XC    ATRMEL,ATRMEL       SET A(TERMINAL DEFN ELEMENT)                 
         MVI   SYSNUMSC,0          INITIALISE FOR SYSNUMS                       
         LA    R3,CTTDATA                                                       
*                                                                               
DISP2    CLI   0(R3),0                                                          
         BE    DISPEND                                                          
         CLI   0(R3),X'1F'         PRINCIPAL ID                                 
         BE    DISPPID                                                          
         CLI   0(R3),X'20'         ID                                           
         BE    DISPID                                                           
         CLI   0(R3),X'21'         SYSTEM                                       
         BE    DISPSS                                                           
         CLI   0(R3),X'23'         PROGRAM EXCEPTION                            
         BE    DISPEX                                                           
         CLI   0(R3),X'24'         APPLICATION ID                               
         BE    DISPAP                                                           
         CLI   0(R3),X'25'         TERMINAL DEFINITION                          
         BE    DISPTD                                                           
         CLI   0(R3),CTTOUELQ      TIMEOUT ELEMENT                              
         BE    DISPTO                                                           
*                                                                               
DISP4    SR    R4,R4               BUMP TO NEXT ELEMENT                         
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     DISP2                                                            
         EJECT                                                                  
* DISPLAY PRINCIPAL ID                                                          
*                                                                               
DISPPID  DS    0H                                                               
         USING CTPIDD,R3                                                        
         MVC   TRMPID,CTPID                                                     
         B     DISP4                                                            
* ADD AN ID ELEMENT TO ID BLOCK (BLOCK1)                                        
*                                                                               
DISPID   SR    R1,R1                                                            
         IC    R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MH    R4,=H'20'                                                        
         LA    R4,BLOCK1(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR IT                                 
         MVC   1(19,R4),0(R4)                                                   
         IC    R1,1(R3)                                                         
         OC    2(2,R3),2(R3)                                                    
         BNZ   *+10                                                             
         MVC   2(2,R3),=C'L='                                                   
         CLC   2(2,R3),=X'0001'                                                 
         BNZ   *+10                                                             
         MVC   2(2,R3),=C'A='                                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R3)       MOVE IN ID                                   
         B     DISP4                                                            
* ADD A PROGRAM EXCEPTION ELEMENT TO BLOCK (BLOCK2)                             
*                                                                               
DISPEX   DS    0H                                                               
         USING CTPRGD,R3                                                        
         MVC   PGNAME(4),CTPRGRAM  IF NOT IN SYSTEM MODE DISPLAY TSPP           
         CLI   SYSTEM,0                                                         
         BE    DISPEX2                                                          
         MVC   APDUB(4),CTPRGRAM                                                
         MVI   APDUB,C'0'                                                       
         GOTO1 VHEXIN,APPARM,APDUB,APDUB+4,4                                    
         CLC   SYSTEM,APDUB+4      EXCEPTION FOR THIS SYSTEM                    
         BNE   DISP4                                                            
         MVC   PROGRAM,APDUB+5     YES - GET PROGRAM NAME                       
         L     RF,=A(GETPRGN)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'                                                    
         BE    DISP4                                                            
*                                                                               
DISPEX2  DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,EXCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,EXCNT                                                         
         MH    R4,=H'20'                                                        
         LA    R4,BLOCK2(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR                                    
         MVC   1(19,R4),0(R4)                                                   
         MVC   0(4,R4),PGNAME      FIRST HALF IS NAME (TSPP OR XXXX)            
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'='                                                       
         MVC   2(1,R4),CTPRGTST    MOVE TEST LEVEL                              
         B     DISP4                                                            
         DROP  R3                                                               
* SAVE A(SYSTEM ELEMENT) IF IT'S THE ONE REQUESTED                              
*                                                                               
DISPSS   CLI   SYSTEM,0            IS A SYSTEM BEING DISPLAYED                  
         BE    DISPSS1             NO                                           
         USING CTSYSD,R3                                                        
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   *+8                                                              
         ST    R3,ASYSEL           SAVE A(SYSTEM ELEMENT)                       
DISPSS1  MVC   SYSNUMS,CTSYSNUM                                                 
         L     RF,=A(GETSEN)       GET SE NAME AND ADD TO LIST                  
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     DISP4                                                            
         DROP  R3                                                               
* ADD A VTAM APPLICATION ID TO AP BLOCK (BLOCK 3)                               
*                                                                               
DISPAP   SR    R1,R1                                                            
         IC    R1,APCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,APCNT                                                         
         MH    R4,=H'20'                                                        
         LA    R4,BLOCK3(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR IT                                 
         MVC   1(19,R4),0(R4)                                                   
         IC    R1,1(R3)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R3)       MOVE IN APPLICATION ID                       
         B     DISP4                                                            
* SAVE A(TERMINAL ELEMENT) IF WE ARE DISPLAYING IT                              
*                                                                               
DISPTD   TM    TERMINFO,X'80'      TEST IF PASSWORD INPUT                       
         BO    DISP4                                                            
         ST    R3,ATRMEL                                                        
         B     DISP4                                                            
* DISPLAY TIME OUT ELEMENT DATA                                                 
*                                                                               
         USING CTTOUD,R3                                                        
DISPTO   EQU   *                                                                
         EDIT  CTTOUADV,(4,TRMATOU),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         B     DISP4                                                            
         DROP  R3                                                               
* PUT BLOCKS TO TWA                                                             
*                                                                               
DISPEND  CLI   IDCNT,0             VALID ID'S                                   
         BE    DISPEND2                                                         
         ZIC   R0,IDCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(3,TRMIDSH),(20,BLOCK1),(R0),        *        
               RR=APRELO                                                        
*                                                                               
DISPEND2 CLI   EXCNT,0             PROGRAM EXCEPTIONS                           
         BE    DISPEND4                                                         
         ZIC   R0,EXCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(2,TRMPE1H),(20,BLOCK2),(R0),        *        
               RR=APRELO                                                        
*                                                                               
DISPEND4 OC    ASYSEL,ASYSEL       PROGRAM ACCESS                               
         BZ    DISPEND6                                                         
         L     RF,=A(DISPSYS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         ZIC   R0,PGCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(2,TRMPA1H),(20,BLOCK1),(R0),        *        
               RR=APRELO                                                        
*                                                                               
DISPEND6 SR    R0,R0               APPLICATION IDS                              
         ICM   R0,1,APCNT                                                       
         BZ    DISPEND7                                                         
         TM    TERMINFO,X'80'      ONLY FOR BASIC TERMINAL DISPLAY              
         BO    DISPEND7                                                         
         GOTO1 =V(SCINKEY),APPARM,(2,TRMTAPLH),(20,BLOCK3),(R0),       *        
               RR=APRELO                                                        
*                                                                               
DISPEND7 TM    TERMINFO,X'80'      TERMINAL INFO                                
         BO    DISPENDA                                                         
         OC    ATRMEL,ATRMEL                                                    
         BZ    DISPEND8                                                         
         L     RF,=A(DISPTRM)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         SR    R0,R0                                                            
         ICM   R0,1,ATCNT                                                       
         BZ    DISPENDA                                                         
         GOTO1 =V(SCINKEY),APPARM,(2,TRMATB1H),(20,BLOCK4),(R0),       *        
               RR=APRELO                                                        
         B     DISPENDA                                                         
*                                                                               
DISPEND8 LARL  RE,DEVTBL                                                        
         MVC   TRMDEV(8),0(RE)                                                  
         LARL  RE,TYPTBL                                                        
         MVC   TRMTYP(8),0(RE)                                                  
*                                                                               
DISPENDA MVI   TRMSYS,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   TRMSYS+1(L'TRMSYS-1),TRMSYS                                      
         CLI   SYSNUMSC,0                                                       
         BE    DISPENDB                                                         
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DISPENDB                                                         
         LA    R1,L'TRMSYS                                                      
         SR    R1,RF                                                            
         BNP   DISPENDB                                                         
         SRL   R1,1                                                             
         LA    RE,TRMSYS(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DISPENDB OI    TRMSYSH+6,X'80'                                                  
*                                                                               
DISPENDD MVC   SAVSYS,SYSTEM       SAVE LAST DISPLAYED SYSTEM#                  
         GOTO1 ADISACT,CTTREC                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
VALSEL   LA    R2,APRECKEY                                                      
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,X'FF'       FLAG FOR FIRST PASS                          
         XC    SELDATA,SELDATA                                                  
*                                                                               
         LA    R4,LSTTERMH                                                      
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         MVC   CTTKTID,SELTERM    BUILD AN INITIAL KEY                          
         CLC   SELPWRD,=CL10'YES'                                               
         BE    *+10                                                             
         MVC   CTTKPASS,SELPWRD                                                 
         TM    SELDEV,X'80'        IF REQUEST IS FOR PRINTERS ONLY              
         BZ    *+8                                                              
         MVI   CTTKSPAR+5,C'P'     SET KEY TO READ PRINTER PASSIVES             
*                                                                               
         OI    LSTHEADH+6,FVOXMT                                                
         MVC   LSTHEAD,SOFTHEAD                                                 
         OC    SELPWRD,SELPWRD                                                  
         BNZ   VALSEL20                                                         
         MVC   LSTHEAD,HARDHEAD                                                 
*                                                                               
VALSEL20 LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
GETSEL   LA    R2,IOKEY                                                         
         MVC   CTTKEY,APRECKEY                                                  
         CLI   CTTKTYP,X'FF'        TEST FIRST TIME FLAG                        
         BNE   GSGREC1                                                          
         MVI   CTTKTYP,C'T'                                                     
         OC    CTTKLINE,CTTKLINE   (IF NO KEY ENTERED                           
         BNZ   GSGREC3                 READ PAST PASSIVES)                      
         MVI   CTTKLINE+L'CTTKLINE-1,1                                          
         B     GSGREC3             READ HIGH                                    
GSGREC1  TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GSGREC2                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSGREC4                                                          
         B     GETSELN                                                          
GSGREC2  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GSGREC4                                                          
GSGREC3  LA    R1,IOCONFIL+IOHI+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     GSGREC5             (I/O PARAMS ALREADY SET)                     
GSGREC4  LA    R1,IOCONFIL+IOSQ+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
GSGREC5  BNE   GETSELN             (EOF)                                        
*                                                                               
GETSELY  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTTKEY),CTTKEY                                        
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
DISSEL   EQU   *                                                                
*                                                                               
         L     R4,APPARM                                                        
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
DISSELX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
VALREQ   L     R8,AREP                                                          
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REQREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REQWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REQDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         LA    R4,REQTERMH                                                      
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         LA    R2,APRECKEY         BUILD AN INITIAL KEY                         
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,C'T'                                                     
         MVC   CTTKTID,SELTERM                                                  
         CLC   SELPWRD,=CL10'YES'                                               
         BE    *+10                                                             
         MVC   CTTKPASS,SELPWRD                                                 
         TM    SELDEV,X'80'        IF REQUEST IS FOR PRINTERS ONLY              
         BZ    *+8                                                              
         MVI   CTTKSPAR+5,C'P'     SET KEY TO READ PRINTER PASSIVES             
*                                                                               
VALREQ20 MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TERMINAL LIST                                      *         
***********************************************************************         
PRTREP   EQU   *                                                                
         L     R8,AREP                                                          
         MVC   REPM1+5(L'SOFTHEAD-5),SOFTHEAD+5                                 
         OC    SELPWRD,SELPWRD                                                  
         BNZ   PRTREP20                                                         
         MVC   REPM1+5(L'HARDHEAD-5),HARDHEAD+5                                 
*                                                                               
PRTREP20 LA    R2,IOKEY                                                         
         MVC   CTTKEY,APRECKEY                                                  
         OC    CTTKLINE,CTTKLINE   (IF NO KEY ENTERED                           
         BNZ   *+8                    READ PAST PASSIVES)                       
         MVI   CTTKLINE+L'CTTKLINE-1,1                                          
         LA    R1,IOHI+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)         GO GET REC WIV                             
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     PRGREC1                                                          
*                                                                               
PRGREC   LA    R1,IOSQ+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)       GO GET NEXT REC                              
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
PRGREC1  BNE   PRTREPX                                                          
*                                                                               
         LA    R4,REPP1-14                                                      
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF               GO BUILD A PRINT LINE                        
*                                                                               
         GOTO1 VREPORT,REPD                                                     
         B     PRGREC                                                           
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                  ERROR EXITS                                  
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                INPUT FIELD INVALID                          
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXIT                INPUT FIELD TOO LONG                         
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXIT                INPUT FIELD TOO SHORT                        
EFNH     MVC   FVMSGNO,=AL2(FVFNOTV)  ??                                        
         B     EXIT                INPUT FIELD ERROR                            
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     EXIT                I/O ERROR                                    
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXIT                RECORD NOT FOUND                             
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                RECORD TO BIG                                
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXIT                DUPLICATE                                    
EEAP     MVC   FVMSGNO,=AL2(CE#ERAPP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                CAN NOT HAVE MORE THAN ONE APPLIC            
EWAP     MVC   FVMSGNO,=AL2(CE#WAAPP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                WARNING MORE THAN ONE APPLIC                 
EDDSX    MVC   FVMSGNO,=AL2(CE#DDSXT)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                CANT CHANGE DDSX TERMINALS IN US             
*                                                                               
REPDESCL DC    C'TERMINAL LIST'                                                 
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'TERMINAL LIST'                                           
         SPEC  H2,57,C'-------------'                                           
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
*                                  HEADINGS FOR LIST/SEL SCREEN                 
SOFTHEAD DC    CL(L'LSTHEAD)'Act  Terminal  Password    Systems AuthoriX        
               sed           ID List'                                           
HARDHEAD DC    CL(L'LSTHEAD)'Act  Terminal  Dev  Type  Cty  Ag Of Nde LX        
               ine CuDv  Stat    Application Ids'                               
SPACES   DC    40C' '                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,RA                                                            
                                                                                
***********************************************************************         
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                                
***********************************************************************         
GETSE    NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETSE,RB                                                         
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GSE'    INSERT NAME                                  
*                                                                               
         L     R3,ASYSFACS                                                      
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R3,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
                                                                                
*********************************************************************           
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R4=A(SCANNER BLOCK ENTRY)                                                     
*********************************************************************           
GETPRGX  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETPRGX,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GPR'    INSERT NAME                                  
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GETPRGX2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R4)                                                
         BE    *+16                                                             
         BXLE  R3,RE,GETPRGX2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     *+10                                                             
*                                                                               
         MVC   PROGRAM,PGMNUM                                                   
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
**********************************************************************          
GETSEN   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETSEN,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GSN'    INSERT NAME                                  
*                                                                               
GETSEN1  CLI   SYSNUMSC,0          TEST FIRST CALL - NUMBER OF ITEMS            
         BNE   GETSEN2                                                          
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
         MVC   SYSNAMS(8),=C'TERMINAL'                                          
         CLI   PASSWORD,0                                                       
         BE    *+10                                                             
         MVC   SYSNAMS(8),=C'PASSWORD'                                          
         NC    SYSNAMS+1(7),=8X'BF'                                             
         MVC   SYSNAMS+9(7),=C'SYSTEMS'                                         
         NC    SYSNAMS+10(6),=8X'BF'                                            
         LA    RE,SYSNAMS+17       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
*                                                                               
GETSEN2  L     R3,ASYSFACS         SEARCH SE LIST FOR SE NUM AT SYSNUMS         
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R3                                                       
         CLC   SYSNUMS,SEOVSYS                                                  
         BE    GETSEN3                                                          
         BXLE  R3,RE,*-10                                                       
         LA    R3,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
GETSEN3  L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYSNUMSC       TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYSNUMSC         BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    GETSENX                                                          
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
*                                                                               
GETSENX  XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
                                                                                
**********************************************************************          
* SET PROGRAM NAME FROM PROGRAM NUMBER                                          
**********************************************************************          
GETPRGN  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETPRGN,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GPN'    INSERT NAME                                  
*                                                                               
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
         CLC   PGMNUM,PROGRAM                                                   
         BE    *+16                                                             
         BXLE  R3,RE,*-10                                                       
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     *+10                                                             
*                                                                               
         MVC   PGNAME,PGMNAME                                                   
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
                                                                                
**********************************************************************          
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R4=A(SCANNER BLOCK ENTRY)                                                     
**********************************************************************          
GETPGAX  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETPGAX,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GPA'    INSERT NAME                                  
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GETPGAX2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R4)                                                
         BE    *+16                                                             
         BXLE  R3,RE,GETPGAX2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGAXX                                                         
         MVC   PROGRAM,PGMNUM                                                   
         CLI   PGMALNUM,0          USE ACCESS OVERRIDE IF SET                   
         BE    *+10                                                             
         MVC   PROGRAM,PGMALNUM                                                 
GETPGAXX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* BUILD PROGRAM ACCESS LIST INTO BLOCK1 AND SET PGCNT TO NUMBER OF              
* ENTRIES IN BLOCK.                                                             
**********************************************************************          
DISPSYS  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DISPSYS,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DSY'    INSERT NAME                                  
*                                                                               
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         L     R3,ASYSEL           GET A(PGMS) INTO APGM                        
         USING CTSYSEL,R3                                                       
         LA    R4,CTSYSPGM         R4=A(AUTHS)                                  
         ZIC   R9,CTSYSLEN         R9=PGM NUMBER                                
         CLI   CTSYSLEN,16         CHECK FOR ALL= VALUE ONLY                    
         BE    DISPSYS8                                                         
*                                                                               
DISPSYS2 CH    R9,=H'16'                                                        
         BNH   DISPSYS8                                                         
         MVC   PROGRAM,0(R4)                                                    
         LA    R4,1(R4)                                                         
         BAS   RE,GETPGAN          GET PROGRAM NAME                             
         CLI   PROGRAM,X'FF'       SET TO X'FF' IF N/F                          
         BE    DISPSYS6                                                         
*                                                                               
DISPSYS4 SR    R1,R1                                                            
         IC    R1,PGCNT            BUMP BLOCK COUNT                             
         LA    RE,1(R1)                                                         
         STC   RE,PGCNT                                                         
         MH    R1,=H'20'                                                        
         LA    R1,BLOCK1(R1)       GET A(BLOCK ENTRY)                           
         MVI   0(R1),C' '                                                       
         MVC   1(19,R1),0(R1)                                                   
         LR    R8,R1                                                            
         MVC   0(4,R8),PGNAME                                                   
         LA    R8,4(R8)                                                         
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C'='                                                       
         MVI   2(R8),C'Y'          AUTH IS Y N OR XXXX                          
         CLC   0(2,R4),=X'000F'                                                 
         BE    DISPSYS6                                                         
         MVI   2(R8),C'N'                                                       
         CLC   0(2,R4),=X'0000'                                                 
         BE    DISPSYS6                                                         
         GOTO1 VHEXOUT,APPARM,(R4),2(R8),2,=C'TOG'                              
*                                                                               
DISPSYS6 LA    R8,CTSYSALL         EXIT IF ALL=VALUE JUST DONE                  
         CR    R8,R4                                                            
         BE    DISPSYSX                                                         
         LA    R4,2(R4)                                                         
         SH    R9,=H'3'                                                         
         B     DISPSYS2                                                         
*                                                                               
DISPSYS8 LA    R4,CTSYSALL                                                      
         MVC   PGNAME,=CL8'ALL'                                                 
         B     DISPSYS4                                                         
*                                                                               
DISPSYSX XIT1                                                                   
         DROP  R3                                                               
                                                                                
**********************************************************************          
* SET PROGRAM NAME FROM PROGRAM ACCESS NUMBER                                   
**********************************************************************          
GETPGAN  NTR1                                                                   
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GETPGAN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPGANY                                                         
         CLI   PGMALNUM,0                                                       
         BE    *+14                                                             
         CLC   PGMALNUM,PROGRAM    OR ACCESS OVERRIDE (IF SET)                  
         BE    GETPGANY                                                         
         BXLE  R3,RE,GETPGAN2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGANX                                                         
GETPGANY MVC   PGNAME,PGMNAME                                                   
GETPGANX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CONVERT THE X'21' ELEMENT TO THE NEW FORMAT                                   
**********************************************************************          
CNVELM21 NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING CNVELM21,RB                                                      
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+CEL'    INSERT NAME                                  
*                                                                               
         MVC   BLOCK1(L'APELEM),APELEM  COPY THE ELEMENT OVER                   
         LA    R9,BLOCK1           POINT FROM WHERE TO COPY                     
         USING CTSYSD,R9                                                        
         LA    R5,APELEM+CTSYSPGM-CTSYSD  WHERE TO COPY                         
         LA    R6,1                FIRST PROGRAM                                
         LA    R0,16               LENGTH IS HEADER FIRST                       
         LA    R4,CTSYSPGM                                                      
CNV21LP  CLC   0(2,R4),CTSYSALL    DEFAULT?                                     
         BE    CNV21NX             YES, SKIP TO NEXT ONE                        
         STC   R6,0(R5)            STORE THE PROGRAM NUMBER                     
         MVC   1(2,R5),0(R4)       AND ITS AUTHORIZATION CODE                   
         AH    R0,=H'3'            LENGTH IS CHANGED BY 3                       
         LA    R5,3(R5)            NEXT POSTION FOR NEXT PROGRAM                
CNV21NX  LA    R6,1(R6)            NEXT PROGRAM NUMBER                          
         LA    R4,2(R4)            NEXT AUTHORIZATION CODE                      
         CH    R6,=H'64'           DID WE DO ALL 64 PROGRAMS?                   
         BNH   CNV21LP             NO, CONTINUE UNTIL WE'RE DONE                
         LA    R9,APELEM           STORE THE NEW LENGTH OF THE ELEMENT          
         STC   R0,CTSYSLEN                                                      
         XIT1                                                                   
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* BUILD TERMINAL INFO INTO BLOCK4 AND SET ATCNT TO NUMBER OF                    
* ENTRIES IN BLOCK.                                                             
**********************************************************************          
DISPTRM  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DISPTRM,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DTM'    INSERT NAME                                  
*                                                                               
         L     R3,ATRMEL           GET A(TERMINAL INFO ELEMENT)                 
         USING CTTRMD,R3                                                        
*                                                                               
         LARL  RE,DEVTBL           DISPLAY DEVICE                               
DTDEV1   CLI   0(RE),EOT           END OF LIST                                  
         BE    DTDEVX                                                           
         CLC   CTTRMDEV,8(RE)                                                   
         BE    *+12                                                             
         LA    RE,DEVTBLL(RE)                                                   
         B     DTDEV1                                                           
DTDEV2   MVC   TRMDEV(8),0(RE)     DISPLAY DEVICE NAME                          
DTDEVX   EQU   *                                                                
*                                                                               
         LARL  RE,TYPTBL           DISPLAY TYPE                                 
DTTYP1   CLI   0(RE),EOT           END OF LIST                                  
         BE    DTTYPX                                                           
         CLC   CTTRMTYP,8(RE)                                                   
         BE    *+12                                                             
         LA    RE,TYPTBLL(RE)                                                   
         B     DTTYP1                                                           
DTTYP2   MVC   TRMTYP(8),0(RE)     DISPLAY TYPE NAME                            
DTTYPX   EQU   *                                                                
*                                                                               
DTCTY    L     R1,ACTRY            DISPLAY COUNTRY                              
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTTRMCTY                                                
         BE    DTCTY1                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   TRMCTRY,=C'**?** '                                               
         B     DTCTYX                                                           
DTCTY1   MVC   TRMCTRY,CTRYSHR      DISPLAY COUNTRY SHORT NAME                  
DTCTYX   EQU   *                                                                
         DROP  R1                                                               
*                                                                               
DTAGY    MVC   TRMAGY,CTTRMAGY     DISPLAY AGENCY TWO CHR ALPHA ID              
*                                                                               
DTOFC    MVC   TRMOFC(1),CTTRMOFC  DISPLAY OFFICE CODE CHR                      
         CLI   TRMOFC,C'*'                                                      
         BNE   DTOFCX                                                           
         TM    CTTRMFL1,X'01'      SHOW *1 DDS LEVEL                            
         BZ    *+8                                                              
         MVI   TRMOFC+1,C'1'                                                    
         TM    CTTRMFL1,X'02'      SHOW *2 DDS LEVEL                            
         BZ    *+8                                                              
         MVI   TRMOFC+1,C'2'                                                    
DTOFCX   EQU   *                                                                
*                                                                               
DTNDE    SR    RF,RF               DISPLAY NODE NAME                            
         IC    RF,CTTRMNDE                                                      
         SLL   RF,2                                                             
         LARL  RE,NODETBL                                                       
         AR    RE,RF                                                            
         MVC   TRMNODE(3),0(RE)                                                 
         OI    TRMNODEH+6,X'80'                                                 
*                                                                               
DTLNE    MVC   TRMLINE(4),CTTRMLNE  DISPLAY LINE NAME                           
         OI    TRMLINEH+6,X'80'                                                 
*                                                                               
DTCUDV   OC    CTTRMCU(2),CTTRMCU  DISPLAY HEX CONTROL UNIT                     
         BZ    DTCUDVX                                                          
         GOTO1 VHEXOUT,APPARM,CTTRMCU,TRMCUDV,2                                 
         OI    TRMCUDVH+6,X'80'                                                 
DTCUDVX  EQU   *                                                                
*                                                                               
DTSTAT   MVC   APDUB(1),CTTSTAT    DISPLAY STATUS BYTE                          
         MVI   APDUB+1,0                                                        
         CLI   CTTKEY+6,C'P'       TEST PRINTER PASSIVE                         
         BNE   *+8                                                              
         OI    APDUB+1,X'01'       SET PRINTER PASSIVE                          
         LA    R0,100                                                           
         LA    RE,CTTDATA                                                       
DTSTAT1  CLI   0(RE),0             TEST END OF RECORD                           
         BE    DTSTAT5                                                          
DTSTAT2  CLI   0(RE),X'25'         TEST TERMINAL DEFN ELEMENET                  
         BNE   DTSTAT3                                                          
         CLI   CTTRMDEV-CTTRMD(RE),X'81'                                        
         BNE   *+8                                                              
         OI    APDUB+1,X'04'       SET PRINTER                                  
         CLI   CTTRMDEV-CTTRMD(RE),X'82'                                        
         BNE   *+8                                                              
         OI    APDUB+1,X'44'       SET PRINTER/SHUTTLE                          
         TM    CTTRMAT1-CTTRMD(RE),X'02'                                        
         BZ    DTSTAT4                                                          
         OI    APDUB+1,X'08'       SET AUTO MODE                                
         B     DTSTAT4                                                          
DTSTAT3  CLI   0(RE),X'29'         TEST PRINTERQ ELEMENT                        
         BNE   DTSTAT4                                                          
         OI    APDUB+1,X'10'       SET PRINTERQ ELEMENT PRESENT                 
         B     DTSTAT5                                                          
DTSTAT4  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         ICM   RF,1,1(RE)                                                       
         BZ    DTSTAT5                                                          
         AR    RE,RF                                                            
         BCT   R0,DTSTAT1                                                       
DTSTAT5  GOTO1 VHEXOUT,APPARM,APDUB,TRMSTAT,2                                   
         OI    TRMSTATH+6,X'80'                                                 
DTSTATX  EQU   *                                                                
*                                                                               
DTAT1    MVC   APDUB(1),CTTRMAT1   ATTRIBUTE BYTE ONE                           
         CLI   APDUB,0                                                          
         BE    DTAT1X                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(4,R4),=C'AT1='                                                 
         LA    R4,4(R4)                                                         
         BAS   RE,DTATB            AT1=ABCDEFGH                                 
DTAT1X   EQU   *                                                                
*                                                                               
DTAT2    MVC   APDUB(1),CTTRMAT2   ATTRIBUTE BYTE TWO                           
         CLI   APDUB,0                                                          
         BE    DTAT2X                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(4,R4),=C'AT2='                                                 
         LA    R4,4(R4)                                                         
         BAS   RE,DTATB            AT2=ABCDEFGH                                 
DTAT2X   EQU   *                                                                
*                                                                               
DTAT3    MVC   APDUB(1),CTTRMAT3   ATTRIBUTE BYTE TWO                           
         CLI   APDUB,0                                                          
         BE    DTAT3X                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(4,R4),=C'AT3='                                                 
         LA    R4,4(R4)                                                         
         BAS   RE,DTATB            AT2=ABCDEFGH                                 
DTAT3X   EQU   *                                                                
*                                                                               
DTESC    SR    R0,R0               ESCAPE SEQUENCE ????                         
         ICM   R0,1,CTTRMESC                                                    
         BZ    DTESCX                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(4,R4),=C'ESC='                                                 
         LA    R4,4(R4)                                                         
         EDIT  (R0),(3,(R4)),ALIGN=LEFT,WRK=APWORK                              
DTESCX   EQU   *                                                                
*                                                                               
DTLTY    MVC   APDUB(1),CTTRMLTY   LINE TYPE                                    
         CLI   APDUB,0                                                          
         BE    DTLTYX                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(6,R4),=C'LTYPE='                                               
         LA    R4,6(R4)                                                         
         MVC   0(1,R4),APDUB                                                    
DTLTYX   EQU   *                                                                
*                                                                               
DTLSP    MVC   APDUB(2),CTTRMLSP   LINE SPEED                                   
         SR    R0,R0                                                            
         ICM   R0,3,APDUB                                                       
         BZ    DTLSPX                                                           
         SLL   R0,3                CONVERT FROM CPS TO BAUD                     
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(7,R4),=C'LSPEED='                                              
         LA    R4,7(R4)                                                         
         EDIT  (R0),(5,(R4)),ALIGN=LEFT,WRK=APWORK                              
DTLSPX   EQU   *                                                                
         SPACE 2                                                                
DISPPRT  TM    CTTRMDEV,X'80'      TEST IF PRINTER DEVICE                       
         BZ    DISPTRMX                                                         
*                                                                               
DTPSP    MVC   APDUB(2),CTTRMPSP   PRINTER SPEED                                
         SR    R0,R0                                                            
         ICM   R0,3,APDUB                                                       
         BZ    DTPSPX                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(7,R4),=C'PSPEED='                                              
         LA    R4,7(R4)                                                         
         EDIT  (R0),(5,(R4)),ALIGN=LEFT,WRK=APWORK                              
DTPSPX   EQU   *                                                                
*                                                                               
DTPST    MVC   APDUB(1),CTTRMPST   PRINTER TYPE                                 
         CLI   APDUB,0                                                          
         BE    DTPSTX                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(6,R4),=C'PTYPE='                                               
         LA    R4,6(R4)                                                         
         MVC   0(1,R4),APDUB                                                    
DTPSTX   EQU   *                                                                
*                                                                               
DTPRQ    MVC   APDUB(1),CTTRMPRQ   PRINTER NUMBER OF QUEUE ENTRYS               
         SR    R0,R0                                                            
         ICM   R0,1,APDUB                                                       
         BZ    DTPRQX                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(7,R4),=C'PQUEUE='                                              
         LA    R4,7(R4)                                                         
         EDIT  (R0),(3,(R4)),ALIGN=LEFT,WRK=APWORK                              
DTPRQX   EQU   *                                                                
*                                                                               
DTPBS    MVC   APDUB(2),CTTRMPBS   PRINTER BUFFER SIZE                          
         SR    R0,R0                                                            
         ICM   R0,3,APDUB                                                       
         BZ    DTPBSX                                                           
         BAS   RE,DTNXT            SET R4 TO NEXT BLOCK ENTRY                   
         MVC   0(6,R4),=C'PBUFF='                                               
         LA    R4,6(R4)                                                         
         EDIT  (R0),(5,(R4)),ALIGN=LEFT,WRK=APWORK                              
DTPBSX   EQU   *                                                                
*                                                                               
         B     DISPTRMX                                                         
         SPACE 2                                                                
DTNXT    SR    R1,R1               BUMP TO NEXT OUTPUT BLOCK                    
         IC    R1,ATCNT                                                         
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,ATCNT                                                         
         MH    R4,=H'20'                                                        
         LA    R4,BLOCK4(R4)       R4=A(NEXT BLOCK ENTRY)                       
         MVI   0(R4),C' '                                                       
         MVC   1(19,R4),0(R4)      CLEAR BLOCK                                  
         BR    RE                                                               
         SPACE 2                                                                
DTATB    LA    R1,DTATBITS         BYTE IN DUB TO ATT A/B/C/D/E/F/G/H           
         LA    R0,8                                                             
DTATB1   MVC   APDUB+7(1),APDUB                                                 
         NC    APDUB+7(1),0(R1)                                                 
         BZ    *+14                                                             
         MVC   0(1,R4),8(R1)                                                    
         LA    R4,1(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DTATB1                                                        
         BR    RE                                                               
DTATBITS DC    X'8040201008040201',C'ABCDEFGH'                                  
*                                                                               
DISPTRMX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                            *          
**********************************************************************          
         USING LSTTERMH,R4                                                      
VALPARS  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALPARS,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VPA'    INSERT NAME                                  
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VPTRM    EQU   *                                                                
         GOTO1 AFVAL,LSTTERMH      STORE TERMINAL                               
         BNE   VPTRMX              (IF ENTERED)                                 
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VPTRM1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VPTRM2              FOR KEY COMPARE IN GETREC                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VPTRM1                                                        
VPTRM2   STC   RE,SELKEYCL                                                      
         MVC   SELTERM,FVIFLD                                                   
         MVC   SELTERML,FVILEN                                                  
         MVC   SELTRMSP,0(RF)                                                   
VPTRMX   EQU   *                                                                
*                                                                               
VPPWD    EQU   *                                                                
         GOTO1 AFVAL,LSTPWRDH      STORE PASSWORD                               
         BNE   VPPWDX              (IF ENTERED)                                 
         MVC   SELPWRD,FVIFLD                                                   
VPPWDX   EQU   *                                                                
*                                                                               
VPAPL    EQU   *                                                                
         GOTO1 AFVAL,LSTTAPLH      STORE APPLICATION ID                         
         BNE   VPAPLX              (IF ENTERED)                                 
         MVC   SELTAPL,FVIFLD                                                   
         MVC   SELTAPLL,FVILEN                                                  
VPAPLX   EQU   *                                                                
*                                                                               
VPSYS    EQU   *                   VALIDATE SYSTEM                              
         GOTO1 AFVAL,LSTSYSH                                                    
         BNE   VPSYSX                                                           
         OC    SELPWRD,SELPWRD     ONLY ALLOWED WITH PASSWORD                   
         BNZ   VPSYS2                                                           
VPSYS1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPSYS2   EQU   *                                                                
         L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(,RF)                                                        
         USING SYSLSTD,RF                                                       
         ZIC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
VPSYS3   CLI   SYSLNUM,0                                                        
         BE    VPSYS1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    VPSYS4                                                           
         LA    RF,SYSLLEN(,RF)                                                  
         B     VPSYS3                                                           
VPSYS4   MVC   SELSYS,SYSLNUM                                                   
VPSYSX   EQU   *                                                                
*                                                                               
VPDEV    EQU   *                                                                
         GOTO1 AFVAL,LSTDEVH       VALIDATE DEVICE                              
         BNE   VPDEVX              (IF ENTERED)                                 
         LARL  RE,DEVTBL                                                        
         ZIC   R1,FVILEN                                                        
         BCTR  R1,0                                                             
VPDEV1   CLI   0(RE),EOT                                                        
         BNE   VPDEV3                                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPDEV3   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         BE    VPDEV2                                                           
         LA    RE,DEVTBLL(,RE)                                                  
         B     VPDEV1                                                           
VPDEV2   MVC   SELDEV,8(RE)                                                     
VPDEVX   EQU   *                                                                
*                                                                               
VPTYP    EQU   *                                                                
         GOTO1 AFVAL,LSTTYPH       VALIDATE TYPE                                
         BNE   VPTYPX              (IF ENTERED)                                 
         LARL  RE,TYPTBL                                                        
         ZIC   R1,FVILEN                                                        
         BCTR  R1,0                                                             
VPTYP1   CLI   0(RE),EOT                                                        
         BNE   VPTYP3                                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPTYP3   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         BE    VPTYP2                                                           
         LA    RE,TYPTBLL(,RE)                                                  
         B     VPTYP1                                                           
VPTYP2   MVC   SELTYP,8(RE)                                                     
VPTYPX   EQU   *                                                                
*                                                                               
VPCTY    EQU   *                                                                
         GOTO1 AFVAL,LSTCTRYH      VALIDATE COUNTRY                             
         BNE   VPCTYX              (IF ENTERED)                                 
         L     R1,ACTRY                                                         
         LH    RE,0(,R1)                                                        
         L     RF,2(,R1)                                                        
         LA    R1,6(,R1)                                                        
         USING CTRYTABD,R1                                                      
         ZIC   R8,FVILEN                                                        
         SH    R8,=H'1'                                                         
VPCTY1   EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   CTRYSHR(0),FVIFLD                                                
         BE    VPCTY2                                                           
         BXLE  R1,RE,VPCTY1                                                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPCTY2   MVC   SELCTY,CTRYCODE                                                  
         CLI   SELCTY,0            (CANT SET TO ZERO COS                        
         BNE   VPCTYX                                                           
         MVI   SELCTY,X'FF'           THAT WOULD MEAN DONT FILTER)              
VPCTYX   EQU   *                                                                
         DROP  R1                                                               
*                                                                               
VPAGY    EQU   *                                                                
         GOTO1 AFVAL,LSTAGYH       STORE AGENCY                                 
         BNE   VPAGYX              (IF ENTERED)                                 
         MVC   SELAGY,FVIFLD                                                    
VPAGYX   EQU   *                                                                
*                                                                               
VPOFC    EQU   *                                                                
         GOTO1 AFVAL,LSTOFCH       STORE OFFICE CODE                            
         BNE   VPOFCX              (IF ENTERED)                                 
         MVC   SELOFC,FVIFLD                                                    
VPOFCX   EQU   *                                                                
*                                                                               
VPNODE   EQU   *                                                                
         GOTO1 AFVAL,LSTNODEH      STORE NODE                                   
         BNE   VPNODEX             (IF ENTERED)                                 
         LARL  RE,NODETBL          ????                                         
VPNODE1  CLI   0(RE),X'FF'                                                      
         BNE   VPNODE2                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPNODE2  CLC   0(3,RE),FVIFLD                                                   
         BE    VPNODE3                                                          
         LA    RE,4(,RE)                                                        
         B     VPNODE1                                                          
VPNODE3  MVC   SELNODE,3(RE)                                                    
VPNODEX  EQU   *                                                                
*                                                                               
VPLINE   EQU   *                                                                
         GOTO1 AFVAL,LSTLINEH      STORE LINE ID                                
         BNE   VPLINEX             (IF ENTERED)                                 
         MVC   SELLINE,FVIFLD                                                   
VPLINEX  EQU   *                                                                
*                                                                               
VPCUDV   EQU   *                                                                
         GOTO1 AFVAL,LSTCUDVH      STORE CU AND DV                              
         BNE   VPCUDVX                                                          
         GOTO1 VHEXIN,APPARM,FVIFLD,SELCUDV,4                                   
         OC    APPARM+8(4),APPARM+8                                             
         BNZ   VPCUDVX                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPCUDVX  EQU   *                                                                
*                                                                               
VPESC    EQU   *                                                                
         GOTO1 AFVAL,LSTESCH       STORE ESCAPE SEQUENCE NUMBER                 
         BNE   VPESCX                                                           
         GOTO1 =V(NUMVAL),APPARM,LSTESC,(X'01',0),RR=APRELO                     
         CLI   0(R1),X'FF'                                                      
         BE    VPES020                                                          
VPES010  L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    VPES020                                                          
         C     R1,=F'255'                                                       
         BH    VPES020                                                          
         STCM  R1,1,SELESC                                                      
         B     VPESCX                                                           
VPES020  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPESCX   EQU   *                                                                
*                                                                               
VPSTAT   EQU   *                                                                
         GOTO1 AFVAL,LSTSTATH      STATUS BYTE INFO                             
         BNE   VPSTATX                                                          
         GOTO1 VHEXIN,APPARM,FVIFLD,SELSTAT,4                                   
         OC    APPARM+8(4),APPARM+8                                             
         BNZ   VPSTATX                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         XIT1  ,                                                                
VPSTATX  EQU   *                                                                
*                                                                               
VALPARSX XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS       *          
**********************************************************************          
GETREC   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETREC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GRE'    INSERT NAME                                  
*                                                                               
         B     GETRECIO            PRESERVE VALUE OF R1 ON ENTRY                
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECX                                                          
         L     R2,AIOAREA1                                                      
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTTKTID-CTTKEY),CTTKEY                                  
         BNE   GETRECX                                                          
*                                                                               
GRSTATUS TM    SELDEV,X'80'        PRINTER PASSIVE                              
         BO    GRSTATUX                                                         
         TM    CTTSTAT,X'01'       MASTER RECORD ONLY                           
         BO    GETRECSQ                                                         
GRSTATUX EQU   *                                                                
*                                                                               
GRTRM    CLI   SELTRMSP,C' '      TERMINAL - FILTER ONLY IF IT                  
         BNH   GRTRMX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GRTRM1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTTKTID(0),SELTERM                                               
         BH    GETRECX             (NO MORE RELEVENT RECORDS)                   
GRTRM1   GOTO1 ATXTFLT,APPARM,(SELTERML,SELTERM),(8,CTTKTID)                    
         BNE   GETRECSQ                                                         
GRTRMX   EQU   *                                                                
*                                                                               
GRSYS    EQU   *                   FILTER ON SYSTEM                             
         OC    SELSYS,SELSYS                                                    
         BZ    GRSYSX                                                           
         LR    R3,R2                                                            
         MVI   APELEM,X'21'        GET SYSTEM ELEMS                             
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECSQ                                                         
         B     GRSYS3                                                           
GRSYS2   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECSQ                                                         
         CLI   0(R3),X'21'                                                      
         BNE   GRSYS2                                                           
         USING CTSYSD,R3                                                        
GRSYS3   CLC   CTSYSNUM,SELSYS                                                  
         BNE   GRSYS2                                                           
GRSYSX   EQU   *                                                                
*                                                                               
GRGTEL   OC    CTTKPASS,CTTKPASS   GET TERMINAL DEFINITION ELEM                 
         BZ    GRGTEL1             FROM NETWORK RECORD                          
         CLC   TRMDEFID,CTTKTID    OR CHECK SAVED EL                            
         BNE   GRGTEL2             IS FOR THIS TERMINAL                         
         LA    R3,TRMDEF                                                        
         B     GRGTELX                                                          
                                                                                
GRGTEL1  XC    APELEM,APELEM                                                    
         MVI   APELEM,X'25'                                                     
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         USING CTTRMD,R3                                                        
         BZ    GRGTEL2                                                          
         CLI   CTTRMLEN,32                                                      
         BNE   GRGTEL2             (WRONG EL, USE DEFAULT)                      
         MVC   TRMDEF,0(R3)        SAVE EL                                      
         MVC   TRMDEFID,CTTKTID    & ID                                         
         B     GRGTELX                                                          
                                                                                
GRGTEL2  LA    R3,TRMDEF           NO EL, SET UP DEFAULT                        
         XC    TRMDEF,TRMDEF                                                    
         MVI   CTTRMDEV,1                                                       
         MVI   CTTRMTYP,1                                                       
         MVI   CTTRMCTY,1                                                       
         MVC   TRMDEFID,CTTKTID                                                 
GRGTELX  EQU   *                                                                
*                                                                               
GRPWD    EQU   *                   PASSWORD                                     
         CLC   SELPWRD,=C'YES       '                                           
         BE    GRPWDX                                                           
         CLC   SELPWRD,CTTKPASS                                                 
         BNE   GETRECSQ                                                         
GRPWDX   EQU   *                                                                
*                                                                               
GRDEV    EQU   *                                                                
         OC    SELDEV,SELDEV       DEVICE FILTER                                
         BZ    GRDEVX                                                           
         CLC   SELDEV,CTTRMDEV                                                  
         BNE   GETRECSQ                                                         
GRDEVX   EQU   *                                                                
*                                                                               
GRTYP    EQU   *                                                                
         OC    SELTYP,SELTYP       TYPE FILTER                                  
         BZ    GRTYPX                                                           
         CLC   SELTYP,CTTRMTYP                                                  
         BNE   GETRECSQ                                                         
GRTYPX   EQU   *                                                                
*                                                                               
GRCTY    EQU   *                                                                
         OC    SELCTY,SELCTY       COUNTRY FILTER                               
         BZ    GRCTYX              (ZERO MEANS DONT FILTER,                     
         CLI   SELCTY,X'FF'           FF MEANS FILTER ON ZERO)                  
         BNE   GRCTY1                                                           
         CLI   CTTRMCTY,0                                                       
         BNE   GETRECSQ                                                         
         B     GRCTYX                                                           
GRCTY1   CLC   SELCTY,CTTRMCTY                                                  
         BNE   GETRECSQ                                                         
GRCTYX   EQU   *                                                                
*                                                                               
GRAGY    OC    SELAGY,SELAGY       AGENCY FILTER                                
         BZ    *+14                                                             
         CLC   SELAGY,CTTRMAGY                                                  
         BNE   GETRECSQ                                                         
GRAGYX   EQU   *                                                                
*                                                                               
GROFC    OC    SELOFC,SELOFC       OFFICE FILTER                                
         BZ    *+14                                                             
         CLC   SELOFC,CTTRMOFC                                                  
         BNE   GETRECSQ                                                         
GROFCX   EQU   *                                                                
*                                                                               
GRNODE   OC    SELNODE,SELNODE     NODE FILTER                                  
         BZ    *+14                                                             
         CLC   SELNODE,CTTRMNDE                                                 
         BNE   GETRECSQ                                                         
GRNODEX  EQU   *                                                                
*                                                                               
GRLINE   OC    SELLINE,SELLINE     LINE FILTER                                  
         BZ    *+14                                                             
         CLC   SELLINE,CTTRMLNE                                                 
         BNE   GETRECSQ                                                         
GRLINEX  EQU   *                                                                
*                                                                               
GRCU     OC    SELCU,SELCU         CU FILTER                                    
         BZ    *+14                                                             
         CLC   SELCU,CTTRMCU                                                    
         BNE   GETRECSQ                                                         
GRCUX    EQU   *                                                                
*                                                                               
GRDV     OC    SELDV,SELDV         DV FILTER                                    
         BZ    *+14                                                             
         CLC   SELDV,CTTRMDV                                                    
         BNE   GETRECSQ                                                         
GRDVX    EQU   *                                                                
*                                                                               
GRESC    OC    SELESC,SELESC       ESCAPE SEQUENCE NUMBER FILTER                
         BZ    GRESCX                                                           
         CLC   SELESC,CTTRMESC                                                  
         BNE   GETRECSQ                                                         
GRESCX   EQU   *                                                                
*                                                                               
GRSTAT   OC    SELSTAT,SELSTAT     STATUS BYTES FILTER                          
         BZ    GRSTATX                                                          
         MVC   APDUB(1),CTTSTAT                                                 
         MVI   APDUB+1,0                                                        
         NI    APDUB,255-X'82'     TURN OFF DELETED & X'26' EL BITS             
         CLI   CTTKEY+6,C'P'                                                    
         BNE   *+8                                                              
         OI    APDUB+1,X'01'                                                    
         LA    R0,100                                                           
         LA    RE,CTTDATA                                                       
GRSTAT1  CLI   0(RE),0             TEST END OF RECORD                           
         BE    GRSTAT5                                                          
GRSTAT2  CLI   0(RE),X'25'         TEST TERMINAL DEFN ELEMENET                  
         BNE   GRSTAT3                                                          
         CLI   CTTRMDEV-CTTRMD(RE),X'81'                                        
         BNE   *+8                                                              
         OI    APDUB+1,X'04'       SET PRINTER                                  
         CLI   CTTRMDEV-CTTRMD(RE),X'82'                                        
         BNE   *+8                                                              
         OI    APDUB+1,X'44'       SET PRINTER/SHUTTLE                          
         TM    CTTRMAT1-CTTRMD(RE),X'02'                                        
         BZ    GRSTAT4                                                          
         OI    APDUB+1,X'08'       SET AUTO MODE                                
         B     GRSTAT4                                                          
GRSTAT3  CLI   0(RE),X'29'         TEST PRINTERQ ELEMENT                        
         BNE   GRSTAT4                                                          
         OI    APDUB+1,X'10'       SET PRINTERQ ELEMENT PRESENT                 
         B     GRSTAT5                                                          
GRSTAT4  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         ICM   RF,1,1(RE)                                                       
         BZ    GRSTAT5                                                          
         AR    RE,RF                                                            
         BCT   R0,GRSTAT1                                                       
GRSTAT5  TM    SELSTAT+1,X'80'     TEST IF WANT EQUALITY                        
         BZ    GRSTAT6                                                          
         MVC   APDUB+4(2),SELSTAT                                               
         NI    APDUB+5,X'7F'       TURN OFF EQUALITY BIT                        
         CLC   APDUB+4(2),APDUB                                                 
         BNE   GETRECSQ                                                         
         B     GRSTATX                                                          
GRSTAT6  NC    APDUB(2),SELSTAT                                                 
         BZ    GETRECSQ                                                         
GRSTATX  EQU   *                                                                
*                                                                               
GRTAPL   EQU   *                   TERMINAL APPLICATION ID FILTER               
         OC    SELTAPL,SELTAPL                                                  
         BZ    GRTAPLX                                                          
         OC    CTTKPASS,CTTKPASS   (DATA IN NETWORK RECORD)                     
         BZ    GRTAPL1                                                          
         CLC   TAPLTRM,CTTKTID                                                  
         BNE   GETRECSQ                                                         
         B     GRTAPLX                                                          
GRTAPL1  XC    TAPLTRM,TAPLTRM     LOOK THRU APPL ID ELS                        
         MVC   TAPLIDS,=CL18' '                                                 
         LR    R3,R2                                                            
         MVI   APELEM,X'24'                                                     
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BNZ   GRTAPL2                                                          
         USING CTAPLD,R3                                                        
*&&UK*&& CLC   SELTAPL,=CL8'LADV1'  (IF NO ELEM TRY DEFAULT)                    
*&&US*&& CLC   SELTAPL,=CL8'ADV1'   (IF NO ELEM TRY DEFAULT)                    
         BNE   GETRECSQ                                                         
*&&UK*&& MVC   TAPLIDS,=CL8'LADV1'                                              
*&&US*&& MVC   TAPLIDS,=CL8'ADV1'                                               
         MVC   TAPLTRM,CTTKTID                                                  
         B     GRTAPLX                                                          
GRTAPL2  CLC   CTAPLID(3),=C'ALL'                                               
         BE    GRTAPLX                                                          
         ZIC   RF,CTAPLLEN         CHECK IF APPLID MATCHES FILTER               
         SH    RF,=H'4'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAPLIDS(0),CTAPLID                                               
GRTAPL3  CLC   SELTAPL,TAPLIDS                                                  
         BNE   GRTAPL4                                                          
         MVC   TAPLTRM,CTTKTID                                                  
         B     GRTAPLX                                                          
GRTAPL4  MVC   TAPLIDS,=CL18' '                                                 
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECSQ                                                         
         CLI   0(R3),X'24'                                                      
         BNE   GRTAPL4                                                          
         B     GRTAPL2                                                          
GRTAPLX  EQU   *                                                                
*                                                                               
GETRECX  XIT1  ,                                                                
*                                  RETURN WITH CC= IF REC IS OK                 
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF TERMINAL DATA                                       *         
***********************************************************************         
LINE     NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING LINE,RB                                                          
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+LIN'    INSERT NAME                                  
*                                                                               
         L     R2,AIOAREA1                                                      
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         MVC   LISTTERM,CTTKTID                                                 
         OC    SELPWRD,SELPWRD                                                  
         BZ    LNGTDEF             NETWORK INFO REQUIRED                        
*                                                                               
         MVC   LISTPWRD,CTTKPASS   PASSWORD                                     
*                                                                               
LNSYS    EQU   *                   SYSTEMS AUTHORISED                           
         LA    R8,APWORK                                                        
         MVI   0(R8),C' '                                                       
         MVC   1(63,R8),0(R8)                                                   
         LA    R3,CTTDATA                                                       
LNSYS1   CLI   0(R3),X'21'                                                      
         BE    LNSYS6                                                           
LNSYS2   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNSYS5                                                           
         B     LNSYS1                                                           
         USING CTSYSD,R3                                                        
LNSYS6   L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(,RF)                                                        
         USING SYSLSTD,RF                                                       
LNSYS3   CLI   SYSLNUM,0                                                        
         BE    LNSYS2                                                           
         CLC   SYSLNUM,CTSYSNUM                                                 
         BE    LNSYS4                                                           
         LA    RF,SYSLLEN(,RF)                                                  
         B     LNSYS3                                                           
LNSYS4   MVC   0(3,R8),SYSLNAME                                                 
         LA    R8,4(,R8)                                                        
         B     LNSYS2                                                           
         DROP  RF                                                               
LNSYS5   GOTO1 =V(SQUASHER),APPARM,APWORK,64,RR=RB                              
         GOTO1 =V(CHOPPER),APPARM,(64,APWORK),(27,LISTSYS),1,RR=RB              
LNSYSX   EQU   *                                                                
*                                                                               
LNIDS    EQU   *                   ID LIST                                      
         CLI   APACTN,ACTREP       EXTENDED LIST FOR REPORT ACTION              
         BE    LNRIDS                                                           
         LA    R8,APWORK                                                        
         MVI   0(R8),C' '                                                       
         MVC   1(79,R8),0(R8)                                                   
         LA    R9,7                                                             
         LR    R3,R2                                                            
         MVI   APELEM,X'1F'        GET PRINCIPAL ID ELEM                        
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNIDS1                                                           
         USING CTPIDD,R3                                                        
         MVC   0(10,R8),CTPID                                                   
         LA    R8,11(,R8)                                                       
LNIDS1   LR    R3,R2                                                            
         MVI   APELEM,X'20'        GET ORDINARY ID ELEMS                        
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNIDS3                                                           
         B     LNIDS4                                                           
LNIDS2   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNIDS3                                                           
         CLI   0(R3),X'20'                                                      
         BNE   LNIDS2                                                           
         USING CTIDD,R3                                                         
LNIDS4   MVC   0(10,R8),CTID                                                    
         LA    R8,11(,R8)                                                       
         BCT   R9,LNIDS2                                                        
LNIDS3   GOTO1 =V(SQUASHER),APPARM,APWORK,80,RR=RB                              
         GOTO1 =V(CHOPPER),APPARM,(80,APWORK),(22,LISTIDS),1,RR=RB              
         B     LINEX                                                            
*                                                                               
LNRIDS   EQU   *                   REPORT ID LIST                               
         LA    R8,LISTIDS                                                       
         LR    R3,R2                                                            
         MVI   APELEM,X'1F'        GET PRINCIPAL ID ELEM                        
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNRIDS1                                                          
         USING CTPIDD,R3                                                        
         MVC   0(10,R8),CTPID                                                   
         LA    R0,10                                                            
         CLI   0(R8),C' '                                                       
         BE    *+16                                                             
         LA    R8,1(R8)                                                         
         BCT   R0,*-12                                                          
         LA    R8,1(R8)                                                         
         MVI   0(R8),C','                                                       
         LA    R8,1(R8)                                                         
LNRIDS1  LR    R3,R2                                                            
         MVI   APELEM,X'20'        GET ORDINARY ID ELEMS                        
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNRIDSA                                                          
         B     LNRIDS4                                                          
LNRIDS2  ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNRIDSA                                                          
         CLI   0(R3),X'20'                                                      
         BNE   LNRIDS2                                                          
         USING CTIDD,R3                                                         
LNRIDS4  LR    RF,R8                                                            
         LA    RF,12(RF)                                                        
         SR    RF,R4                                                            
         LA    RE,L'REPPS                                                       
         CR    RF,RE                                                            
         BL    LNRIDS5                                                          
         LA    R4,L'REPPS(R4)                                                   
         LA    R8,LISTIDS                                                       
LNRIDS5  OC    CTID(2),CTID                                                     
         BZ    LNRIDS6                                                          
         MVC   0(10,R8),CTID                                                    
         B     LNRIDS7                                                          
LNRIDS6  MVI   0(R8),C'L'                                                       
         MVI   1(R8),C'='                                                       
         MVC   2(8,R8),CTID+2                                                   
LNRIDS7  LA    R0,10                                                            
         CLI   0(R8),C' '                                                       
         BE    *+16                                                             
         LA    R8,1(R8)                                                         
         BCT   R0,*-12                                                          
         LA    R8,1(R8)                                                         
         MVI   0(R8),C','                                                       
         LA    R8,1(R8)                                                         
         B     LNRIDS2                                                          
LNRIDSA  LA    RF,LISTIDS                                                       
         CR    R8,RF                                                            
         BE    LINEX                                                            
         BCTR  R8,0                                                             
         MVI   0(R8),C' '                                                       
         B     LINEX                                                            
*                                                                               
LNGTDEF  EQU   *                   * NETWORK INFO *                             
         LA    R3,TRMDEF           -> SAVED TERML DEFN EL                       
         USING CTTRMD,R3                                                        
*                                                                               
LNDEV    EQU   *                                                                
         LARL  RE,DEVTBL           GET DEVICE TEXT                              
LNDEV1   CLI   0(RE),EOT                                                        
         BE    LNDEVX                                                           
         CLC   CTTRMDEV,8(RE)                                                   
         BE    *+12                                                             
         LA    RE,DEVTBLL(,RE)                                                  
         B     LNDEV1                                                           
         MVC   LISTDEV,DEVTBLL(RE) SHOW SHORT NAME                              
LNDEVX   EQU   *                                                                
*                                                                               
LNTYP    EQU   *                                                                
         LARL  RE,TYPTBL           GET TYPE TEXT                                
LNTYP1   CLI   0(RE),EOT                                                        
         BE    LNTYPX                                                           
         CLC   CTTRMTYP,8(RE)                                                   
         BE    *+12                                                             
         LA    RE,TYPTBLL(,RE)                                                  
         B     LNTYP1                                                           
         MVC   LISTTYP,0(RE)                                                    
LNTYPX   EQU   *                                                                
*                                                                               
LNCTY    EQU   *                   COUNTRY                                      
         L     R1,ACTRY                                                         
         LH    RE,0(,R1)                                                        
         L     RF,2(,R1)                                                        
         LA    R1,6(,R1)                                                        
         USING CTRYTABD,R1                                                      
         CLC   CTRYCODE,CTTRMCTY                                                
         BE    LNCTY1                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   LISTCTRY,=C'*?*'                                                 
         B     LNCTYX                                                           
LNCTY1   EQU   *                                                                
         MVC   LISTCTRY,CTRYSHR                                                 
LNCTYX   EQU   *                                                                
         DROP  R1                                                               
*                                                                               
LNAGY    EQU   *                                                                
         MVC   LISTAGY,CTTRMAGY    AGENCY                                       
LNAGYX   EQU   *                                                                
*                                                                               
LNOFC    EQU   *                                                                
         MVC   LISTOFC+1(1),CTTRMOFC OFFICE CODE                                
         CLI   CTTRMOFC,C'*'                                                    
         BNE   LNOFCX                                                           
         MVC   LISTOFC(1),CTTRMOFC                                              
         MVI   LISTOFC+1,C' '                                                   
         TM    CTTRMFL1,X'01'                                                   
         BZ    *+8                                                              
         MVI   LISTOFC+1,C'1'                                                   
         TM    CTTRMFL1,X'02'                                                   
         BZ    *+8                                                              
         MVI   LISTOFC+1,C'2'                                                   
LNOFCX   EQU   *                                                                
*                                                                               
LNNODE   XR    RF,RF              NODE                                          
         IC    RF,CTTRMNDE                                                      
         SLL   RF,2                                                             
         LARL  RE,NODETBL          ???                                          
         AR    RE,RF                                                            
         MVC   LISTNDE,0(RE)                                                    
LNNODEX  EQU   *                                                                
*                                                                               
LNLINE   EQU   *                                                                
         MVC   LISTLINE,CTTRMLNE   LINE ID                                      
LNLINEX  EQU   *                                                                
*                                                                               
LNCUDV   EQU   *                                                                
         OC    CTTRMCU(2),CTTRMCU  CUDV                                         
         BZ    LNCUDVX                                                          
         GOTO1 VHEXOUT,APPARM,CTTRMCU,LISTCUDV,2                                
LNCUDVX  EQU   *                                                                
*                                                                               
LNSTAT   EQU   *                                                                
         MVC   APDUB(1),CTTSTAT    DISPLAY STATUS BYTE                          
         MVI   APDUB+1,0                                                        
         CLI   CTTKEY+6,C'P'       TEST PRINTER PASSIVE                         
         BNE   *+8                                                              
         OI    APDUB+1,X'01'                                                    
         LA    R0,100                                                           
         LA    RE,CTTDATA                                                       
LNSTAT1  CLI   0(RE),0             TEST END OF RECORD                           
         BE    LNSTAT5                                                          
LNSTAT2  CLI   0(RE),X'25'         TEST TERMINAL DEFN ELEMENET                  
         BNE   LNSTAT3                                                          
         CLI   CTTRMDEV-CTTRMD(RE),X'81'                                        
         BNE   *+8                                                              
         OI    APDUB+1,X'04'       SET PRINTER                                  
         CLI   CTTRMDEV-CTTRMD(RE),X'82'                                        
         BNE   *+8                                                              
         OI    APDUB+1,X'44'       SET PRINTER/SHUTTLE                          
         TM    CTTRMAT1-CTTRMD(RE),X'02'                                        
         BZ    LNSTAT4                                                          
         OI    APDUB+1,X'08'       SET AUTO MODE                                
         B     LNSTAT4                                                          
LNSTAT3  CLI   0(RE),X'29'         TEST PRINTERQ ELEMENT                        
         BNE   LNSTAT4                                                          
         OI    APDUB+1,X'10'       SET PRINTERQ ELEMENT PRESENT                 
         B     LNSTAT5                                                          
LNSTAT4  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         ICM   RF,1,1(RE)                                                       
         BZ    LNSTAT5                                                          
         AR    RE,RF                                                            
         BCT   R0,LNSTAT1                                                       
LNSTAT5  GOTO1 VHEXOUT,APPARM,APDUB,LISTSTAT,2                                  
         TM    APDUB,X'08'                                                      
         BZ    LNSTATX                                                          
         TM    APDUB+1,X'10'                                                    
         BO    LNSTATX                                                          
         MVI   LISTSTAT+4,C'*'     SHOW AUTO MODE WITH NO PRINTERQ EL           
LNSTATX  EQU   *                                                                
*                                                                               
LNTAPL   OC    CTTKPASS,CTTKPASS   TERM APPLIC ID                               
         BZ    LNTAPL1             IF NETWORK REC GET APPL ELS                  
         CLC   TAPLTRM,CTTKTID     ELSE CHECK SAVED IS FOR THIS TRM             
         BNE   LNTAPLX                                                          
         MVC   LISTAPL,TAPLIDS                                                  
         B     LNTAPLX                                                          
LNTAPL1  XC    TAPLTRM,TAPLTRM                                                  
         LA    R8,TAPLIDS                                                       
         MVC   TAPLIDS,=CL18' '                                                 
         LR    R3,R2                                                            
         MVI   APELEM,X'24'                                                     
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNTAPLX                                                          
         MVC   TAPLTRM,CTTKTID     YES, SAVE TERM ID                            
         B     LNTAPL4             & GO SAVE APPL                               
LNTAPL3  ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNTAPLX                                                          
         CLI   0(R3),X'24'                                                      
         BNE   LNTAPL3                                                          
         USING CTAPLD,R3                                                        
LNTAPL4  ZIC   RF,CTAPLLEN                                                      
         SH    RF,=H'4'            OVERHEAD + 1 FOR EX                          
         LA    RE,CTAPLID                                                       
*&&UK*&& CLC   0(4,RE),=C'LADV'                                                 
*&&US*&& CLC   0(3,RE),=C'ADV'                                                  
         BNE   LNTAPL5                                                          
*&&UK*&& LA    RE,4(,RE)           PT AT APPL# (IE: 1 OR 2)                     
*&&UK*&& SH    RF,=H'7'            FOR LADV + LEN OF ELEM WHITE SPC             
*&&US*&& LA    RE,3(,RE)           PT AT APPL# (IE: 1 OR 2)                     
*&&US*&& SH    RF,=H'6'            FOR ADV + LEN OF ELEM WHITE SPC              
LNTAPL5  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RE)       MOVE IN NEXT APPL                            
         CLC   TAPLIDS+9(2),=2C' ' IS THERE ROOM FOR IT?                        
         BE    LNTAPL6                                                          
         BCTR  R8,0                NO - INSERT + INSTEAD                        
         MVI   0(R8),C'+'                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R8),=CL18' '                                                 
         B     LNTAPLX                                                          
LNTAPL6  LA    R8,2(RF,R8)                                                      
         B     LNTAPL3                                                          
LNTAPLX  MVC   LISTAPL,TAPLIDS                                                  
*                                                                               
LINEX    XIT1  ,                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*    ROUTINE TO DISPLAY ATTRIBUTE BYTES                               *         
***********************************************************************         
DSPATB   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DSPATB,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DPB'    INSERT NAME                                  
*                                                                               
         TM    APDUB,X'80'       BYTE IN DUB TO ATT A A/B/C/D/E/F/G/H           
         BZ    *+12                                                             
         MVI   0(R4),C'A'                                                       
         LA    R4,1(R4)                                                         
         TM    APDUB,X'40'                                                      
         BZ    *+12                                                             
         MVI   0(R4),C'B'                                                       
         LA    R4,1(R4)                                                         
         TM    APDUB,X'20'                                                      
         BZ    *+12                                                             
         MVI   0(R4),C'C'                                                       
         LA    R4,1(R4)                                                         
         TM    APDUB,X'10'                                                      
         BZ    *+12                                                             
         MVI   0(R4),C'D'                                                       
         LA    R4,1(R4)                                                         
         TM    APDUB,X'08'                                                      
         BZ    *+12                                                             
         MVI   0(R4),C'E'                                                       
         LA    R4,1(R4)                                                         
         TM    APDUB,X'04'                                                      
         BZ    *+12                                                             
         MVI   0(R4),C'F'                                                       
         LA    R4,1(R4)                                                         
         TM    APDUB,X'02'                                                      
         BZ    *+12                                                             
         MVI   0(R4),C'G'                                                       
         LA    R4,1(R4)                                                         
         TM    APDUB,X'01'                                                      
         BZ    *+12                                                             
         MVI   0(R4),C'H'                                                       
         LA    R4,1(R4)                                                         
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* SUBROUTINE TO DELETE A RECORD WITH KEY=DELKEY                                 
***********************************************************************         
DLTREC   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DLTREC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DLR'    INSERT NAME                                  
*                                                                               
         L     R2,AIOAREA2         DELETE RECORD WITH KEY DELKEY                
         MVC   DELSAVE,APRECKEY                                                 
         MVC   IOKEY(L'DELKEY),DELKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    27(R2),X'80'        SET DELETE FLAG                              
         LA    R1,IOWRITE+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
DLTRECX  MVC   IOKEY(L'DELSAVE),DELSAVE                                         
         L     R2,AIOAREA1                                                      
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE ANY ASOCIATED PRINTER NAME RECORDS                           *         
* R2=A(PRINTER TERMINAL RECORD)                                       *         
***********************************************************************         
DELPNM   NTR1  BASE=*                                                           
         LA    R3,CTTDATA          READ PNAME XREF ELEMENTS IN RECORD           
*                                                                               
DEPN010  CLI   0(R3),0                                                          
         BE    DEPNOK                                                           
         CLI   0(R3),CTPNXELQ                                                   
         BE    DEPN030                                                          
DEPN020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DEPN010                                                          
*                                                                               
         USING CTPNXD,R3                                                        
DEPN030  EQU   *                                                                
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         USING CTPNREC,R2                                                       
         XC    CTPNKEY,CTPNKEY                                                  
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,CTPNXUID                                                
         MVC   CTPNKNAM,CTPNXNAM                                                
*                                                                               
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO ERROR                                     
         BE    DEPN040             OVERWRITE IF EXISTS                          
         TM    IOERR,IOEDEL                                                     
         BO    DEPN050                                                          
         TM    IOERR,IOERNF                                                     
         BO    DEPN050                                                          
         DC    H'00'                                                            
DEPN040  EQU   *                                                                
         GOTO1 ASETACT,CTPNREC                                                  
         OI    CTPNSTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DEPN050  EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
         B     DEPN020                                                          
*                                                                               
DEPNOK   SR    RC,RC                                                            
DEPNNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2,R3                                                            
         USING CTTREC,R2                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RESTORE ANY ASOCIATED PRINTER NAME RECORDS                          *         
* R2=A(PRINTER TERMINAL RECORD)                                       *         
***********************************************************************         
         DS    0D                                                               
RESPNM   NTR1  BASE=*                                                           
         LA    R3,CTTDATA          READ PNAME XREF ELEMENTS IN RECORD           
*                                                                               
REPN010  CLI   0(R3),0                                                          
         BE    REPNOK                                                           
         CLI   0(R3),CTPNXELQ                                                   
         BE    REPN030                                                          
         SR    RF,RF                                                            
REPN020  IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     REPN010                                                          
*                                                                               
         USING CTPNXD,R3                                                        
REPN030  EQU   *                                                                
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         USING CTPNREC,R2                                                       
         XC    CTPNKEY,CTPNKEY                                                  
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,CTPNXUID                                                
         MVC   CTPNKNAM,CTPNXNAM                                                
*                                                                               
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO ERROR                                     
         BE    REPN050                                                          
         TM    IOERR,IOEDEL                                                     
         BO    REPN040                                                          
         TM    IOERR,IOERNF                                                     
         BO    REPN050                                                          
         DC    H'00'                                                            
REPN040  EQU   *                                                                
         GOTO1 ASETACT,CTPNREC                                                  
         NI    CTPNSTAT,X'FF'-X'80'                                             
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
REPN050  EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
         B     REPN020                                                          
*                                                                               
REPNOK   SR    RC,RC                                                            
REPNNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2,R3                                                            
         USING CTTREC,R2                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COPY ANY EXISTING PRINTERQ SECONDARY PAGE RECORDS OVER FOR NEW LUID *         
* R2=A(PRINTER TERMINAL RECORD)                                       *         
***********************************************************************         
         DS    0D                                                               
CPYPQP   NTR1  BASE=*                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA1                                                      
         MVC   CTTKEY,CPYKEY                                                    
         MVC   CTTKPASS,=CL10' '                                                
         MVI   CTTKPASS,C'1'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
*                                                                               
CPQP010  EQU   *                                                                
         GOTO1 AIO,IOREAD+IOCONFIL+IO1                                          
         BNE   CPQP100                                                          
         MVC   CTTKTID,KEYSAVE+(CTTKTID-CTTKEY)                                 
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    CPQP020                                                          
         TM    IOERR,IOEDEL                                                     
         BO    CPQP020                                                          
         TM    IOERR,IOERNF                                                     
         BO    *+6                                                              
         DC    H'00'                                                            
         LA    R1,IOADD+IOCONFIL+IO1                                            
         B     *+8                                                              
*                                                                               
CPQP020  EQU   *                                                                
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVI   CTTKTID-1,C'P'                                                   
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    CPQP030                                                          
         TM    IOERR,IOEDEL                                                     
         BO    CPQP030                                                          
         TM    IOERR,IOERNF                                                     
         BO    *+6                                                              
         DC    H'00'                                                            
         LA    R1,IOADD+IOCONFIL+IO1                                            
         B     *+8                                                              
*                                                                               
CPQP030  EQU   *                                                                
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
         SR    RF,RF                                                            
         IC    RF,CTTKPASS                                                      
         MVC   CTTKEY,CPYKEY                                                    
         LA    RF,1(RF)                                                         
         STC   RF,CTTKPASS                                                      
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         B     CPQP010                                                          
*                                                                               
CPQP100  EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
         B     CPQPOK                                                           
*                                                                               
CPQPOK   SR    RC,RC                                                            
CPQPNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
         USING CTTREC,R2                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE ANY EXISTING PRINTERQ SECONDARY PAGE RECORDS                 *         
* R2=A(PRINTER TERMINAL RECORD)                                       *         
***********************************************************************         
         DS    0D                                                               
DELPQP   NTR1  BASE=*                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA1                                                      
         MVC   CTTKEY,APRECKEY                                                  
         MVC   CTTKPASS,=CL10' '                                                
         MVI   CTTKPASS,C'1'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
*                                                                               
DPQP010  EQU   *                                                                
         GOTO1 AIO,IORDUPD+IOCONFIL+IO1                                         
         BNE   DPQP100                                                          
         GOTO1 ASETACN,CTTREC                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    CTTSTAT,X'80'                                                    
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                  PROCESS ASSOCIATED PASSIVE RECORD            
         MVI   CTTKTID-1,C'P'        SET RECORD KEY/STATUS FOR PASSIVE          
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 ASETACN,CTTREC                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    CTTSTAT,X'80'                                                    
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
DPQP020  EQU   *                                                                
         SR    RF,RF                                                            
         IC    RF,CTTKPASS                                                      
         MVC   CTTKEY,APRECKEY                                                  
         LA    RF,1(RF)                                                         
         STC   RF,CTTKPASS                                                      
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         B     DPQP010                                                          
*                                                                               
DPQP100  EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
         MVI   IOERR,0                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DPQPOK                                                           
*                                                                               
DPQPOK   SR    RC,RC                                                            
DPQPNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
         USING CTTREC,R2                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RESTORE ANY EXISTING PRINTERQ SECONDARY PAGE RECORDS                *         
* R2=A(PRINTER TERMINAL RECORD)                                       *         
***********************************************************************         
         DS    0D                                                               
RESPQP   NTR1  BASE=*                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA1                                                      
         MVC   CTTKEY,APRECKEY                                                  
         MVC   CTTKPASS,=CL10' '                                                
         MVI   CTTKPASS,C'1'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
*                                                                               
RPQP010  EQU   *                                                                
         GOTO1 AIO,IORDUPD+IOCONFIL+IO1                                         
         BL    RPQP100                                                          
         BE    RPQP020                                                          
         TM    IOERR,IOERNF                                                     
         BNZ   RPQP100                                                          
         TM    IOERR,IOEDEL                                                     
         BZ    RPQP100                                                          
         CLC   IOKEYSAV(CTTKPASS-CTTKEY),CTTKEY                                 
         BNE   RPQP100                                                          
         GOTO1 ASETACN,CTTREC                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         NI    CTTSTAT,X'FF'-X'80'                                              
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                  PROCESS ASSOCIATED PASSIVE RECORD            
         MVI   CTTKTID-1,C'P'        SET RECORD KEY/STATUS FOR PASSIVE          
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BH    *+6                                                              
         DC    H'00'                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
         GOTO1 ASETACN,CTTREC                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         NI    CTTSTAT,X'FF'-X'80'                                              
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
RPQP020  EQU   *                                                                
         SR    RF,RF                                                            
         IC    RF,CTTKPASS                                                      
         MVC   CTTKEY,APRECKEY                                                  
         LA    RF,1(RF)                                                         
         STC   RF,CTTKPASS                                                      
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         B     RPQP010                                                          
*                                                                               
RPQP100  EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
         MVI   IOERR,0                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     RPQPOK                                                           
*                                                                               
RPQPOK   SR    RC,RC                                                            
RPQPNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
         USING CTTREC,R2                                                        
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* LU DEVICE/TYPE/NODE TABLE                                                     
**********************************************************************          
       ++INCLUDE CTREPLUTAB                                                     
*                                                                               
**********************************************************************          
* DSECT TO COVER TEMP W/S                                                       
**********************************************************************          
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
                                                                                
**********************************************************************          
* TWA AND SAVED STORAGE                                                         
**********************************************************************          
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGEND2D                                                       
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF2D                                                       
         ORG                                                                    
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB2D                                                       
         ORG   SAVOVER                                                          
*                                  WORKING STORAGE SAVED IN TWA0                
SAVKEY   DS    XL(L'CTTKEY)        SAVE LAST RECORD KEY FOR COPY)               
SAVSYS   DS    XL1                 SAVE LAST DISPLAYED SYSTEM#                  
SAVPROM  DS    XL1                 SAVE PROMPTED STATE                          
SAVCLRL  EQU   *-SAVOVER                                                        
                                                                                
**********************************************************************          
* LIST/SELECT LINE LAYOUT                                                       
**********************************************************************          
LISTD    DSECT                                                                  
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTTERM DS    CL8                                                              
         DS    CL2                                                              
LISTDEV  DS    CL3                                                              
         DS    CL2                                                              
LISTTYP  DS    CL4                                                              
         DS    CL2                                                              
LISTCTRY DS    CL3                                                              
         DS    CL2                                                              
LISTAGY  DS    CL2                                                              
         DS    CL1                                                              
LISTOFC  DS    CL2                                                              
         DS    CL1                                                              
LISTNDE  DS    CL3                                                              
         DS    CL1                                                              
LISTLINE DS    CL4                                                              
         DS    CL1                                                              
LISTCUDV DS    CL4                                                              
         DS    CL2                                                              
LISTSTAT DS    CL6                                                              
         DS    CL2                                                              
LISTAPL  DS    CL18                                                             
         ORG   LISTDEV                                                          
LISTPWRD DS    CL10                                                             
         DS    CL2                                                              
LISTSYS  DS    CL27                                                             
         DS    CL2                                                              
LISTIDS  DS    CL22                                                             
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
**********************************************************************          
* DSECT TO COVER LOCAL W/S                                                      
**********************************************************************          
LOCALD   DSECT                                                                  
DUB      DS    D                                                                
RETURN   DS    F                                                                
ATERMVAL DS    A                                                                
ASYSEL   DS    A                                                                
ATRMEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
AGIDBUF  DS    A                                                                
*                                                                               
SELDATA  DS    0XL(SELDATAX-SELTERM)                                            
SELTERM  DS    CL8                 TERMINAL                                     
SELTRMSP DS    CL1                 1ST SPECIAL CHAR                             
SELTERML DS    CL1                 (L'DATA ENTERED)                             
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELPWRD  DS    CL10                PASSWORD                                     
SELTAPL  DS    CL8                 TERMINAL APPLICATION ID                      
SELTAPLL DS    XL1                 (L'DATA ENTERED)                             
SELSYS   DS    CL7                 SYSTEM                                       
SELDEV   DS    XL1                 DEVICE                                       
SELTYP   DS    XL1                 TYPE                                         
SELCTY   DS    XL1                 COUNTRY CODE                                 
SELAGY   DS    CL2                 ALPHA AGENCY                                 
SELOFC   DS    CL1                 OFFICE CODE                                  
SELNODE  DS    CL1                 NODE                                         
SELLINE  DS    CL4                 LINE                                         
SELCUDV  DS    0CL2                CHANNEL-UNIT-DEVICE                          
SELCU    DS    XL1                                                              
SELDV    DS    XL1                                                              
SELESC   DS    XL1                 ESCAPE SEQUENCE                              
SELSTAT  DS    XL2                 STATUS BYTES                                 
SELDATAX EQU   *                                                                
*                                                                               
FLDCNT   DS    XL1                                                              
LINECNT  DS    XL1                                                              
TABLE    DS    8XL20                                                            
*                                                                               
TAPLTRM  DS    CL8                                                              
TAPLIDS  DS    CL18                                                             
TRMDEFID DS    CL8                                                              
TRMDEF   DS    CL32                                                             
*                                                                               
TERMID   DS    CL8                                                              
*                                                                               
SVTRMEL  DS    CL32                                                             
SVAGYAP  DS    CL2                                                              
SVAGYAI  DS    CL2                                                              
SVSTAT   DS    XL1                                                              
TERMINFO DS    XL1                                                              
TAPLCNT  DS    XL1                                                              
TAPLFLAG DS    XL1                                                              
TAPLFMUQ EQU   X'01'                                                            
TAPLFREQ EQU   X'02'                                                            
TVIDFLAG DS    XL1                                                              
TVIDALLQ EQU   X'01'               VALID ID = 'ALL'                             
TDEVFLAG DS    XL1                                                              
TDEVWEBQ EQU   X'01'               WEB TERMINAL                                 
PASSWORD DS    CL1                                                              
SVPSWRD  DS    CL10                                                             
SYSTEM   DS    CL1                                                              
PROGRAM  DS    CL1                                                              
PGNAME   DS    CL8                                                              
AGYID    DS    CL2                                                              
WILDCLEN DS    XL1                                                              
*                                                                               
XXCNT    DS    0C                                                               
IDCNT    DS    CL1                                                              
EXCNT    DS    CL1                                                              
PGCNT    DS    CL1                                                              
APCNT    DS    CL1                                                              
ATCNT    DS    CL1                                                              
XXCNTL   EQU   *-XXCNT                                                          
*                                                                               
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
SYSNUMSC DS    XL1                                                              
         DS    XL1                                                              
MASTERID DS    CL10                                                             
*                                                                               
FLAG     DS    XL1                                                              
*                                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLOCK3   DS    20CL32                                                           
BLOCK4   DS    20CL32                                                           
*                                                                               
DELKEY   DS    CL25                                                             
DELSAVE  DS    CL25                                                             
*                                                                               
CPYKEY   DS    XL(L'IOKEY)                                                      
KEYSAVE  DS    XL(L'IOKEY)                                                      
*                                                                               
LOCALX   EQU   *                                                                
*                                                                               
TENK     EQU   10*1024                                                          
EIGHTYK  EQU   80*1024                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTGEN0D   05/14/19'                                      
         END                                                                    
