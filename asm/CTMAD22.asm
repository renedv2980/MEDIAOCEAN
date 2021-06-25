*          DATA SET CTMAD22    AT LEVEL 052 AS OF 06/22/10                      
*PHASE TA0C22A                                                                  
*INCLUDE CONVMOS                                                                
         TITLE 'TA0C22 - $MAD PRESTO UPLOAD ORDER'                              
TA0C22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C22,RA,RR=R2                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         ST    R2,APRELO           SAVE RELOCATION FACTOR                       
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1  ,                                                                
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         GOTO1 SETSYS,DMCB,=C'ACCOUNT',=CL8'ACCDIR',=CL8'ACCMST'                
         BE    INIT05                                                           
* IF THERE WAS AN ERROR, CHECK IF ERROR SWITCHING SYSTEMS                       
* IF ERROR SWITCHING SYSTEMS, CHANGE TO FILE READ-ONLY                          
         CLC   MDACTION,=Y(ERSWITCH)                                            
         BNE   EXIT                                                             
         XC    MDACTION,MDACTION                                                
         B     ERRUPRO                                                          
*                                                                               
INIT05   MVI   LENKEY,L'ACCKEY                                                  
         MVI   DISPDSKA,ACCKDA-ACCRECD                                          
*                                  DON'T ALLOW UPLOAD TO READ ONLY FILE         
         GOTO1 GETFACT,DMCB,(X'80',BYTE),(SYSNUM,F#SEIND)                       
         TM    BYTE,SEIRONLY                                                    
         BO    ERRUPRO                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYB)   TODAY'S DATE                      
*                                                                               
* SET UP GLOBAL VALUES FOR THE COUNTRY/COMPANY/UNIT/LEDGER                      
*                                                                               
         L     RF,AUTL                                                          
         USING UTLD,RF                                                          
         MVC   CUL(1),TAGYB                                                     
         MVC   CTRY,TCTRY                                                       
         DROP  RF                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   CASHVAL,CCASHVAL                                                 
         MVC   GETPROF,CGETPROF                                                 
         DROP  RF                                                               
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES+0                                    
         MVI   FLAG1,0                                                          
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CPYRECD,R6                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUL         READ COMPANY RECORD                          
         GOTO1 HIGH                                                             
         CLC   CPYKEY(CPYKEND),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         LA    R3,CPYELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CPYELD,R6                                                        
         MVC   CUL+1(2),CPYPROD                                                 
         MVC   COMPALPH,CPYALPHA   AGENCY ALPHA                                 
         CLI   CPYLN,CPYLN3Q       TEST ELEMENT LONG ENOUGH                     
         BL    INIT10              TO HOLD CURRENCY VALUES                      
*                                                                               
*&&UK                                                                           
         MVC   COMPCUR,CPYCURR     PRIMARY AND SECONDARY CURRENCY               
         MVC   COMPCURS,CPYCURRS                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA9,CPYSTAT9                                                
*                                                                               
         XC    WORK,WORK           GET ORDER PROGRAM PROFILE                    
         MVC   WORK(4),=C'AAUF'                                                 
         CLI   CTRY,CTRYGERQ       TEST FOR GERMANY                             
         BE    *+10                YES                                          
         MVC   WORK(4),=C'AORD'    NO-CHANGE PROGRAM NAME                       
         NI    WORK,X'FF'-X'40'                                                 
         MVC   WORK+4(1),CUL       COMPANY CODE                                 
         MVC   WORK+12(L'COMPALPH),COMPALPH TWO BYTE AGENCY ALPHA               
         GOTO1 GETPROF,DMCB,(X'C0',WORK),PPROF1,DATAMGR                         
*&&                                                                             
         DROP  R6                                                               
*                                                                               
INIT10   GOTO1 SETHEIR                                                          
         BNE   ERROBJ                                                           
*                                                                               
         L     RE,=V(CONVMOS)                                                   
         A     RE,APRELO                                                        
         ST    RE,CONVMOS                                                       
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  EVERYTHING WILL HAPPEN HERE           
* BECAUSE WE NEED ONLY ONE TRANSACTION TO READ THE TEMPFILE AND WRITE           
* THE ORDER RECORD.                                                             
*                                                                               
PROCSTRT NTR1  ,                                                                
         GOTO1 WRKLOC              LOCATE RECENTLY UPLOADED TEMP FILE           
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,PROCTMP          PROCESS TEMP FILE OBJECTS                    
*                                                                               
*                                  PUT EOD OBJECT AS SIGN OF SUCCESS            
         GOTO1 PUTITEM,DMCB,A(ITEOD),0                                          
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT DOES NOTHING HERE.                    
*                                                                               
PROCMID  NTR1  ,                                                                
*                                                                               
PMX      B     XIT                                                              
         SPACE 3                                                                
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1  ,                                                                
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE TEMP FILE OBJECTS CREATING RECORDS.                
*                                                                               
PROCTMP  NTR1  ,                                                                
*                                  GET ORDER OBJECT FROM TEMP FILE              
         MVI   WRITORD,C'N'                                                     
         LA    RE,TMPAREA                                                       
         LA    RF,L'TMPAREA                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 WRKGET,DMCB,TMPHDR                                               
         CLC   TMPHDR(8),=C'*EOFEOF*'  IF END OF TEMP FILE THEN ERROR           
         BE    ERREOT                                                           
*                                  IF NOT ORDER HEADER THEN ERROR               
         CLC   TMPTYPE,=A(ITPROUPL)                                             
         BNE   ERRUOBJ                                                          
*                                                                               
         CLI   WRITORD,C'Y'        TEST PENDING ORDER WRITE                     
         BNE   *+8                 NO                                           
         BAS   RE,UPDATE           YES                                          
*                                                                               
         BAS   RE,INITORD          INITIALIZE FOR ORDER OBJECT HANDLING         
         BAS   RE,PROCORD          PROCESS ORDER HEADER OBJECT                  
         MVI   WRITORD,C'Y'        SET PENDING ORDER WRITE                      
         BAS   RE,BLDORD           BUILD ORDER RECORD IN IO1                    
*                                                                               
*                                  GET NEXT OBJECT FROM TEMP FILE               
PT10     LA    RE,TMPAREA                                                       
         LA    RF,L'TMPAREA                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         GOTO1 WRKGET,DMCB,TMPHDR                                               
         CLC   TMPHDR(8),=C'*EOFEOF*'  IF END OF TEMP FILE THEN ERROR           
         BE    ERREOT                                                           
*                                                                               
         CLC   TMPTYPE,=A(ITEOD)   IF END OF DATA OBJECT THEN DONE              
         BE    PT20                                                             
*                                                                               
         CLC   TMPTYPE,=A(ITPROTXT) IF ORDER TEXT OBJECT CALL PROCTXT           
         BNE   PT15                                                             
*&&UK*&& BAS   RE,PROCTXT                                                       
         B     PT10                                                             
*                                  IF ORDER LINE THEN CALL PROCORDL             
PT15     CLC   TMPTYPE,=A(ITPROLIN)                                             
         BNE   ERRUOBJ                                                          
         BAS   RE,PROCORDL                                                      
         B     PT10                                                             
*                                                                               
PT20     CLI   WRITORD,C'Y'                                                     
         BNE   PTX                                                              
*                                                                               
         OC    NORDLIN,NORDLIN     TEST FOR ANY ORDER LINE OBJECTS              
         BZ    ERRNOWC             NO-THIS IS A PROBLEM                         
*                                                                               
         BAS   RE,UPDATE                                                        
*                                                                               
PTX      B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE FOR A NEW ORDER OBJECT                              
*                                                                               
INITORD  NTR1  ,                                                                
*                                                                               
         LA    RE,OLDBUFF          CLEAR BUFFERS FOR OLD AND NEW                
         LA    RF,L'OLDBUFF        ORDER AMOUNT ELEMENTS                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   OLDLEN,=H'3'                                                     
*                                                                               
         LA    RE,NEWBUFF                                                       
         LA    RF,L'NEWBUFF                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   NEWLEN,=H'3'                                                     
*                                                                               
         MVI   COMMSTAT,0          ZERO EXTRA STATUS FLAGS FOR COMM WCS         
         MVI   NCOMSTAT,0          AND FOR NCOMMS                               
         XC    NORDLIN,NORDLIN     AND ORDERLINE OBJECT COUNTER                 
*                                                                               
INITORDX B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE ORDER HEADER OBJECT.                               
* AT ENTRY, TMPDATA CONTAINS THE OBJECT                                         
* ON EXIT, ELEMENT CONTAINS AN ORDER ELEMENT                                    
*                                                                               
PROCORD  NTR1  ,                                                                
         LA    R4,TMPDATA                                                       
         USING PROUOBJD,R4                                                      
         MVC   CURRENCY,PROUCUR    EXTRACT CURRENCY CODE                        
         MVC   SERVER,PROUSERV     EXTRACT 16 BYTE SQL SERVER                   
         L     R1,TMPHDR                                                        
         S     R1,=F'8'            COMPUTE OBJECT DATA LENGTH                   
         LA    RF,PROUSERV+L'PROUSERV-PROUOBJD                                  
         CR    R1,RF               TEST FOR 8 OR 16 BYTE SERVER                 
         BNL   PO05                                                             
         MVC   SERVER,SPACES       ITS 8                                        
         MVC   SERVER(8),PROUSERV                                               
*                                                                               
PO05     MVI   ACTION,CHAO                                                      
         GOTO1 GETORD,DMCB,PROUORDN                                             
         BE    PO10                                                             
         MVI   ACTION,ADDO                                                      
*&&US*&& MVC   SVTRSDT,TODAYB      YES, USE TODAY'S DATE                        
*                                                                               
PO10     LA    R6,ELEMENT                                                       
         USING ORDELD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         CLI   ACTION,ADDO         TEST ADDING ORDER                            
         BE    PO20                YES                                          
*                                                                               
PO15     LA    R3,ORDELQ                                                        
         MVC   AIO,AIO2            ORDER RECORD IS IN IO2                       
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,ORDLN            COPY EXISTING ELEMENT TO BUFFER              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),ORDELD                                                
         LA    R6,ELEMENT          RESTORE POINTER TO ELEMENT                   
         MVC   AIO,AIO1            AND RECORD                                   
*                                                                               
PO20     MVI   ORDEL,ORDELQ                                                     
*&&US*&& MVI   ORDLN,ORDLN2Q       DIFFERENT ELEMENT LENGTHS US/UK              
*&&UK*&& MVI   ORDLN,ORDLN3Q                                                    
*                                                                               
         CLC   PROUEXAC,SPACES     CHECK IF EXPENSE ACCOUNT IS SPACES           
         BNH   PO23                                                             
         OI    FLAG1,F1EXPORD      MUST BE AN EXPENSE ORDER                     
         CLC   PROUDOF(PROUXANL),SPACES  CHECK IF EXPENSE ANALYSIS              
         BNH   PO21                                                             
         OI    FLAG1,F1EXPANL      EXPENSE ANALYSIS USED                        
         MVC   EXPADOF,PROUDOF     DEBIT OFFICE                                 
         MVC   EXPACOF,PROUCOF     CREDIT OFFICE                                
         MVC   EXPAAOF,PROUAOF     ANALYSIS OFFICE                              
         MVC   EXPADEP,PROUDEP     DEPARTMENT                                   
         MVC   EXPAPER,PROUPER     PERSON                                       
*                                                                               
PO21     CLC   PROUCLI(PROUJOB-PROUCLI),SPACES CHECK IF ANAL BY CLT/PRD         
         BNH   PO23                                                             
         MVC   EXPACLI,PROUCLI                                                  
         MVC   EXPAPRO,PROUPRO                                                  
         OI    FLAG1,F1EXPANL      EXPENSE ANALYSIS USED                        
*                                                                               
PO23     OI    ORDSTAT,ORDSPRES    SET PRESTO ORDER FLAG                        
         TM    FLAG1,F1EXPANL      EXPENSE ANALYSIS USED?                       
         BNO   *+8                                                              
         OI    ORDSTAT,ORDSANL     SET ORDER IS ANALYZED FLAG                   
         GOTO1 HEXIN,DMCB,PROUSTAT,HALF,L'PROUSTAT                              
         MVC   STATUS,HALF+1       SAVE STATUS                                  
         CLI   STATUS,CLOSSTAT     TEST FOR CLOSURE                             
         BNE   PO25                                                             
*                                                                               
         CLI   ACTION,CHAO         TEST CHANGING EXISTING ORDER                 
         BE    *+12                                                             
         MVI   ACTION,ADDCLOSO     NO-SET TO ADD CLOSED ORDER                   
         B     PO25                                                             
*                                                                               
         MVI   ACTION,CLOSEO       SET ACTION TO CLOSE                          
*                                                                               
PO25     BAS   RE,VALJOB                                                        
         MVC   ORDSUPC,CUL                                                      
         MVC   ORDSUPU(L'ORDSUP-1),PROUSUPP                                     
         GOTO1 VALSUP,ORDSUP                                                    
         BNE   ERRSUP                                                           
         TM    FLAG1,F1EXPORD      IF EXPENSE ORDER, VALIDATE EXP ACCT          
         BNO   *+8                                                              
         BAS   RE,VALEXP                                                        
         MVC   ORDJOB,JOBKEY                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(0,PROUORDT),(1,ORDDATE)                             
         MVC   ORDAUTH,PROUAUTH    AUTHORIZOR                                   
*&&US                                                                           
         GOTO1 DATCON,DMCB,(0,PROUORDU),(1,ORDDDTE)                             
         MVC   ORDATTN,PROUATTN    ATTENTION                                    
*&&                                                                             
*&&UK*&& MVC   ORDREQD,PROUREQD    REQUIRED BY                                  
*                                                                               
         CLI   ACTION,CHAO         TEST CHANGING ORDER                          
         BNE   POX                 YES-ALL DONE                                 
*                                                                               
         ZIC   R1,ORDAMNO          UPDATE LAST AMENDMENT NUMBER/DATE            
         LA    R1,1(R1)                                                         
         STC   R1,ORDAMNO                                                       
         GOTO1 DATCON,DMCB,(5,0),(1,ORDAMDT)                                    
*                                                                               
POX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO BUILD ORDER RECORD KEY AND ORDER ELEMENT IN IO1                
*                                                                               
BLDORD   NTR1  ,                                                                
         L     R4,AIO1                                                          
         USING ORDRECD,R4                                                       
         LR    RE,R4                                                            
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         XC    ORDKEY,ORDKEY       BUILD KEY/ATTACH ORDER ELEMENT               
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUL                                                      
         MVC   ORDKORD,ORDNUM                                                   
         MVC   ORDRLEN,=Y(ORDRFST-ORDKEY+1)                                     
         CLI   STATUS,CLOSSTAT     TEST FOR STATUS=CLOSED                       
         BNE   *+8                                                              
*&&US*&& MVI   ORDRSTAT,ORDSFMCH   SET STATUS=CLOSED(FULLY MATCHED)             
*&&UK*&& MVI   ORDRSTAT,ORDSFMCH+ORDCLOSE FULLY MATCH + CLOSED IN ORD           
         MVI   ORDRSTA2,ORDSAPPR   ALL NON BRANDOCEAN ORDERS ARE FULLY          
*                                                     APPROVED                  
*                                                                               
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
*&&US                                                                           
* ADD STATUS ELEMENT WITH DATE ORDER ADDED IF WE HAVE IT                        
         OC    SVTRSDT,SVTRSDT                                                  
         BZ    BLDORD05                                                         
         USING TRSELD,R6           ADD ELEMENT WITH DATE ADDED                  
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSDATE,SVTRSDT                                                  
         GOTO1 ADDELEM,DMCB,TRSELD                                              
         DROP  R6                                                               
*                                                                               
BLDORD05 DS    0H                                                               
*&&                                                                             
*                                                                               
BLDORDX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS ORDER TEXT UPLOAD OBJECTS ADDING STANDARD              
* COMMENT ELEMENTS TO ORDER RECORD IN IO1                                       
*                                                                               
PROCTXT  NTR1  ,                                                                
         MVC   SAVEAIO,AIO                                                      
         MVC   AIO,AIO1                                                         
         LA    R4,TMPDATA                                                       
         USING PROTOBJD,R4                                                      
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          BUILD STANDARD COMMENT ELEMENT               
         USING SCMELD,R6                                                        
         MVI   SCMEL,SCMELQ                                                     
         GOTO1 HEXIN,DMCB,PROTSEQ,SCMSEQ,L'PROTSEQ                              
         MVC   SCMNARR(L'PROTEXT),PROTEXT                                       
*                                                                               
         LA    R1,L'PROTEXT                                                     
         LA    RE,SCMNARR+L'PROTEXT-1                                           
*                                                                               
PROCTXT2 CLI   0(RE),C' '          TEST FOR LAST SIGNIFICANT CHARACTER          
         BH    PROCTXT4                                                         
*                                                                               
         BCTR  RE,0                BACK UP ONE CHARACTER                        
         BCT   R1,PROCTXT2                                                      
*                                                                               
PROCTXT4 LTR   R1,R1               TEST FOR ZERO LENGTH                         
         BZ    PROCTXTX            NO-EXIT NOW                                  
*                                                                               
         LA    R1,SCMLN1Q(R1)      FORM ELEMENT LENGTH                          
         STC   R1,SCMLN                                                         
         GOTO1 ADDELEM,DMCB,SCMELD                                              
*                                                                               
PROCTXTX MVC   AIO,SAVEAIO                                                      
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO MANAGE THE UPDATEING OF AN ORDER, CONTRA-HEADER, AND           
* PSEUDO SJ TRANSACTION                                                         
*                                                                               
UPDATE   NTR1  ,                                                                
         CLI   ACTION,ADDO                                                      
         BE    UPDATE05                                                         
         CLI   ACTION,ADDCLOSO     TEST ADDING CLOSED ORDER                     
         BE    UPDATE05                                                         
*                                                                               
         BAS   RE,CHKWC            CHECK FOR WORKCODE CHANGES                   
*                                                                               
         CLI   INVOICED,C'Y'       TEST FOR INVOICED ORDER                      
         BNE   UPDATE05            NO                                           
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST CURRENCY CONVERTED AGY                  
         BZ    UPDATE05                                                         
         TM    OLDSTAT,ORDSSECC    TEST ORDERS WAS IN SECONDARY CURR            
         BZ    UPDATE02            NO                                           
*                                                                               
         CLI   SECCURR,C'Y'        YES-TEST STILL IN SECONDARY CURR             
         BE    UPDATE05            YES-OK                                       
         B     ERRCURR                                                          
*                                                                               
UPDATE02 CLI   SECCURR,C'N'        TEST ORDER STILL IN PRIMARY CURR             
         BNE   ERRCURR             NO-REJECT CHANGE                             
*&&                                                                             
*                                                                               
UPDATE05 BAS   RE,UPORD                                                         
         BAS   RE,ADDCON           ADD A CONTRA HEADER                          
*                                                                               
         CLI   ACTION,ADDO         TEST ADDING ORDER                            
         BE    *+12                YES                                          
         CLI   ACTION,ADDCLOSO     TEST ADDING CLOSED ORDER                     
         BNE   UPDATE10                                                         
         GOTO1 ADDSJ,DMCB,AIO1     ADD PSEUDO SJ TRANSACTION                    
         CLI   ACTION,ADDCLOSO                                                  
         BNE   UPDATE20                                                         
*                                                                               
UPDATE10 BAS   RE,UPDSJ            UPDATE THE SJ ITEM                           
*                                                                               
UPDATE20 MVI   WRITORD,C'N'        RESET WRITE FLAG                             
*                                                                               
UPDATEX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR WORKCODE CHANGES ON A CHANGED ORDER                  
*                                                                               
CHKWC    NTR1  ,                                                                
         MVI   INVOICED,C'N'       SET INVOICED FLAG                            
         SR    R3,R3               R3=ELEMENT COUNT                             
         LA    R6,OLDELS           R6=A(ORDER AMOUNT ELS)                       
         USING OAMELD,R6                                                        
         SR    R0,R0                                                            
*                                                                               
CHKWC2   CLI   OAMEL,OAMELQ                                                     
         BNE   CHKWC4                                                           
*                                                                               
         CP    OAMINUM,=P'0'       TEST FOR INVOICED ITEM                       
         BE    *+8                                                              
         MVI   INVOICED,C'Y'       YES                                          
         LA    R3,1(R3)            INCREMENT ELEMENT COUNT                      
         IC    R0,OAMLN                                                         
         AR    R6,R0               NEXT ELEMENT                                 
         B     CHKWC2                                                           
*                                                                               
CHKWC4   CLI   INVOICED,C'Y'       TEST ANY INVOICED WORKCODES                  
         BNE   CHKWCX              NO                                           
*                                                                               
         LA    R6,NEWELS           COUNT NUMBER OF NEW ELEMS                    
         SR    R2,R2                                                            
         SR    R0,R0                                                            
*                                                                               
CHKWC6   CLI   OAMEL,OAMELQ                                                     
         BNE   CHKWC8                                                           
*                                                                               
         LA    R2,1(R2)                                                         
         IC    R0,OAMLN                                                         
         AR    R6,R0                                                            
         B     CHKWC6                                                           
*                                                                               
CHKWC8   CR    R2,R3               TEST SAME NUMBER OF ELEMENTS                 
         BNE   ERRWORK             NO-STOP THIS CHANGE                          
         LA    R5,NEWELS           R5=NEW ELEMENTS                              
         LA    R6,OLDELS           R6=OLD ELEMENTS                              
*                                                                               
CHKWC10  CLC   OAMWORK,OAMWORK-OAMELD(R5) TEST SAME WORKCODE                    
         BNE   ERRWORK             NO-ITS AN ERROR                              
*                                                                               
         MVC   BYTE,OAMSTAT                                                     
         MVC   HALF(1),OAMSTAT-OAMELD(R5)                                       
         NI    BYTE,OAMSNOCM                                                    
         NI    HALF,OAMSNOCM                                                    
         CLC   BYTE,HALF           TEST SAME STATUS                             
         BNE   ERRWORK             NO-STOP ORDER UPDATE                         
*                                                                               
         ZIC   R1,OAMLN                                                         
         LA    R6,0(R1,R6)         NEXT OLD ELEMENT                             
         ZIC   R1,OAMLN-OAMELD(R5)                                              
         LA    R5,0(R1,R5)         NEXT NEW ELEMENT                             
         BCT   R2,CHKWC10                                                       
*                                                                               
CHKWCX   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* THIS ROUTINE EXTRACTS THE CLI/PRO/JOB CODES AND VALIDATES THE JOB             
* ROUTINE ALSO EXTRACTS THE CLI/PRO/JOB OFFICES TO FORM COMPOSITE               
* LATER                                                                         
*                                                                               
VALJOB   NTR1  ,                                                                
         LA    R4,TMPDATA                                                       
         USING PROUOBJD,R4                                                      
*                                                                               
         TM    FLAG1,F1EXPORD      MUST BE AN EXPENSE ORDER                     
         BO    VALJOBX                                                          
*                                                                               
         MVC   MYCLI,PROUCLI       EXTRACT CLI/PRO/JOB FROM MAD OBJECT          
         MVC   MYPRO,PROUPRO                                                    
         MVC   MYJOB,PROUJOB                                                    
         MVC   CLIOFF,SPACES                                                    
         MVC   PROOFF,SPACES                                                    
         MVC   JOBOFF,SPACES                                                    
*                                                                               
         LA    R2,BIGKEY                                                        
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(3),CUL      CUL                                          
*                                                                               
         LA    R3,ACTKACT          ADD CLI/PRO/JOB TO KEY                       
         ZIC   RF,LCLI                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),MYCLI                                                    
*                                                                               
         GOTO1 HIGH                READ THE CLIENT                              
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   ERRJOKY                                                          
*                                                                               
         MVC   AIO,AIO2            USE IO2 FOR CLI/PRO/JOB                      
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 GETELEM,DMCB,RSTELQ,0                                            
         BNE   VALJOB05                                                         
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIL   TEST FOR LOCKED CLIENT                       
*        BNZ   ERRJOCL             COMMENTED OUT                                
*                                                                               
VALJOB05 GOTO1 GETELEM,DMCB,PPRELQ,0                                            
         BNE   VALJOB10                                                         
         USING PPRELD,R6                                                        
         MVC   CLIOFF,PPRGAOFF     GET THE CLIENT OFFICE                        
         OC    CLIOFF,SPACES       SPACE PAD THE FIELD                          
*                                                                               
VALJOB10 ZIC   RF,LCLI                                                          
         LA    R3,0(R3,RF)                                                      
         ZIC   RF,LPRO                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),MYPRO                                                    
*                                                                               
         GOTO1 HIGH                READ THE PRODUCT                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   ERRJOKY                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 GETELEM,DMCB,RSTELQ,0                                            
         BNE   VALJOB15                                                         
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIL   TEST FOR LOCKED PRODUCT                      
*        BNZ   ERRJOCL             COMMENTED OUT                                
*                                                                               
VALJOB15 GOTO1 GETELEM,DMCB,PPRELQ,0                                            
         BNE   VALJOB20                                                         
         USING PPRELD,R6                                                        
         MVC   PROOFF,PPRGAOFF     GET THE PRODUCT OFFICE                       
         OC    PROOFF,SPACES       SPACE PAD THE FIELD                          
*                                                                               
VALJOB20 ZIC   RF,LPRO                                                          
         LA    R3,0(R3,RF)                                                      
         ZIC   RF,LJOB                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),MYJOB                                                    
*                                                                               
         GOTO1 HIGH                MAKE SURE VALID JOB                          
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   ERRJOKY                                                          
         MVC   JOBKEY,ACTKEY                                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 GETELEM,DMCB,PPRELQ,0                                            
         BNE   VALJOB30                                                         
         MVC   JOBOFF,PPRGAOFF     GET THE JOB OFFICE                           
         OC    JOBOFF,SPACES                                                    
*                                                                               
VALJOB30 LA    R3,RSTELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         TM    RSTSTAT,RSTSACIC+RSTSACIL TEST EITHER CLOSED OR LOCKED           
         BNZ   ERRJOCL                                                          
*                                                                               
         MVC   AIO,AIO1            RESTORE IO POINTER                           
*                                                                               
VALJOBX  B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO VALIDATE THE EXPENSE ACCOUNT                                   
*                                                                               
VALEXP   NTR1  ,                                                                
         LA    R4,TMPDATA                                                       
         USING PROUOBJD,R4                                                      
*                                                                               
         CLC   =C'SE',PROUEXAC                                                  
         BE    VALEXP10                                                         
         CLC   =C'SA',PROUEXAC                                                  
         BE    VALEXP10                                                         
         CLC   =C'SB',PROUEXAC                                                  
         BNE   ERRJOKY                                                          
*                                                                               
VALEXP10 LA    R6,BIGKEY                                                        
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKULA,PROUEXAC                                                 
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE      TEST FOR RECORD FOUND                        
         BNE   ERRJOKY                                                          
         MVC   JOBKEY,ACTKEY                                                    
*                                                                               
         MVC   AIO,AIO2            READ SUPPLIER INTO IO2                       
         GOTO1 GETREC                                                           
*                                                                               
         LA    R3,ABLELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BNE   ERRBALE                                                          
*                                                                               
         LA    R3,RSTELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIL                                                
         BO    ERRJOCL                                                          
*                                                                               
         MVC   AIO,AIO1            RESTORE IO POINTER                           
*                                                                               
VALEXPX  B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO VALIDATE THE SUPPLIER AND TO EXTRACT SUPPLIER INFO             
* ON ENTRY, R1=A(SUPPLIER ACCOUNT CODE)                                         
* ON EXIT, CC=EQ IF FOUND, CC=NEQ IF RNF                                        
*                                                                               
VALSUP   NTR1  ,                                                                
         LA    R6,BIGKEY                                                        
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,0(R1)                                                   
         MVC   SUPCODE,ACTKCULA                                                 
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE      TEST FOR RECORD FOUND                        
         BNE   NO                  NO-EXIT RIGHT NOW                            
*                                                                               
         MVC   AIO,AIO2            READ SUPPLIER INTO IO2                       
         GOTO1 GETREC                                                           
*                                                                               
VALSUP10 LA    R3,NAMELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NAMELD,R6                                                        
         MVC   SUPNAME,SPACES                                                   
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SUPNAME(0),NAMEREC                                               
*                                                                               
VALSUP20 LA    R3,RSTELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         MVC   SUPSTAT,RSTSTAT                                                  
*                                                                               
         MVC   AIO,AIO1            RESTORE IO POINTER                           
*                                                                               
VALSUPX  B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET AN ORDER IF ITS ON FILE                                    
* ON ENTRY, P1=A(ORDER NUMBER)                                                  
* ON EXIT, CC=EQ IF RECORD FOUND AND AIO2=A(ORDER), CC=NEQ FOR RNF              
*                                                                               
GETORD   NTR1  ,                                                                
         L     R3,0(R1)            GET A(ORDER NUMBER)                          
         LA    R2,BIGKEY                                                        
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUL                                                      
         MVC   ORDKORD,0(R3)                                                    
         MVC   ORDNUM,0(R3)        ALSO SAVE IT IN STORAGE                      
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
GETORD2  CLC   ORDKEY,KEYSAVE      TEST IF RECORD FOUND                         
         BNE   NO                  NO-EXIT RIGHT NOW                            
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO              POINT R3 AT RECORD                           
         TM    ORDRSTAT-ORDRECD(R3),ORDSFMCH   TEST FOR CLOSED ORDER            
         BO    ERRCLOSE            YES-REJECT THIS UPLOAD                       
*                                                                               
         MVC   OLDSTAT,ORDRSTAT-ORDRECD(R3)  SAVE OLD STATUS                    
*                                                                               
* SAVE ORDER AMOUNT ELEMENTS IN OLDBUFF                                         
*                                                                               
         LA    R3,OAMELQ           ORDER AMOUNT ELEMENT                         
         GOTO1 GETELEM,DUB,(R3),0                                               
*                                                                               
GETORD4  BNE   GETORD6             ALL DONE                                     
         GOTO1 PUTBUFF,DMCB,('OLD',(R6))                                        
         GOTO1 NEXTELEM,DUB,(R3),0                                              
         B     GETORD4                                                          
*                                                                               
GETORD6  DS    0H                                                               
*&&US                                                                           
* SAVE ORDER ADDED DATE, IF THERE IS ONE                                        
*                                                                               
         XC    SVTRSDT,SVTRSDT                                                  
         TM    OLDSTAT,ORDSLDEL    TEST IF RE-ADDING DELETED ORDER              
         BNO   GETORD8                                                          
         MVC   SVTRSDT,TODAYB      YES, THEN USE TODAY'S DATE                   
         B     GETORD10                                                         
*                                                                               
         USING TRSELD,R6           OTHERWISE, CHECK IF ONE IS THERE             
GETORD8  GOTO1 GETELEM,DUB,TRSELQ,0      STATUS ELEMENT                         
         BNE   GETORD10                                                         
         MVC   SVTRSDT,TRSDATE     SAVE DATE ORDER ADDED                        
         DROP  R6                                                               
*&&                                                                             
*                                                                               
GETORD10 MVC   AIO,AIO1            RESTORE IO POINTER                           
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE FLAG                
*                                                                               
GETORDX  B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE ORDER LINE OBJECT.  NOTE-MINOR STATUS              
* BIT IS NEEDED FOR PROPER DISPLAY IN INPUT PROGRAM ORDER RECALL(US)            
*                                                                               
PROCORDL NTR1  ,                                                                
         LA    R4,TMPDATA                                                       
         USING PRULOBJD,R4                                                      
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING OAMELD,R6                                                        
         MVI   OAMEL,OAMELQ                                                     
         MVI   OAMLN,OAMLN2Q                                                    
*                                                                               
         LA    RE,COMMSTAT         RE=A(EXTRA STATUS FLAGS)                     
         CLI   PRULFLAG,C'N'       TEST ORDER LINE IS NON-COMM                  
         BNE   POL05                                                            
         OI    OAMSTAT,OAMSNOCM    YES-SET FLAG ACCORDINGLY                     
         LA    RE,NCOMSTAT                                                      
*                                                                               
POL05    OC    OAMSTAT,0(RE)       SET OTHER STATUS FLAGS ON                    
*&&US*&& OI    0(RE),OAMSMAMI      SET MINOR FLAG FOR 2ND-NTH WC                
         MVC   OAMWORK,PRULWC      WORKCODE                                     
         GOTO1 HEXIN,DMCB,PRULAMNT,FULL,L'PRULAMNT                              
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         ZAP   OAMAMNT,DUB         ORDER LINE AMOUNT                            
*                                                                               
         ZAP   OAMINUM,=P'0'       INITIALIZE INVOICED DATA                     
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,=P'0'                                                    
*                                                                               
         MVI   SECCURR,C'N'        SET SECONDARY CURRENCY FLAG TO NO            
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST CURRENCY CONVERTED AGENCY               
         BZ    POL10               NO                                           
*                                                                               
* CALL CASHVAL TO DEAL WITH CURRENCY CONVERSION                                 
*                                                                               
         CLC   CURRENCY,COMPCURS   TEST FOR SECONDARY CURRENCY                  
         BNE   *+8                 NO-PRIMARY WAS INPUT                         
         MVI   SECCURR,C'Y'        YES-SECONDARY WAS INPUT                      
*                                                                               
         MVC   WORK+0(L'CURRENCY),CURRENCY  INPUT CURRENCY                      
         MVC   WORK+3(L'COMPCUR),COMPCURS   SECONDARY CURRENCY                  
         CLI   SECCURR,C'N'                 TEST INPUT=PRIMARY                  
         BE    *+10                         YES                                 
         MVC   WORK+3(L'COMPCUR),COMPCUR    NO-CONVERT TO PRIMARY               
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                    
         ZAP   PRIMAMT,DUB         SET PRIMARY CURRENCY = INPUT AMT             
         ZAP   SECAMT,DMCB+12(8)   SET SEC CURRENCY = CONVERSION                
         CLI   SECCURR,C'N'        TEST INPUT CURRENCY = PRIMARY                
         BE    POL08                                                            
*                                                                               
         ZAP   PRIMAMT,DMCB+12(8)  NO-SWITCH THE AMOUNTS                        
         ZAP   SECAMT,DUB                                                       
*                                                                               
POL08    ZAP   OAMAMNT,PRIMAMT                                                  
*&&                                                                             
*                                                                               
POL10    CLI   ACTION,ADDO         TEST ADDING ORDER                            
         BE    POL20               YES-DON'T NEED TO MERGE DATA                 
         CLI   ACTION,ADDCLOSO                                                  
         BE    POL20                                                            
*                                                                               
         GOTO1 GETOAM,DMCB,OAMWORK                                              
         BNE   POL20                                                            
*                                                                               
* COPY INVOICE VALUES FROM OLD ELEMENT                                          
*                                                                               
         L     R5,0(R1)            GET A(OLD ORDER AMOUNT ELEMENT)              
         MVC   OAMINUM,OAMINUM-OAMELD(R5)                                       
         MVC   OAMIVAL,OAMIVAL-OAMELD(R5)                                       
         MVC   OAMTVAL,OAMTVAL-OAMELD(R5)                                       
         MVC   OAMLAST,OAMLAST-OAMELD(R5)                                       
*                                                                               
POL20    GOTO1 PUTBUFF,DMCB,('NEW',OAMELD)                                      
         L     R1,NORDLIN                                                       
         LA    R1,1(R1)            INCREMENT ORDER LINE OBJECT COUNTER          
         ST    R1,NORDLIN                                                       
*                                                                               
POLX     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO SEARCH FOR AN OLD ORDER AMOUNT ELEMENT                         
*                                                                               
* AT ENTRY, P1=A(WORKCODE)                                                      
* ON EXIT, CC=EQ IF FOUND, CC=NEQ IF NOT FOUND, P1=A(ELEMENT)                   
*                                                                               
GETOAM   NTR1  ,                                                                
         LR    R3,R1               SAVE A(PARM LIST)                            
         L     R4,0(R1)            R4=A(WORKCODE)                               
         SR    R0,R0               USE R0 FOR EL LENGTHS                        
         LA    R6,OLDELS           R6=A(ORDER AMOUNT ELEMENTS)                  
         USING OAMELD,R6                                                        
*                                                                               
GETOAM2  CLI   OAMEL,OAMELQ        TEST FOR ORDER AMOUNT ELEMENT                
         BNE   GETOAMN             NO-AT EOT                                    
         CLC   OAMWORK,0(R4)       MATCH ON WORKCODE                            
         BE    GETOAM10            YES                                          
         IC    R0,OAMLN                                                         
         AR    R6,R0                                                            
         B     GETOAM2             NEXT ELEMENT                                 
*                                                                               
GETOAM10 ST    R6,0(R3)            SAVE ELEMENT ADDRESS                         
         B     GETOAMY                                                          
*                                                                               
GETOAMN  XC    0(4,R3),0(R3)                                                    
         B     NO                  EXIT WITH CC=NEQ                             
*                                                                               
GETOAMY  B     YES                                                              
         DROP  R6                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PUT AN ORDER AMOUNT ELEMENT INTO BUFFER                        
* AT ENTRY, P1 BYTE 0 = OLD OR NEW ELEMENTS                                     
*           P1 BYTES(1-3) = A(ELEMENT)                                          
*                                                                               
PUTBUFF  NTR1  ,                                                                
         L     R6,0(R1)            R6=A(ELEMENT)                                
         LA    R6,0(R6)            CLEAR HOB                                    
         LA    R3,OLDBUFF          R3=BUFFER ADDRESS                            
         CLI   0(R1),OLD           TEST FOR OLD BUFFER                          
         BE    *+8                                                              
         LA    R3,NEWBUFF                                                       
         GOTO1 HELLO,DMCB,(C'P',CORETAB),(R3),(R6),=C'ADD=END'                  
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE THE ORDER RECORD                                        
* AT ENTRY, ACTION=ADD,ADD CLOSED, CHANGE OR CLOSE EXISTING ORDER               
*           AIO1=A(ORDER KEY PLUS ORDER ELEMENT)                                
*                                                                               
UPORD    NTR1  ,                                                                
         L     R4,AIO1                                                          
         GOTO1 UPDOAM,DMCB,(R4)                                                 
         GOTO1 UPDXPAN,DMCB,(R4)                                                
         BAS   RE,UPDSERV                                                       
*&&UK*&& GOTO1 UPDCUR,DMCB,(R4)                                                 
                                                                                
         USING CPTRBLK,XTCPTRBK                                                 
         CLI   ACTION,ADDO                                                      
         BE    UPORD15                                                          
         CLI   ACTION,ADDCLOSO                                                  
         BE    UPORD15                                                          
                                                                                
         LA    R2,BIGKEY                                                        
         USING ORDRECD,R2                                                       
         MVC   ORDKEY,0(R4)        SET KEY FROM RECORD                          
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   ORDKEY,KEYSAVE      TEST IF RECORD FOUND                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,ORDKDA                                                      
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'D',AIO2),(C'K',CPTRBLK),SVDA,0,ACOMFACS          
                                                                                
UPORD10  MVC   AIO,AIO1            RESTORE IO AREA POINTER                      
         GOTO1 PUTREC                                                           
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,SVDA,0,ACOMFACS                 
         CLC   OLDSTAT,ORDRSTAT-ORDRECD(R4) TEST CHANGE IN STATUS               
         BE    UPORD30             NO-NO NEED TO UPDATE POINTER                 
                                                                                
         MVC   ORDKSTAT,ORDRSTAT-ORDRECD(R4)   UPDATE KEY STATUS                
         GOTO1 WRITE                                                            
         B     UPORD30                                                          
                                                                                
UPORD15  GOTO1 ADDREC                                                           
         MVC   SVDA,KEY                                                         
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,SVDA,0,ACOMFACS                 
         B     UPORDX                                                           
                                                                                
UPORD30  MVI   RDUPDATE,C'N'                                                    
*                                                                               
UPORDX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A CONTRA-ACCOUNT HEADER AND ADD IT TO THE FILE.          
*                                                                               
ADDCON   NTR1  ,                                                                
         LA    R3,ORDELQ           GET ORDER HEADER ELEMENT                     
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ORDELD,R6                                                        
         LA    R2,BIGKEY           BUILD KEY AND CHECK IF ITS THERE             
         USING CHDRECD,R2                                                       
         MVC   CHDKEY,SPACES                                                    
         MVC   CHDKCULA,ORDJOB                                                  
         MVC   CHDKWRK,=C'**'      WORKCODE IS '**' ON SJ                       
         MVC   CHDKCULC,ORDSUP                                                  
         TM    FLAG1,F1EXPORD      IF EXPENSE ORDER, USE SPACES INSTEAD         
         BNO   ADDCON05                                                         
         MVC   CHDKWRK,SPACES      WORKCODE IS '  ' ON SJ                       
         MVI   CHDKCCPY,C' '       COMPANY CODE IS ' '                          
ADDCON05 XC    CHDKNULL,CHDKNULL   ZERO LAST 6 BYTES OF KEY                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   CHDKEY,KEYSAVE      TEST IF FOUND                                
         BE    ADDCONX             YES-ALL DONE NOW                             
         DROP  R2                                                               
*                                                                               
ADDCON10 L     R4,AIO2             BUILD NEW RECORD IN IO2                      
         USING CHDRECD,R4                                                       
         LR    RE,R4                                                            
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
         ST    R4,AIO                                                           
*                                                                               
         MVC   CHDKEY,KEYSAVE      SET INITIAL KEY                              
         MVC   CHDRLEN,=Y(CHDRFST-CHDKEY+1) SET INITIAL REC LEN                 
*                                                                               
ADDCON20 XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING CACELD,R6                                                        
         MVI   CACEL,CACELQ                                                     
         MVC   CACCNT,SUPCODE                                                   
         TM    FLAG1,F1EXPORD      IF EXPENSE ORDER, USE SPACES INSTEAD         
         BNO   *+8                                                              
         MVI   CACCNTC,C' '                                                     
         MVC   CACNAME,SUPNAME                                                  
         LA    R2,L'CACNAME        COMPUTE NAME LENGTH                          
         LA    R1,CACNAME+L'CACNAME-1                                           
         CLI   0(R1),C' '          TEST FOR SIGNIFICANT BYTE                    
         BH    *+10                                                             
         BCTR  R1,0                BACK UP POINTER                              
         BCT   R2,*-10                                                          
         LA    R2,CACLN1Q(R2)                                                   
         STC   R2,CACLN                                                         
         GOTO1 ADDELEM,DMCB,CACELD                                              
*                                                                               
ADDCON30 GOTO1 ADDREC                                                           
         MVC   AIO,AIO1            RESET IO AREA POINTER                        
*                                                                               
ADDCONX  B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO BUILD AND ADD A SJ TRANSACTION ITEM FOR ORDER                  
*                                                                               
* AT ENTRY, P1=A(ORDER RECORD)                                                  
*                                                                               
ADDSJ    NTR1  ,                                                                
         L     R5,0(R1)                                                         
         USING ORDRECD,R5                                                       
         MVC   SAVEAIO,AIO                                                      
         L     R4,AIO2             R4=A(TRANSACTION RECORD)                     
         USING TRNRECD,R4                                                       
*                                                                               
         LR    RE,R4               CLEAR IO AREA                                
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
ADDSJ2   ST    R5,AIO              GET ORDER ELEMENT                            
         LA    R3,ORDELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ORDELD,R6                                                        
*                                                                               
         MVC   TRNKCULA,ORDJOB     ACCOUNT = JOB                                
         MVC   TRNKCULC,ORDSUP     CONTRA=SUPPLIER                              
         MVC   TRNKWORK,=C'**'                                                  
         TM    FLAG1,F1EXPORD      IF EXPENSE ORDER, USE SPACES INSTEAD         
         BNO   ADDSJ3                                                           
         MVC   TRNKWORK,SPACES     WORKCODE IS '  '                             
         MVI   TRNKCCPY,C' '       COMPANY CODE IS ' '                          
ADDSJ3   MVC   TRNKDATE,ORDDATE    TRANSACTION DATE=ORDER DATE                  
         MVC   TRNKREF,ORDKORD     TRANSACTION REFERENCE=ORDER NUMBER           
         MVC   TRNRLEN,=Y(TRNRFST-TRNRECD+1)                                    
*                                                                               
ADDSJ4   ST    R4,AIO                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING TRNELD,R6                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVI   TRNTYPE,12          TYPE 12                                      
         OI    TRNSTAT,TRNSDR                                                   
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   TRNMOS(1),WORK+1    SECOND DIGIT OF YEAR                         
         MVC   TRNMOS+1(1),WORK+3  SECOND DIGIT OF MONTH                        
         CLI   WORK+2,C'1'         TEST MONTH = 10-12                           
         BNE   ADDSJ5                                                           
*                                                                               
         NI    TRNMOS+1,X'C3'                                                   
         ZIC   R1,TRNMOS+1                                                      
         LA    R1,1(R1)            (10=A, 11=B, 12=C)                           
         STC   R1,TRNMOS+1                                                      
*                                                                               
ADDSJ5   MVC   TRNBREF,SPACES                                                   
         ZAP   TRNAMNT,=P'0'                                                    
         MVC   TRNANAL,=C'**'                                                   
         MVI   TRNNARR,C' '        PUT A BLANK IN NARRATIVE                     
         GOTO1 ADDELEM,DMCB,TRNELD                                              
*&&UK*&& GOTO1 NARR,DMCB,TRNRECD,ORDRECD  ADD TRANS NARRATIVE                   
*                                                                               
         LA    R3,TRNELQ           POINT R6 AT TRANSACTION EL ON RECD           
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 CONVMOS,DMCB,(X'FD',TRNELD),WORK                                 
         MVC   TRNRSMOS,WORK       SET TRANSACTION MOS                          
         MVC   TRNRSTYP,TRNTYPE    TRANSACTION TYPE                             
*                                                                               
         GOTO1 UPDOAM,DMCB,TRNRECD                                              
*                                                                               
ADDSJ8   CLI   SECCURR,C'Y'        TEST ORDER CURRENCY = SECONDARY              
         BNE   ADDSJ10             NO                                           
*                                                                               
*&&UK                                                                           
         LA    R6,ELEMENT          BUILD AN OCAEL CONTAINING                    
         USING OCAELD,R6           SECONDARY CURRENCY AMOUNT                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   OCAEL,OCAELQ                                                     
         MVI   OCALN,OCALN1Q+OCANTRYL                                           
         MVI   OCANUM,1                                                         
         MVI   OCANTYPE,QOAMAMNT                                                
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH,SECAMT                                                  
         GOTO1 ADDELEM,DMCB,OCAELD                                              
         DROP  R6                                                               
*&&                                                                             
ADDSJ10  DS    0H                                                               
*                                                                               
*&&US                                                                           
* ADD STATUS ELEMENT WITH DATE SJ POSTING ADDED (TODAY)                         
         USING TRSELD,R6           ADD ELEMENT WITH DATE ADDED                  
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSDATE,TODAYB                                                   
         GOTO1 ADDELEM,DMCB,TRSELD                                              
         DROP  R6                                                               
*&&                                                                             
*                                                                               
         GOTO1 UPDXPAN,DMCB,AIO    ADD EXPENSE ANALYSIS ELEMENT                 
*                                                                               
         CLI   ACTION,CHAO         IF ACTION CHANGE ORDER, CHECK IF             
         BNE   ADDSJ12              NEED TO RE-ADD A SJ TRANS RECORD            
         CLI   BYTE,C'P'           CHECK FLAG IF NEED TO DO A PUTREC            
         BE    ADDSJ15                                                          
ADDSJ12  GOTO1 ADDREC              NO, ADD NEW RECORD                           
         B     ADDSJ20                                                          
*                                                                               
ADDSJ15  GOTO1 PUTREC              YES, WRITE OVER RECORD                       
         MVC   BIGKEY+TRNKSTAT-TRNKEY(L'TRNKSTA),TRNRSTA                        
         GOTO1 WRITE               WRITE OVER THE STATUS BYTES IN KEY           
*                                                                               
ADDSJ20  MVC   AIO,SAVEAIO                                                      
*                                                                               
ADDSJX   B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE A SJ ORDER ITEM                                         
* AT ENTRY, AIO1=A(ORDER RECORD)                                                
*                                                                               
UPDSJ    NTR1  ,                                                                
         L     R5,AIO1                                                          
         USING ORDRECD,R5                                                       
         ST    R5,AIO                                                           
*                                                                               
         LA    R3,ORDELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ORDELD,R6                                                        
*                                                                               
         LA    R4,BIGKEY           R4=A(TRANSACTION KEY)                        
         USING TRNRECD,R4                                                       
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCULA,ORDJOB     ACCOUNT = JOB                                
         MVC   TRNKCULC,ORDSUP     CONTRA=SUPPLIER                              
         MVC   TRNKWORK,=C'**'                                                  
         TM    FLAG1,F1EXPORD      IF EXPENSE ORDER, USE SPACES INSTEAD         
         BNO   UPDSJ05                                                          
         MVC   TRNKWORK,SPACES     WORKCODE IS '  '                             
         MVI   TRNKCCPY,C' '       COMPANY CODE IS ' '                          
UPDSJ05  MVC   TRNKDATE,ORDDATE    TRANSACTION DATE=ORDER DATE                  
         MVC   TRNKREF,ORDKORD     TRANSACTION REFERENCE=ORDER NUMBER           
*                                                                               
UPDSJ10  MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 HIGH                                                             
         MVI   BYTE,0              CLEAR FLAG                                   
         CLC   TRNKEY,KEYSAVE      CHECK IF SJ RECORD EXISTS                    
         BNE   UPDSJ11                                                          
         CLI   ACTION,CHAO         IF ACTION CHANGE ORDER, CHECK IF             
         BNE   UPDSJ20              TRYING TO RE-USE A DELETED ORDER            
         TM    TRNKSTAT,TRNSDELT   IF SJ RECORD DEL, NEED TO RE-ADD IT          
         BNO   UPDSJ20                                                          
         MVI   BYTE,C'P'           SET FLAG TO DO PUTREC                        
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
UPDSJ11  CLI   ACTION,CLOSEO                                                    
         BE    UPDSJ12                                                          
         GOTO1 ADDSJ,DMCB,ORDRECD                                               
*                                                                               
UPDSJ12  B     UPDSJ30                                                          
*                                                                               
UPDSJ20  MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         CLI   ACTION,CLOSEO       TEST TO CLOSE ORDER                          
         BE    UPDSJ25                                                          
         CLI   ACTION,ADDCLOSO     TEST ADDING CLOSED ORDER                     
         BE    UPDSJ25                                                          
*                                                                               
*&&UK*&& GOTO1 NARR,DMCB,AIO2,ORDRECD  SJ RECORD TRANS NARRATIVE                
         GOTO1 UPDOAM,DMCB,AIO2                                                 
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST EURO ZONE AGENCY                        
         BZ    UPDSJ22                                                          
*                                                                               
         GOTO1 DELELEM,DMCB,OCAELQ,0                                            
         CLI   SECCURR,C'N'                                                     
         BE    UPDSJ22                                                          
*                                                                               
         LA    R6,ELEMENT          BUILD AN OCAEL CONTAINING                    
         USING OCAELD,R6           SECONDARY CURRENCY AMOUNT                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   OCAEL,OCAELQ                                                     
         MVI   OCALN,OCALN1Q+OCANTRYL                                           
         MVI   OCANUM,1                                                         
         MVI   OCANTYPE,QOAMAMNT                                                
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH,SECAMT                                                  
         GOTO1 ADDELEM,DMCB,OCAELD                                              
*&&                                                                             
UPDSJ22  GOTO1 PUTREC                                                           
         B     UPDSJ30                                                          
*                                                                               
UPDSJ25  L     R3,AIO2             R3=A(TRANSACTION RECORD)                     
         OI    TRNRSTAT-TRNRECD(R3),TRNSDELT SET DELETE BIT IN RECORD           
         GOTO1 PUTREC                                                           
         OI    TRNKSTAT,TRNSDELT                                                
         GOTO1 WRITE                                                            
*                                                                               
UPDSJ30  MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
UPDSJX   B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE EXPENSE ANALYSIS ELEMENT (X'97')                        
* AT ENTRY, P1=A(RECORD)                                                        
*                                                                               
UPDXPAN  NTR1  ,                                                                
         L     R4,0(R1)            R4=A(RECORD)                                 
         MVC   SAVEAIO,AIO         SAVE THE IO POINTER                          
         ST    R4,AIO              POINT AT TARGET RECORD                       
*                                                                               
         LA    R5,TMPDATA                                                       
         USING PROUOBJD,R5                                                      
*                                                                               
         TM    FLAG1,F1EXPANL      CHECK IF EXPENSE ANALYSIS USED               
         BNO   UPDXPANX                                                         
         USING EXOELD,R6           ADD EXPENSE ORDER ANALYSIS ELEMENT           
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   EXOEL,EXOELQ                                                     
         MVI   EXOLN,EXOLNQ                                                     
         MVC   EXOCLI(EXPANLQ),EXPANAL  EXPENSE ANALYSIS FIELDS                 
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST'),(R4),(R6),=C'ADD=CODE'              
         DROP  R5,R6                                                            
*                                                                               
UPDXPANX MVC   AIO,SAVEAIO         RESTORE IO POINTER                           
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE SERVER IN X'DB' ELEMENT                                 
* AT ENTRY, AIO1=A(ORDER RECORD)                                                
*                                                                               
UPDSERV  NTR1  ,                                                                
         MVC   AIO,AIO1                                                         
*&&UK                                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ        BUILD A FREE FORM ELEMENT                    
         MVI   FFTLN,FFTLN1Q+3                                                  
         MVI   FFTTYPE,FFTTOFFC    WITH THE OFFICE CODE                         
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,2           LENGTH OF OFFICE CODE                        
         MVC   FFTDATA(2),CLIOFF                                                
         CLC   PROOFF,SPACES       TEST FOR PRODUCT OFFICE OVERRIDE             
         BNH   *+10                                                             
         MVC   FFTDATA(2),PROOFF   YES                                          
         CLC   JOBOFF,SPACES       TEST FOR JOB OFFICE OVERRIDE                 
         BNH   *+10                                                             
         MVC   FFTDATA(2),JOBOFF                                                
         GOTO1 ADDELEM,DMCB,FFTELD                                              
*                                                                               
*&&                                                                             
         OC    SERVER,SERVER       TEST IF SERVER UPLOADED                      
         BZ    UPDSERVX            NO                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ        BUILD A FREE FORM ELEMENT                    
         MVI   FFTTYPE,FFTTSQLI    WITH THE ORIGINATING SQL SERVER ID           
         MVC   FFTDATA(L'SERVER),SERVER                                         
*                                                                               
         LA    R1,L'SERVER         COMPUTE LENGTH OF SERVER STRING              
         LA    RF,SERVER+L'SERVER-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTDATA-FFTELD(R1) COMPUTE ELEMENT LENGTH                     
         STC   R1,FFTLN                                                         
         GOTO1 ADDELEM,DMCB,FFTELD                                              
*                                                                               
UPDSERVX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO MAINTAIN THE TRANSACTION NARRATIVE FOR THE                     
* SJ TRANSACTION RECORD - CALLED FROM ADDSJ AND UPDSJ                           
*                                                                               
* AT ENTRY, P1=A(TRANSACTION RECORD), P2=A(ORDER RECORD)                        
*                                                                               
NARR     NTR1  ,                                                                
         LM    R4,R5,0(R1)         R4=A(TRANS REC),R5=A(ORDER REC)              
         L     R2,AIO              SAVE AIO IN R2                               
         CLI   PPROF1+11,0         TEST UK PROFILE TO ADD TRANS NARR            
         BE    NARRX               NO-EXIT RIGHT NOW                            
         ST    R4,AIO              POINT AIO AT TRANSACTION                     
*                                                                               
* EXTRACT THE EXISTING TRANSACTION ELEMENT INTO A WORK AREA                     
*                                                                               
NARR10   LA    R3,TRNELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R6                                                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         ZIC   R1,TRNLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),TRNELD                                                
*                                                                               
         LA    R6,ELEMENT                                                       
         MVI   TRNNARR,C' '                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
*                                                                               
* NOW SEARCH FOR ANY NARRATIVE ON THE MAIN ORDER RECORD                         
*                                                                               
NARR20   ST    R5,AIO                                                           
         LA    R3,SCMELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BNE   NARR50                                                           
*                                                                               
         LR    RE,R6               MOVE A(COMMENT EL) TO RE                     
         USING SCMELD,RE                                                        
         LA    R6,ELEMENT          RESTORE R6=A(TRANS EL)                       
         ZIC   R1,SCMLN                                                         
         SHI   R1,SCMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRNNARR(0),SCMNARR                                               
         LA    RF,TRNNARR+1(R1)    POSITION TO FIRST BYTE PAST                  
         MVI   0(RF),C' '          NARRATIVE AND APPEND A BLANK                 
         LA    R1,TRNLN1Q+2(R1)    COMPUTE NEW TRANSACTION EL LENGTH            
         STC   R1,TRNLN                                                         
*                                                                               
* REPLACE THE TRANSACTION ELEMENT WITH THE NEW VERSION                          
*                                                                               
NARR50   ST    R4,AIO                                                           
         GOTO1 DELELEM,DMCB,TRNELQ,0                                            
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
NARRX    ST    R2,AIO              RESTORE AIO TO ENTRY VALUE                   
         B     XIT                                                              
         DROP  R6,RE                                                            
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE A RECORD'S ORDER AMOUNT ELEMENTS                        
*                                                                               
* AT ENTRY, P1=A(RECORD) ORDER OR SJ ORDER                                      
*                                                                               
UPDOAM   NTR1  ,                                                                
         L     R4,0(R1)            R4=A(RECORD)                                 
         MVC   SAVEAIO,AIO         SAVE THE IO POINTER                          
         ST    R4,AIO              POINT AT TARGET RECORD                       
*                                                                               
         LA    R3,OAMELQ           DELETE ORDER AMOUNT ELEMENTS                 
         GOTO1 DELELEM,DMCB,(R3),0                                              
*                                                                               
         LA    R6,NEWELS           R6=ELEMENT POINTER                           
         USING OAMELD,R6                                                        
*                                                                               
         SR    R4,R4               USE TO COUNT # OF NON-COMM WC'S              
         SR    R5,R5               USE TO COUNT # OF COMM WC'S                  
UPDOAM5  CLI   OAMEL,OAMELQ        TEST FOR ORDER AMOUNT ELEMENT                
         BNE   UPDOAM10            NO-CLEAN UP BEFORE EXITING                   
*                                                                               
         TM    OAMSTAT,OAMSNOCM    TEST NON-COMMISSIONABLE                      
         BNO   *+12                                                             
         AHI   R4,1                INCREMENT NUMBER OF NON-COMM WC'S            
         B     *+8                                                              
         AHI   R5,1                INCREMENT NUMBER OF COMM W'CS                
*                                                                               
         ZIC   R2,OAMLN                                                         
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST'),AIO,(R6),=C'ADD=CODE'               
         LA    R6,0(R2,R6)         POINT TO NEXT ELEMENT                        
         B     UPDOAM5                                                          
*                                                                               
UPDOAM10 L     R6,AIO                                                           
         CLI   0(R6),ORDKTYPQ      CHECK IF ORDER RECORD                        
         BNE   UPDOAM30                                                         
         LA    R3,ORDELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ORDELD,R6                                                        
         NI    ORDSTAT,X'FF'-ORDSTY10                                           
         CHI   R4,4                CHECK IF MORE THAN 4 WC'S NON-COMM           
         BH    UPDOAM20                                                         
         CHI   R5,4                CHECK IF MORE THAN 4 WC'S COMM               
         BNH   UPDOAM30                                                         
UPDOAM20 OI    ORDSTAT,ORDSTY10    THEN TURN ON THIS BIT                        
*                                                                               
UPDOAM30 MVC   AIO,SAVEAIO         RESTORE IO POINTER                           
*                                                                               
UPDOAMX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE AN ORDER RECORD FOR CURRENCY                            
*                                                                               
* AT ENTRY, P1=A(RECORD) ORDER                                                  
*                                                                               
*&&UK                                                                           
UPDCUR   NTR1  ,                                                                
         L     R4,0(R1)            R4=A(RECORD)                                 
         USING ORDRECD,R4                                                       
         MVC   SAVEAIO,AIO         SAVE THE IO POINTER                          
         ST    R4,AIO              POINT AT TARGET RECORD                       
         TM    COMPSTA7,CPYSSCNV   TEST SECONDARY CURRENCY                      
         BZ    UPDCURX             NO                                           
*                                                                               
         CLI   SECCURR,C'Y'        TEST ORDER IN SECONDARY CURRENCY             
         BNE   *+8                                                              
         OI    ORDRSTAT,ORDSSECC   YES-SET SECONDARY CURRENCY BIT               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING OCAELD,R6                                                        
         MVI   OCAEL,OCAELQ                                                     
         MVI   OCALN,OCALN1Q+OCANTRYL                                           
         MVI   OCANUM,1                                                         
         MVI   OCANTYPE,QOAMAMNT                                                
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH,SECAMT                                                  
         GOTO1 ADDELEM,DMCB,OCAELD                                              
*                                                                               
UPDCURX  MVC   AIO,SAVEAIO                                                      
         B     XIT                                                              
         DROP  R4,R6                                                            
*&&                                                                             
         EJECT                                                                  
*                                  UNEXPECTED END OF TEMP FILE                  
ERREOT   MVC   APPLERR,=Y(PREREOT)                                              
         B     ERROBJ                                                           
*                                  INVALID UPLOAD OBJECT                        
ERRUOBJ  MVC   APPLERR,=Y(PRERUOBJ)                                             
         B     ERROBJ                                                           
*                                  INVALID JOB KEY IN UPLOAD                    
ERRJOKY  MVC   APPLERR,=Y(PRERJOKY)                                             
         B     ERROBJ                                                           
*                                  JOB IS CLOSED OR LOCKED                      
ERRJOCL  MVC   APPLERR,=Y(PRERJOCL)                                             
         B     ERROBJ                                                           
*                                  INVALID SUPPLIER                             
ERRSUP   MVC   APPLERR,=Y(PRERSUP)                                              
         B     ERROBJ                                                           
*                                  ORDER IS CLOSED ON MAINFRAME                 
ERRCLOSE MVC   APPLERR,=Y(PREROCLS)                                             
         B     ERROBJ                                                           
*                                  WORKCODES CANNOT BE CHANGED ON               
ERRWORK  MVC   APPLERR,=Y(PREROWRK)  INVOICED ORDER                             
         B     ERROBJ                                                           
*                                  UPLOAD TO READ ONLY FILE                     
ERRUPRO  MVC   APPLERR,=Y(PRERUPRO)                                             
         B     ERROBJ                                                           
*                                  NO ORDER LINE OBJECTS                        
ERRNOWC  MVC   APPLERR,=Y(PRERNOWC)                                             
         B     ERROBJ                                                           
*                                                                               
ERRCURR  MVC   APPLERR,=Y(PREROCUR) CANNOT CHANGE CURRENCY ON                   
         B     ERROBJ               INVOICED ORDER                              
*                                                                               
ERRBALE  MVC   APPLERR,=Y(PRERBALE) BALANCE ELEMENT MISSING ON EXP REC          
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,A(ITGENERR),4,FULL                                  
         BNE   EXIT                                                             
         B     SETMDLST                                                         
*                                                                               
SETMDLST MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
EXIT     CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   EXIT10                                                           
         GOTO1 WRKSENT             MARK FILE AS SENT                            
*                                                                               
EXIT10   L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
CORETAB  DC    C'CORETAB'                                                       
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
APRELO   DS    A                                                                
SAVEAIO  DS    A                                                                
*                                                                               
CONVMOS  DS    A                                                                
CASHVAL  DS    V                                                                
GETPROF  DS    V                                                                
*                                                                               
CTRY     DS    XL1                                                              
COMPALPH DS    CL2                 AGENCY ALPHA                                 
COMPCUR  DS    CL3                 COMPANY PRIMARY CURRENCY                     
COMPCURS DS    CL3                 COMPANY SECONDARY CURRENCY                   
COMPSTA7 DS    X                   COMPANY STATUS BYTE 7                        
COMPSTA9 DS    X                   COMPANY STATUS BYTE 9                        
*                                                                               
MYCLI    DS    CL6                                                              
MYPRO    DS    CL6                                                              
MYJOB    DS    CL6                                                              
CLIOFF   DS    CL2                 CLIENT OFFICE                                
PROOFF   DS    CL2                 PRODUCT OFFICE                               
JOBOFF   DS    CL2                 JOB OFFICE                                   
ORDNUM   DS    CL6                 ORDER NUMBER                                 
STATUS   DS    XL1                 ORDER STATUS VALUE                           
JOBKEY   DS    CL(L'ACTKEY)                                                     
SUPCODE  DS    CL15                                                             
SUPNAME  DS    CL36                                                             
SUPSTAT  DS    XL1                                                              
SERVER   DS    CL16                ORIGINATING SQL SERVER                       
CURRENCY DS    CL3                 CURRENCY CODE                                
PRIMAMT  DS    D                   PRIMARY CURRENCY AMOUNT                      
SECAMT   DS    D                   SECONDARY CURRENCY AMOUNT                    
SECCURR  DS    CL1                 SECONDARY CURRENCY ORDER (Y/N)               
XTCPTRBK DS    XL128               USED FOR PADDLE CPTRBLK CALLS                
SVDA     DS    XL4                 SAVED DISK ADDRESS                           
                                                                                
TODAYB   DS    XL2                 TODAY'S DATE                                 
ACTION   DS    XL1                 ADD, CHANGE, CLOSE                           
WRITORD  DS    CL1                 Y/N=WRITE OF ORDER PENDING                   
INVOICED DS    CL1                 Y/N                                          
OLDSTAT  DS    XL1                 ORDER STATUS - BEFORE UPLOAD                 
*                                                                               
*&&US                                                                           
SVTRSDT  DS    XL2                 SAVE ORDER ADDED DATE                        
*&&                                                                             
*                                                                               
COMMSTAT DS    XL1                 EXTRA STATUS FLAGS                           
NCOMSTAT DS    XL1                                                              
FLAG1    DS    X                                                                
F1EXPORD EQU   X'80'               EXPENSE ORDER                                
F1EXPANL EQU   X'40'               EXPENSE ANALYSIS USED                        
NORDLIN  DS    F                   NUMBER OF ORDERLINE OBJECTS                  
*                                                                               
SPACES   DS    CL80                                                             
*                                                                               
PPROF1   DS    XL16                PROGRAM PROFILE                              
*                                                                               
EXPANAL  DS    0C                  EXPENSE ANALYSIS FIELDS                      
EXPACLI  DS    CL6                 CLIENT CODE                                  
EXPAPRO  DS    CL6                 PRODUCT CODE                                 
EXPADOF  DS    CL2                 DEBIT OFFICE                                 
EXPACOF  DS    CL2                 CREDIT OFFICE                                
EXPAAOF  DS    CL2                 ANALYSIS OFFICE                              
EXPADEP  DS    CL3                 DEPARTMENT                                   
EXPAPER  DS    CL7                 PERSON                                       
EXPANLQ  EQU   *-EXPANAL                                                        
*                                                                               
TMPHDR   DS    F                   LENGTH OF TMPAREA + 4 (FROM WRKGET)          
TMPAREA  DS    0XL2000             AREA TO READ TEMP FILE RECORD                
TMPTYPE  DS    XL4                 MAD OBJECT TYPE                              
TMPDATA  DS    XL1996              MAD OBJECT DATA                              
*                                                                               
         DS    0D                                                               
OLDBUFF  DS    0XL(20*OAMLN2Q+2)                                                
OLDLEN   DS    H                                                                
OLDELS   DS    XL(20*OAMLN2Q)      OLD ORDER AMOUNT ELEMENTS                    
*                                                                               
         DS    0D                                                               
NEWBUFF  DS    0XL(20*OAMLN2Q+2)                                                
NEWLEN   DS    H                                                                
NEWELS   DS    XL(20*OAMLN2Q)      NEW ORDER AMOUNT ELEMENTS                    
         SPACE 2                                                                
* EQUATES FOR OVERLAY                                                           
*                                                                               
OLD      EQU   C'O'                                                             
NEW      EQU   C'N'                                                             
ADDO     EQU   1                                                                
ADDCLOSO EQU   2                   ADD A CLOSED ORDER                           
CHAO     EQU   3                                                                
CLOSEO   EQU   4                                                                
*                                                                               
OPENSTAT EQU   0                                                                
PARTSTAT EQU   1                                                                
FULLSTAT EQU   2                                                                
CLOSSTAT EQU   3                                                                
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* ACPRESTOD                                                                     
       ++INCLUDE ACPRESTOD                                                      
         EJECT                                                                  
*                                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACPRESTOQ                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPRESTOQ                                                      
         PRINT ON                                                               
* CTMADPREST                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTMADPREST                                                     
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
CPTRBLKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACLDCPTRD                                                      
         PRINT ON                                                               
*&&UK                                                                           
* ACTOBACCOD                                                                    
       ++INCLUDE ACTOBACCOD                                                     
*&&                                                                             
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052CTMAD22   06/22/10'                                      
         END                                                                    
