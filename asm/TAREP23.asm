*          DATA SET TAREP23    AT LEVEL 067 AS OF 05/01/02                      
*PHASE T70323A,*                                                                
         TITLE 'T70323 - BILLED ACTIVITY REPORT'                                
T70323   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70323                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING LOCALD,R7                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCRAGYH),SCRAGYNH  AGENCY             
         MVC   TIFAGY,TGAGY                                                     
         SPACE 1                                                                
         LA    R2,SCRCLIH          CLIENT                                       
         XC    SCRCLIN,SCRCLIN                                                  
         OI    SCRCLINH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK4                                                              
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCRCLINH                        
         MVC   TIFCLI,TGCLI                                                     
         SPACE 1                                                                
VK4      LA    R2,SCRPRDH          PRODUCT                                      
         XC    SCRPRDN,SCRPRDN                                                  
         OI    SCRPRDNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK6                                                              
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SCRPRDNH                        
         MVC   TIFPRD,TGPRD                                                     
         SPACE 1                                                                
VK6      LA    R2,SCRESTH          ESTIMATE                                     
         MVI   FLTESTLN,0                                                       
         CLI   5(R2),0                                                          
         BE    VK8                                                              
         MVC   FLTEST,8(R2)        SAVE INPUT                                   
         OC    FLTEST,SPACES                                                    
         MVC   FLTESTLN,5(R2)      AND L'INPUT                                  
         SPACE 1                                                                
VK8      LA    R2,SCRPDDH          VALIDATE DETAIL PERIOD                       
         ST    R2,APERFLD                                                       
         CLI   5(R2),0             IF THERE'S NO INPUT                          
         BNE   *+12                                                             
         CLI   SCRRONL,C'Y'        THEN REQUEST MUST BE FOR RECAP ONLY          
         BE    VK10                                                             
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   DTLSTRT,PVALPSTA    SAVE PWOS DATES FOR LOCAL FILTERING          
         MVC   DTLEND,PVALPEND                                                  
         MVC   TIQPSTR,PVALPSTA    SAVE FOR SYSIO IN CASE NO RECAP              
         MVC   TIQPEND,PVALPEND                                                 
         SPACE 1                                                                
         LA    R2,SCRDBASH         VALIDATE DETAIL BASIS                        
         GOTO1 ANY                                                              
         CLI   8(R2),C'O'          ON/OFF CAMERA                                
         BE    VK9                                                              
         CLI   8(R2),C'C'          CATEGORY, ON/OFF CAMERA                      
         BE    VK9                                                              
         CLI   8(R2),C'I'          INVOICE LEVEL TOTALS ONLY                    
         BNE   FLDINV                                                           
VK9      MVC   DTLBASIS,8(R2)                                                   
         SPACE 1                                                                
VK10     LA    R2,SCRPDRH          VALIDATE RECAP DATES                         
         MVI   CAPBASIS,0                                                       
         SPACE 1                                                                
         CLI   5(R2),0             IF THERE'S NO INPUT                          
         BNE   VK11                                                             
         CLI   SCRRBASH+5,0        THEN INSURE NO INPUT IN RECAP BASIS          
         BNE   FLDMISS                                                          
         CLI   SCRRONLH+5,0        AND IN RECAP ONLY FIELD                      
         BNE   FLDMISS                                                          
         B     VK14                                                             
         SPACE 1                                                                
VK11     LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TIQPSTR,PVALPSTA    SET PWOS DATES FOR SYSIO                     
         MVC   TIQPEND,PVALPEND                                                 
         SPACE 1                                                                
         LA    R2,SCRRBASH         VALIDATE RECAP BASIS                         
         GOTO1 ANY                                                              
         CLI   8(R2),C'E'          ESTIMATE PERIOD                              
         BE    VK12                                                             
         CLI   8(R2),C'B'          BILLING QUARTER                              
         BE    VK12                                                             
         CLI   8(R2),C'I'          INVOICE NUMBER                               
         BE    VK12                                                             
         CLI   8(R2),C'T'          TOTALS ONLY                                  
         BNE   FLDINV                                                           
VK12     MVC   CAPBASIS,8(R2)                                                   
         SPACE 1                                                                
         LA    R2,SCRRONLH         VALIDATE RECAP ONLY                          
         CLI   5(R2),0                                                          
         BE    VK13                                                             
         CLI   8(R2),C'N'          NO                                           
         BE    VK13                                                             
         CLI   8(R2),C'Y'          YES                                          
         BNE   FLDINV                                                           
         LA    R1,SCRPDRH          HAVE REPORT DISPLAY RECAP PERIOD             
         ST    R1,APERFLD                                                       
VK13     MVC   CAPONLY,8(R2)                                                    
         SPACE 1                                                                
VK14     LA    R2,SCRCOMMH         SUPPRESS AGENCY COMMISSION                   
         MVI   CALCCOMM,C'N'                                                    
         CLI   8(R2),C'Y'          YES                                          
         BE    VK16                                                             
         CLI   5(R2),0             NO INPUT                                     
         BE    *+12                                                             
         CLI   8(R2),C'N'          OR NO                                        
         BNE   FLDINV                                                           
         MVI   CALCCOMM,C'Y'       MEANS GO AHEAD AND CALC.                     
         SPACE 1                                                                
VK16     LA    R2,SCRSUMMH         PRINT COMMERCIAL SUMMARY                     
         CLI   5(R2),0                                                          
         BE    VK18                                                             
         CLI   8(R2),C'Y'          YES                                          
         BE    VK18                                                             
         CLI   8(R2),C'N'          NO                                           
         BNE   FLDINV                                                           
VK18     MVC   PRTCOMML,8(R2)                                                   
         SPACE 1                                                                
         LA    R2,SCRTITH          TITLE                                        
         MVC   TITLE(L'DEFTITLE),DEFTITLE  SET DEFAULT TITLE                    
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   TITLE,8(R2)                                                      
         OC    TITLE,SPACES                                                     
         GOTO1 CENTER,DMCB,TITLE,L'TITLE                                        
         SPACE 1                                                                
         LA    R2,SCRSTITH         SUB-TITLE                                    
         MVC   SUBTITLE,8(R2)                                                   
         OC    SUBTITLE,SPACES                                                  
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 CENTER,DMCB,SUBTITLE,L'SUBTITLE                                  
         SPACE 1                                                                
VK20     LA    R2,SCROPTH          VALIDATE OPTIONS                             
         GOTO1 VALOPTS                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         BAS   RE,INIT             INTIALIZE                                    
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
         BAS   RE,OUTDRIVE         PRINT THE REPORT                             
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES FOR REPORT                               
         SPACE 1                                                                
INIT     NTR1                                                                   
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVC   TIQSTAFF,TGCTSTAF   STAFF ID                                     
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         MVI   TIQDTYPE,TIQDBILL   SET FILTERING ON BILL DATE                   
         OI    TIQFLAG2,TIQFSUB    READ SUBSIDIARY INVOICES ONLY                
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9070294'  LOAD DPG PROGRAM                      
         MVC   ADPGPROG,0(R1)                                                   
         GOTO1 INITDRIV            DRIVER INITIALIZATION ROUTINES               
         SPACE 1                                                                
         L     R2,AGLOBAL          R2=A(DRIVER W/S)                             
         USING GLOBALD,R2                                                       
         OI    GLINDS,GLPALDET     SET WE WANT ALL DETAILS                      
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,GLAHOOK                                                       
         SPACE 1                                                                
         MVC   GLOPTS+0(1),CALCCOMM  CALC. AGENCY COMMISSION                    
         MVC   GLOPTS+1(1),CAPONLY   RECAP ONLY                                 
         MVC   GLOPTS+2(1),PRTCOMML  PRINT COMMLS INCLUDED SUMMARY              
         MVC   GLOPTS+3(1),CAPBASIS  RECAP BASIS                                
         MVC   GLOPTS+4(1),DTLBASIS  DETAIL BASIS                               
         MVC   GLOPTS+5(1),SCRCLIH+5 CLIENT FILTER INDICATOR                    
         SPACE 1                                                                
         BAS   RE,INTDRIVE         INITIALIZE DRIVER                            
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO                                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         MVI   OKDETAIL,C'N'       OK TO PROCESS DETAIL REPORT                  
         MVI   INVOICE,C'N'        THIS IS AN INVOICE RECORD                    
         SPACE 1                                                                
         CLI   TIMODE,PROCREC      PROCESS RECORD (CHECK)                       
         BE    IOH10                                                            
         SPACE 1                                                                
         CLI   TIMODE,PROCINV      PROCESS INVOICE                              
         BNE   IOHX                                                             
         MVC   AIO,TIAREC          SET A(INVOICE RECORD)                        
         GOTO1 MYTRACE2,DMCB,=C'INPUT RECORD',AIO                               
         BAS   RE,FILTINV          FILTER INVOICE                               
         MVC   AIO,AIO1                                                         
         BNE   IOHX                                                             
         MVI   INVOICE,C'Y'                                                     
         SPACE 1                                                                
IOH10    CLI   CAPONLY,C'Y'        SKIP DETAIL PD TEST IF RECAP ONLY            
         BE    IOH14                                                            
         CLC   TIBIDATE,DTLSTRT    TEST WITHIN DETAIL PERIOD                    
         BL    IOH14                                                            
         CLC   TIBIDATE,DTLEND                                                  
         BH    IOH14                                                            
         MVI   OKDETAIL,C'Y'       SET OK TO PROCESS DETAIL                     
         SPACE 1                                                                
IOH14    CLI   DTLBASIS,C'I'       IF DETAIL FOR INVOICE ONLY                   
         BE    *+12                                                             
         CLI   CAPONLY,C'Y'        OR PROCESSING RECAP ONLY                     
         BNE   *+8                                                              
         MVI   TIMODE,PROCREC      MAKE SYSDRIVE THINK THIS IS DTL              
         SPACE 1                                                                
         GOTO1 MYTRACE2,DMCB,=C'INPUT RECORD',AIO                               
         SPACE 1                                                                
         BAS   RE,INDRIVE          PASS RECORD TO DRIVER                        
         SPACE 1                                                                
         CLI   DTLBASIS,C'I'       IF DETAIL FOR INVOICE ONLY                   
         BE    *+12                                                             
         CLI   CAPONLY,C'Y'        OR PROCESSING RECAP ONLY                     
         BNE   *+8                                                              
         MVI   TIMODE,PROCNOCK     SET TO NOT PASS CHECK RECORDS                
         SPACE 1                                                                
IOHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILTER INVOICE                                        
         SPACE 1                                                                
*                                  AIO=A(INVOICE RECORD)                        
FILTINV  NTR1                                                                   
         CLI   FLTESTLN,0          TEST ESTIMATE FILTER DEFINED                 
         BE    FINVX                                                            
         SPACE 1                                                                
         LA    R3,FLTEST           R3=A(FILTER)                                 
         LA    R4,TGNAME           R4=A(WHERE CURRENT ESTIMATE WILL BE)         
         ZIC   RE,FLTESTLN                                                      
         BCTR  RE,0                RE=L'INPUT-1                                 
         LA    RF,0(RE,R3)         RF=A(LAST CHARACTER INPUT)                   
         SPACE 1                                                                
         LA    R2,L'FLTEST         SET L'COMPARE                                
         CLI   0(RF),C'*'          IF LAST CHARACTER INPUT IS ASTERISK          
         BNE   *+10                                                             
         LR    R2,RE               L'COMPARE IS L'INPUT-1                       
         B     FINV10                                                           
         SPACE 1                                                                
         CLI   0(R3),C'*'          ELSE IF FIRST CHARACTER IS ASTERISK          
         BNE   FINV10                                                           
         LR    R2,RE               L'COMPARE IS L'INPUT-1                       
         LA    R3,1(R3)            SET TO FILTER FROM 2ND CHARACTER             
         SPACE 1                                                                
FINV10   SH    R2,=H'1'            SUBTRACT ONE FOR EXECUTED COMPARE            
         BM    FINVX               ONLY INPUT WAS '*' - DON'T FILTER            
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTEST  GET ESTIMATE NUMBER             
         BNE   FINVNO                           NONE FOUND - REJECT             
         SPACE 1                                                                
         CLI   FLTEST,C'*'         IF 1ST CHAR OF FILTER IS ASTERISK            
         BNE   FINV20                                                           
         L     RF,TGELEM           GET L'CURRENT ESTIMATE FROM EL.              
         ZIC   R1,1(RF)                                                         
         SH    R1,=AL2(TANUMBER+1-TANUD)  R1=L'CURRENT ESTIMATE NO. - 1         
         SR    R1,R2                                                            
         BM    FINVNO              L'FILTER GREATER THAN L'ESTIMATE             
         AR    R4,R1               FILTER END OF ESTIMATE ONLY                  
         SPACE 1                                                                
FINV20   EX    R2,*+8              COMPARE                                      
         B     *+10                                                             
         CLC   0(0,R4),0(R3)                                                    
         BNE   FINVNO                                                           
         SPACE 1                                                                
FINVX    B     YES                 OK - RETURN CC EQ                            
         SPACE 1                                                                
FINVNO   MVI   TIMODE,PROCNOCK     NOT OK - RETURN CC NE                        
         B     NO                                                               
         EJECT                                                                  
*              DRIVER INTERFACE ROUTINES                                        
         SPACE 2                                                                
INTDRIVE DS    0H                                                               
         MVI   BYTE,GLINIT                                                      
         B     ALLDRIVE                                                         
         SPACE 3                                                                
INDRIVE  DS    0H                                                               
         MVI   BYTE,GLINPUT                                                     
         B     ALLDRIVE                                                         
         SPACE 3                                                                
OUTDRIVE DS    0H                                                               
         MVI   BYTE,GLOUTPUT                                                    
         B     ALLDRIVE                                                         
         SPACE 3                                                                
*                                  BYTE=MODE                                    
ALLDRIVE NTR1                                                                   
         L     R2,AGLOBAL                                                       
         USING GLOBALD,R2                                                       
         MVC   GLMODE,BYTE                                                      
         CLI   GLMODE,GLINPUT        IF IN INPUT MODE                           
         BNE   ALLDR2                                                           
****     MVC   GLOPTS+19(1),         PASS RUNTIME OPTIONS- N/D                  
         MVC   GLOPTS+18(1),OKDETAIL                     - OK FOR DTL           
         MVC   GLOPTS+17(1),INVOICE                      - THIS IS INV          
ALLDR2   GOTO1 DRIVER,DMCB,(R2)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         L     R2,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
         MVC   WORK,0(R2)          MOVE LITERAL TO WORK                         
         LA    R4,WORK(R3)                                                      
         MVC   0(7,R4),=C' (AIO?)' ADD I/O AREA LITERAL                         
         LA    R3,7(R3)            BUMP L'LITERAL                               
         CLC   AIO,AIO1                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'1'          SET CURRENT I/O AREA LITERAL                 
         CLC   AIO,AIO2                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'2'                                                       
         CLC   AIO,AIO3                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'3'                                                       
         GOTO1 TRACE,DMCB,AIO,0,WORK,(R3)                                       
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE HANDLES STORAGE TRACES                                   
         SPACE 1                                                                
MYTRACE2 NTR1                                                                   
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R2,AGLOBAL                                                       
         USING GLOBALD,R2                                                       
         CLI   GLHOOK,GLPRINT      ABOUT TO PRINT A LINE                        
         BNE   HK4                                                              
         CLI   NOPRINT,C'Y'        IF SWITCH IS SET                             
         BNE   HKX                                                              
         MVI   GLHOOK,GLDONT       SET TO SUPPRESS THIS LINE                    
         MVI   NOPRINT,C'N'        AND RESET SWITCH                             
         B     HKX                                                              
         SPACE 1                                                                
HK4      CLI   GLHOOK,GLHEAD       PROCESSING HEADINGS                          
         BNE   HKX                                                              
         GOTO1 GENHEAD             TAREPGEN WILL HANDLE                         
HKX      B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
DEFTITLE DC    C'BILLED ACTIVITY REPORT'                                        
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
FLTESTLN DS    XL1                 L'ESTIMATE FILTER INPUT                      
FLTEST   DS    CL15                ESTIMATE FILTER                              
*                                                                               
DTLSTRT  DS    PL3                 START DATE FOR DETAIL                        
DTLEND   DS    PL3                 END DATE FOR DETAIL                          
*                                                                               
CALCCOMM DS    CL1                 Y=CALCULATE COMMISSION                       
CAPONLY  DS    CL1                 Y=PRINT RECAP ONLY                           
PRTCOMML DS    CL1                 Y=PRINT COMMLS INCLUDED SUMMARY              
CAPBASIS DS    CL1                 E=EST PD, B=BILL QTR, I=INV#, T=TOTS         
DTLBASIS DS    CL1                 O=ON/OFF, C=CAT,ON/OFF, I=INV                
*                                                                               
OKDETAIL DS    CL1                 Y=OK TO PROCESS DETAIL                       
INVOICE  DS    CL1                 Y=THIS IS INVOICE RECORD                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE3D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DRGLOBAL                                                                      
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067TAREP23   05/01/02'                                      
         END                                                                    
