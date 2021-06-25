*          DATA SET RESIN10S   AT LEVEL 091 AS OF 05/01/02                      
*PHASE T80610A,*                                                                
         TITLE 'T80610 - REPPAK SINGLE INVOICE (SIN) - CHANGE MODULE'           
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESIN10 --- REP SINGLE INVOICE - CHANGE MODULE             *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG23/89 (MRR) --- HISTORY LOST                                   *           
*                                                                   *           
* 31OCT89  (EFJ) --- FIX P3 IN RECUP CALL AND CK FOR ERRS ON RET    *           
*                                                                   *           
* 07NOV95  (RHV) --- ADD 'NET?' FIELD TO ALLOW AUTOMATIC            *           
*                    CALCULATION OF NET INVOICE AMOUNTS             *           
*                                                                   *           
* 15MAY96  (WSB) --- ADD 'CHADDS' ACTION FOR AUTOMATIC SCRIPTING TO *           
*                    MAINTAIN STA CONTROL REC BESIDES 'CHA' STUFF   *           
*                                                                   *           
* 20MAY96  (WSB) --- CHANGE RESINGEN TO ADD 600 BYTES OF LOCAL      *           
*                    WORKING STORAGE                                *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80610   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80610                                                         
         USING GENOLD,RC                                                        
         USING T806FFD,RA                                                       
         L     RC,0(R1)                                                         
         LA    R9,IOAREA           SET A(CONTRACT RECORD)                       
         AH    R9,=Y(RCONREC-IOAREA)                                            
*                                  DISPLACE TO CONTRACT RECORD                  
         USING RCONREC,R9                                                       
*                                                                               
         LA    R2,SINACTH                                                       
         EJECT                                                                  
* GET CONTRACT                                                                  
         MVC   KEY+28(4),TWAKADDR                                               
         BAS   RE,GETREC                                                        
         PRINT GEN                                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RCONREC                                     
         PRINT NOGEN                                                            
* CHECK IF K DATES SAME AS WHEN DISPLAYED (ANOTHER TERMINAL MAY CHANGE)         
         LA    R3,DISERR                                                        
         CLC   TWAKDTES,RCONDATE   DIFF. DATES?                                 
         BE    *+14                                                             
         XC    TWAKADDR,TWAKADDR                                                
         B     ERROR                                                            
*                                                                               
         BAS   RE,VNET             VALIDATE 'NET?' FIELD                        
         BZ    C050                ON NO ERROR                                  
         LA    R2,SINNETTH         POINT TO FLD HEADER                          
         LA    R3,02               ERR MSG                                      
         B     ERROR                                                            
*                                                                               
* PREPARE TO EDIT INVOICE AMOUNTS                                               
C050     LA    R2,SININ1H          FIRST INVOICE FIELD                          
         LA    R3,INVERR                                                        
* BUILD INVOICE ELEMENT                                                         
         MVC   WORK(2),=X'040A'                                                 
         MVC   WORK+4(2),THISWEEK  MONDAY DATE                                  
         LA    R5,TWAMONS          LIST OF 2-BYTE K YR-MONTHS                   
         SR    R4,R4                                                            
         EJECT                                                                  
* LOOP TO EDIT INVOICE AMT                                                      
C100     C     R2,LASTFLD          INPUT                                        
         BH    C550                                                             
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     FIELD LEN                                    
         CLC   0(2,R5),ACCMON      MUST BE LOWER THAN CURRENT                   
         BL    C110             ACCOUNTING MONTH                                
         CLC   RCONKREP,=C'ME'     SO MEEKER CAN ADD INVOICE DATA               
         BE    C110                                                             
         LA    R3,231                                                           
         B     ERROR                                                            
*                                                                               
* EDIT AMT                                                                      
C110     BAS   RE,NETCASH          NET CASH AMOUNT & VALIDATION ROUTINE         
         BZ    C115                ON NO ERROR                                  
         LA    R3,202              ERR MSG                                      
         B     ERROR                                                            
*                                                                               
* GET PREVIOUS INVOICE AMT FOR THIS MONTH                                       
C115     GOTO1 VGETMON,(R1),(4,RCONREC),(R5),FULL                               
         CLC   DUB(4),FULL                                                      
         BE    C500                                                             
* ADD DIFFERENCE TO CONREC                                                      
         MVI   BYTE4,1             WRITE SW                                     
         L     R6,DUB              NEW AMT                                      
         S     R6,FULL             SUBTRACT OLD INV AMT                         
         ST    R6,FULL                                                          
         MVC   WORK+6(4),FULL      AMT DIFFERENCE                               
         MVC   WORK+2(2),0(R5)     YR-MONTH                                     
*                                                                               
* CHECK CONREC FOR WEEK INV BUCKET                                              
         LA    R7,RCONELEM                                                      
*                                                                               
C200     CLC   WORK(6),0(R7)                                                    
         BL    ADDBUCK                                                          
         BE    ADDAMT                                                           
* NEXT ELEM                                                                     
         IC    R4,1(R7)                                                         
         LA    R7,0(R4,R7)         NEXT ELEM                                    
         CLI   0(R7),0             LAST ELEM?                                   
         BNE   C200                                                             
*ADD INV BUCKET TO CONREC                                                       
ADDBUCK  GOTO1 VRECUP,(R1),(2,RCONREC),WORK,(C'R',(R7))                         
         CLI   8(R1),C'R'                                                       
         BE    C500                                                             
         LA    R3,68               ERROR MSG NUMBER                             
         B     ERROR                                                            
* ADD INV AMT TO EXISTING INV BUCKET                                            
ADDAMT   MVC   FULL,6(R7)          OLD AMT                                      
         L     R8,FULL                                                          
         MVC   FULL,WORK+6         NEW AMT                                      
         A     R8,FULL                                                          
         ST    R8,FULL                                                          
         MVC   6(4,R7),FULL                                                     
         LTR   R8,R8               ZERO BUCKET                                  
         BNE   C500                                                             
* DELETE ZERO BUCKET                                                            
         GOTO1 VRECUP,(R1),(2,RCONREC),(R7),(C'R',(R7))                         
         CLI   8(R1),C'R'                                                       
         BE    *+12                                                             
         LA    R3,68                                                            
         B     ERROR                                                            
*                                                                               
* NEXT INV AMT FIELD                                                            
C500     IC    R4,0(R2)                                                         
         LA    R2,0(R4,R2)         NEXT INPUT FIELD                             
         IC    R4,0(R2)                                                         
         LA    R2,0(R4,R2)         NEXT MN                                      
         IC    R4,0(R2)                                                         
         LA    R2,0(R4,R2)         NEXT INV FIELD                               
         LA    R5,2(R5)            NEXT TWAMONS                                 
         CLI   0(R5),0             LAST MONTH?                                  
         BNE   C100                                                             
*                                                                               
*                                                                               
C550     CLI   BYTE4,1             WRITE SW                                     
         BNE   EXIT                                                             
* WRITE REC                                                                     
         MVC   RCONINVD,TODAY                                                   
         PRINT GEN                                                              
         GOTO1 VMOVEREC,(R1),RCONREC,IOAREA                                     
         PRINT NOGEN                                                            
         BAS   RE,PUTREC                                                        
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',0),F#TSTAT6    GET TSTAT6 FROM UTL          
         L     R1,0(R1)          LOAD ADDR OF BLOCK PASSED FROM GETFACT         
         TM    0(R1),TST6SCRP                 IS A SCRIPT RUNNING?              
         BO    C650                           YES                               
*                                                                               
* SEE IF 'CHADDS' ENTERED (USED FOR TESTING SCRIPTED REQUESTS)                  
         CLI   SINACTH+5,6         6 CHARACTERS ENTERED?                        
         BNE   EXIT                                                             
         CLC   =C'CHADDS',SINACT   ACTION 'CHADDS'?                             
         BNE   EXIT                                                             
*                                                                               
C650     BAS   RE,SCRIPTED         ROUTINE TO PROCESS SCRIPTED REQUESTS         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO MAINTAIN STATION CONTROL REC AND GENERATE TURN-AROUND              
* ENTRY FOR STATIONS BEING INVOICED AUTOMATICALLY VIA A SCRIPT                  
*                                                                               
SCRIPTED NTR1                                                                   
         THMS  DDSTIME=YES         GET CURRENT TIME                             
         ST    R0,TIME             TIME                                         
         ST    R1,FULL             DDS TIME OFFSET                              
         AP    TIME,FULL                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,DUB)                                       
*                                                                               
         CP    TIME,=PL4'240000'   PAST MIDNIGHT?                               
         BL    SCR80                                                            
*                                                                               
         SP    TIME,=PL4'240000'   YES, ADJUST                                  
         GOTO1 VADDAY,DMCB,DUB,DUB,F'1'                                         
*                                                                               
SCR80    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,DUB),(3,TDAY)                                    
*                                                                               
         LA    R1,SINMNTH          SAVE ADDRESS OF END                          
         LA    R2,SININ1H          FIRST INVOICE FIELD                          
*                                                                               
         LA    R3,0                NOTHING ENCOUNTERED YET                      
SCR100   CLI   5(R2),0             IS ANYTHING ENTERED?                         
         BE    *+6                 NO                                           
         LR    R3,R2               SAVE VALUE OF CURRENT FIELD                  
*                                                                               
         ZIC   R4,0(R2)                                                         
         AR    R2,R4               NEXT DIFF                                    
         ZIC   R4,0(R2)                                                         
         AR    R2,R4               NEXT MN                                      
*                                                                               
         CR    R2,R1               AT END OF SCREEN?                            
         BE    SCR120              YES                                          
*                                                                               
         ZIC   R4,0(R2)                                                         
         AR    R2,R4               NEXT INV FIELD                               
*                                                                               
         B     SCR100                                                           
*                                                                               
SCR120   DS    0H                                                               
         LTR   R3,R3               WAS ANYTHING ENTERED?                        
         BZ    EXIT                NO, GO OUT                                   
*                                                                               
         ZIC   R4,SINMN1H          TOTAL LENGTH OF MONTH FIELD                  
         SR    R3,R4               HEADER OF LAST MONTH WITH DATA               
         GOTO1 VDATVAL,DMCB,(2,8(R3)),WORK     LAST MONTH WITH DATA             
         MVC   WORK+4,=C'01'                                                    
         GOTO1 VDATCON,DMCB,WORK,(3,WORK+6)                                     
*                                                                               
         NI    FLAGS,X'FF'-X'80'   RECORD NOT FOUND YET                         
         XC    KEY,KEY                                                          
K        USING RCTLREC,KEY         FIND STATION CONTROL RECORD                  
         MVI   K.RCTLKTYP,RCTLKTYQ   X'21' RECORD                               
         MVC   K.RCTLKREP,RCONKREP                                              
         MVC   K.RCTLKSTA,RCONKSTA                                              
         DROP  K                                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     IS THE RECORD OUT THERE YET?                 
         BE    SCR150              YES                                          
*                                                                               
         XC    IOAREA(256),IOAREA  NO                                           
         MVC   IOAREA(27),KEYSAVE  MOVE NEW HEADER TO IOAREA                    
         MVI   IOAREA+28,34        MOVE IN RECORD LEN                           
         B     *+16                                                             
*                                                                               
SCR150   OI    FLAGS,X'80'         RECORD ALREADY EXISTS                        
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'        FIND UPLOAD ELEMENTS                         
         BAS   RE,GETEL                                                         
         BNE   SCR170              THERE ARE NONE, CREATE A NEW ONE             
UPEL     USING RCTLUPEL,R6                                                      
SCR160   CLC   UPEL.RCTLUPDT,TDAY    WAS THIS ONE CREATED TODAY?                
         BNE   *+14                  NO, FIND NEXT ONE                          
         CLC   UPEL.RCTLUPYM,WORK+6  YES, IS IT FOR CURRENT DATA MON?           
         BE    SCR200                YES, DON'T NEED TO CREATE NEW ONE          
         DROP  UPEL                                                             
         BAS   RE,NEXTEL           ANOTHER ELEMENT?                             
         BE    SCR160              YES                                          
*                                                                               
SCR170   DS    0H                                                               
         XC    WORK2,WORK2         CREATE NEW UPLOAD ELEMENT                    
UPEL     USING RCTLUPEL,WORK2                                                   
         MVI   UPEL.RCTLUPEC,RCTLUPEQ   X'20' ELEMENT                           
         MVI   UPEL.RCTLUPLN,RCTLUPX    ELEMENT LENGTH                          
         MVC   UPEL.RCTLUPYM,WORK+6     DATA YR/MO                              
         MVC   UPEL.RCTLUPDT,TDAY       TODAY'S DATE                            
         MVC   UPEL.RCTLUPTM,TIME       CURRENT TIME                            
         DROP  UPEL                                                             
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,WORK2,=C'ADD=CODE'         
*                                                                               
SCR200   DS    0H                                                               
         NI    FLAGS,X'FF'-X'40'   DON'T GENERATE '16' TA REQ YET               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'30'        FIND INVOICE ELEMENTS                        
         BAS   RE,GETEL                                                         
         BNE   SCR230              NO INVOICE ELEMENTS, CREATE NEW ONE          
IVEL     USING RCTLIVEL,R6                                                      
SCR220   CLC   IVEL.RCTLIVYM,WORK+6  IS THIS ONE FOR CURRENT DATA MON?          
         BE    SCR260                YES, DON'T CREATE NEW ONE                  
         DROP  IVEL                                                             
         BAS   RE,NEXTEL           ANOTHER ELEMENT?                             
         BE    SCR220              YES                                          
*                                                                               
SCR230   DS    0H                                                               
         OI    FLAGS,X'40'         GENERATE '16' REPORT                         
         XC    WORK2,WORK2         CREATE NEW INVOICE ELEMENT                   
IVEL     USING RCTLIVEL,WORK2                                                   
         MVI   IVEL.RCTLIVEC,RCTLIVEQ   X'30' ELEMENT                           
         MVI   IVEL.RCTLIVLN,RCTLIVX    ELEMENT LENGTH                          
         MVC   IVEL.RCTLIVYM,WORK+6     DATA YR/MO                              
         MVC   IVEL.RCTLIVDT,TDAY       TODAY'S DATE                            
         MVC   IVEL.RCTLIVTM,TIME       CURRENT TIME                            
         DROP  IVEL                                                             
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,WORK2,=C'ADD=CODE'         
*                                                                               
SCR260   DS    0H                                                               
         TM    FLAGS,X'80'         DOES RECORD EXIST YET?                       
         BO    *+12                YES                                          
*                                                                               
         BAS   RE,ADDREC           NO                                           
         B     *+8                                                              
*                                                                               
         BAS   RE,PUTREC                                                        
*                                                                               
         TM    FLAGS,X'40'         GENERATE '16' REPORT?                        
         BZ    *+8                 NO                                           
*                                                                               
         BAS   RE,TA16REP          YES                                          
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* ROUTINE TO GENERATE '16' REPORT TURNAROUND REQUEST                            
*                                                                               
TA16REP  NTR1                                                                   
* CREATE REQUEST HEADER                                                         
TA25     DS    0H                                                               
         XC    WORK2(26),WORK2                                                  
RQHI     USING RQHIUSR,WORK2                                                    
         MVI   RQHI.RQHNUMB,16     REQUEST NUMBER                               
         MVI   RQHI.RQHORIG+1,106  ORIGIN ID NUM (FOR PRINTING AT DDS)          
         DROP  RQHI                                                             
*                                                                               
* FIND WHETHER REPORT GOES TO REP'S PRINT QUEUE OR SHOULD BE PRINTED            
* AT DDS AND DELIVERED                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'            LOOK UP ID RECORD ON CONTROL FILE            
TWA      USING TWAD,RA                                                          
         MVC   KEY+23(2),TWA.TWAUSRID     USER ID NUMBER                        
         DROP  TWA                                                              
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,IOAREA                   
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         LA    R5,IOAREA                                                        
         CLC   KEY(25),0(R5)       CHECK THE KEY                                
         BE    *+6                                                              
         DC    H'0'                ID RECORD NOT THERE--DIE                     
         LA    R5,28(R5)                                                        
TA40     EQU   *                                                                
         CLI   0(R5),X'42'         OUTPUT TYPE ELEMENT?                         
         BE    TA45                YES                                          
         ZIC   R0,1(R5)            NO, BUMP TO NEXT ELEMENT                     
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF RECORD?                               
         BNE   TA40                NO                                           
         B     TA60                YES, NO ELEMENT, PRINT AT DDS                
TA45     EQU   *                                                                
         CLI   6(R5),C'#'          SEND TO PQ?                                  
         BNE   TA60                NO, PRINT AT DDS                             
*                                                                               
* PUT T/A TO REP'S PQ                                                           
RQHI     USING RQHIUSR,WORK2                                                    
         MVC   RQHI.RQHOUT,=C'DIRECT'                                           
TWA      USING TWAD,RA                                                          
         MVC   RQHI.RQHDEST,TWA.TWAUSRID    DESTINATION                         
         MVC   RQHI.RQHORIG,TWA.TWAUSRID    ORIGIN                              
         DROP  TWA                                                              
*                                                                               
* THE RQHFLNK BIT CAUSES DATA MANAGER NOT TO OVERRIDE ORIGIN ID                 
* WE NEED THIS SINCE WE WANT TO 'LUMP' THE REQUESTS TOGETHER BEFORE             
* DIRECTING THEM TO THE REP'S PRINT QUEUE                                       
*                                                                               
         OI    RQHI.RQHFLAG,RQHFLNK+RQHFTWO                                     
         DROP  RQHI                                                             
*                                                                               
* FILL IN REQUEST CARD                                                          
TA60     DS    0H                                                               
         MVI   WORK2+26,C' '                                                    
         MVC   WORK2+27(79),WORK2+26     CLEAR OUT REQUEST CARD                 
*                                                                               
REQ      USING REQD,WORK2+26                                                    
         MVC   REQ.QPROG,=C'16'    '16' REPORT                                  
*                                                                               
TA78     DS    0H                                                               
         MVC   REQ.QREP,REPALPHA                                                
* FIND ORIGINATING OFFICE                                                       
         MVC   REQ.QREQOFF,RCONKOFF                                             
*                                                                               
         LR    R3,RA                                                            
         USING TWAD,R3                                                          
         CLC   =C'O=',TWAACCS                                                   
         BNE   *+10                                                             
         MVC   REQ.QREQOFF,TWAACCS+2   ORIGINATING OFFICE                       
         DROP  R3                                                               
*                                                                               
TA150    MVC   REQ.QUESTOR,RCONBUYR                                             
         MVI   REQ.QUESTOR+11,C'*'  DON'T ADD 'CLOSED' COMMENT TO CONTR         
         MVC   REQ.QSTATION,RCONKSTA                                            
         MVC   REQ.QGROUP(2),RCONKGRP                                           
         MVC   REQ.QSTART(4),WORK  START DATE (LAST DATA MONTH)                 
         MVC   REQ.QEND(4),WORK    END DATE (LAST DATA MONTH)                   
         MVI   REQ.QSEQ,C'S'                                                    
         MVI   REQ.QACCTOPT,C'P'                                                
         MVI   REQ.QOPTION1,C'U'   UPDATE                                       
         MVI   REQ.QOPTION2,C'D'   DON'T CLEAR INVOICE DOLLARS                  
         MVI   REQ.QOPTION3,C'N'                                                
         DROP  REQ                                                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REPREQ',WORK2,WORK2,         X        
               (TERMNAL,0)                  ADD REQUEST                         
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* ROUTINE TO VALIDATE CASH VALUES W/TRAILING 'G' GROSS OR 'N' NET               
*                                                                               
NETCASH  NTR1                                                                   
         GOTO1 VCASHVAL,(R1),8(R2)                                              
         TM    4(R2),X'80'         FIELD UPDATED?                               
         BO    NC5                                                              
         MVC   DUB(4),DMCB+4                                                    
         B     NCYES                                                            
NC5      CLI   DMCB,X'FF'          FIELD IS ALL NUMERIC?                        
         BNE   NC20                                                             
         ZIC   R4,5(R2)            INPUT LENGTH                                 
         BCTR  R4,0                INPUT LEN - 1                                
         LA    R5,8(R2)                                                         
         AR    R5,R4               LAST CHAR OF INPUT                           
         CLI   0(R5),C'N'                                                       
         BNE   NC10                                                             
         TM    SINNETTH+4,X'08'                                                 
         BNO   NC8                                                              
         LH    R7,OPTNET$$                                                      
         B     NC15                                                             
NC8      LH    R7,=H'85'           DEFAULT 85%                                  
         B     NC15                                                             
NC10     CLI   0(R5),C'G'                                                       
         BNE   NCNO                                                             
         LH    R7,=H'100'                                                       
NC15     ST    R4,4(R1)            FIELD LEN - 1 IN DMCB                        
         GOTO1 VCASHVAL,(R1),8(R2)                                              
         CLI   DMCB,X'FF'          ARE ALL BUT LAST CHAR NUMERIC?               
         BE    NCNO                                                             
         B     NC30                                                             
NC20     LH    R7,OPTNET$$                                                      
NC30     L     R5,4(R1)            BINARY DOLLAR AMT                            
         SR    R4,R4                                                            
         M     R4,=F'100'          X 100                                        
         DR    R4,R7               DIV BY %                                     
         SR    R4,R4                                                            
         CVD   R5,DUB              NET AMT IN DUB                               
         SRP   DUB(8),64-2,5                                                    
         SRP   DUB(8),2,0                                                       
         CVB   R5,DUB                                                           
         XC    DUB,DUB                                                          
         ST    R5,DUB                                                           
NCYES    SR    R1,R1                                                            
         B     *+8                                                              
NCNO     LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
         XIT1                                                                   
*                                                                               
* ROUTINE TO VALIDATE 'NET?' FIELD                                              
*                                                                               
VNET     NTR1                                                                   
         LA    R2,SINNETTH         NET? FIELD                                   
         CLI   5(R2),0             ZERO LEN?                                    
         BNE   VN10                                                             
         MVC   OPTNET$$,=H'100'    DEFAULT TO GROSS AMT (100%)                  
         B     VNYES                                                            
VN10     CLI   8(R2),C'N'          CASE OF 'N'                                  
         BNE   VN20                                                             
         MVC   OPTNET$$,=H'100'    GROSS AMT                                    
         B     VNYES                                                            
VN20     CLI   8(R2),C'Y'          CASE OF 'Y'                                  
         BNE   VN30                                                             
         MVC   OPTNET$$,=H'85'     DEFUALT NET AMT (85%)                        
         B     VNYES                                                            
VN30     TM    4(R2),X'08'         NUMERIC ENTRY?                               
         BZ    VNNO                ERROR                                        
VN50     PACK  DUB,8(2,R2)         CONVERT INPUT                                
         CVB   R4,DUB                                                           
         STH   R4,OPTNET$$                                                      
VNYES    SR    R1,R1                                                            
         B     *+8                                                              
VNNO     LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL R6,34,ELCODE                                                     
ELCODE   DS    X                                                                
       ++INCLUDE RESINGEN                                                       
GENOLD   DSECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENCTL                                                       
*                                                                               
         ORG   LOCALVAR                                                         
         DS    0F                  FULLWORD ALIGNMENT                           
TIME     DS    PL4                 CURRENT TIME                                 
TDAY     DS    XL3                 TODAY'S DATE - YMD BINARY                    
FLAGS    DS    X                   X'80'=RECORD ALREADY EXISTS                  
*                                  X'40'=GENERATE REPORT TURNAROUND REQ         
*                                                                               
REQD     DSECT                                                                  
       ++INCLUDE REGENREQ                                                       
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
*                                                                               
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
*                                                                               
       ++INCLUDE FATWA                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091RESIN10S  05/01/02'                                      
         END                                                                    
