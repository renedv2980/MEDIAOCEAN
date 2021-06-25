*          DATA SET SRRMQ00    AT LEVEL 004 AS OF 01/04/10                      
*PHASE T17400A                                                                  
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
* JUL01/07 (BU ) --- ORIGINAL ENTRY: CLONED FROM SRDXM00 (W HOA)      *         
* JUL12/07 (BU ) --- ADD PROFILE FOR STATION CLOSE DATA               *         
* AUG22/08 (KUI) --- UPDATE AMOUNTS INSTEAD OF ADDING FOR REPEATED    *         
*                    UPLOADS                                          *         
* OCT27/08 (KUI) --- UPDATE AMOUNTS ALWAYS OVERWRITE ENTIRE MONTH     *         
* AUG03/09 (KUI) --- CLEAR HELLO CALL PARAMETER BUG                   *         
* DEC07/09 (RCRI)--- REMOVE REFERENCES TO FACIDTAB                    *         
* JAN04/10 (KUI) --- COMMENT OUT SNDERROR FOR NOW                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T17400   TITLE 'SRRMQ00 ($RMQ) - REP MQ UPDATE FACILITY'                        
T17400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$RMQ**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC = A(WORKING STORAGE)                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         MVC   SRPARMS(8*4),0(R1)  SAVE SERVICE REQUEST PARAMETER LIST          
SRPARMSD USING SRPARMD,SRPARMS                                                  
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9          R9 = A(SYSTEM FACILITIES)                    
         L     RF,VSYSFAC2         GET THE SPOT SYSFAC                          
*                                  USE SPOT SYSFAC EVEN THOUGH REP              
         USING SPSYSFAC,RF                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RF                                                               
*                                                                               
         MVC   AUTL,SRPARMSD.SRQAUTL                                            
         L     RF,AUTL                                                          
         MVC   ATBUFF,TBUFF-UTLD(RF)  GETTING OUR MSG FROM TSAR BUFFER          
         L     RF,ATBUFF                                                        
         SHI   RF,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(RF)          GET THE LENGTH OF THE MESSAGE                
         STCM  RE,3,MQMSGLEN                                                    
         CHI   RE,4064             IF L(MESSAGE) > 4064                         
         BL    MAIN10                                                           
*                                                                               
*    LARGER RECORDS ARE BUFFERED IN 31-BIT ADDRESS SPACE.                       
*                                                                               
**** SWITCH INTO XA MODE (31-BIT ADDRESSING)                                    
         BRAS  RE,ON31                                                          
**** GRAB ADDRESS FROM TCBRBUFF                                                 
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         L     RF,SSBTKADR                                                      
         MVC   ATBUFF,TCBRBUFF-TCBD(RF)  SAVE "REAL" ADDRESS OF TBUFF           
         DROP  R8                                                               
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
         BRAS  RE,OFF31                                                         
*                                                                               
MAIN10   BAS   RE,INITLIZE         INITIALIZE COMMON VARIABLES                  
*                                                                               
         BAS   RE,PROCTYPE         PROCESS WHAT WAS PASSED IN TBUFF             
*                                                                               
YES      SR    RC,RC               SET CC TO EQ                                 
NO       LTR   RC,RC               SET CC TO NEQ                                
XIT      XIT1                      RETURN TO CALLER                             
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZES COMMON VARIABLES                                                  
***********************************************************************         
INITLIZE NTR1                                                                   
         MVI   BITFLAG1,0                                                       
         MVC   DATADISP,=H'34'                                                  
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA1-WORKD                                                    
         ST    R7,AIO1                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA2-WORKD                                                    
         ST    R7,AIO2                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,SPULAREA-WORKD                                                
         ST    R7,ASPLAREA                                                      
*                                                                               
         LR    R7,RC               FOR SAVING MY MESSAGES                       
         AHI   R7,WRKRIOA-WORKD                                                 
         ST    R7,AWRKRIOA                                                      
         LR    RE,R7               NOTHING IN IT YET EXCEPT L(LENGTH)           
         LH    RF,=Y(WRECQLNQ)                                                  
         XCEFL                                                                  
         L     RE,AWRKRIOA                                                      
         MVI   1(RE),2                                                          
*                                                                               
         MVC   AWRKRBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LR    R7,RC               FOR EDICT                                    
         AHI   R7,HUGEBLCK-WORKD                                                
         ST    R7,AHUGEBLK                                                      
*                                                                               
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   SYSN1,SSBSYSN1                                                   
         MVC   RECLEN,SSBTWAL      SAVE TEMPSTR TWA RECORD LENGTH               
         DROP  R8                                                               
*                                                                               
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         MVC   TERMNUM,TNUM        SAVE TERMINAL NUMBER                         
         MVC   USERNUM,TUSER       SAVE USER ID NUMBER                          
         XC    TUSER,TUSER         DUMMY TERMINALS SHOULD NOT HAVE ID #         
         DROP  R1                                                               
***************                                                                 
* COMFACS STUFF                                                                 
***************                                                                 
         L     R1,SRPARMSD.SRQACOMF    R1 = A(COMFACS)                          
         USING COMFACSD,R1                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VSWITCH,CSWITCH                                                  
         DROP  R1                                                               
***************                                                                 
* CORERES STUFF                                                                 
***************                                                                 
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4,=X'D9000A0C'    SPOOL                                     
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASPOOL,DMCB                                                      
*                                                                               
         MVI   DMCB+7,X'15'           CLUNPK                                    
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VCLUNPK,DMCB                                                     
*                                                                               
         MVI   DMCB+7,X'AC'           REPFACS                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   AREPFACS,DMCB                                                    
***************                                                                 
* GET TODAY'S DATE                                                              
***************                                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,BTODAY)                                 
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    INIT10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
*                                                                               
INIT10   GOTO1 VDATCON,DMCB,(3,BTODAY),(15,JDTTODAY)                            
*                                                                               
INITX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PUTS THE MESSAGE POINTED BY R7 WHOSE LENGTH IS IN R8 INTO AIO1                
*                                                                               
* ON ENTRY:    (R7)                A(MESSAGE)                                   
*              (R8)                L(MESSAGE) SAY N                             
*                                                                               
* ON EXIT:     CLOBBERING AIO1                                                  
*              BYTE  0             L(MESSAGE + 1) = N+1                         
*              BYTES 1-(N)         MESSAGE                                      
***********************************************************************         
PUTINIOA NTR1                                                                   
         L     R6,AIO1                                                          
         LR    R1,R8               L'MSG                                        
         LA    R1,1(R1)            +1 TO INCL LENGTH                            
         STC   R1,0(R6)                                                         
         SHI   R1,2                L'MSG-1                                      
         EX    R1,*+8                                                           
         J     YES                                                              
         MVC   1(0,R6),0(R7)                                                    
         SPACE 2                                                                
***********************************************************************         
* DETERMINES WHICH TYPE OF I/P RECORD WE HAVE AND THEN ROUTES IT TO             
* THE CORRECT PROCESSING ROUTINE                                                
***********************************************************************         
PROCTYPE NTR1                                                                   
         CLC   MQMSGLEN,=AL2(4064) NOT IN TBUFF OF UTL?                         
         BNH   PTYP0020                                                         
*                                                                               
*   MESSAGE EXCEEDS 4064:  WILL BE ADDRESSED AS 31-BIT                          
*                                                                               
**** SET 31-BIT ADDRESSING                                                      
         BRAS  RE,ON31                                                          
*****                                                                           
* PUT 100 SPACES AFTER THE MESSAGE SO WE DON'T PROCESS AS IF THERE WAS          
* RELEVANT DATA THERE (IE: DLNNOT FOR A MG WHEN IT IS A REGULAR DLNNOT)         
*****                                                                           
PTYP0020 L     R7,ATBUFF                                                        
         SHI   R7,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(R7)          LENGTH OF THE MSG IN BUFFER                  
         LA    RE,2(R7,RE)         POINT AFTER THE MESSAGE                      
         XC    0(255,RE),0(RE)                                                  
*****                                                                           
         L     R7,ATBUFF                                                        
         ST    R7,SAVATBUF         SAVE A(REPRMQ)                               
*                                                                               
         LR    R6,R7               R6 = A(I/P REC W/O REPRMQ  INFO)             
         AHI   R6,8                BUMP PAST THE "REPRMQ  "                     
         LA    R1,RMQOBJS                                                       
RMQ      USING RMQOBJD,R1                                                       
*                                                                               
PTYP0040 EQU   *                                                                
         CLI   RMQ.RMQOTID,0       END OF TABLE REACHED?                        
         BE    PTYP0900            YES - EXIT NO FIND                           
*                                                                               
         CLC   RMQ.RMQOTID,0(R6)   ACTION INDICATOR FOUND IN TABLE?             
         BE    PTYP0060            YES                                          
         LA    R1,RMQ.RMQONXT      NO  - SET NEXT SLOT                          
         B     PTYP0040            GO BACK FOR NEXT                             
PTYP0060 EQU   *                                                                
         ZIC   RF,RMQ.RMQORIDN                                                  
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     PTYP0800(RF)                                                     
*                                                                               
PTYP0800 B     PROCINV             PROCESS INVOICE DATA                         
*                                                                               
PTYP0900 J     NO                                                               
*                                                                               
RMQOBJS DC     CL8'INV    ',AL1(RMQINVQ)                                        
RMQOBJL  EQU   *-RMQOBJS                                                        
         DC    X'00'                                                            
         DROP  RMQ                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE INVOICE ACTION                                                  
*    VALIDATES DATA ON INPUT                                                    
*    VALIDATES POWER CODE ENTERED                                               
*    VALIDATES EXISTENCE OF CONTRACT RECORD                                     
*    IF ALL VALIDATION SUCCESSFUL                                               
*       RETRIEVES CONTRACT RECORD                                               
*       BUILDS NEW INVOICE ELEMENT                                              
*       UPDATES CONTRACT RECORD                                                 
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCINV  DS    0H                                                               
         USING RMQRSVID,R7                                                      
*                                                                               
*   SET RMQRIREP AS SEARCH FOR SYSNUM                                           
*                                                                               
         MVC   POWERCDE,RMQRIREP   SET INCOMING REP POWER CODE                  
         BRAS  RE,SWTCHREP         SWITCH CONTROL TO REP SYSTEM                 
         BNE   PINVNO              ERROR RETURN                                 
*                                                                               
*   VALIDATE CALENDAR TYPE INPUT: 2 CHARS                                       
*                                                                               
         CLC   RMQRITYP,=C'BR'     BROADCAST?                                   
         BE    PINV0010                                                         
         CLC   RMQRITYP,=C'CO'     CORPORATE?                                   
         BE    PINV0010                                                         
         LHI   R1,REPCALNV         CALENDAR TYPE NOT VALID                      
*        BRAS  RE,SNDERROR                                                      
*                                                                               
*   VALIDATE CONTRACT NUMBER INPUT: 8 CHARS, 0-9                                
*                                                                               
PINV0010 EQU   *                                                                
         LA    R0,8                SET LOOP CONTROL                             
         LA    R1,RMQRICON         SET A(CONTRACT NUMBER)                       
PINV0020 EQU   *                                                                
         CLI   0(R1),C'0'          MUST BE 8 CHARS, F'0'-F'9'                   
         BL    PINV0040            INVALID                                      
         CLI   0(R1),C'9'                                                       
         BH    PINV0040                                                         
         LA    R1,1(R1)            BUMP TO NEXT FIELD                           
         BCT   R0,PINV0020         GO BACK FOR NEXT                             
         B     PINV0060            NO INVALID CHARACTERS                        
PINV0040 EQU   *                                                                
         LHI   R1,REPCONNV         REP CONTRACT NOT VALID                       
*        BRAS  RE,SNDERROR                                                      
         B     PINVNO              EXIT WITH ERROR                              
PINV0060 EQU   *                                                                
*                                                                               
*   VALIDATE INVOICE AMOUNT INPUT: 12 CHARS, 0-9                                
*                                                                               
         LA    R0,12               SET LOOP CONTROL                             
         LA    R1,RMQRIDOL         SET A(INVOICE AMOUNT)                        
PINV0080 EQU   *                                                                
         CLI   0(R1),C'0'          MUST BE 12 CHARS, F'0'-F'9'                  
         BL    PINV0100            INVALID                                      
         CLI   0(R1),C'9'                                                       
         BH    PINV0100                                                         
         LA    R1,1(R1)            BUMP TO NEXT FIELD                           
         BCT   R0,PINV0080         GO BACK FOR NEXT                             
         B     PINV0120            NO INVALID CHARACTERS                        
PINV0100 EQU   *                                                                
         LHI   R1,INVDOLNV         INVOICE DOLLARS NOT VALID                    
*        BRAS  RE,SNDERROR                                                      
         B     PINVNO              EXIT WITH ERROR                              
PINV0120 EQU   *                                                                
         PACK  DUB,RMQRIDOL        PACK 12 CHARS INTO DUB                       
         CVB   R0,DUB              CONVERT PACKED TO BINARY                     
         ST    R0,INVAMT           SAVE INVOICE AMOUNT                          
*                                                                               
*   VALIDATE MONTH OF SERVICE INPUT: YYMM                                       
*                                                                               
         MVC   WORK(2),RMQRIMOS+2  LOAD MONTH OF SERVICE                        
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),=C'01'    INSERT DAY OF SERVICE                        
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),RMQRIMOS  LOAD YEAR  OF SERVICE                        
         GOTO1 VDATVAL,DMCB,(0,WORK),(X'80',WORK+12)                            
*                                  RETURN YYMMDD EBCDIC                         
*                                                                               
         OC    DMCB,DMCB           VALID DATE?                                  
         BNZ   PINV0140            YES                                          
         LHI   R1,MOSDATNV         MONTH OF SVC DATE NOT VALID                  
*        BRAS  RE,SNDERROR                                                      
         B     PINVNO              EXIT WITH ERROR                              
PINV0140 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(0,WORK+12),(3,INVMOS)                              
*                                  CONVERT MON OF SVC TO BINARY                 
         GOTO1 VDATCON,DMCB,(5,0),(2,INVACTDT)                                  
*                                  SET ACTIVITY DATE TO COMPRESSED              
*                                                                               
                                                                                
*   ALL VALIDATION SUCCESSFUL.  RETRIEVE THE CONTRACT                           
*                                                                               
         GOTO1 VHEXIN,DMCB,RMQRICON,WORK+32,8                                   
*                                                                               
         XC    KEY,KEY             BUILD 8C KEY FROM CON# IN MESSAGE            
         LA    R4,KEY                                                           
*                                                                               
         MVI   0(R4),X'8C'                                                      
         MVC   21(04,R4),RMQRIREP                                               
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),WORK+32(4)  INPUT CON# FROM MQ MESSAGE                   
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R4),WORK                                                    
*                                                                               
         BRAS  RE,HIGHREP                                                       
         CLC   KEY(27),KEYSAVE     CONTRACT FOUND?                              
         BE    PINV0180            YES                                          
         LHI   R1,REPCONNF         NO  - NOT FOUND                              
*        BRAS  RE,SNDERROR                                                      
         B     PINVNO              EXIT WITH ERROR                              
PINV0180 EQU   *                                                                
*                                                                               
* READ ORDER RECORD FOR UPDATE                                                  
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GETREP           READ REP RECORD                              
*                                                                               
         L     R1,AIO1             SET A(CONTRACT RECORD AREA)                  
         USING RCONREC,R1                                                       
         MVC   SAVSTATN,RCONKSTA   SAVE STATION CALL LETTERS                    
*                                                                               
         DROP  R1                                                               
*                                                                               
*   BUILD AN INVOICE (X'04') ELEMENT                                            
*                                                                               
         XC    ELEM,ELEM           CLEAR ELEMENT                                
         LA    R2,ELEM                                                          
         USING RCONSTEL,R2                                                      
         MVC   RCONSTCO(2),=X'040A'                                             
         CLC   RMQRITYP,=C'CO'     CORPORATE/ALTERNATE CALENDAR?                
         BNE   PINV0190                                                         
         MVI   RCONSTCO,X'54'                                                   
PINV0190 EQU   *                                                                
         MVC   RCONSTYR(2),INVMOS  INSERT MONTH OF SERVICE                      
         MVC   RCONSTWK,INVACTDT   INSERT ACTIVITY DATE                         
         MVC   RCONSTAM,INVAMT     INSERT INVOICE AMOUNT (W/PENNIES)            
*                                                                               
* REMOVE ANY BUCKETS FOR THIS MONTH OF SERVICE                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(RCONSTCO,AIO1),         X        
               (2,ELEM+2)                                                       
*&&DO                                                                           
         L     R6,AIO1                                                          
         MVC   ELCODE,RCONSTCO                                                  
         BAS   RE,GETEL            ANY INVOICE BUCKETS?                         
         BNE   PINV0400                                                         
         CLC   RCONSTYR(2),2(R6)   YES, SAME MONTH AND YEAR?                    
         BNE   PINV0400                                                         
*                                                                               
* CONTRACT HAS EXISTING BUCKETS FOR THIS MONTH/YEAR. SO WE NEED TO              
* ADD A NEW BUCKET WITH THE DIFFERENCE                                          
*                                                                               
         SR    R4,R4               TOTAL DOLLARS FROM OTHER ACT DATES           
PINV0200 EQU   *                                                                
         L     R5,6(R6)            CALCULATE TOTAL FOR THIS MONTH               
         AR    R4,R5                                                            
         BAS   RE,NEXTEL                                                        
         BNE   PINV0300                                                         
         CLC   RCONSTYR(2),2(R6)   YES, SAME MONTH AND YEAR?                    
         BE    PINV0200                                                         
*                                                                               
PINV0300 EQU   *                                                                
         L     R5,RCONSTAM         ADJUST AMOUNT                                
         SR    R5,R4                                                            
         STCM  R5,15,RCONSTAM                                                   
*                                                                               
PINV0400 EQU   *                                                                
*&&                                                                             
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),AIO,ELEM,0                        
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(27),0(R6)       MOVE REC TO KEY FOR CLARITY                  
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUTREP           REWRITE RECORD                               
         BRAS  RE,CLOSDATE         CHECK UPDATE FOR STATION CLOSE DATE          
         J     YES                                                              
PINVNO   EQU   *                                                                
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* SWITCHES CONTROL TO THE CORRECT REP SYSTEM                                    
*                                                                               
* ON ENTRY:    R7                  A(INVOICE RECORD W/O HDR INFO)               
*              USERID              SET TO POWERCODE OF REP AGENCY               
*                                                                               
* ON EXIT:     REPSENUM            REP'S SENUM                                  
***********************************************************************         
SWTCHREP NTR1                                                                   
         USING RMQRSVID,R7                                                      
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         JNE   EXITPRG             EXIT PROGRAM, SHOULD WAIT FOR IT             
*                                                                               
         XC    KEY,KEY             LOOK FOR THE USER ID RECORD                  
         LA    R4,KEY                                                           
         USING CT9BREC,R4                                                       
         MVI   CT9BKTYP,CT9BKTYQ   SET    KEY TYPE '9B'                         
         MVI   CT9BKSUB,CT9BKS01   SET SUBKEY TYPE '01'                         
         MVC   CT9BKAGY,POWERCDE   SET POWERCODE OF REP                         
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CT9BREC,R6                                                       
*                                                                               
         CLC   CT9BKEY(4),KEY      POWER CODE ON FILE?                          
         BE    SWREP00             YES                                          
         LHI   R1,REPIDNG          REP ID NOT VALID                             
SWREPERR EQU   *                                                                
*        BRAS  RE,SNDERROR                                                      
         B     SWREPNO                                                          
*                                                                               
SWREP00  EQU   *                                                                
         MVC   HALF,CT9BKNUM       SAVE ID NUMBER                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             LOOK FOR THE USER ID RECORD                  
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ    SET    KEY TYPE 'I'                          
         MVC   CTIKNUM,HALF        SET ID OF REP                                
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1            RETRIEVE CONTROL USER ID RECORD              
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
*                                                                               
         CLC   CTIREC(25),KEY      USER ID RECORD ON FILE?                      
         BE    SWREP02             YES                                          
         LHI   R1,REPNUSER         USER ID NOT ON FILE                          
         B     SWREPERR                                                         
SWREP02  EQU   *                                                                
         SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
         LHI   R1,REPIDNO2         SET 'NO DESCRIPTION ELEMENT'                 
         MVI   ELCODE,2            DESCRIPTION ELEM FOR ID NUM                  
         BRAS  RE,FIRSTEL                                                       
         BNE   SWREPERR                                                         
*                                                                               
         NI    BITFLAG1,X'FF'-BF1PSSWD   PASSWORD NOT REQUIRED YET              
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,7                                                         
         BRAS  RE,FIRSTEL                                                       
         BNE   SWREP05             NOT REQUIRED IF NO X'07' ELEM                
         TM    2(R6),X'80'         PASSWORD REQUIRED?                           
         BZ    *+8                                                              
         OI    BITFLAG1,BF1PSSWD                                                
*                                                                               
SWREP05  L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         LHI   R1,REPIDN21         SET 'NO REP SYSTEMS NUMBER '                 
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,FIRSTEL                                                       
         BNE   SWREPERR            ERROR IF WE CAN'T FIND REP SYS ELEM          
         USING CTSYSD,R6                                                        
SWREP10  CLI   CTSYSNUM,8          REP SYSTEM?                                  
         BE    SWREP15                                                          
         BRAS  RE,NEXTEL                                                        
         BE    SWREP10                                                          
         B     SWREPERR                                                         
*                                                                               
SWREP15  MVC   REPSENUM,CTSYSSE                                                 
         DROP  R6                                                               
*                                                                               
* CHECK IF FILE IS READ-ONLY. IF SO, POST ERROR AND CONSOLE WARNING             
*                                                                               
         L     R1,VSELIST                                                       
         ZICM  RE,0(R1),2          SET UP BXLE USING (RE,RF)                    
         ICM   RF,15,2(R1)                                                      
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
*                                                                               
SWREP30  CLC   SESYS,REPSENUM                                                   
         BNE   SWREP50                                                          
         TM    SEIND,SEISETRO+SEIRONLY    READ-ONLY STATUS?                     
         BZ    SWREP60                                                          
*                                                                               
         MVC   DUPMESS,RMQRINV     WRITE OUT TO CONSOLE                         
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',('DUPMSGQ',DUPMSG)                       
         B     SWREPNO                                                          
*                                                                               
         LHI   R1,RDONLYER         FILE IS READ-ONLY                            
         B     SWREPERR                                                         
*                                                                               
SWREP50  BXLE  R1,RE,SWREP30       CHECK NEXT ENTRY IN SELIST                   
         LHI   R1,FILENTFD         FILE NOT FOUND                               
         B     SWREPERR                                                         
*                                                                               
SWREP60  MVC   DMCB(1),REPSENUM    SWITCH TO REP  SYSTEM                        
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BE    SWREP70                                                          
         CLI   DMCB+4,2            SWITCHED, BUT SYSTEM NOT OPENED?             
         JE    EXITPRG             YES, SHOULD WAIT UNTIL OPENED                
         LHI   R1,REPIDNAU         SET 'NOT AUTHORIZED        '                 
         B     SWREPERR            USER NOT AUTHORIZED                          
SWREP70  EQU   *                                                                
*                                                                               
SWREPYES J     YES                                                              
*                                                                               
SWREPNO  J     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* SWITCHES SYSTEM WHETHER THE PROGRAM HAS BEEN AUTHORIZED TO SWITCH             
* SYSTEMS OR NOT                                                                
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     SYSTEM SENUM TO SWITCH TO                    
***********************************************************************         
SWTCHSYS NTR1                                                                   
         MVC   DMCB+1(3),=3X'FF'   DON'T CARE IF PROGRAM IS AUTHORIZED          
         GOTO1 VSWITCH,DMCB,,0                                                  
         CLI   DMCB+4,0            SUCCESSFUL?                                  
         BNE   SWSYSNO             NO                                           
*                                                                               
SWSYSYES J     YES                                                              
*                                                                               
SWSYSNO  J     NO                                                               
         EJECT                                                                  
***********************************************************************         
*  CLOSDATE:                                                                    
*        ACCESSES REP PROFILE                                                   
*        CHECKS PROFILE #60                                                     
*        IF ON:                                                                 
*           RETRIEVES STATION RECORD FOR ORDER UPDATED                          
*           UPDATES CLOSE DATE IF MONTH OF SERVICE IS LATER THAN                
*              CURRENT CLOSE DATE                                               
***********************************************************************         
CLOSDATE NTR1                                                                   
*                                                                               
         GOTO1 GETPROF,DMCB,('RREPQCNT',CONPROFS)                               
         TM    CONPROFS+CNTMQINB,CNTMQINA                                       
         BZ    CDAT0900            DON'T CLOSE STATION                          
STA      USING RSTAKEY,KEY                                                      
         XC    STA.RSTAKEY,STA.RSTAKEY                                          
         MVI   STA.RSTAKEY,2       SET FOR STATION RECORD                       
         MVC   STA.RSTAKREP,POWERCDE                                            
         MVC   STA.RSTAKSTA,SAVSTATN                                            
         MVC   AIO,AIO1                                                         
*                                                                               
         BRAS  RE,HIGHREP          RETRIEVE KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                KEY MUST BE ON FILE                          
*                                                                               
         DROP  STA                                                              
*                                                                               
* READ STATN RECORD FOR UPDATE                                                  
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GETREP           READ STA RECORD                              
*                                                                               
         L     R2,AIO1             SET A(CONTRACT RECORD AREA)                  
         USING RSTAREC,R2                                                       
         CLC   RSTACLDT,INVMOS     STATION CLOSE VS NEW DATA CLOSE              
         BNL   CDAT0900            STATION CLOSE REMAINS AS IS                  
         MVC   RSTACLDT,INVMOS     REPLACE STATION CLOSE                        
         LA    R6,RSTAELEM         FIND ACTIVITY ELEMENT X'23'                  
CDAT0020 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    CDAT0160            NO ELT FOUND - INSERT FRESH ELT              
         CLI   0(R6),X'23'         ACTIVITY ELEMENT?                            
         BE    CDAT0040            YES                                          
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     CDAT0020            GO BACK FOR NEXT                             
CDAT0040 EQU   *                                                                
*                                  ELT FOUND: OVERRIDE DATE/FLAG                
         GOTO1 VDATCON,DMCB,(5,WORK),(3,WORK)                                   
*                                  GET TODAY'S DATE                             
         MVC   2(3,R6),WORK        INSERT TODAY'S DATE INTO ELEMENT             
         MVI   5(R6),X'08'         RESET 'CLOSED BY' FLAG                       
         MVC   7(8,R6),ALLSPCES    CLEAR LUID                                   
         B     CDAT0200                                                         
CDAT0160 EQU   *                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),RSTAREC,NEW23ELT,0                
*                                  INSERT ACTIVITY ELT FOR STATION              
CDAT0200 EQU   *                                                                
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(27),0(R6)       MOVE REC TO KEY FOR CLARITY                  
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUTREP           REWRITE STATION RECORD                       
CDAT0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GETPROF:                                                                      
*        RETRIEVE PARTICULAR REP PROFILE SPECIFIED IN CALL,                     
*        STORE AT OPTIONAL LOCATION                                             
***********************************************************************         
GETPROF  NTR1                                                                   
         ZIC   R3,0(R1)            PROFILE TYPE (CON, SEL, ETC)                 
         L     R2,0(R1)            A(STORAGE FOR PROFILE)                       
         XC    0(10,R2),0(R2)                                                   
*                                                                               
K        USING RREPKEY,KEY                                                      
         XC    K.RREPKEY,K.RREPKEY                                              
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,POWERCDE                                              
         DROP  K                                                                
*                                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD ??                             
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,GETREP                                                        
*                                                                               
         L     R6,AIO                                                           
         LA    R6,RREPELEM-RREPREC(R6)                                          
GPROF02  CLI   0(R6),0                                                          
         BE    GETPROFX            NO PROFILE ELEMENT                           
         CLI   0(R6),X'04'                                                      
         BE    GPROF04                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GPROF02                                                          
*                                                                               
GPROF04  DS    0H                                                               
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLM   R3,1,RREPPGM1       CORRECT PROGRAM?                             
         BE    GPROF20             YES                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GETPROFX            NOT FOUND. USE DEFAULTS.                     
*                                                                               
GPROF20  MVC   0(10,R2),RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GETPROFX EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
* COMMON EXIT POINTS                                                            
***********************************************************************         
EXITPRG  L     RD,SAVERD           EXIT THE PROGRAM                             
         J     XIT                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DUPMSG   DS    0C                                                               
         DC    C'AUTONOTE*SKUIN,SKUIN:'                                         
         DC    C'READ-ONLY ERROR: '                                             
DUPMESS  DS    CL(RMQRNEXT-RMQRINV)                                             
DUPMSGQ EQU   *-DUPMSG                                                          
*                                                                               
ALLSPCES DC    132C' '                                                          
         SPACE 1                                                                
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMADD    DC    C'DMADD  '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC'                                                        
*                                                                               
*                                                                               
GFILE    DC    CL8'GFILE'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
SPTFILE  DC    C'SPTFILE'                                                       
REPFILE  DC    C'REPFILE'                                                       
SPTDIR   DC    C'SPTDIR'                                                        
REPDIR   DC    C'REPDIR'                                                        
CTFILE   DC    C'CTFILE'                                                        
STATION  DC    C'STATION'                                                       
XSPDIR   DC    CL8'XSPDIR'                                                      
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* PUTS OUT A MESSAGE TO THE CONSOLE AND HARDCOPY                                
*                                                                               
* ON ENTRY:    WHCHEDCT            ON WHAT EDICT FILE PROBLEM OCCURED           
*              EDCTFDSK            ADDRESS OF THE EDICT RECORD                  
*              PARAM  1            OFFSET FROM BEGINNING OF PROGRAM             
***********************************************************************         
WTOMSG   NTR1  BASE=*,LABEL=*                                                   
         XC    MSG2,MSG2                                                        
         MVC   MSG2(4),DMCB        COPY OFFSET FROM BEG OF PROGRAM              
         MVC   MSG2+4(4),EDCTFDSK                                               
         MVC   MSG2+7(1),RECNUM                                                 
         MVC   MSG1SYS,WHCHEDCT                                                 
         GOTO1 VHEXOUT,DMCB,MSG2+4,MSG1ADDR,4                                   
         GOTO1 VHEXOUT,DMCB,MSG2,MSG1DISP,4                                     
*                                                                               
         XC    MSG2,MSG2                                                        
         MVC   MSG2,0(R7)                                                       
         XR    R0,R0                                                            
         WTO   TEXT=((MSGSTARL,D),(MSGPROBL,D),(MSGSTARL,D),(MSG1L,D), X        
               (MSG2L,D),(0,E)),DESC=2                                          
***********************                                                         
***********************                                                         
         ZIC   R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
*                                                                               
         CH    R1,EDCTRPBQ         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   WTO30               YES, NO PROBLEM                              
*                                                                               
         ZIC   R2,EDCTFDSK+2       NO                                           
         LA    R2,1(R2)                                                         
         MVI   RECSKIP,0                                                        
         OI    BITFLAG1,BF1SKPTB   SKIP TRACK/BLOCK READ                        
*                                                                               
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   WTO20               YES                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,EDCTFDSK                                                    
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST       ANY MORE TRACKS?                             
         BH    WTO30               NO MORE, UPDATE AS WELL                      
         STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
*                                                                               
WTO10    LA    R2,1                                                             
WTO20    STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         MVI   EDCTFDSK+3,0                                                     
*                                                                               
WTO30    DS    0H                                                               
         DC    H'0'                SO WE GET AN IMAGE                           
         LTORG                                                                  
MSGSTARL DC    H'80'                                                            
         DC    80C'*'                                                           
MSGPROBL DC    H'80'                                                            
         DC    CL49'**$DAR PROBLEM**  PLEASE CONTACT  WHOA AT EXT5324'          
         DC    CL31' IF YOU SEE THIS MESSAGE!!!!!!'                             
MSG1L    DC    H'80'                                                            
MSG1     DC    CL80' '                                                          
         ORG   MSG1                                                             
         DC    CL04'EDCT'                                                       
MSG1SYS  DC    CL01'?'                                                          
         DC    CL07', ADDR='                                                    
MSG1ADDR DC    CL08'????????'                                                   
         DC    CL07', DISP='                                                    
MSG1DISP DC    CL08'????????'                                                   
         ORG   MSG1+L'MSG1                                                      
MSG2L    DC    H'80'                                                            
MSG2     DC    CL80' '                                                          
*&&                                                                             
*&&DO                                                                           
         EJECT                                                                  
***********************************************************************         
* SENDS AN ERROR NOTIFICATION OUT                                               
*                                                                               
* ON ENTRY:    R7 A(INVOICE MSG)                                                
*              R1 HAS ERROR NUMBER                                              
*                                                                               
***********************************************************************         
SNDERROR NTR1  BASE=*,LABEL=*                                                   
         STC   R1,SAVERROR         SAVE ERROR NUMBER                            
*                                                                               
         L     RE,ASPLAREA         PUT MSG TO BUFFER FOR MQPUT                  
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
         L     R3,ASPLAREA         USE ASPLAREA AS ERRNOT IS SMALL              
*                                                                               
*   CHANGE WHEN REGULAR QUEUE IS IDENTIFIED                                     
*                                                                               
         MVC   0(16,R3),=CL16'DDSTESTINGQUEUE*'                                 
         MVC   16(4,R3),=C'0064'   SET OVERALL CONTROL LENGTH                   
         LA    R3,20(R3)                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',0(R3))                                 
*                                  INSERT DATE INTO CONTROL                     
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,DUB                                                           
         AP    PACKOF4B,DUB(4)                                                  
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    SERR0020                                                         
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,0(R3),0(R3),F'1'                                     
*                                                                               
SERR0020 ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF             FOUR CHARACTER OUTPUT: HH:SS                 
         LA    R3,6(R3)            SLOT TO TIME                                 
         GOTO1 VHEXOUT,DMCB,HALF,0(R3),L'HALF                                   
         LA    R3,4(R3)            SLOT TO ERROR NUMBER                         
         EDIT  SAVERROR,(4,0(R3)),FILL=0                                        
*                                  INSERT ERROR NUMBER                          
         LA    R3,4(R3)            SLOT TO ERROR MESSAGE                        
         LA    R1,ERRLIST                                                       
SERR0040 EQU   *                                                                
         CLI   0(R1),0             END OF LIST?                                 
         BNE   *+6                                                              
         DC    H'0'                YES - UNRECOGNIZED ERROR???                  
         CLC   SAVERROR,0(R1)      ERROR IN LIST?                               
         BE    SERR0060            YES                                          
         LA    R1,LERRLIST(R1)     NO  - BUMP TO NEXT SLOT                      
         B     SERR0040            GO BACK FOR NEXT                             
SERR0060 EQU   *                                                                
         MVC   0(LERRMSG,R3),1(R1) INSERT ERROR MESSAGE                         
         LA    R3,LERRMSG(R3)      SLOT TO ACTUAL DATA                          
*                                                                               
         L     R7,ATBUFF           USE TBUFF FOR ERROR CREATION                 
         CLC   MQMSGLEN,=AL2(4064) IN 31-BIT MODE (MSG TOO LONG)?               
         BNH   SERR0080            NO                                           
*                                                                               
**** SET 31-BIT ADDRESSING                                                      
*                                                                               
         BAS  RE,ON31ERR           YES                                          
*****                                                                           
SERR0080 EQU   *                                                                
         ZICM  R1,MQMSGLEN,2       LENGTH OF DATA IN MESSAGE                    
         MOVE  ((R3),(R1)),0(R7)                                                
         ZICM  R1,MQMSGLEN,2       LENGTH OF DATA AGAIN                         
         AR    R3,R1               ADD LENGTH OF MESSAGE                        
*                                                                               
*                                                                               
SERR0100 L     R2,ASPLAREA                                                      
         SR    R3,R2               R3 = L(MSG) WITH CRLF                        
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CMQIO-COMFACSD(RF)                                            
         GOTOR (RF),DMCB,=CL8'PUT',ASPLAREA,(R3),0,0,DUB                        
*                                                                               
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
*                                                                               
SERR0120 BAS   RE,OFF31ERR         IN CASE WE WERE IN 31-BIT MODE               
*                                                                               
         J     XIT                                                              
*                                                                               
ON31ERR  O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31ERR N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
ERRLIST  EQU   *                                                                
*                      1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.                           
         DC    X'01',C'POWER CODE NOT FOUND          '                          
LERRLIST EQU   *-ERRLIST                                                        
LERRMSG  EQU   *-ERRLIST-1                                                      
         DC    X'02',C'CONTRACT NUMBER NOT NUMERIC   '                          
         DC    X'03',C'CONTRACT NUMBER NOT ON FILE   '                          
         DC    X'04',C'INVOICE DOLLARS NOT NUMERIC   '                          
         DC    X'05',C'MONTH OF SERVICE DATE INVALID '                          
         DC    X'06',C'NO CTRL DESCRIPTION ELT FOUND '                          
         DC    X'07',C'NO CTRL REP SYSTEMS NUMBER    '                          
         DC    X'08',C'USER NOT AUTHORIZED           '                          
         DC    X'09',C'CONTROL USER ID NOT FOUND     '                          
         DC    X'0A',C'CALENDAR TYPE INVALID         '                          
         DC    X'0B',C'FILE IS READ-ONLY             '                          
         DC    X'0C',C'FILE NOT FOUND                '                          
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*&&DO                                                                           
***********************************************************************         
* SENDS A SUCCESS MESSAGE BACK TO SBS FOR CONFIRMATIONS AND REJECTIONS          
*                                                                               
* ON ENTRY:    R7                  A(DARE MESSAGE), BUT ATBUFF ALSO             
***********************************************************************         
SNDSUCSS NTR1  BASE=*,LABEL=*                                                   
         L     RE,ASPLAREA         PUT MSG TO BUFFER FOR MQPUT                  
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
         L     R3,ASPLAREA         USE ASPLAREA AS ERRNOT IS SMALL              
         MVC   0(16,R3),=CL16'RADIOEDIXML*****'                                 
         LA    R3,16(R3)                                                        
*                                                                               
         L     R7,ATBUFF           USE TBUFF FOR ERROR CREATION                 
         CLC   MQMSGLEN,=AL2(4064) IN 31-BIT MODE (MSG TOO LONG)?               
         BNH   SNDSC05                                                          
**** SET 31-BIT ADDRESSING                                                      
         BRAS  RE,ON31             YES, WE ARE                                  
*****                                                                           
SNDSC05  AHI   R7,8                BUMP PAST THE "REDIXML "                     
*****                                                                           
         USING RDLNNOTD,R3                                                      
         MVC   RDNTTID,=C'ERRNOT'  SPECIAL ACK ERROR NOTIFICATION               
         MVC   RDNTORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   RDNTFRID,RDNTTOID-RDLNNOTD(R7)   SWAP TO/FROM                    
         MVC   RDNTTOID,RDNTFRID-RDLNNOTD(R7)                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',RDNTDATE)                              
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,DUB                                                           
         AP    PACKOF4B,DUB(4)                                                  
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    SNDSC10                                                          
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,RDNTDATE,RDNTDATE,F'1'                               
*                                                                               
SNDSC10  ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,RDNTTIME,L'HALF                                
*                                                                               
         MVC   RDNTRPCN,QREPCON                                                 
         MVC   RDNTRTRN,QRETURN                                                 
         MVC   RDNTEFLG,=C'ACK'    ACKNOWLEDGEMENT TYPE                         
*                                                                               
         MVC   RDNTTDTE,RDNTDATE-RDLNNOTD(R7)   RECEIVED DATE/TIME              
         MVC   RDNTTTIM,RDNTTIME-RDLNNOTD(R7)                                   
         LA    R3,RDLNNOTL(R3)                                                  
*                                                                               
SNDSC50  MVC   0(2,R3),=X'0D25'    CRLF                                         
         LA    R3,2(R3)                                                         
*                                                                               
*****    CLI   QMED,C'R'           RADIO?                                       
*****    BNE   SNDSC90             NO, DON'T WORRY ABOUT XM1 AND XM2            
*                                                                               
         LR    RE,R7               LOCATE AGYXM1 AND XM2, R7 SHOULD             
         SR    RF,RF                                                            
         ICM   RF,3,MQMSGLEN       MESSAGE LENGTH                               
SNDSC55  CLC   0(6,RE),=C'AGYXM1'                                               
         BE    SNDSC60                                                          
         AHI   RE,1                                                             
         BCT   RF,SNDSC55                                                       
         DC    H'0'                DIE IF WE REACH THIS AS WE SHOULDN'T         
*                                                                               
SNDSC60  MVC   0(PAGYXM1L,R3),0(RE)     COPY AGYXM1                             
         MVC   PAGYXM1L(2,R3),=X'0D25'  PUT OUT CRLF                            
         LA    R3,PAGYXM1L+2(R3)                                                
         AHI   RE,PAGYXM1L+2                                                    
*                                                                               
         MVC   0(PAGYXM2L,R3),0(RE)     COPY AGYXM2                             
         MVC   PAGYXM2L(2,R3),=X'0D25'  PUT OUT CRLF                            
         LA    R3,PAGYXM2L+2(R3)                                                
         AHI   RE,PAGYXM2L+2                                                    
*                                                                               
         CLC   =C'AGYXM3',0(RE)    DID THEY PROVIDE US WITH AGYXM3?             
         BNE   SNDSC63             NO, THEY WEREN'T NICE ENOUGH                 
         MVC   0(PAGYXM3L,R3),0(RE)  COPY AGYXM3                                
         B     SNDSC66                                                          
*                                                                               
         USING PAGYXM3D,R3                                                      
SNDSC63  MVI   PAX3TID,C' '         SPACE-FILL LINE                             
         MVC   PAX3TID+1(PAGYXM3L-1),PAX3TID                                    
*                                                                               
         MVC   PAX3TID,=C'AGYXM3'                                               
         MVC   PAX3ORDR,6(RE)       COPY ORDER NUMBER FROM AGYXM2               
SNDSC66  MVC   PAX3MESG,0(R7)       TELL SELLER WHAT MSG CAUSED ERROR           
***************                                                                 
* IF WE CAN SEND BACK THE STATION CALL LETTERS DO SO                            
***************                                                                 
         LA    RF,RORJQSTA-RORDREJD(R7)                                         
         CLC   =C'ORDREJ',0(R7)                                                 
         BE    SNDSC70                                                          
         CLC   =C'CANREJ',0(R7)                                                 
         BE    SNDSC70                                                          
         LA    RF,ROCFQSTA-RORDCFMD(R7)                                         
         CLC   =C'ORDCFM',0(R7)                                                 
         BE    SNDSC70                                                          
         CLC   =C'CANCFM',0(R7)                                                 
         BE    SNDSC70                                                          
         CLC   =C'ORDCAN',0(R7)                                                 
         BNE   SNDSC75             DON'T KNOW WHERE THE STATION IS              
*                                                                               
SNDSC70  MVC   PAX3QSTA,0(RF)                                                   
*                                                                               
SNDSC75  MVC   PAGYXM3L(2,R3),=X'0D25'    CRLF                                  
         LA    R3,PAGYXM3L+2(R3)                                                
         DROP  R3                                                               
*                                                                               
         USING PAGYTLRD,R3                                                      
         MVC   PATLTID,=C'AGYTLR'                                               
         MVC   PATLORDR,6(RE)       COPY ORDER NUMBER                           
         MVC   PATLNMRC,=C'000005'                                              
         MVC   PATLSPTS,=C'000000'                                              
         MVC   PATLTOTL,=10C'0'                                                 
         LA    R3,PAGYTLRL(R3)                                                  
         DROP  R3                                                               
*                                                                               
         MVC   0(2,R3),=X'0D25'    CRLF                                         
         LA    R3,2(R3)                                                         
*                                                                               
SNDSC90  L     R2,ASPLAREA                                                      
         SR    R3,R2               R3 = L(MSG) WITH CRLF                        
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CMQIO-COMFACSD(RF)                                            
         GOTOR (RF),DMCB,=CL8'PUT',ASPLAREA,(R3),0,0,DUB                        
*                                                                               
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
SNDSCX   BRAS  RE,OFF31            IN CASE WE WERE IN 31-BIT MODE               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*========================================================                       
* SPTDIR COMMANDS                                                               
*========================================================                       
         SPACE 1                                                                
ADDDIR   DS    0H                                                               
         BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
ADDXKEY  DS    0H                                                               
         BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(XSPDIR)                                                      
*                                                                               
HIGH     MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHREP  MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(REPDIR)                                                      
*                                                                               
SEQ      BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
SEQREP   BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(REPDIR)                                                      
*                                                                               
WRITE    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHCT   MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(CTFILE)                                                      
*                                                                               
WRTCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
ADDCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
GODIR    NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)         COMMAND                                      
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)         FILE NAME                                    
         A     RE,RELO                                                          
         ST    RE,DMCB+4                                                        
         CLC   =C'CTFILE',0(RE)    ARE WE LOOKING AT THE CONTROL FILE           
         BNE   GODIR10                                                          
         LA    RE,KEY                                                           
         ST    RE,DMCB+8                                                        
         L     RE,AIO                                                           
         ST    RE,DMCB+12                                                       
         B     GODIR20                                                          
*                                                                               
GODIR10  LA    RE,KEYSAVE                                                       
         ST    RE,DMCB+8                                                        
         LA    RE,KEY                                                           
         ST    RE,DMCB+12                                                       
*                                                                               
GODIR20  GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         SPACE 1                                                                
*========================================================                       
* FILE COMMANDS                                                                 
*========================================================                       
         SPACE 1                                                                
GET      BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'               DSPL TO DISK ADDRESS                         
         SPACE 1                                                                
GETREP   BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(REPFILE)                                                     
         DC    H'28'               DSPL TO DISK ADDRESS                         
*                                                                               
PUT      BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
         SPACE 1                                                                
PUTREP   BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(REPFILE)                                                     
         DC    H'28'               DSPL TO DISK ADDRESS                         
*                                                                               
ADD      BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
*                                                                               
GOFILE   NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB             SET COMMAND ADDRESS                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB+4           SET FILENAME ADDRESS                         
*                                                                               
         LA    RE,KEY                                                           
         AH    RE,8(R1)            GET DSPL OF DISK ADDRESS IN KEY              
         ST    RE,DMCB+8                                                        
*                                                                               
         MVC   DMCB+12(4),AIO                                                   
*                                                                               
         LA    RE,DMWORK                                                        
         ST    RE,DMCB+16                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'02'         DELETED RECORD?                              
         BNZ   GOFILX              YEAH, WE DON'T WANNA DIE                     
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOFILX   MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RMQOBJD  DSECT                                                                  
RMQOTID  DS    CL8                 ACTION INDICATOR                             
RMQORIDN DS    XL1                 RETURN ID NUMBER                             
RMQONXT  DS    0C                                                               
*                                                                               
RMQD     DSECT                                                                  
       ++INCLUDE REGENRMQ                                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
AUTL     DS    A                                                                
ATBUFF   DS    A                                                                
EDICTFL  DS    A                   A(EDICT FILE)                                
AIO      DS    A                                                                
AIO1     DS    A                   A(IOAREA #1)                                 
AIO2     DS    A                   A(IOAREA #2)                                 
ASPLAREA DS    A                   A(SPOOL AREA)                                
AWRKRIOA DS    A                   A(IO AREA USED BY EDICT)                     
AWRKRBUF DS    A                   A(WORKER BUFFER AREA)                        
AHUGEBLK DS    A                   A(HUGE BLOCK)                                
*                                                                               
VADDAY   DS    V                                                                
VDATCON  DS    V                                                                
VDATVAL  DS    V                                                                
VGETFACT DS    V                                                                
VHELLO   DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VLOCKET  DS    V                                                                
VRECUP   DS    V                                                                
VCLUNPK  DS    V                                                                
VDARREPS DS    V                                                                
VMSPACK  DS    V                                                                
VMSUNPK  DS    V                                                                
VSWITCH  DS    V                                                                
*                                                                               
ASPOOL   DS    A                                                                
AREPFACS DS    A                                                                
*                                                                               
SRPARMS  DS    8F                  SERVICE REQUEST PARAMETERS                   
EDCTFDSK DS    F                   EDICTFIL DISK ADDRESS   TTTTBB00             
EDCTFDSP DS    F                   EDICTFIL DISP ADDRESS   DD999999             
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SAVATBUF DS    A                   SAVED A(ATBUFF)                              
*                                                                               
EDCTFRPT DS    H                   EDICTFIL PHYSICAL RECORDS PER TRACK          
EDCTFTPD DS    H                   EDICTFIL TRACKS PER DAY                      
EDCTFATQ EQU   40                     ON ADV                                    
EDCTFRTQ EQU   40                     ON REP                                    
EDCTFTTQ EQU   2                      ON TEST                                   
EDCTFMTQ EQU   2                      ON MEL                                    
EDCTRPBQ DS    H                   EDICTFIL LOGICAL RECORDS PER BLOCK           
EDCTLRCL DS    H                   EDICTFIL LOGICAL RECORD LENGTH               
EDCTFLST DS    H                   LAST TRACK FOR GIVEN DAY                     
FRSTTRK  DS    H                   FIRST TRAC FOR GIVEN DAY                     
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
HALF     DS    H                                                                
DATADISP DS    H                                                                
MQMSGLEN DS    H                   L(MQ MESSAGE) (BE IN XA MODE > 4064)         
DMINBTS  DS    X                                                                
*                                                                               
AGENCY   DS    XL2                 AGENCY POWER CODE                            
USERID   DS    CL10                CONTROL USER ID                              
REP      DS    CL10                REP 'FROM ID'                                
POWERCDE DS    CL2                 POWER CODE FOR REP READ                      
SAVERROR DS    XL1                 ERROR ENCOUNTERED                            
SAVSTATN DS    CL5                 STATION OF ORDER IN PROCESS                  
CONPROFS DS    CL10                CONTRACT PROFILES                            
*                   .0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9                    
NEW23ELT DC    XL20'2314000000080040404040404040400000000000'                   
         DS    0F                                                               
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
JDTTODAY DS    PL4                 JULIAN DATE OF TODAY                         
QREPCON  DS    CL8                 COPY OF REP CONTRACT NUMBER                  
QRETURN  DS    CL16                COPY OF RETURN TO SENDER DATA                
*                                                                               
QMED     DS    CL1                 EBCDIC MEDIA                                 
QBUYER   DS    CL3                        BUYER CODE                            
QCLT     DS    CL3                        CLIENT                                
QPRD1    DS    CL3                        PRODUCT 1                             
QPRD2    DS    CL3                        PRODUCT 2                             
QEST1    DS    CL3                        ESTIMATE 1                            
QSTA     DS    CL8                        STATION                               
QMGGROUP DS    CL3                        MAKEGOOD GROUP CODE                   
QMNUMCD  DS    CL2                        MGE MAKEGOOD CODE (INTO BUY)          
QCSHTRDE DS    CL1                        CASH OR TRADE                         
QFLTNUM  DS    CL2                        FLIGHT NUMBER                         
*                                                                               
WHCHEDCT DS    XL1                 WHICH EDICT FILE (A)DV OR (R)EP              
*                                                                               
RECSKIP  DS    XL1                 # OF RECORDS TO BUMP INTO BLOCK              
RECNUM   DS    XL1                 # OF RECORDS INTO BLOCK                      
REPSENUM DS    XL1                 SPOT SYSTEM SENUM                            
BYTE     DS    C                                                                
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
BF1SKPTB EQU   X'80'                - SKIP TRACK/BLOCK                          
BF1SKIPB EQU   X'40'                - SKIP RECORD BUMP                          
BF1FRSTR EQU   X'20'                - FIRST RECORD IN BLOCK                     
BF1DPEND EQU   X'10'                - DARE RECORD PENDING                       
BF1NWCOM EQU   X'08'                - NEED TO ADD REP COMMENT RECORD            
BF1IGRCM EQU   X'04'                - IGNORE REST OF REP COMMENTS               
BF1PSSWD EQU   X'02'                - PASSWORD REQUIRED                         
BF1YSDAY EQU   X'01'                - USING PRIOR BUSINESS DAY'S INFO           
*                                                                               
RMQNUMB  DS    XL1                 ACTION INDICATOR                             
RMQINVQ  EQU   1                                                                
*                                                                               
BTODAY   DS    XL3                 TODAY'S DATE IN BINARY (YMD)                 
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SYSN1    DS    CL1                 1 CHR FACPAK NAME                            
*                                                                               
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DMWORK   DS    12D                                                              
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
ELCODE   DS    XL1                                                              
*                                                                               
INVAMT   DS    F                   INVOICE AMOUNT                               
INVMOS   DS    F                   INVOICE MONTH OF SERVICE                     
INVACTDT DS    F                   INVOICE ACTIVITY DATE                        
*                                                                               
ELEM     DS    CL256                                                            
*                                                                               
IOA1     DS    6000C               I/O AREA                                     
IOA2     DS    6000C               I/O AREA                                     
*                                                                               
SPULAREA DS    XL3200                                                           
*                                                                               
WRKRIOA  DS    14336C              BYTES 0-1   = LENGTH OF RECORD               
*                                  BYTE  2     = L(DATA)+1                      
*                                  BYTES 3-??  = DATA                           
WRECQLNQ EQU   *-WRKRIOA                                                        
*                                                                               
HUGEBLCK DS    18432C              HUGE OUTPUT BLOCK                            
WORKX    EQU   *                                                                
*                                                                               
*  ERROR EQUATES                                                                
REPIDNG  EQU   1                   POWER CODE ENTERED NOT FOUND                 
REPCONNV EQU   2                   CON # ENTERED INVALID                        
REPCONNF EQU   3                   CON # ENTERED NOT ON FILE                    
INVDOLNV EQU   4                   INVOICE DOLLARS ENTERED INVALID              
MOSDATNV EQU   5                   MONTH OF SERVICE DATE INVALID                
REPIDNO2 EQU   6                   NO DESCRIPTION ELEMENT                       
REPIDN21 EQU   7                   NO REP SYSTEMS NUMBER                        
REPIDNAU EQU   8                   NOT AUTHORIZED                               
REPNUSER EQU   9                   CONTROL USER ID NOT FOUND                    
REPCALNV EQU   10                  CALENDAR TYPE INVALID                        
RDONLYER EQU   11                  FILE IS READONLY                             
FILENTFD EQU   12                  FILE NOT FOUND                               
*                                                                               
         EJECT                                                                  
* DMWRKRD                                                                       
* DMWRKRK                                                                       
* FADSECTS                                                                      
* FAPQPL                                                                        
* CTGENFILE                                                                     
* CTGENDARE                                                                     
* CTGENRAD                                                                      
* DMGREQUS                                                                      
* DMPRTQD                                                                       
* DMPRTQS                                                                       
* DMPRTQK   <--- PREFIXED WITH 'SR'                                             
* DDCOMFACS                                                                     
* DMFILTABD                                                                     
* TASYSWORKD                                                                    
* DMREQHDRA                                                                     
* DDEDICTFIL                                                                    
* FAFACTS                                                                       
* SPTRPAT                                                                       
* SPTRSHIP                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENDARE                                                      
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQS                                                        
*PREFIX=SR                                                                      
       ++INCLUDE DMPRTQK                                                        
*PREFIX=                                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMFILTABD                                                      
       ++INCLUDE TASYSWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE DMWRKFL                                                        
*********INCLUDE DMWRKFK                                                        
       ++INCLUDE DDEDICTFIL                                                     
       ++INCLUDE FAFACTS                                                        
*********INCLUDE SPTRPAT                                                        
*********INCLUDE SPTRSHIP                                                       
AGYHDRD  DSECT                                                                  
*********INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
*********INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
*********INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
*********INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
*********INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SRRMQFFD                                                       
         EJECT                                                                  
RREPRECD DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
       ++INCLUDE RECNTPROF                                                      
*                                                                               
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
RDARRECD DSECT                                                                  
*********INCLUDE REGENDAR                                                       
         EJECT                                                                  
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
RMKGRECD DSECT                                                                  
*********INCLUDE REGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE REPFACSQ                                                       
RAG2RECD DSECT                                                                  
*********INCLUDE REGENAGY2                                                      
         EJECT                                                                  
*********INCLUDE SPDARDARED                                                     
         EJECT                                                                  
*********INCLUDE SPDARMKGDD                                                     
         EJECT                                                                  
*********INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
*********INCLUDE SPGENDRMKN                                                     
         EJECT                                                                  
*********INCLUDE SPGENDRMKO                                                     
         EJECT                                                                  
*********INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
*********INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*********INCLUDE DDPERVALD                                                      
         EJECT                                                                  
*********INCLUDE DDDARETABD                                                     
         EJECT                                                                  
       ++INCLUDE SPSYSFAC                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRRMQ00   01/04/10'                                      
         END                                                                    
