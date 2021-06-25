*          DATA SET PRSFM03    AT LEVEL 164 AS OF 02/25/15                      
*PHASE T41C03A                                                                  
                                                                                
         TITLE 'T41C03 - PRINT SFM - BUY COPY/MOVE/DELETE'                      
                                                                                
*                                                                               
*                                                                               
* BPLA 02/15    ERREX2 BUG FIX                                                  
*                                                                               
* KWAN 04/06/11 NO Copy/Move for iDesk Estimates                                
*                                                                               
* KWAN 11/03/10 NO COPY/MOVE FOR LOCKED PUBS                                    
*                                                                               
* SMYE  05/09    DISALLOW COPY/MOVE ACTIONS IF USER IS SET UP TO USE            
*                  EIO (ENHANCED INSERTION ORDERS) IN ADBUYER                   
*                                                                               
* SMYE  02/08    ADD USER ID NUM AND "ADDED BY" DATA TO THE X'A7'               
*                  PID ELEMENT AT DELPID AND CLRB40 AND CLEAR                   
*                "FROM ADB AND FROM IDESK" BITS IN PBDSTAT2 BYTE                
*                                                                               
* BOBY 06/00/07  ADD DATE PASSIVE POINTERS TO NEW BUYS                          
*                                                                               
* SMYE 06/15/06  IN VK60 DISALLOW PRODUCT CODE AAA AS COPY OR MOVE              
*                  TARGET                                                       
*                                                                               
* BOBY 01/31/06  PREVENT MOVE AND CANCEL OF BUYS WITH INVOICE DATA              
*                                                                               
* SMYE 03/04/05  DELETE NEW INVOICE ELEMENTS (X'51'& X'52') ON ACTION           
*                  COPY ONLY                                                    
*                DO NOT OUTPUT X'F1' (GENCON ACTIVITY ELEMENT)                  
*                                                                               
* SMYE  07/02    FIX LOCK TESTING FOR SUB-CLIENTS                               
*                                                                               
* SMYE 03/25/02  DELETE SPECIAL ZENITH PAY "FIX" ELEMENTS (X'A5')               
*                IN "TO" BUYS FOR COPY AND MOVE                                 
*                                                                               
* SMYE 12/20/01  NO LONGER ISSUE LOCKS + NO UNLOCKING                           
*                                                                               
* SMYE  10/01    FIX MAKELIVE BUG AND ADD UNLOCK TO SCANERR EXIT                
*                                                                               
* SMYE  08/01    IF "TO" PRODUCT IS SAME AS "FROM" PRODUCT, DO NOT              
*                DELETE LEGAL WARNING OVERRIDE ELEMENT (X'94')                  
*                (SEE THE 07/01 CHANGE BELOW)                                   
*                                                                               
* SMYE  07/01    DELETE LEGAL WARNING OVERRIDE ELEMENT (X'94')                  
*                IN "TO" BUYS FOR COPY AND MOVE - ALSO SET PBDSTAT              
*                "NO TRAFFIC" BIT TO THAT OF THE "TO" PRODUCT                   
*                                                                               
* SMYE  05/07/01 ADD LOGIC FOR DATA LOCKING                                     
*                                                                               
* BPLA  02/00    REMOVE PROFILE CHECK FOR SERIAL NUMBERS                        
*                                                                               
* BPLA  01/00    CHANGES FOR ADDING SERIAL NUMBERS WHEN COPYING                 
*                OR MOVING INSERTIONS  - PASSIVE POINTERS ADDED ALSO            
*                                                                               
* SMYE  12/00    ADD FROZEN-BY-DATE LOGIC TO LOGIC DISALLOWING                  
*                MAKELIVE, MOVE AND COPY IF FOR A "FROZEN"                      
*                CLIENT (X'12' IN PCLSTAT FOR FROZEN-BY-DATE)                   
*                                                                               
* SMYE  12/01/00 FIX TINY BUG AT VK10X                                          
*                                                                               
* SMYE  08/00    NO-OPPED "PREVIOUSLY VALIDATED" TEST FOR MEDIA AT VK5          
*                AS IT WAS CAUSING PROBLEMS COMING BACK FROM SELECTING          
*                A PUB AFTER A PUB "NAME SEARCH" (SEE *NVAL*)                   
*                                                                               
* KWAN  07/00    CORRECT ERROR MSG AND CURSOR PLACEMENT WHEN                    
*                "TO" FIELD IS BLANK                                            
*                                                                               
* SMYE  3/00     DISALLOW CANCEL ACTION IF "FULLY PAID INSERTION                
*                CONTROL" IN BY PROFILE IS D OR B                               
*                                                                               
* BPLA  11/99                                                                   
*                                                                               
* BPLA  11/99    FIX DATE OF CHANGE ELEMENT WHEN MAKING LIVE                    
*                                                                               
* BPLA  9/99     ADD CODE FOR PATCHING TO COPY/MOVE TO A                        
*                NEW CLIENT                                                     
*                                                                               
* BPLA  6/99     FOR ACTIONS COPY OR MOVE:                                      
*                IF THE "TO" ESTIMATE IS NON-COST2                              
*                 - REMOVE OLD FACTOR ELEMENT                                   
*                IF THE "TO" ESTIMATE IS COST2                                  
*                 - ADD A FACTOR ELEMENT (IF NEEDED)                            
*                                                                               
* SMYE 4/21/98   DISALLOW MAKELIVE, MOVE AND COPY IF FOR A "FROZEN"             
*                CLIENT (X'02' IN PCLSTAT)                                      
*                                                                               
* SMYE 11/3/97   IF MOVING OR COPYING TO A SFH (SPECIAL FINANCIAL               
*                HANDLING) ESTIMATE (X'01' IN PESTTEST), SET PBDSTAT            
*                IN BUY TO X'0C' ("HELD" SFH) IN CLRB15 PROC                    
*                                                                               
* SMYE 2/24/97   DELETE INV MATCH ELEMENT (X'50') ON ACTION COPY                
*                ALSO SET OFF X'02' AS WELL AS X'01' IN PBDCNDA FIELD           
*                                                                               
* SMYE 8/21/96   DISALLOW "ZZZ" MAKELIVE IF ESTIMATE INVOLVED IS A              
*                TEST ESTIMATE FOR ANY PRODUCT CODE                             
*                                                                               
* AROT 1/5/95    ADD CODE FOR FUNCTIONS NOT AVAILABLE WITH A RECORD             
*                THAT HAS AN ADJACENCY CODE                                     
*                                                                               
* BPLA 11/12/93  IF SVCPROF+12 IS 'T' (NO CONTRACT REQ FOR TEST BUY)            
*                DISALLOW MAKELIVE (SINCE I MAY MAKE SOME BUYS LIVE             
*                BUT NOT OTHERS)                                                
*                                                                               
* BPLA 11/1/93  IF DOING ALL PUBS-ALLOW MAKELIVE AND CANCEL FOR                 
*               WSJ BUYS, ALSO ALLOW COPYING AND MOVING OF WSJ                  
*               BUYS IF DOING ALL PUBS AND COPYING/MOVING TO SAME               
*               PRODUCT (CAN DO SINCE JOB CODE WILL NOT BE CLEARED).            
*                                                                               
* BPLA 8/12/93  DISALLOW ACTION CANCEL AND MOVE FOR UPLOADED ESTS               
*                                                                               
* BPLA 8/4/93   IF CONTRACT FOUND WITH MAX/ISSUE DISALLOW                       
*               COPY AS WELL AS MAKELIVE                                        
* BPLA 6/9/93   IF CONTRACT FOUND WITH MAX/ISSUE                                
*               ACROSS ZONES/EDTS - DISALLOW MAKELIVE                           
* BPLA 3/22/93  WHEN MAKING LIVE ADD CHGELEM (X'24) AS DO $BUY                  
*               AND $MBC                                                        
* BPLA 6/4/92   ADD COMMISSION BILLING ELEMENT LOGIC                            
*               ALSO LARGER PAY ELEMENTS                                        
*                                                                               
* BPLA 3/10/92  CHECK CONTRACTS FOR MAX/ISSUE DATA IF FOUND                     
*               DISALLOW MAKE-LIVE                                              
*                                                                               
* BPLA 3/9/92   ASR FOR MAKE-LIVES                                              
*               BUG IN FNDCON FIXED - WAS USING BPUB INSTEAD OF                 
*               PUB FROM PBUYREC                                                
* BPLA 1/29/92  WHEN CLEARING BUY SET PBDSTAT TO X'00'                          
*                                                                               
* ROSA 4/17/90  COMPARE AFTER TM SHOULD BE BNO NOT BNE            BUG01         
********************************************************************            
*                                                                  *            
* I/O AREA USAGE                                                   *            
*      IO1 - ALL TRANSIENT RECORDS (CLT/PRD/EST + NEW BUYREC)      *            
*      IO2 - TEMPORARY AREA                                        *            
*      IO3 - CONTRACT RECORD                                       *            
*                                                                  *            
********************************************************************            
T41C03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C03,RA,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING T41CFFD,R3                                                       
*                                                                               
         SPACE 1                                                                
*                                                                               
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*                                                                               
         OI    GENSTAT4,NODUPDIE                                                
*                                                                               
*        SO GENCON WON'T DIE WHEN I TRY TO REUSE DELETED POINTER                
*                                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,PTODAY)                                
*        PACKED FORMAT NEEDED FOR CHANGE ELEMENT                                
*                                                                               
         BRAS  RE,PID                                                           
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         CLI   MODE,PRINTREP                                                    
         BE    PRINTIT                                                          
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
*                                                                               
DATALOCK EQU   80                  DATA LOCKED FOR OFFLINE PROCESSING           
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         MVI   PQSW,1              SUPPRESS PRTQUE OPEN                         
*                                                                               
         BAS   RE,TSTNEWKY         SEE IF ANY KEY FIELDS CHANGED                
         BNZ   VK5                 YES                                          
         MVC   KEY,SVLSTKEY        SEE IF CONTINUATION                          
         XC    SVLSTKEY,SVLSTKEY   BUT DON'T START HERE AGAIN                   
         OC    KEY(25),KEY                                                      
*NOP*    BNZ   PR122               YES                                          
         BNZ   PR120               YES                                          
*                                                                               
VK5      DS    0H                                                               
         MVI   LOCKSW,0            TURN OFF LOCK TEST SWITCH                    
*                                                                               
         XC    SVLSTKEY,SVLSTKEY   BUT DON'T START HERE AGAIN                   
         XC    BUYTOTS,BUYTOTS     CLEAR ACCUMULATORS                           
*                                                                               
         OC    SFMOUT1,SFMOUT1     CLEAR PREVIOUS MESSAGES                      
         BZ    *+14                                                             
         XC    SFMOUT1,SFMOUT1                                                  
         OI    SFMOUT1H+6,X'80'                                                 
*                                                                               
         OC    SFMOUT2,SFMOUT2                                                  
         BZ    *+14                                                             
         XC    SFMOUT2,SFMOUT2                                                  
         OI    SFMOUT2H+6,X'80'                                                 
*                                                                               
         OC    SFMOUT3,SFMOUT3                                                  
         BZ    *+14                                                             
         XC    SFMOUT3,SFMOUT3                                                  
         OI    SFMOUT3H+6,X'80'                                                 
*                                                                               
         OC    SFMOUT4,SFMOUT4                                                  
         BZ    *+14                                                             
         XC    SFMOUT4,SFMOUT4                                                  
         OI    SFMOUT4H+6,X'80'                                                 
*                                                                               
*NOP*    XC    SVLSTKEY,SVLSTKEY                                                
         LA    R2,SFMMEDH                                                       
*NVAL*   TM    4(R2),X'20'         TEST PREVIOUSLY VALID                        
*NVAL*   BO    VK10                                                             
         BAS   RE,CLRMED                                                        
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET VALID                                    
*                                                                               
VK10     LA    R2,SFMCLTH                                                       
         TM    4(R2),X'20'         TEST PREVIOUSLY VALID                        
         BO    VK20                                                             
         BAS   RE,CLRCLT                                                        
         GOTO1 VALICLT                                                          
*                                                                               
******** NOW AWAYS READ BY PROFILE                                              
*                                                                               
         L     R6,AIO                                                           
         USING PCLTRECD,R6                                                      
*                                                                               
         XC    SVPROF,SVPROF       CLEAR PROFILE AREA                           
*                                  GET BUY PROFILE                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P0BY'                                                 
         MVC   WORK+4(3),PCLTKAGY  AGY/MED                                      
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         CLI   ACTNUM,ACTCNCL     SEE IF CANCELLING                             
         BNE   VK10B                                                            
         CLI   SVPROF+7,C'B'                                                    
         BE    VK10A               CANCEL NOT ALLOWED                           
         CLI   SVPROF+7,C'D'                                                    
         BNE   VK10B                                                            
*                                                                               
VK10A    DS    0H                                                               
         LHI   RF,CANMSGQ          CANCEL NOT ALLOWED FOR CLIENT                
         NI    SFMMEDH+4,X'DF'       UNVALIDATE MEDIA                           
         J     V2ERR                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
VK10B    CLI   ACTNUM,ACTMKLIV    SEE IF MAKING LIVE                            
         BNE   VK10D                                                            
         CLI   SVCPROF+12,C'T' SEE IF CONTRACT ONLY REQ FRO LIVE BUYS           
         BNE   VK10T               TEST FOR "FROZEN" CLIENT                     
*                                                                               
         LHI   RF,MBCMSGQ          USE MBC OR BUY TO MAKE LIVE                  
         NI    SFMMEDH+4,X'DF'       UNVALIDATE MEDIA                           
         J     V2ERR                                                            
*                                                                               
VK10D    CLI   ACTNUM,ACTCOPY     SEE IF COPYING                                
         BE    VK10T                                                            
         CLI   ACTNUM,ACTMOVE     SEE IF MOVING                                 
         BNE   VK11                                                             
*                                                                               
VK10T    L     R6,AIO                                                           
         USING PCLTRECD,R6                                                      
*COST2*                                                                         
         MVC   SVCLTST,PCLTSTAT    SAVE STATUS                                  
         XC    SVCFDTA(3),SVCFDTA                                               
         ZAP   SVC2FAC,=P'0'                                                    
         ZAP   SVE2FAC,=P'0'                                                    
*                                                                               
         DROP  R6                                                               
*                                   CHECK FOR COST2 FACTOR ELEMENT              
         L     R6,AIO                                                           
         MVI   ELCODE,X'45'                                                     
         BAS   RE,GETEL                                                         
         BNE   VK10X                                                            
         MVC   SVC2FAC(5),2(R6)       SAVE COST2 FACTOR                         
*COST2*                                                                         
VK10X    DS    0H                                                               
*                                                                               
*NOP*    TM    PCLTSTAT,X'02'      FROZEN ?                                     
         TM    SVCLTST,X'02'       FROZEN ?                                     
         BNO   VK11                NO                                           
         TM    SVCLTST,X'10'       FROZEN WITH DATE ?                           
         BNO   VK10XE              NO - CLIENT FROZEN ERROR                     
*                                  GET FREEZE STATUS ELEMENT                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'47'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   SVCFDTA(3),2(R6)    SAVE INDICATOR(1) AND BIN YM DATE TO         
         B     VK11                  TEST FOR FROZEN DATES AT VK99              
*                                                                               
VK10XE   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(42),=C'** FUNCTION NOT ALLOWED - CLIENT FROZEN *X        
               *'                                                               
         NI    SFMMEDH+4,X'DF'       UNVALIDATE MEDIA                           
         GOTO1 ERREX2                                                           
*                                                                               
VK11     XC    SADVDATA,SADVDATA                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   VK15                                                             
         MVC   SADVDATA,2(R6)                                                   
*                                                                               
VK15     DS    0H                                                               
         CLI   ACTNUM,ACTMOVE      SEE IF MOVING                                
         BE    *+8                                                              
         CLI   ACTNUM,ACTCOPY      SEE IF COPYING                               
         BNE   VK18                                                             
         BRAS  RE,CKEIO            CHECK FOR USE OF EIO                         
*                                                                               
VK18     MVC   SFMCLNM,CLTNM                                                    
         OI    4(R2),X'20'                                                      
*                                                                               
VK20     LA    R2,SFMPUBH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         BAS   RE,CLRPUB                                                        
*                                                                               
         CLI   5(R2),0          CHK FOR INPUT                                   
         BNE   VK21                                                             
         MVI   ERROR,MISSING                                                    
         B     SCANERR                                                          
*                                                                               
VK21     CLI   5(R2),3                                                          
         BNE   VK22                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK22                                                             
         XC    BPUB,BPUB                                                        
         MVC   PUBNM,=CL20'ALL PUBLICATIONS'                                    
         B     VK24                                                             
*                                                                               
VK22     GOTO1 VALIPUB                                                          
         CLI   ACTNUM,ACTCOPY      Copy?                                        
         JE    *+12                                                             
         CLI   ACTNUM,ACTMOVE      Move?                                        
         JNE   VK24                                                             
         TM    SVPUBLSW,PUBLCKDQ   Pub is locked?                               
         JZ    VK24                                                             
         LHI   R2,PLCKDERQ                                                      
VK22_20  BRAS  RE,GET_ETXT                                                      
         LA    R2,SFMPUBH                                                       
         J     TRAPERR2                                                         
*                                                                               
VK24     MVC   SFMPBNM,PUBNM                                                    
         OI    4(R2),X'20'                                                      
         BRAS  RE,CKPUBLOC         Check for Pub locks                          
         JE    *+12                                                             
         LHI   R2,PLCKDE2Q                                                      
         J     VK22_20                                                          
*                                                                               
VK30     DS    0H                                                               
         XC    QSTART(12),QSTART                                                
         XC    BSTART(6),BSTART                                                 
         LA    R2,SFMDTSH          ALWAYS VALIDATE DATES                        
         GOTO1 VALIPERD                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
VK40     LA    R2,SFMOPTSH                                                      
         MVI   REALLO,0                                                         
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK)                                      
*                                                                               
         LA    R4,BLOCK                                                         
VK40B    CLI   0(R4),0                                                          
         BE    VK40X                                                            
         CLC   12(4,R4),=C'REAL'     ONLY CHECK FOR 4 CHARS                     
         BNE   VK40ERR                                                          
         CLI   22(R4),C'N'                                                      
         BE    VK40C                                                            
         CLI   22(R4),C'Y'                                                      
         BNE   VK40ERR                                                          
         CLC   SFMPRDT(3),=C'ZZZ'       'TO' PRD MUST BE ZZZ                    
         BNE   VK40ERR                                                          
         MVI   REALLO,C'Y'                                                      
*                                                                               
VK40C    LA    R4,32(R4)                                                        
         B     VK40B                                                            
*                                                                               
VK40ERR  MVI   ERROR,INVALID                                                    
         B     SCANERR                                                          
*                                                                               
VK40X    OI    4(R2),X'20'                                                      
         B     VK50                                                             
         EJECT                                                                  
* VALIDATE 'FROM' PRODUCT AND ESTIMATE                                          
         SPACE 1                                                                
VK50     BAS   RE,CLRPRES                                                       
*                                                                               
         LA    R2,SFMPRDFH                                                      
         GOTO1 VALIPRD                                                          
         MVC   SVPRDF,QPRD                                                      
*                                                                               
         L     R6,AIO                                                           
         USING PPRDRECD,R6                                                      
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         USING PPRDELEM,R6                                                      
*                                                                               
         XC    FROMADJ,FROMADJ                                                  
         OC    PPRDEXCL,PPRDEXCL                                                
         BZ    VK50A                                                            
         MVC   FROMADJ,PPRDEXCL     STORE TO COMPARE WITH 'TO' PRODUCT          
         CLI   ACTNUM,ACTMKLIV                                                  
         BNE   VK50A                                                            
*                                                                               
VK50CHK  OC    PPRDEXCL,PPRDEXCL                                                
         BZ    VK50A                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(46),=C' ** FUNCTION NOT AVAILABLE FOR THIS PRODUX        
               CT **'                                                           
         GOTO1 ERREX2                                                           
*                                                                               
VK50A    LA    R2,SFMESTFH                                                      
         GOTO1 VALIEST                                                          
*                                                                               
         BRAS  RE,CKIDKEST       Check for iDesk Estimate                       
         JE    *+12                                                             
         LA    R2,SFMESTFH                                                      
         J     TRAPERR2                                                         
*                                SEE IF PATCHING TO CLIENT                      
         MVC   SVESTF,BEST                                                      
*                                                                               
*        IF CANCELLING OR MOVING - DISALLOW FOR UPLOADED ESTS                   
*                                                                               
         CLI   ACTNUM,ACTCNCL                                                   
         BE    VK50B                                                            
         CLI   ACTNUM,ACTMOVE                                                   
         BNE   VK50X                                                            
*                                                                               
VK50B    DS    0H                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO                                                           
         MVC   KEY(12),0(R6)      MOVE AGY/MED/REC CODE/CLT/PRD/EST             
         MVI   KEY+3,X'90'                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   VK50X                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         OI    DMINBTS,X'80'                                                    
         MVC   CONHEAD(48),=C'** UPLOADED EST - CANCEL AND MOVE ARE INVX        
               ALID **'                                                         
         NI    SFMMEDH+4,X'DF'       UNVALIDATE MEDIA                           
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
VK50X    L     R6,AIO                                                           
         USING PESTRECD,R6                                                      
         MVI   ELCODE,7                                                         
         BAS   RE,GETEL                                                         
         USING PESTELEM,R6                                                      
         SPACE 1                                                                
* SAVE 'FROM' EST STATUS AND DATES                                              
         SPACE 1                                                                
         MVC   SVSTATF,PESTTEST                                                 
         MVC   SVRTYPF,PESTRTYP         SAVE RATE TYPE (FROM EST)               
         MVC   SVFALLO,PESTZZZ      SAVE ALLOCATION                             
         GOTO1 DATCON,DMCB,PESTST,(3,SVPERFST)                                  
         GOTO1 (RF),(R1),PESTEND,(3,SVPERFND)                                   
         DROP  R6                                                               
*                                                                               
         MVI   ERROR,BADFREST                                                   
         CLI   ACTNUM,ACTMKLIV     TEST ACTION=MAKELIVE                         
         BNE   *+12                                                             
         TM    SVSTATF,X'80'       TEST FROM EST IN TEST                        
         BO    SCANERR             YES - ERROR                                  
*                                                                               
         OC    BSTART,BSTART       TEST REQ PERIOD INPUT                        
         BZ    VK52                                                             
*                                                                               
         MVI   ERROR,NOTINEST                                                   
         CLC   BSTART,SVPERFST     REQUEST START TO EST START                   
         BL    SCANERR                                                          
         CLC   BEND,SVPERFND       REQUEST END TO EST END                       
         BH    SCANERR                                                          
*                                                                               
VK52     DS    0H                  VALIDATE ADCODE                              
         LA    R2,SFMADCH           NOW THAT WE HAVE QPRD                       
         TM    4(R2),X'20'                                                      
         BO    VK54                                                             
         GOTO1 VALIADC                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VK54     CLI   ACTNUM,ACTMKLIV     MAKELIVE AND CANCEL MUST NOT HAVE            
         BE    VK56                   'TO' DATA                                 
         CLI   ACTNUM,ACTCNCL                                                   
         BNE   VK60                                                             
VK56     MVI   ERROR,INVALID                                                    
         LA    R2,SFMPRDTH                                                      
         CLI   5(R2),0                                                          
         BNE   SCANERR                                                          
         LA    R2,SFMESTTH                                                      
         CLI   5(R2),0                                                          
         BNE   SCANERR                                                          
         EJECT                                                                  
         OC    BSTART,BSTART                                                    
         BNZ   *+10                                                             
         MVC   BSTART(6),SVPERFST  USE ESTIMATE DATES AS DEFAULT                
         B     VK90                                                             
         SPACE 1                                                                
* VALIDATE 'TO' PRODUCT AND ESTIMATE                                            
         SPACE 1                                                                
VK60     MVC   SVPRDT,SVPRDF       ASSUME 'TO' PRD = 'FROM' PRD                 
         LA    R2,SFMPRDTH                                                      
*                                                                               
         BRAS  RE,CKTOAAA          TEST FOR A 'TO' PRD CODE OF AAA              
*                                    WHICH IS NOT ALLOWED                       
         CLI   5(R2),0             TEST 'TO' PRD ENTERED                        
         BE    VK62                                                             
*                                                                               
         CLI   PTOCLT,C' '        SEE IF PATCHING TO CLT                        
         BNH   VK60P                                                            
         MVC   SAVCLT(3),QCLT     SAVE REAL CLIENT                              
         MVC   QCLT,PTOCLT                                                      
*                                                                               
VK60P    GOTO1 VALIPRD                                                          
*                                SEE IF PATCHING TO CLIENT                      
         CLI   PTOCLT,C' '                                                      
         BNH   VK60P5                                                           
         MVC   QCLT(3),SAVCLT     RESTORE REAL CLIENT                           
*                                                                               
VK60P5   DS    0H                                                               
         MVC   SVPRDT,QPRD                                                      
*                                                                               
         L     R6,AIO                                                           
         USING PPRDRECD,R6                                                      
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         USING PPRDELEM,R6                                                      
*                                                                               
         MVC   SVPRSTAT,PPRDSTAT   SAVE "TO" PRODUCT STATUS BYTE                
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BE    VK60CHK                                                          
         CLI   ACTNUM,ACTMOVE                                                   
         BNE   VK60A                                                            
         OC    PPRDEXCL,PPRDEXCL                                                
         BZ    VK60A                                                            
         XC    TOADJ,TOADJ                                                      
         MVC   TOADJ,PPRDEXCL                                                   
         BAS   RE,VKCHKB                                                        
         CH    R7,=H'1'                                                         
         BNE   VK60A                                                            
*                                                                               
*                                                                               
VK60CHK  OC    PPRDEXCL,PPRDEXCL                                                
         BZ    VK60A                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(46),=C' ** FUNCTION NOT AVAILABLE FOR THIS PRODUX        
               CT **'                                                           
         GOTO1 ERREX2                                                           
*                                                                               
*** SET TOFROM SWITCH                                                           
VK60A    MVI   TOFROM,X'11'    PRD TO PRD                                       
         CLC   SVPRDF,=C'ZZZ'                                                   
         BE    VK61                                                             
         CLC   SVPRDT,=C'ZZZ'                                                   
         BNE   VK62                                                             
         MVI   TOFROM,X'12'   PRD TO ZZZ                                        
         MVI   REALLO,C'Y'    MUST SET REALLOCATION SWITCH                      
         B     VK62                                                             
*                                                                               
VK61     MVI   TOFROM,X'22'   ZZZ TO ZZZ                                        
         CLC   SVPRDT,=C'ZZZ'                                                   
         BE    VK62                                                             
         MVI   TOFROM,X'21'   ZZZ TO PRD                                        
         B     VK62                                                             
*                                                                               
VK60ERR  MVI   ERROR,BADZZZ                                                     
         B     SCANERR                                                          
*                                                                               
VK62     LA    R2,SFMESTTH                                                      
         CLI   5(R2),0             TEST 'TO' EST ENTERED                        
         BNE   VK65                                                             
*                                                                               
         MVI   5(R2),3             SET INPUT LENGTH                             
         LH    R0,BEST             SET 'TO' EST = 'FROM' EST                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(3,R2),DUB                                                      
*                                                                               
VK65     DS    0H                                                               
*                                                                               
         CLI   PTOCLT,C' '        SEE IF PATCHING TO CLT                        
         BNH   VK65P                                                            
         MVC   SAVCLT(3),QCLT     SAVE REAL CLIENT                              
         MVC   QCLT,PTOCLT                                                      
*                                                                               
VK65P    DS    0H                                                               
         GOTO1 VALIEST                                                          
*                                                                               
         BRAS  RE,CKIDKEST       Check for iDesk Estimate                       
         JE    *+12                                                             
         LA    R2,SFMESTTH                                                      
         J     TRAPERR2                                                         
*                                SEE IF PATCHING TO CLIENT                      
         CLI   PTOCLT,C' '                                                      
         BNH   VK65P5                                                           
         MVC   QCLT(3),SAVCLT     RESTORE REAL CLIENT                           
*                                                                               
VK65P5   DS    0H                                                               
         MVC   SVESTT,BEST                                                      
*                                                                               
         L     R6,AIO                                                           
         USING PESTRECD,R6                                                      
         MVI   ELCODE,7                                                         
         BAS   RE,GETEL                                                         
         USING PESTELEM,R6                                                      
         SPACE 1                                                                
* SAVE 'TO' EST STATUS AND DATES                                                
         SPACE 1                                                                
         MVC   SVSTATT,PESTTEST                                                 
         MVC   SVRTYPT,PESTRTYP     SAVE RATE TYPE (TO EST)                     
         MVC   SVTALLO,PESTZZZ      SAVE ALLOCATION                             
*COST2*                                                                         
         ZAP   SVE2FAC,=P'0'    SAVE "TO" ESTIMATE'S COST2 FACTOR               
         TM    SVCLTST,X'08'        SEE IF COST2 CLIENT                         
         BNO   *+10                                                             
         MVC   SVE2FAC,PESTCF       COST2 FACTOR                                
*COST2*                                                                         
         TM    TOFROM,X'02'         SEE IF 'TO' EST IS ZZZ                      
         BNO   VK70                                                             
         LA    R2,SFMALLOH          FOR CURSOR                                  
*                                                                               
         CLI   REALLO,C'Y'          SEE IF REALLOCATING                         
         BNE   VK67                                                             
         OC    SVTALLO,=47C' '                                                  
         CLI   SFMALLOH+5,0         SEE IF ALLOCATION INPUT IN SCREEN           
         BE    VK66                                                             
*                                   ALLOCATION INPUT                            
         CLI   SVTALLO,C' '         SEE IF 'TO' ZZZ EST HAD ALLO                
         BE    VK68                 NO THEN ALLOW INPUT                         
         OC    SFMALLO,=47C' '                                                  
         CLC   SFMALLO(47),SVTALLO  MUST MATCH 'TO' ZZZ ALLO                    
         BE    VK68                                                             
         MVI   ERROR,ALOERR2                                                    
         B     SCANERR                                                          
*                                                                               
VK66     FOUT  SFMALLOH,SVTALLO,47                                              
         LA    R2,SFMALLOH         SET INPUT LENGTH                             
         CLC   SVTALLO,=47C' '                                                  
         BE    VK68                                                             
         LA    R5,47(R2)                                                        
         CLI   7(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         STC   R5,5(R2)                                                         
         B     VK68                                                             
*                                                                               
VK67     DS    0H                  HERE IF NOT REALLOCATING                     
         CLI   SFMALLOH+5,0        NO INPUT ALLOWED IN ALLOCATION               
         BE    VK67C               ALLOCATION WILL STAY THE SAME                
*                                  AS ON "FROM' BUYS                            
         LA    R2,SFMOPTSH         SET CURSOR TO OPTIONS                        
         MVI   ERROR,MISSING       OPTIONS SHOULD BE REALLO=Y                   
         B     SCANERR                                                          
*                                                                               
VK67C    CLI   SVTALLO,C' '      SEE IF 'TO' EST HAS ALLO                       
         BNH   VK70              NO - THEN OK TO USE OLD ALLOS                  
         CLC   SVFALLO,SVTALLO   YES - THEN IT MUST MATCH 'FROM' EST            
         BE    VK70                                                             
         MVI   ERROR,ALOERR2                                                    
         B     SCANERR                                                          
*                                  HERE IF REALLOCATING                         
VK68     CLI   SFMALLOH+5,0        ALLOCATION WILL BE INPUT BY NOW              
         BNE   VK70                                                             
         MVI   ERROR,MISSING                                                    
         B     SCANERR                                                          
*                                                                               
VK70     GOTO1 DATCON,DMCB,PESTST,(3,SVPERTST)                                  
         GOTO1 (RF),(R1),PESTEND,(3,SVPERTND)                                   
         DROP  R6                                                               
*                                                                               
VK71     DS    0H                                                               
         TM    TOFROM,X'02'         SEE IF 'TO' EST IS ZZZ                      
         BNO   VK71X                                                            
         CLI   REALLO,C'Y'          SEE IF REALLOCATING                         
         BNE   VK71X                                                            
         CLI   ACTNUM,ACTMOVE      SEE IF MOVING                                
         BNE   VK71X                                                            
*                                  MUST SET UP DUMMY BUY AND HAVE               
*                                  SET21 EDIT ALLOCATION LINE NOW               
*                                  TO CATCH ERRORS BEFORE ACTION MOVE           
*                                  CAN DELETE BUYS                              
         L     R6,AIO                                                           
         XC    0(250,R6),0(R6)                                                  
         XC    250(250,R6),250(R6)                                              
         USING PBUYREC,R6                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,SVPRDF                                                  
         MVC   PBUYKPUB(6),BPUB                                                 
         MVC   PBUYKDAT,BSTART     SET REQ START                                
         MVC   PBUYLEN,=H'149'    33 + 116 = 149                                
         MVC   PBDELEM(2),=X'2074'                                              
         MVC   SVKEY,PBUYREC                                                    
         BAS   RE,SET21                                                         
         XC    SVKEY,SVKEY         JUST IN CASE                                 
         DROP  R6                                                               
*                                                                               
VK71X    LA    R2,SFMESTTH         RESET R2                                     
*                                                                               
         OC    BSTART,BSTART       TEST REQ PERIOD INPUT                        
         BZ    VK72                NO                                           
         MVI   ERROR,NOTINEST                                                   
         CLC   BSTART,SVPERTST     REQ START TO EST START                       
         BL    SCANERR                                                          
         CLC   BEND,SVPERTND       REQ END TO EST END                           
         BH    SCANERR                                                          
*                                                                               
         EJECT                                                                  
* NOW MAKE SURE ESTIMATE DATES OVERLAP *                                        
         SPACE 1                                                                
VK72     MVI   ERROR,NOOVRLAP                                                   
         LA    R2,SFMESTFH                                                      
         CLC   SVPERTND,SVPERFST   'TO' EST END TO 'FR' EST START               
         BL    SCANERR                                                          
         CLC   SVPERTST,SVPERFND   'TO' EST START TO 'FR' EST END               
         BH    SCANERR                                                          
         SPACE 1                                                                
* LIMIT REQUEST PERIOD TO LATEST START/EARLIEST END DATE *                      
         SPACE 1                                                                
         OC    BSTART,BSTART                                                    
         BNZ   VK80                                                             
         MVC   BSTART(6),SVPERFST                                               
         CLC   BSTART,SVPERTST                                                  
         BH    *+10                                                             
         MVC   BSTART,SVPERTST                                                  
         CLC   BEND,SVPERTND                                                    
         BL    *+10                                                             
         MVC   BEND,SVPERTND                                                    
         SPACE 1                                                                
* FINAL VALIDATION ON INPUT PARAMETERS *                                        
         SPACE 1                                                                
VK80     LA    R2,SFMPRDFH                                                      
         CLC   SVPRDT(6),SVPRDF    TEST FROM PRD/EST = TO PRD EST               
         BNE   VK82                                                             
*                                                                               
         CLI   SFMPRDTH+5,0        POSSIBLE THAT NOTHING IN "TO" FLD            
         BNE   *+16                                                             
         LA    R2,SFMPRDTH         PLACE CURSOR ON "TO" FLD                     
         MVI   ERROR,MISSING                                                    
         B     *+8                                                              
*                                                                               
         MVI   ERROR,SAMEDATA                                                   
         CLI   ACTNUM,ACTMKLIV     TEST MAKELIVE                                
         BNE   SCANERR                                                          
         B     VK99                                                             
*                                                                               
VK82     MVI   ERROR,BADTOEST                                                   
         TM    SVSTATF,X'80'       TEST FROM EST IS TEST                        
         BO    VK83                YES - TO EST STATUS IRRELEVANT               
         TM    SVSTATT,X'80'       FROM IS NOT - TEST TO EST IS TEST            
         BO    SCANERR             YES - ERROR                                  
         B     VK83                                                             
*                                                                               
VK83     MVI   ERROR,BADRTMAT                                                   
         CLC   SVRTYPF,SVRTYPT     TO AND FROM EST RATES TYPES                  
         BNE   SCANERR             MUST MATCH                                   
*                                                                               
*                                                                               
VK85     TM    TOFROM,X'22'        SEE IF PROCESSING ZZZ TO ZZZ                 
         BNO   VK90                                               BUG01         
         CLI   SFMALLOH+5,0      SEE IF ALLOCATION LINE ENTERED                 
         BNE   VK90             YES - MISSING BRAND ESTS WILL BE                
*                               FOUND IN SET21                                  
         LA    R2,SFMESTTH      CURSOR TO 'TO' EST                              
         MVI   ERROR,INVEST                                                     
*                             ZECHK MUST CHECK ALL BRANDS                       
*                             SINCE I DON'T KNOW THE 'TO' ALLOCATION            
         BRAS  RE,ZECHK         GO CHECK FOR BRAND ESTS FOR 'TO' EST            
         BNZ   SCANERR           CC NOT EQUAL - INVALID 'TO' EST                
*                                                                               
VK90     OC    BPUB,BPUB           PUB = ALL                                    
         BE    VK95                                                             
         CLI   ACTNUM,ACTCNCL      ACTION = CANCEL                              
         BE    VK95                                                             
         CLI   ACTNUM,ACTMKLIV     AND ACTION = MAKELIVE                        
         BNE   VK99                                                             
VK95     LA    R2,SFMADCH                                                       
         CLI   5(R2),0             REQUIRES ADCODE                              
         BNE   VK99                                                             
         LA    R2,SFMDTSH                                                       
         CLI   5(R2),0                OR DATES                                  
         BNE   VK99                                                             
         MVI   ERROR,MISSING                                                    
         B     SCANERR                                                          
*                                                                               
VK99     DS    0H       ***** TEST FOR CLIENT FROZEN-BY-DATE *****              
*                                                                               
         CLI   ACTNUM,ACTCNCL      ACTION = CANCEL                              
         BE    VK100                                                            
         TM    SVCLTST,X'10'       FROZEN BY DATE ?                             
         BNO   VK100               NO                                           
         LA    R2,CONACTH          PLACE CURSOR ON ACTION FIELD                 
         MVI   ERROR,FRZNDATE      ERROR 110 - INCLUDES FROZEN DATES            
*                                                                               
* SVCFDTA(1) CONTAINS INDICATOR FROM FREEZE STATUS ELEM IN PCLTREC              
*                                                                               
         TM    SVCFDTA,X'08'       LOCK THIS MONTH AND ALL FORWARD ?            
         BO    VK99FOR             YES                                          
         TM    SVCFDTA,X'04'       LOCK THIS MONTH AND ALL PRIOR   ?            
         BO    VK99BAK             YES                                          
         TM    SVCFDTA,X'02'       LOCK THIS MONTH ONLY            ?            
         BO    *+6                 YES                                          
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
* SVCFDTA+1 CONTAINS DATE (YM) FROM FREEZE STATUS ELEM IN PCLTREC               
*                                                                               
         CLC   SVCFDTA+1(2),BSTART                                              
         BL    VK99OK                                                           
         CLC   SVCFDTA+1(2),BEND                                                
         BH    VK99OK                                                           
         B     SCANERR              NO BUYING FOR THIS MONTH                    
*                                                                               
VK99FOR  CLC   SVCFDTA+1(2),BEND                                                
         BH    VK99OK                                                           
         B     SCANERR             NO BUYING FOR THIS MONTH & FORWARD           
*                                                                               
VK99BAK  CLC   SVCFDTA+1(2),BSTART                                              
         BL    VK99OK                                                           
         B     SCANERR             NO BUYING FOR THIS MONTH & PRIOR             
*                                                                               
VK99OK   DS    0H                                                               
*                                                                               
         EJECT                                                                  
* PROCESS REQUEST *                                                             
         SPACE 1                                                                
VK100    DS    0H                                                               
         CLI   ACTNUM,ACTCOPY      SEE IF COPYING                               
         BE    VK200                                                            
         CLI   ACTNUM,ACTMKLIV     SEE IF MAKE LIVE                             
         BNE   VK300                                                            
*                                                                               
         CLC   SVPRDF,=C'ZZZ'      'FROM' ESTIMATE ZZZ ?                        
         BNE   VK200               NO - SKIP CHECK FOR TEST ESTIMATES           
*                                                                               
*                   DISALLOW "ZZZ" MAKELIVE IF ANY TEST ESTIMATE FOUND          
*                     FOR ANY PRODUCT WITH SELECTED ESTIMATE NUMBER             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PESTRECD,R6         GET ESTIMATE RECORD                          
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,QCLT                                                    
         MVC   SVKEY,KEY                                                        
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
*                                                                               
VK110    GOTO1 HIGH                                                             
*                                                                               
VK130    CLC   KEY(7),SVKEY        CHK AGY/MED/CODE/CLT                         
         BNE   VK190               DONE                                         
         CLC   KEY+10(2),SVESTF    SEE IF 'FROM' EST                            
         BH    VK170                IF HIGH - SKIP TO NEXT PRD                  
         BL    VK180                IF LOW - READ FOR 'FROM' EST                
*                                                                               
         MVC   AIO,AIO1            EQUAL -  GET RECORD TO TEST                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,7                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING PESTELEM,R6                                                      
         TM    PESTTEST,X'80'      TEST ESTIMATE ?                              
         BNO   VK170               NO - OK - TEST NEXT                          
*                                  YES - ERROR                                  
         LA    R2,SFMESTFH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRDTERR),PRDTERR                                       
         MVC   CONHEAD+L'PRDTERR+1(3),KEY+7    PRODUCT CODE                     
         GOTO1 ERREX2                                                           
*                                                                               
VK170    MVI   KEY+10,X'FF'        SKIP TO NEXT PRD                             
         B     VK110                                                            
*                                                                               
VK180    MVC   KEY+10(2),SVESTF    READ FOR 'FROM' EST                          
         B     VK110                                                            
*                                                                               
PRDTERR  DC    C'** INVALID - TEST ESTIMATE FOUND FOR PRODUCT ='                
*                                                                               
VK190    DS    0H                                                               
         DROP  R6                                                               
*                                                                               
VK200    DS    0H                  GO READ CONTRACTS AND CHECK                  
*                                  IF ANY HAVE MAX/ISSUE DATA                   
*                                  IF SO I MUST DISALLOW MAKE-LIVE              
*                                  AND COPYING                                  
         BRAS  RE,CKMAX                                                         
*                                                                               
VK300    DS    0H                                                               
         OI    SFMPRDFH+4,X'20'    SET VALIDATED                                
         OI    SFMESTFH+4,X'20'    SET VALIDATED                                
         OI    SFMPRDTH+4,X'20'    SET VALIDATED                                
         OI    SFMESTTH+4,X'20'    SET VALIDATED                                
         OI    SFMALLOH+4,X'20'    SET VALIDATED                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PBUYREC,R6                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,SVPRDF                                                  
         MVC   PBUYKPUB(6),BPUB                                                 
         MVC   PBUYKDAT,BSTART     SET REQ START                                
         DROP  R6                                                               
         MVC   SVKEY,KEY                                                        
         OC    BPUB,BPUB           TEST PUB=ALL                                 
         BZ    PR120                                                            
*                                                                               
PR102    DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 HIGH                                                             
         B     PR106                                                            
*                                                                               
PR104    DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 SEQ                                                              
*                                                                               
PR106    CLC   KEY(PBUYKDAT-PBUYKEY),SVKEY  M/TY/C/P/PUB                        
         BNE   PR900                                                            
         LA    RE,(PBUYKDAT-PBUYKEY)+KEY                                        
         CLC   0(3,RE),BEND              TEST PAST REQ END DATE                 
         BH    PR900                                                            
         LA    RE,(PBUYKEST-PBUYKEY)+KEY                                        
         CLC   0(2,RE),SVESTF            ACTUAL TO 'FROM' EST                   
         BNE   PR104                                                            
         LA    RE,PBUYKACT-PBUYKEY+KEY   POINT TO PASSIVE FIELD                 
         OC    0(3,RE),0(RE)                                                    
         BNZ   PR104                     SKIP PASSIVE POINTERS                  
         B     PR150                                                            
         EJECT                                                                  
* PROCESSING FOR PUB = ALL                                                      
         SPACE 1                                                                
PR120    DS    0H                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,DMCB            ADDR OF GETFACT WORKAREA                      
         USING FACTSD,R4                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,FATMAXIO                                                    
         M     R0,=F'90'           SET LIMIT = 90 PCT OF MAX                    
         D     R0,=F'100'                                                       
         STH   R1,SVMAXIO                                                       
         DROP  R4                                                               
*                                                                               
PR122    DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 HIGH                                                             
         B     PR126                                                            
*                                                                               
PR124    DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 SEQ                                                              
*                                                                               
PR126    CLC   KEY(10),SVKEY       M/TY/C/P                                     
         BNE   PR900                                                            
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,DMCB             ADDR OF GETFACT WORKAREA                     
         USING FACTSD,R4                                                        
         CLC   FATIOCNT,SVMAXIO    TEST REACHED LIMIT                           
         BL    PR128               NO                                           
         MVC   SVLSTKEY,KEY        SAVE START KEY FOR NEXT TIME                 
         B     PR900               AND GET OUT NOW                              
         DROP  R4                                                               
*                                                                               
PR128    DS    0H                                                               
         CLC   KEY(16),SVKEY       SAME TY/A/M/C/PUB                            
         BNE   PR130                                                            
*                                                                               
         CLC   KEY+16(3),BEND      TEST PAST REQ END DATE                       
         BH    PR132                                                            
         CLC   KEY+19(2),SVESTF    ACTUAL TO 'FROM' EST                         
         BNE   PR124                                                            
         OC    KEY+21(3),KEY+21    TEST PASSIVE                                 
         BNZ   PR124               YES - SKIP                                   
         B     PR150                                                            
* NEW PUB *                                                                     
PR130    XC    KEY+16(9),KEY+16    CLEAR PAST END DATE                          
         MVC   KEY+16(3),BSTART    SET START DATE                               
         MVC   SVKEY,KEY                                                        
         B     PR122               AND READ HIGH AGAIN                          
*                                                                               
PR132    MVC   KEY+16(3),=X'FFFFFF' PAST END DATE - FORCE NEW PUB               
         B     PR122                                                            
         EJECT                                                                  
* WE HAVE AN INSERTION *                                                        
         SPACE 1                                                                
PR150    MVC   SVKEY,KEY           SAVE CURRENT KEY                             
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 GETREC                                                           
         L     RE,DMCB+8           GET ADDRESS OF DISK ADDR                     
         MVC   DMDSKADD,0(RE)      AND SAVE DISK ADDRESS                        
*                                                                               
         L     R6,AIO                                                           
         USING PBUYRECD,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         USING PBDELEM,R6                                                       
         OC    QADCODE,QADCODE     TEST FILTER ON ADCODE                        
         BZ    *+14                                                             
         CLC   QADCODE,PBDJOB      MATCH AD CODES                               
         BNE   PR210                                                            
*                                                                               
***************                    DO NOT COPY WSJ BUYS                         
***************                    (WALL STREET JOURNAL SCREEN)                 
***************                    IF NOT DOING ALL PUBS                        
***************                    OR IF COPYING OR MOVING TO A                 
***************                    DIFFERENT PRODUCT                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'35'        WSJ ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   PR155               IF FOUND                                     
         OC    BPUB,BPUB           SEE IF DOING ALL PUBS                        
         BNZ   PR210               NO - I MUST SKIP WSJ BUYS                    
*                                                                               
         CLI   ACTNUM,ACTMOVE      SEE IF COPYING OR MOVING                     
         BE    PR152                                                            
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   PR155                                                            
*                                                                               
PR152    DS    0H                                                               
         CLC   SVPRDF,SVPRDT       SEE IF "FROM" PRD = "TO" PRD                 
         BNE   PR210               NO THEN I MUST SKIP WSJ BUYS                 
*                                                                               
PR155    DS    0H                  FIRST TEST FOR LOCKED RECORD                 
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BE    PR156               LOCK NOT RELEVANT FOR COPY                   
*                                                                               
         CLI   LOCKSW,C'Y'         LOCK TESTED ALREADY ?                        
         BE    PR156               YES - CONTINUE                               
*                                                                               
         BRAS  RE,TSTLOK           TEST FOR CLIENT OR CLT/PUB LOCK              
         BE    PR156               HAVE NOW SET A LOCK MYSELF                   
*                                                                               
PR155E   MVI   ERROR,DATALOCK      REC LOCKED FOR OFFLINE PROCESSING            
         B     SCANERR                                                          
*                                                                               
PR156    DS    0H                                                               
         CLI   ACTNUM,ACTMKLIV                                                  
         BE    PR200                                                            
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   PR160                                                            
         BAS   RE,CLRBUY                                                        
         BAS   RE,ADDBUY                                                        
         B     PR174                                                            
*                                                                               
PR160    CLI   ACTNUM,ACTMOVE                                                   
         BNE   PR170                                                            
*                                                                               
         BRAS  RE,CHKINV           CHECK FOR INVOICE DATA                       
         BE    PR165               OKAY TO MOVE                                 
*                                  INVOICE ATTACHED                             
         L     RF,MVCNOBYS         BUMP NUMBER OF BUYS NOT MOVED                
         AHI   RF,1                                                             
         ST    RF,MVCNOBYS                                                      
*                                                                               
         B     PR174               BYPASS MOVE                                  
*                                                                               
PR165    DS    0H                                                               
*                                                                               
         BAS   RE,DELBUY                                                        
         BAS   RE,CLRBUY                                                        
         BAS   RE,ADDBUY                                                        
         B     PR174                                                            
*                                                                               
PR170    CLI   ACTNUM,ACTCNCL                                                   
         BNE   PR172                                                            
*                                                                               
         BRAS  RE,CHKINV           CHECK FOR INVOICE DATA                       
         BE    PR171               OKAY TO CANCEL                               
*                                                                               
*                                  INVOICE ATTACHED                             
         L     RF,DELNOBYS         BUMP NUMBER OF BUYS NOT DELETED              
         AHI   RF,1                                                             
         ST    RF,DELNOBYS                                                      
*                                                                               
         B     PR174               BYPASS CANCEL                                
*                                                                               
PR171    DS    0H                                                               
*                                                                               
         BAS   RE,DELBUY                                                        
         B     PR174                                                            
*                                                                               
PR172    DC    H'0'                                                             
*                                                                               
PR174    MVC   KEY,SVKEY           RESTORE LAST KEY READ                        
         OI    DMINBTS,X'08'       IT MAY VERY WELL BE DELETED                  
         GOTO1 HIGH                RESTORE FOR SEQ                              
         B     PR210                                                            
         EJECT                                                                  
PR200    DS    0H                  *** ACTION = MAKELIVE ***                    
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLI   PBDBFD,C'T'         TEST 'TEST' INSERTION                        
*NOP*    BNE   PR210                                                            
         BNE   PR207                                                            
         XC    BLOCK(256),BLOCK                                                 
         LA    R7,BLOCK                                                         
         USING PVALUESD,R7                                                      
*                                                                               
         GOTO1 VGETINS,DMCB,(C'X',(R6)),(R7),PBUYKPRD                           
*                                                                               
         L     R1,LIVDOLS                                                       
         A     R1,GROSS                                                         
         ST    R1,LIVDOLS                                                       
         L     R1,LIVBUYS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,LIVBUYS                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PBDELEM,R6                                                       
*                                                                               
         MVI   PBDBFD,0            MAKE THE BUY LIVE                            
         MVC   PBDBUYDT,BTODAY     SET NEW BUY DATE                             
         MVC   PBDDATE,BTODAY      SET LAST CHANGE=TODAY                        
         MVI   PBDDTIND,0                                                       
         MVI   PBDDTIN2,0                                                       
         MVI   PBDDTIN3,X'04'      SET LAST ACTION=MAKELIVE                     
*                                                                               
         MVI   ELCODE,X'24'        DELETE ACTIVITY ELEMENTS                     
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'FF'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING PCHGELEM,R4                                                      
         MVI   PCHGELEM,X'24'                                                   
         MVI   PCHGLEN,PCHGNEWS     NEW LENGTH 11                               
         MVC   PCHGDAT,PTODAY       PACKED                                      
         MVI   PCHGIND3,X'04'       MAKE LIVE                                   
*                                                                               
         MVC   PCHGGRS(L'SVSFMPID),SVSFMPID                                     
         MVI   PCHGGRS+L'SVSFMPID,0                                             
*                                                                               
         L     R7,AIO                                                           
         USING PBUYRECD,R7                                                      
         CLC   PBUYREC+25(2),=H'3975'    SEE IF I HAVE ROOM                     
         BNL   PR205                                                            
         GOTO1 VRECUP,DMCB,(1,PBUYREC),ELEM,(R6)                                
*                                                                               
         DROP  R4                                                               
         DROP  R7                                                               
*                                                                               
PR205    MVI   ACTELOPT,C'N'       SUPPRESS ACTIVITY ELEMENT                    
         GOTO1 PUTREC                                                           
*                                                                               
         BAS   RE,UPDASR           DO AOTOMATIC SPACE RESERVATION               
*                                  FOR MAKE-LIVES                               
PR207    DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
*                                                                               
PR210    OC    BPUB,BPUB           TEST PUB = 'ALL'                             
         BZ    PR124                                                            
         B     PR104                                                            
         DROP  R6                                                               
         EJECT                                                                  
PR900    DS    0H                                                               
         OC    SVLSTKEY,SVLSTKEY   TEST CONTINUATION PENDING                    
         BNZ   PR901                                                            
*                                                                               
         LA    R2,SFMMEDH                                                       
         MVI   ERROR,NORECS                                                     
*                                                                               
         OC    MVCNOBYS,MVCNOBYS   IF NO ACTIVITY SKIP RECAP                    
         BNZ   PR901                                                            
         OC    DELNOBYS,DELNOBYS   IF NO ACTIVITY SKIP RECAP                    
         BNZ   PR901                                                            
         OC    BUYTOTS,BUYTOTS                                                  
         BZ    SCANERR                                                          
*                                                                               
PR901    CLI   ACTNUM,ACTMKLIV     TEST ACTION=MAKELIVE                         
         BE    PR940                                                            
*                                                                               
         LA    R2,SFMOUT1H                                                      
*                                                                               
         ICM   R0,15,DELBUYS                                                    
         BZ    PR905                                                            
*                                                                               
         MVC   8(16,R2),=C'999 BUYS DELETED'                                    
*                                                                               
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
*                                                                               
         EDIT  (R0),(3,8(R2))                                                   
*                                                                               
         L     R0,DELDOLS                                                       
         EDIT  (R0),(15,25(R2)),2,FLOAT=$,ALIGN=LEFT                            
*                                                                               
         OI    6(R2),X'80'         SET TO XMT                                   
*                                                                               
         LA    R2,SFMOUT2H         ADVANCE OUTPUT POINTER                       
*                                                                               
PR905    DS    0H                                                               
*                                                                               
         ICM   R0,15,ADDBUYS                                                    
         BZ    PR910                                                            
*                                                                               
         MVC   8(14,R2),=C'999 BUYS ADDED'                                      
*                                                                               
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
*                                                                               
         EDIT  (R0),(3,8(R2))                                                   
*                                                                               
         L     R0,ADDDOLS                                                       
         EDIT  (R0),(15,25(R2)),2,FLOAT=$,ALIGN=LEFT                            
*                                                                               
         OI    6(R2),X'80'         SET TO XMT                                   
*                                                                               
         LA    R2,SFMOUT3H         ADVANCE OUTPUT POINTER                       
*                                                                               
PR910    DS    0H                                                               
*                                                                               
         ICM   R0,15,MVCNOBYS      NUMBER OF BUYS NOT MOVED                     
         BZ    PR920                                                            
*                                                                               
         MVC   8(43,R2),=C'999 BUYS NOT MOVED BECAUSE INVOICE ATTACHED'         
*                                                                               
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
*                                                                               
         EDIT  (R0),(3,8(R2))                                                   
*                                                                               
         OI    6(R2),X'80'         SET TO XMT                                   
*                                                                               
         LA    R2,SFMOUT4H         ADVANCE OUTPUT POINTER                       
*                                                                               
PR920    DS    0H                                                               
*                                                                               
         ICM   R0,15,DELNOBYS      NUMBER OF BUYS NOT CANCELLED                 
         BZ    PR930                                                            
*                                                                               
      MVC   8(45,R2),=C'999 BUYS NOT DELETED BECAUSE INVOICE ATTACHED'          
*                                                                               
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
*                                                                               
         EDIT  (R0),(3,8(R2))                                                   
*                                                                               
         OI    6(R2),X'80'         SET TO XMT                                   
*                                                                               
PR930    DS    0H                                                               
*                                                                               
         B     PR999                                                            
         SPACE 1                                                                
PR940    DS    0H                  ACTION=MAKELIVE                              
         LA    R2,SFMMEDH                                                       
         MVI   ERROR,NOTSTBUY                                                   
         ICM   R0,15,LIVBUYS                                                    
         BZ    SCANERR                                                          
*                                                                               
         LA    R2,SFMOUT1H                                                      
         MVC   8(18,R2),=C'999 BUYS MADE LIVE'                                  
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         L     R0,LIVDOLS                                                       
         EDIT  (R0),(15,30(R2)),2,FLOAT=$,ALIGN=LEFT                            
         OI    6(R2),X'80'         SET TO XMT                                   
         B     PR999                                                            
*                                                                               
PR999    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'** ACTION COMPLETED **'                           
         LA    R2,SFMMEDH       CURSOR TO MEDIA                                 
         OC    SVLSTKEY,SVLSTKEY   TEST CONTINUATION ALLOWED                    
         BZ    *+10                                                             
         MVC   CONHEAD(32),=C'HIT ENTER TO CONTINUE PROCESSING'                 
*NOP*    B     PRX                                                              
*                                                                               
PRX      DS    0H                                                               
         CLC   CONHEAD(9),=C'** ACTION'                                         
         BNE   PRXX                                                             
         NI    SFMESTTH+4,X'DF'    INVALIDATE "TO" ESTIMATE                     
         XC    SFMESTT,SFMESTT     CLEAR AND FOUT                               
         FOUT  SFMESTTH                                                         
         NI    SFMPRDTH+4,X'DF'    INVALIDATE "TO" PRODUCT                      
         XC    SFMPRDT,SFMPRDT     CLEAR AND FOUT                               
         FOUT  SFMPRDTH                                                         
         XC    SVLSTKEY,SVLSTKEY   CLEAR "CONTINUATION" KEY                     
PRXX     DS    0H                                                               
         MVI   LOCKSW,0            TURN OFF LOCK TEST SWITCH                    
*NOP*    BRAS  RE,UNLOCK           UNLOCK KEY SET IN THIS APPLICATION           
*                                                                               
         GOTO1 ERREX2              EXIT WITH MESSAGE IN PLACE                   
*                                                                               
*                                                                               
PRINTIT  DS    0H                                                               
         GOTO1 OPENPQ                                                           
*****************************************************************               
         DC    H'0'                PRINT REPORT HERE                            
*****************************************************************               
         EJECT                                                                  
CLRMED   NI    SFMMEDH+4,X'DF'                                                  
*                                                                               
CLRCLT   NI    SFMCLTH+4,X'DF'                                                  
         XC    SFMCLNM,SFMCLNM                                                  
         OI    SFMCLNMH+6,X'80'                                                 
*                                                                               
CLRPUB   NI    SFMPUBH+4,X'DF'                                                  
         XC    SFMPBNM,SFMPBNM                                                  
         OI    SFMPBNMH+6,X'80'                                                 
*                                                                               
CLRPRES  NI    SFMADCH+4,X'DF'                                                  
         NI    SFMPRDFH+4,X'DF'                                                 
         NI    SFMESTFH+4,X'DF'                                                 
         NI    SFMPRDTH+4,X'DF'                                                 
         NI    SFMESTTH+4,X'DF'                                                 
*                                                                               
         XC    SVTODATA,SVTODATA                                                
         XC    SVFRDATA,SVFRDATA                                                
         BR    RE                                                               
         EJECT                                                                  
******************************************                                      
* SUBROUTINE TO REMOVE UNNEEDED ELEMENTS *                                      
* AND INSERT BLANK ONES FOR PAY/BILL/IO  *                                      
******************************************                                      
         SPACE 1                                                                
CLRBUY   NTR1                                                                   
         MVI   ELCODE,X'A5'        SPECIAL PAY "FIX" ELEMENTS (ZENITH)          
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'24'        TRACKING ELEMENTS                            
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'A7'        PID ELEM                                     
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'25'        PAY ELEMENTS                                 
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'26'        BILL ELEMENTS                                
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'28'        OPEN BILLING ELEMS                           
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'29'        REBATE BILL ELEMS                            
         BAS   RE,DELEL                                                         
*                                                                               
         CLI   ACTNUM,ACTCOPY      COPY ACTION ?                                
         BNE   CLRB2               NO                                           
         MVI   ELCODE,X'50'        INV MATCH ELEMENT                            
         BAS   RE,DELEL                                                         
         MVI   ELCODE,X'51'        NEW INVOICE ELEMENTS                         
         BAS   RE,DELEL                                                         
         MVI   ELCODE,X'52'        MATCHING STATUS ELEMENTS                     
         BAS   RE,DELEL                                                         
*                                                                               
CLRB2    MVI   ELCODE,X'70'        INSORD ELEMENTS                              
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'76'        AUTO RATE CHANGE TRACK                       
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'90'        UPLOAD ELEM                                  
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'F1'        GENCON ACTIVITY ELEM                         
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'79'        SHIPPING LIST ELEMENTS                       
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'94'        LEGAL WARNING OVERRIDE ELEMENT               
         CLC   SVPRDF,SVPRDT       FROM PRD = TO PRD ?                          
         BE    *+8                 YES - DO NOT DELETE ELEMENT                  
         BAS   RE,DELEL                                                         
*                                                                               
         MVI   ELCODE,X'99'        SERIAL NUMBER ELEMENT                        
         BAS   RE,DELEL                                                         
*                                                                               
*COST2*                                                                         
         MVI   ELCODE,X'91'       CLEAR OLD COST2 ELEMENT                       
         BAS   RE,DELEL                                                         
*COST2*                                                                         
*                                                                               
         TM    TOFROM,X'21'        SEE IF ZZZ TO PRD                            
         BNO   CLRB5                                                            
*                                                                               
CLRB4    MVI   ELCODE,X'21'        DELETE PRD ALLOC ELEMS                       
         BAS   RE,DELEL                                                         
         B     CLRB10                                                           
*                                                                               
CLRB5    CLI   REALLO,C'Y'         SEE IF REALLOCATING                          
         BE    CLRB4               YES - GO DELETE OLD X'21' ELEMS              
         SPACE 1                                                                
* NOW INSERT NEW ELEMENTS *                                                     
*                                                                               
         SPACE 1                                                                
CLRB10   DS    0H                                                               
         CLI   REALLO,C'Y'          SEE IF REALLOCATING                         
         BNE   CLRB15                                                           
         BAS   RE,SET21        GO ADD NEW 21 ELEMS                              
CLRB15   L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PBDELEM,R6                                                       
         MVC   PBDBUYDT,BTODAY     SET CREATION DAT                             
         XC    PBDDATE,PBDDATE     CLEAR LAST CHG DATE                          
         MVI   PBDDTIND,0                                                       
         MVI   PBDDTIN2,0                                                       
         MVI   PBDDTIN3,0                                                       
*                                                                               
         NI    PBDSTAT2,X'FF'-X'A0'  SET OFF "FROM ADB" (X'80') AND             
*                                 "FROM IDESK" (X'20') BITS IN PBDSTAT2         
         MVI   PBDSTAT,0           CLEAR STATUS BYTE                            
*                                                                               
**  TEST SAVED PESTTEST OF TO ESTIMATE FOR SPECIAL FINANCIAL HANDLING           
         TM    SVSTATT,X'01'       "TO" ESTIMATE SFH ?                          
         BNO   *+8                 NO                                           
         OI    PBDSTAT,X'0C'       YES - SET STATUS BYTE TO "HELD" SFH          
*                                                                               
         TM    SVPRSTAT,X'20'      "TO" PRODUCT "NO TRAFFIC" ?                  
         BNO   *+8                 NO                                           
         OI    PBDSTAT,X'20'       YES - SET BUY STATUS BYTE TO SAME            
*                                                                               
*****    NI    PBDCNDA,X'FE'       SET OF X'01' - PAID WITH GST                 
         NI    PBDCNDA,X'FF'-X'03'     SET OFF X'01' - PAID WITH GST            
*                                          AND X'02' - PAID WITH PST            
*                                                                               
         CLC   SVPRDF,SVPRDT       TEST FROM PRD = TO PRD                       
         BE    *+10                                                             
         XC    PBDJOB,PBDJOB       NO - CLEAR ADCODE                            
*                                                                               
         L     R6,AIO              FIND E-O-R                                   
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,GETEL                                                         
         LR    R7,R6               SAVE E-O-R ADDRESS                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'25'          PAY ELEM                                     
         MVI   ELEM+1,24           WAS 22 - NOW BIGGER PAY ELEMENTS             
         BAS   RE,ADDEL                                                         
*                                                                               
         MVI   ELEM,X'26'          BILL ELEM                                    
         MVI   ELEM+1,23                                                        
         LA    R5,ELEM                                                          
         USING PBILELEM,R5                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'21'        SEARCH FOR PRODUCT ELEMENTS                  
         BAS   RE,GETEL                                                         
         BNE   CLRB30              IF NO PRD ELEMS, ADD FOR PRD IN KEY          
         B     *+8                                                              
CLRB20   BAS   RE,NEXTEL                                                        
         BNE   CLRB40                                                           
*                                                                               
         USING PPRELEM,R6                                                       
         MVC   PBPRD,PPRCODE       MOVE PRD CODE TO BILL ELEMENT                
         BAS   RE,ADDEL                                                         
         B     CLRB20                                                           
*                                                                               
CLRB30   L     R6,AIO                                                           
         MVC   PBPRD,SVPRDT                                                     
         BAS   RE,ADDEL                                                         
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
CLRB40   DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'70'          INSORD ELEM                                  
         MVI   ELEM+1,50                                                        
         BAS   RE,ADDEL                                                         
*                                                                               
         XC    ELEM,ELEM           X'A7' PID ELEMENT                            
*BELOW*  MVI   ELEM,X'A7'                                                       
*BELOW*  MVI   ELEM+1,6                                                         
*BELOW*  MVC   ELEM+2(L'SVSFMPID),SVSFMPID                                      
*BELOW*  BAS   RE,ADDEL                                                         
*                                                                               
PPP      USING PPIDELD,ELEM                                                     
         MVI   PPP.PPIDELM,PPIDELQ      'A7'                                    
         MVI   PPP.PPIDELL,PPIDLEQ      '0A'                                    
         MVC   PPP.PPIDPID,SVSFMPID     ADDER PID                               
*                                                                               
         L     RE,ATWA                  POINT TO TWA                            
         USING TWAD,RE                                                          
         MVC   PPP.PPUSRID,TWAUSRID     ADD CONNECT ID NUM                      
         DROP  RE                                                               
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   CLRB40D                                                          
         OI    PPP.PPIDPRG,PPIDCPYQ     "SFM BUY COPY"                          
         B     CLRB40P                                                          
*                                                                               
CLRB40D  DS    0H                                                               
         CLI   ACTNUM,ACTMOVE                                                   
         BNE   CLRB40P                                                          
         OI    PPP.PPIDPRG,PPIDMOVQ     "SFM BUY MOVE"                          
*                                                                               
         DROP  PPP                                                              
*                                                                               
CLRB40P  DS    0H                                                               
         BAS   RE,ADDEL                                                         
*                                                                               
*COST2*                                                                         
         TM    SVCLTST,X'08'        SEE IF COST2 CLIENT                         
         BNO   EXIT                 NO THEN DONE                                
         LA    R1,SVE2FAC                                                       
         OC    SVE2FAC,SVE2FAC     FACTOR PRESENT?                              
         BZ    CLRB42                                                           
         CP    SVE2FAC,=P'0'       ZERO COST2 FACTOR?                           
         BNE   CLRB43                                                           
CLRB42   LA    R1,SVC2FAC                                                       
         CP    SVC2FAC,=P'0'   BAD RECORD - CLIENT MUST HAVE FACTOR             
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLRB43   XC    ELEM,ELEM                                                        
         MVI   ELEM,X'91'                                                       
         MVI   ELEM+1,X'08'                                                     
         MVC   ELEM+2(5),0(R1)      SAVE "TO" FACTOR                            
         BAS   RE,ADDEL                                                         
*COST2*                                                                         
         B     EXIT                                                             
         SPACE 1                                                                
ADDEL    ST    RE,SAVERE                                                        
         GOTO1 VRECUP,DMCB,(1,AIO),ELEM,(R7)                                    
         ZIC   R0,1(R7)            GET LENGTH OF ELEMENT INSERTED               
         AR    R7,R0               KEEP E-O-R POINTER                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
DELEL    NTR1                                                                   
DELEL2   L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         GOTO1 VRECUP,DMCB,(1,AIO),(R6)                                         
         B     DELEL2                                                           
         EJECT                                                                  
**********************************************                                  
* SUBROUTINE TO EDIT ALLOCATION LINE AND ADD                                    
* NEW 21 ELEMS                                                                  
**********************************************                                  
         SPACE 1                                                                
* EDIT PRODUCT ALLOCATIONS                                                      
* SET UP USCAN TABLE                                                            
SET21    NTR1                                                                   
         L     R2,AIO         SET FULL TO ADDR OF FIRST ELEM                    
         LA    R2,33(R2)                                                        
         ST    R2,FULL                                                          
         LA    R2,SFMALLOH                                                      
         L     RF,VUSCAN                                                        
         LA    R1,PARS                                                          
         XC    0(20,R1),0(R1)                                                   
         LA    RE,8(R2)            SET INPUT ADDRESS                            
         ST    RE,UADDR-1                                                       
         MVI   UFRST,X'FF'                                                      
         MVC   USTRNG+1(1),5(R2)    SET STRING LENGTH                           
*                                                                               
* EDIT FIRST FIELD TO SEE IF CHECK DIGIT ENTERED                                
* IF NONE ENTERED, ALL COST/SPACE SHARES ASSUMED EQUAL.                         
*                                                                               
         MVI   BYTE,0              RESET SWITCH                                 
         MVI   UVAL,X'80'                                                       
         MVI   USCN1,C'/'                                                       
         BASR  RE,RF                                                            
         CLI   USTOP,C'/'                                                       
         BE    *+12                                                             
         MVI   UFRST,X'FF'                                                      
         B     EDT2                                                             
*                                                                               
         MVI   ERROR,TOTSHERR                                                   
         BAS   R8,EDTSHR2          VALIDATE FIELD                               
         STC   R0,BYTE                                                          
         OI    BYTE,X'80'                                                       
*                                                                               
EDT2     MVC   USCN1(2),=X'6B60'    STOP ON , OR -                              
*                                                                               
         XC    WORK,WORK           BUILD ELEMENT                                
         MVC   WORK(2),=X'2107'                                                 
* VALIDATE PRD CODE                                                             
*                                                                               
         MVI   ERROR,INVPRD                                                     
         MVI   UVAL,0                                                           
         BASR  RE,RF                                                            
         L     R4,UADDR-1                                                       
         CLC   =C'ZZZ',0(R4)                                                    
         BE    SCANERR                                                          
         CLI   0(R4),C'*'          NO OTHER AGENCY BUYS                         
         BE    SCANERR                                                          
         MVC   WORK+2(3),0(R4)                                                  
         MVI   WORK+5,1            SET DEFAULT SHARES                           
         MVI   WORK+6,1                                                         
         CLI   ULNGTH+1,3                                                       
         BH    SCANERR                                                          
         BE    *+16                                                             
         CLI   ULNGTH+1,2                                                       
         BL    SCANERR                                                          
         MVI   WORK+4,C' '                                                      
         TM    UVAL,X'01'                                                       
         BO    SCANERR                                                          
*                                                                               
         TM    BYTE,X'80'          TEST COST SHARE REQUIRED                     
         BNZ   EDT4                                                             
         CLI   USTOP,C'-'                                                       
         BE    SCANERR                                                          
         B     EDT6                                                             
*                                                                               
EDT4     MVI   ERROR,CSTSHERR                                                   
         BAS   R8,EDTSHR                                                        
         STC   R0,WORK+5                                                        
         CLI   USTOP,C'-'          TEST SPACE SHARE ENTERED                     
         BNE   *+12                                                             
*                                                                               
         MVI   ERROR,SPCSHERR                                                   
         BAS   R8,EDTSHR                                                        
         LTR   R0,R0               SPACE SHARE MUST NOT BE 0                    
         BE    PRTERR                                                           
         STC   R0,WORK+6                                                        
*                                                                               
*  TEST TO BE SURE THE BRAND HAS THE ESTIMATE ON FILE                           
*                                                                               
EDT6     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),SVKEY    AGY/MED                                          
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),SVKEY+4    CLT                                          
         MVC   KEY+7(3),WORK+2     PRD FROM ALLO LINE                           
         MVC   KEY+10(2),SVESTT    'TO' EST                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    EDT6D                                                            
         MVI   ERROR,INVPRD         BAD PRODUCT IN LIST                         
         B     SCANERR                                                          
*                                                                               
EDT6D    DS    0H                                                               
*                                                                               
* TEST FOR DUPLICATE PRD AND INSERT ELEM IN RECORD                              
EDT8     L     R5,FULL             GET FIRST EL ADDRESS                         
         MVI   ELCODE,X'21'                                                     
EDT8A    BAS   R8,ENEXTEL                                                       
         BNE   EDT8B                                                            
         CLC   2(3,R5),WORK+2                                                   
         BNE   EDT8A                                                            
         MVI   ERROR,DUPERR                                                     
         B     PRTERR                                                           
EDT8B    L     R5,FULL             INSERT AFTER LAST X'21' ELEM                 
         LR    R6,R5               SAVE THIS ELEM ADDRESS                       
         BAS   R8,ENEXTEL                                                       
         BE    *-6                                                              
         IC    R0,1(R6)            INSERT AFTER THIS ELEM                       
         AR    R6,R0                                                            
         L     R5,FULL                                                          
         SH    R5,=H'33'           BACK UP TO START OF REC                      
         GOTO1 VRECUP,DMCB,(1,(R5)),WORK,(R6)                                   
*                                                                               
         XC    USCN1(6),USCN1                                                   
         LA    R1,UFRST            SEE IF MORE DATA                             
         L     RF,VUSCAN                                                        
         BASR  RE,RF                                                            
         CLI   ULNGTH+1,0                                                       
         BZ    EDT10                                                            
         MVI   UFRST,X'FF'                                                      
         B     EDT2                                                             
         SPACE 2                                                                
* VALIDATE SUM OF WEIGHTS                                                       
*                                                                               
EDT10    MVI   ELCODE,X'21'                                                     
         L     R5,FULL                                                          
         SR    R7,R7                                                            
         SR    R6,R6                                                            
EDT10A   BAS   R8,ENEXTEL                                                       
         BNE   EDT10B                                                           
         IC    R0,2+3(R5)                                                       
         AR    R7,R0                                                            
         IC    R0,2+4(R5)                                                       
         AR    R6,R0                                                            
         B     EDT10A                                                           
*                                                                               
EDT10B   MVI   ERROR,SHNEQERR      COST SHARE SUM NE SPACE SHARE SUM            
         CR    R7,R6                                                            
         BNE   PRTERR                                                           
         STC   R7,BYTE2                                                         
         TM    BYTE,X'80'          TEST CHECK DIGIT ENTERED                     
         BZ    EDT10C              NO                                           
         MVI   ERROR,SHSUMERR                                                   
         OI    BYTE2,X'80'                                                      
         CLC   BYTE,BYTE2                                                       
         BNE   PRTERR                                                           
EDT10C   L     RE,FULL             GET PBDELEM ADDRESS                          
         LA    RE,PBDWTSUM-PBDELEM(RE)                                          
         MVC   0(1,RE),BYTE2                                                    
         B     EDTX                                                             
*                                                                               
PRTERR   MVI   GETMSYS,4      SET TO GET PRINTPAK ERRORS                        
         B     SCANERR                                                          
*                                                                               
EDTX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
ENEXTEL  CLI   0(R5),0         END OF REC                                       
         BE    ENEXTELX                                                         
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R8                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
ENEXTELX LTR   R5,R5                 SET CC NOT EQUAL                           
         BR    R8                                                               
         SPACE 2                                                                
*                                                                               
EDTSHR   MVI   UVAL,X'80'                                                       
         BASR  RE,RF                                                            
EDTSHR2  DS    0H                                                               
         L     R4,UADDR-1                                                       
         LH    R6,ULNGTH                                                        
         CLI   ULNGTH+1,3                                                       
         BH    PRTERR                                                           
         LTR   R6,R6                                                            
         BZ    PRTERR                                                           
         TM    UVAL,X'01'                                                       
         BO    PRTERR                                                           
         BCTR  R6,0                                                             
         EX    R6,PACKSHR                                                       
         CVB   R0,DUB                                                           
         CH    R0,=H'100'                                                       
         BH    PRTERR                                                           
         BR    R8                                                               
*                                                                               
PACKSHR  PACK  DUB,0(0,R4)                                                      
*                                                                               
FRZNDATE EQU   110                 DATE(S) FROZEN FOR CLIENT (SYS 25)           
*                                                                               
*        OLD PRINTPAK ERROR MESSAGES  -  SET GETMSYS TO 4                       
*                                                                               
PUBCLTER EQU   33                                                               
NOALLERR EQU   70                                                               
SPCSHERR EQU   71                                                               
CSTSHERR EQU   72                                                               
INVSPCSH EQU   73                                                               
INVCSTSH EQU   74                                                               
DUPERR   EQU   77                                                               
SHNEQERR EQU   78                                                               
SHSUMERR EQU   79                                                               
TOTSHERR EQU   88                                                               
         EJECT                                                                  
**********************************************                                  
* SUBROUTINE TO ADD NEW BUY AND ALL POINTERS *                                  
* ON ENTRY AIO CONTAINS OLD BUY RECORD       *                                  
**********************************************                                  
         SPACE 1                                                                
ADDBUY   NTR1                                                                   
         MVC   AIO,AIO1            SET I/O AREA ADDRESS                         
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R7,BLOCK                                                         
         USING PVALUESD,R7                                                      
*                                                                               
         L     R6,AIO                                                           
         USING PBUYRECD,R6                                                      
         GOTO1 VGETINS,DMCB,(C'X',(R6)),(R7),PBUYKPRD                           
*                                                                               
         L     R1,ADDDOLS                                                       
         A     R1,GROSS                                                         
         ST    R1,ADDDOLS                                                       
         L     R1,ADDBUYS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ADDBUYS                                                       
         DROP  R7                                                               
*                                                                               
         CLI   PTOCLT,C' '         SEE IF PATCHING "TO" CLIENT                  
         BNH   *+10                                                             
         MVC   PBUYKCLT,PTOCLT                                                  
*                                                                               
         MVC   PBUYKPRD,SVPRDT     SET NEW PRODUCT                              
         MVC   PBUYKEST,SVESTT     SET NEW ESTIMATE                             
         MVI   PBUYKLIN,0          RESET SUBLINE                                
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),0(R6)       FIND HIGHEST SUBLINE                         
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'      TEST ALL BUT 'DELETED'                       
         GOTO1 HIGH                                                             
         CLC   KEY(24),KEYSAVE                                                  
         BNE   ADDB30                                                           
*                                                                               
ADDB10   TM    KEY+25,X'80'        TEST DELETED                                 
         BZ    ADDB20                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RE,DMCB+8           GET ADDRESS OF DISK ADDR                     
         MVC   DMDSKADD,0(RE)      AND SAVE DISK ADDRESS                        
*                                                                               
         BAS   RE,DELCHK                                                        
         BZ    ADDB40              RE-USE DELETED LINE                          
         EJECT                                                                  
ADDB20   MVC   KEYSAVE,KEY         SAVE LAST KEY                                
         GOTO1 SEQ                                                              
         CLC   KEY(24),KEYSAVE                                                  
         BE    ADDB10                                                           
*                                                                               
ADDB30   L     R6,AIO1             RESTORE BUYREC ADDRESS                       
         ST    R6,AIO                                                           
         MVC   KEY,KEYSAVE         RESTORE LAST GOOD KEY                        
         ZIC   RE,KEY+24                                                        
         LA    RE,1(RE)            BUMP SUBLINE                                 
         STC   RE,24(R6)           SET SUBLINE IN RECORD                        
*                                                                               
         BRAS  RE,NXTSER           FIND AND STORE NEXT SERIAL #                 
*                                                                               
         MVI   ACTELOPT,C'N'       SUPPRESS ACTIVITY ELEMENT                    
         GOTO1 ADDREC              ADD NEW RECORD                               
         L     RE,DMCB+8                                                        
         MVC   DMDSKADD,0(RE)      SAVE DISK ADDRESS                            
         MVC   COMMAND,=CL6'DMADD'                                              
         B     ADDB50                                                           
         EJECT                                                                  
* ADD NEW BUYREC USING DELETED POINTER *                                        
         SPACE 1                                                                
ADDB40   DS    0H                                                               
         MVI   DELSW,C'Y'          SET FLAG FOR USING DELETED RECORD            
         L     R6,AIO1             RESTORE POINTER TO BUYREC                    
         ST    R6,AIO                                                           
         MVC   0(25,R6),KEY        SET KEY IN RECORD                            
*                                                                               
         BRAS  RE,NXTSER           FIND AND STORE NEXT SERIAL #                 
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'DD'      SET FOR NO DUP KEY/ REC DELETED              
         MVI   ACTELOPT,C'N'       SUPPRESS ACTIVITY ELEMENT                    
         GOTO1 ADDREC                                                           
         L     RE,DMCB+8           GET ADDRESS OF DISK ADDR                     
         MVC   DMDSKADD,0(RE)      AND SAVE DISK ADDRESS                        
*                                                                               
         XC    KEY,KEY             NOW UPDATE DIRECTORY                         
         MVC   KEY(25),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+25(2),27(R6)    MOVE CONTROL BYTES                           
         MVC   KEY+27(4),DMDSKADD  MOVE DISK ADDRESS                            
         LA    R1,WORK                                                          
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
         GOTO1 WRITE                                                            
         ST    R6,AIO              RESET TO BUY RECORD                          
         SPACE 1                                                                
ADDB50   DS    0H                                                               
*                                                                               
         BRAS  RE,PUTPPS       ADD SERIAL # PASSIVE POINTER                     
*                                                                               
* ADD CLT/PUB POINTER *                                                         
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(25),0(R6)                                                    
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),10(R6)     PUB                                          
         MVC   KEY+13(3),7(R6)     PRD                                          
*                                                                               
         L     RF,ADD              SET DEFAULT ROUTINE ADDRESS                  
         CLI   DELSW,C'Y'          TEST RE-USING DELETED                        
         BNE   ADDB60                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,WORK                                                          
         L     RF,AIO1                                                          
         MVC   WORK(30),0(RF)    PUT REAL KEY IN IT                             
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
         L     RF,WRITE            SET ROUTINE ADDRESS                          
*                                                                               
ADDB60   MVC   KEY+25(2),27(R6)    MOVE CONTROL BYTES                           
         MVC   KEY+27(4),DMDSKADD  MOVE DISK ADDRESS                            
         GOTO1 (RF)                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO              RESET TO PUBYREC                             
*                                                                               
         MVI   DELSW,0             RESET SWITCH                                 
         EJECT                                                                  
* TEST TO ADD POL POINTERS *                                                    
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ADDB70   BAS   RE,NEXTEL                                                        
         BNE   EQXIT                                                            
*                                                                               
         USING PPRELEM,R6                                                       
         L     RE,AIO                                                           
         MVC   KEY(25),0(RE)       SET RECORD KEY                               
         LA    RE,(PBUYKACT-PBUYKEY)+KEY                                        
         LA    RF,(PBUYKPRD-PBUYKEY)+KEY                                        
         MVC   0(3,RE),0(RF)       MOVE ACTIVE PRD CODE                         
         MVC   0(3,RF),PPRCODE     AND PRODUCT                                  
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'      SET FOR NO REC DELETED TEST                  
         GOTO1 HIGH                                                             
         L     RF,ADD                                                           
         CLC   KEYSAVE(25),KEY                                                  
         BNE   ADDB75                                                           
         LA    R1,WORK                                                          
         L     RF,AIO1                                                          
         MVC   WORK(30),0(RF)    PUT REAL KEY IN IT                             
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
         L     RF,WRITE                                                         
*                                                                               
ADDB75   DS    0H                                                               
         MVC   KEY(25),KEYSAVE     RESTORE KEY                                  
         L     RE,AIO                                                           
         MVC   KEY+25(2),27(RE)    SET CONTROL BYTES                            
         MVC   KEY+27(4),DMDSKADD  DISK ADDRESS                                 
         GOTO1 (RF)                                                             
         L     R1,AIO1                                                          
         ST    R1,AIO         RESET AIO TO MY RECORD                            
         B     ADDB70                                                           
         DROP  R6                                                               
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE TO TEST IF ALL PAY/BILL/IO ELEMENTS ARE INACTIVE *                 
***************************************************************                 
         SPACE 1                                                                
DELCHK   NTR1                                                                   
         L     R6,AIO              POINT TO RECORD                              
         USING PBUYRECD,R6                                                      
*                                                                               
         MVI   ELCODE,X'70'        IO ELEM                                      
         BAS   RE,GETEL                                                         
         BNE   DELCHK20                                                         
*                                                                               
         USING PIOELEM,R6                                                       
DELCHK10 OC    PIODATE,PIODATE                                                  
         BNZ   NEQXIT                                                           
         BAS   RE,NEXTEL                                                        
         BE    DELCHK10                                                         
         DROP  R6                                                               
*                                                                               
DELCHK20 L     R6,AIO                                                           
         MVI   ELCODE,X'25'        PAY ELEM                                     
         BAS   RE,GETEL                                                         
         BNE   DELCHK30                                                         
*                                                                               
         USING PPAYELEM,R6                                                      
DELCHK25 OC    PPDDATE,PPDDATE                                                  
         BNZ   NEQXIT                                                           
         BAS   RE,NEXTEL                                                        
         BE    DELCHK25                                                         
         DROP  R6                                                               
*                                                                               
DELCHK30 L     R6,AIO                                                           
         MVI   ELCODE,X'26'        BILL ELEM                                    
         BAS   RE,GETEL                                                         
         BNE   DELCHK38                                                         
*                                                                               
         USING PBILELEM,R6                                                      
DELCHK35 OC    PBLDATE,PBLDATE                                                  
         BNZ   NEQXIT                                                           
         BAS   RE,NEXTEL                                                        
         BE    DELCHK35                                                         
         DROP  R6                                                               
*                                                                               
DELCHK38 L     R6,AIO                                                           
         MVI   ELCODE,X'28'        OPEN BILL ELEM                               
         BAS   RE,GETEL                                                         
         BNE   DELCHK40                                                         
*                                                                               
         USING PBILELEM,R6                                                      
DELCHK39 OC    PBLDATE,PBLDATE                                                  
         BNZ   NEQXIT                                                           
         BAS   RE,NEXTEL                                                        
         BE    DELCHK39                                                         
         DROP  R6                                                               
*                                                                               
*                                                                               
DELCHK40 DS    0H                                                               
         B     DELCHK50                                                         
*                                                                               
DELCHK50 B     EQXIT               EXIT WITH CC EQ                              
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE TO DELETE BUY AND ALL POINTERS                     *               
* MAJOR POINTER IS DELETED LAST - SO DIR IS SET FOR SEQ ON EXIT *               
*****************************************************************               
         SPACE 1                                                                
DELBUY   NTR1                                                                   
         BAS   RE,UPDASR           UPDATE AUTO-SPACE RESV                       
         MVC   KEY,SVKEY           RESTORE BUY KEY                              
*                                                                               
         CLI   CONSW,C'Y'          TEST ANY CONTRACT READ                       
         BNE   DELB2                                                            
*                                                                               
         MVC   AIO,AIO1            RESTORE BUYREC ADDRESS                       
         GOTO1 GETREC              AND REREAD RECORD                            
         L     RE,DMCB+8           GET ADDRESS OF DISK ADDR                     
         MVC   DMDSKADD,0(RE)      AND SAVE DISK ADDRESS                        
*                                                                               
DELB2    XC    BLOCK(256),BLOCK                                                 
         LA    R7,BLOCK                                                         
         USING PVALUES,R7                                                       
*                                                                               
         L     R6,AIO                                                           
         USING PBUYRECD,R6                                                      
         GOTO1 VGETINS,DMCB,(C'X',(R6)),(R7),PBUYKPRD                           
*                                                                               
         L     R1,DELDOLS                                                       
         A     R1,GROSS                                                         
         ST    R1,DELDOLS                                                       
         L     R1,DELBUYS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,DELBUYS                                                       
*                                                                               
         OI    PBUYCNTL,X'80'      DELETE BUYREC                                
**NEW 5/11/89                                                                   
         MVC   PBDDATE,BTODAY      SET LAST CHANGE DATE                         
**NEW 5/11/89                                                                   
*                                                                               
         BRAS  RE,DELPID                                                        
*                                                                               
*                                                                               
         MVI   ACTELOPT,C'N'       SUPPRESS ACTIVITY ELEMENT                    
         GOTO1 PUTREC                                                           
         NI    PBUYCNTL,X'7F'      UNSET DELETE IND                             
         SPACE 1                                                                
* NOW DELETE X'20' POL POINTERS *                                               
         SPACE 1                                                                
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         L     R6,AIO                                                           
         MVI   ELCODE,X'21'                                                     
         USING PPRELEM,R6                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DELB102  BAS   RE,NEXTEL                                                        
         BNE   DELB120                                                          
         MVC   KEY,SVKEY           RESTORE ORIGINAL KEY                         
         LA    RE,(PBUYKACT-PBUYKEY)+KEY                                        
         LA    RF,(PBUYKPRD-PBUYKEY)+KEY                                        
         MVC   0(3,RE),0(RF)       SET ACTIVE PRD CODE                          
         MVC   0(3,RF),PPRCODE     SET PRD IN KEY                               
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+25,X'80'                                                     
         LA    R1,WORK                                                          
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
*                                                                               
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1            RESET TO BUYREC                              
         B     DELB102                                                          
*                                                                               
         SPACE 1                                                                
* NOW DELETE X'21' PASSIVE POINTER *                                            
         SPACE 1                                                                
DELB120  MVC   KEY,SVKEY           RESTORE ORIGINAL KEY                         
         LA    RE,(PBUYKRCD-PBUYKEY)+KEY                                        
         MVI   0(RE),X'21'                                                      
         LA    RE,(PBUYKPRD-PBUYKEY)+KEY                                        
         LA    RF,(PBUYKPRD-PBUYKEY)+SVKEY                                      
         MVC   0(6,RE),3(RF)       MOVE PUB                                     
         MVC   6(3,RE),0(RF)        AND PRD                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+25,X'80'        SET DELETE BIT                               
         LA    R1,WORK                                                          
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
*                                                                               
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1            RESET TO  PBUYREC                            
         SPACE 1                                                                
* AND DELETE ACTIVE POINTER (THIS CODE MUST BE LAST)                            
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE MAJOR POINTER                        
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+25,X'80'        MARK DELETED                                 
         LA    R1,WORK                                                          
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
*                                                                               
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1            RESET TO  PBUYREC                            
*                                                                               
         BRAS  RE,DELPSV           DELETE DATE PASSIVE POINTERS                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************                      
* UPDATE AUTOMATIC SPACE RESERVATION ELEMENT IN CONTRACT *                      
**********************************************************                      
         SPACE 1                                                                
UPDASR   NTR1                                                                   
         BAS   RE,GETCON           GET CONTRACT IN IO3                          
         BNZ   EXIT                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'85'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
UPDASR2  CLI   SVCPROF+5,C'2'      TEST DOING SLAVE CLIENT                      
         BNE   UPDASR10                                                         
*                                                                               
         USING PASRELEM,R6                                                      
UPDASR4  CLC   PASRCLT,QCLT        MATCH CLIENT CODE                            
         BE    UPDASR10                                                         
         BAS   RE,NEXTEL                                                        
         BE    UPDASR4                                                          
*                                                                               
         LR    R0,R6               SAVE E-O-R ADDRESS                           
         XC    ELEM,ELEM           BUILD NEW ELEMENT FOR THIS CLIENT            
         LA    R6,ELEM                                                          
         MVI   PASRELEM,X'85'                                                   
         MVI   PASRLEN,13                                                       
         MVC   PASRCDAT,BTODAY                                                  
         MVC   PASRCLT,QCLT                                                     
         GOTO1 VRECUP,DMCB,(1,AIO),ELEM,(R0)                                    
         B     UPDASR12                                                         
*                                                                               
UPDASR10 CLC   PASRCDAT,BTODAY     IF LAST ACTIVE TODAY,EXIT                    
         BE    EXIT                                                             
         MVC   PASRLDAT,PASRCDAT   SAVE LAST RUN DATE                           
         MVC   PASRCDAT,BTODAY     SET CURRENT DATE = TODAY                     
*                                                                               
UPDASR12 MVI   ACTELOPT,C'N'       SUPPRESS ACTIVITY ELEMENT                    
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************                                           
* FIND A CONTRACT RECORD FOR BUYREC *                                           
*************************************                                           
         SPACE 1                                                                
GETCON   NTR1                                                                   
         MVI   CONSW,C'N'          SET NO RECORD READ                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PCONRECD,R6                                                      
*                                                                               
         MVC   PCONKAGY,AGENCY                                                  
         MVC   PCONKMED,QMED                                                    
         MVI   PCONKRCD,X'10'                                                   
         MVC   PCONKCLT,QCLT                                                    
         CLI   SVCPROF+5,C'2'       TEST SLAVE CLIENT                           
         BNE   *+10                                                             
         MVC   PCONKCLT,SVCPROF+6   READ MASTER CONTRACT                        
         L     RF,AIO1              TO BUYREC                                   
         MVC   PCONKPUB(6),PBUYKPUB-PBUYKEY(RF)                                 
         GOTO1 HIGH                                                             
         B     GETCON4                                                          
*                                                                               
GETCON2  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
GETCON4  CLC   KEY(13),KEYSAVE     A/M/REC/CL/PUB                               
         BNE   NEQXIT                                                           
*                                                                               
         MVI   CONSW,C'Y'          SET FLAG FOR CONTRACT READ                   
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PCONRECD,R6                                                      
         L     R7,AIO1             POINT TO BUYREC                              
         LA    R7,(PBUYKDAT-PBUYKEY)(R7)                                        
         CLC   PCONSTRT(3),0(R7)   CONTRACT START AFTER BUY DATE                
         BH    GETCON2                                                          
         CLC   PCONEND(3),0(R7)    CONTRACT END BEFORE BUY DATE                 
         BL    GETCON2                                                          
         B     EQXIT               CONTRACT APPLIES - EXIT WITH CC =            
*                                                                               
* SUBROUTINE TO TEST IF ANY KEY FIELDS CHANGED                                  
* IF YES, EXIT WITH CC NEQ                                                      
         SPACE 1                                                                
TSTNEWKY NTR1                                                                   
         TM    SFMMEDH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    SFMCLTH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    SFMPUBH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    SFMDTSH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    SFMADCH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    SFMOPTSH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    SFMPRDFH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    SFMESTFH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    SFMPRDTH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    SFMESTTH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    SFMALLOH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         B     EQXIT                                                            
         EJECT                                                                  
SCANERR  DS    0H                                                               
         MVI   LOCKSW,0            TURN OFF LOCK TEST SWITCH                    
*NOP*    BRAS  RE,UNLOCK           UNLOCK KEY SET IN THIS APPLICATION           
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
**********************************************************************          
*SUBROUTINE TO CHECK IF TO/FRM ADJ CODES ARE THE SAME FOR MOVE       *          
**********************************************************************          
VKCHKA   NTR1                                                                   
         LA    R8,TOADJ                                                         
         LA    R4,2(R8)                                                         
         LA    R7,FROMADJ                                                       
         LA    R5,2(R7)                                                         
ONE      CLC   0(1,R7),0(R8)                                                    
         BE    TWO                                                              
         LA    R7,1(R7)                                                         
         CR    R7,R5                                                            
         BNH   ONE                                                              
         LA    R7,1                                                             
         B     VKCHKX                                                           
TWO      LA    R7,FROMADJ                                                       
         LA    R8,1(R8)                                                         
         CLI   0(R8),X'00'                                                      
         BNE   *+10                                                             
         SR    R7,R7                                                            
         B     VKCHKX                                                           
         CR    R8,R4                                                            
         BNH   ONE                                                              
VKCHKX   XIT1  REGS=(R7)                                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
*SUBROUTINE TO CHECK IF TO ADJ CODES ARE A SUBSET OF FROM CODES      *          
**********************************************************************          
VKCHKB   NTR1                                                                   
         LA    R4,3      FOR BCT                                                
         LA    R8,TOADJ                                                         
VKCK2    CLI   0(R8),C' '      SEE IF HIGHER THAN A SPACE                       
         BNH   VKCK10                                                           
         LA    R5,3      FOR BCT                                                
         LA    R7,FROMADJ                                                       
VKCK4    CLC   0(1,R8),0(R7)                                                    
         BE    VKCK10                                                           
         LA    R7,1(R7)    NEXT 'FROM' CODE                                     
         BCT   R5,VKCK4                                                         
         LA    R7,1    ERROR RETURN- 'TO' CODE NOT AMONG 'FROM' CODES           
         B     VKCKX                                                            
*                                                                               
VKCK10   LA    R8,1(R8)     NEXT 'TO' ADJ CODE                                  
         BCT   R4,VKCK2                                                         
         SR    R7,R7                                                            
*                                                                               
VKCKX    XIT1  REGS=(R7)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
PTOCLT   DC    XL3'00'      PATCH FOR "TO" CLIENT                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        HANDLE 2 BYTE ERROR CODES                                              
*                                                                               
V2ERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2cd IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         JE    *+10                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPUBLOC NTR1  BASE=*,LABEL=*      Check for Pub locks                          
*                                                                               
         OC    BPUB,BPUB           All Pubs?                                    
         JNZ   EQXIT                                                            
         CLI   ACTNUM,ACTCOPY      Copy?                                        
         JE    *+12                                                             
         CLI   ACTNUM,ACTMOVE      Move?                                        
         JNE   EQXIT                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   PUBKPUB(6),BPUB     MOVE PUB/ZONE/EDTN                           
*                                                                               
         XC    FILENAME,FILENAME                                                
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
*                                                                               
CKPL_10  CLC   PUBKMED,QMED        Still same media?                            
         JNE   CKPL_X                                                           
         CLC   PUBKAGY,AGENCY      Same agency?                                 
         JNE   CKPL_20                                                          
         CLI   PUBKCOD,X'81'       Pub record code?                             
         JNE   CKPL_20                                                          
         J     CKPL_30                                                          
*                                                                               
CKPL_20  XC    FILENAME,FILENAME                                                
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 SEQ                                                              
         J     CKPL_10                                                          
*                                                                               
CKPL_30  L     R6,AIO                                                           
         XC    FILENAME,FILENAME                                                
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         USING PUBNAMEL,R6                                                      
         TM    PUBLOCSW,PUBLCKDQ   Pub is locked?                               
         JNZ   CKPL_ERR                                                         
*                                                                               
         J     CKPL_20             Check next pub                               
*                                                                               
CKPL_X   XC    FILENAME,FILENAME                                                
         J     EQXIT                                                            
*                                                                               
CKPL_ERR XC    FILENAME,FILENAME                                                
         J     NEQXIT                                                           
         LTORG                                                                  
         DROP  RB,R6,R4                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIDKEST NTR1  BASE=*,LABEL=*      Check for iDesk Estimats                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'                                                     
         BRAS  RE,GETEL                                                         
         USING PESTELEM,R6                                                      
         TM    PESTTEST,X'20'      iDesk Estimate?                              
         JZ    EQXIT                                                            
         LHI   R2,IDKESTEQ                                                      
         BRAS  RE,GET_ETXT                                                      
         J     NEQXIT                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
**       ROUTINE TO READ BRAND ESTS FOR FROM EST                                
**       AND SEE IF THEY EXIST ON TO EST                                        
***                                                                             
ZECHK    NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO2        BUILD TABLE OF BRANDS IN AIO2                     
         MVI   0(R6),0        INITIALIZE LIST                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGENCY                                                    
         MVC   KEY+2(1),QMED                                                    
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),QCLT                                                    
         GOTO1 HIGH                                                             
         B     ZECHK5                                                           
ZECHK3   GOTO1 SEQ                                                              
         CLC   KEY(7),KEYSAVE      CHK AGY/MED/CODE/CLT                         
         BNE   ZECHK10                                                          
         CLC   KEY+10(2),SVESTF     SEE IF 'FROM' EST                           
         BH    ZECHK5               IF HIGH - SKIP TO NEXT PRD                  
         BL    ZECHK6               IF LOW - READ FOR 'FROM' EST                
         MVC   0(3,R6),KEY+7        EQUAL -  SAVE THIS PRODUCT CODE             
         LA    R6,3(R6)                                                         
         MVI   0(R6),0              SET END OF LIST                             
*                                                                               
ZECHK5   MVI   KEY+10,X'FF'         SKIP TO NEXT PRD                            
         B     ZECHK3                                                           
*                                                                               
ZECHK6   MVC   KEY+10(2),SVESTF                                                 
         B     ZECHK3                                                           
*                                                                               
ZECHK10  L     R6,AIO2                                                          
ZECHK15  CLI   0(R6),0              END OF LIST                                 
         BE    ZECHK40                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGENCY                                                    
         MVC   KEY+2(1),QMED                                                    
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),QCLT                                                    
         MVC   KEY+7(3),0(R6)                                                   
         MVC   KEY+10(2),SVESTT      NOW USE 'TO' EST                           
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         JNE   NEQXIT              NOT EQUAL EXIT                               
         LA    R6,3(R6)                                                         
         B     ZECHK15                                                          
*                                                                               
ZECHK40  J     EQXIT              EXIT WITH CC EQUAL                            
         EJECT                                                                  
CANMSGQ  EQU   285                 CANCEL NOT ALLOWED FOR THIS CLIENT           
MBCMSGQ  EQU   286                 USE MBC OR BUY TO MAKE LIVE FOR CLT          
*                                                                               
GET_ETXT LR    R0,RE                                                            
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     RF,FULL                                                          
         XC    CONHEAD,CONHEAD                                                  
         GOTOR GETTXT,DMCB+12,(R2),0,(C'E',DMCB),0,0,(RF)                       
         OI    CONHEADH+6,X'80'                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TRAPERR2 GOTOR ERREX2                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO FIND A NEW BUY SERIAL NUMBER                                
*                                                                               
*        NXTSER OBTAINS THE LAST                                                
*        BUY SERIAL NUMBER USED (BY READING PRIOR                               
*        PASSIVE POINTERS) THE LAST PASSIVE IS READ FOR UPDATE                  
*        SO THAT NO OTHER TASKS CAN ALLOCATE A SERIAL                           
*        NUMBER FOR THE SAME AGY/MED/CLT UNTIL THIS ONE                         
*        COMPLETES                                                              
*                                                                               
*        A SERIAL NUMBER ELEMENT IS ADDED TO THE INSERTION                      
*                                                                               
*        IMMEDIATELY AFTER ADDING A BUY RECORD, PUTPPS                          
*        MUST BE CALLED TO CREATE THE NEW BUY SERIAL NUMBER                     
*        ON EXIT-DUB CONTAINS 9'S COMPLEMENT OF MY SERIAL #                     
*        - USE IMMEDIATELY IN PUTPPS                                            
*                                                                               
NXTSER   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO             TO PBUYREC                                    
         USING PBUYRECD,R6                                                      
*****    CLI   SVPROF+8,C'Y'      SERIAL # NEEDED?                              
*****    BNE   NXTSERXX                                                         
         MVC   X(32),KEY     SAVE BUY'S KEY                                     
         LA    R7,KEY                                                           
         USING PPSERPPD,R7                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYREC       AGY/MED/CLT FROM BUYREC                     
         MVI   KEY+3,X'99'         PASSIVE POINTER CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      MUST FIND                                    
         BE    NXTSER10            NONE FOUND SO ADD FIRST                      
*                                                                               
NXTSER20 DS    0H                                                               
         ZAP   DUB,=P'1'   FIRST TIME -                                         
         B     NXTSER12                                                         
*                                                                               
*                       LATER-BE SURE AND SET READ FOR UPDATE                   
NXTSER10 ZAP   DUB,=P'1000000000'                                               
         SP    DUB,PSERKNUM                                                     
         AP    DUB,=P'1'    DUB NOW HAS NEXT SERIAL NUMBER                      
NXTSER12 DS    0H                                                               
         ZAP   DOUBLE,=P'1000000000'                                            
         SP    DOUBLE,DUB                                                       
         DROP  R7                                                               
         LA    R7,X+32      BUILD AND ADD SERIAL # ELEM                         
         XC    X+32(20),X+32                                                    
         USING PSERELED,R7                                                      
         MVI   PSERELEM,X'99'                                                   
         MVI   PSERLEN,X'09'                                                    
         MVC   PSERNUM,DUB+3                                                    
         DROP  R7                                                               
*                                                                               
         LA    R5,PBUYREC+33                                                    
         MVI   ELCODE,X'FF'     GET TO END OF RECORD                            
         BAS   R8,ENEXTEL                                                       
         GOTO1 VRECUP,DMCB,(1,PBUYREC),X+32,(R5)                                
         B     NXTSERX                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
NXTSERX  MVC   KEY(32),X         RESTORE KEY                                    
NXTSERXX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
* ***************************************************************               
*        THIS ROUTINE ADDS NEW SERIAL NUMBER'S PASSIVE POINTER                  
*        DOUBLE SHOULD CONTAIN THE 9'S COMPLEMENT OF THE 'REAL'                 
*        SERIAL NUMBER                                                          
*                                                                               
*        THIS ROUTINE MUST BE CALLED IMMEDIATELY AFTER ADDING                   
*        A NEW INSERTION                                                        
*                                                                               
*        THE DISK ADDRESS OF THE NEW BUY IS ASSUMED TO STILL RESIDE IN          
*        DMDSKADD                                                               
*                                                                               
PUTPPS   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO             TO PBUYREC                                    
         USING PBUYRECD,R6                                                      
*******  CLI   SVPROF+8,C'Y'      SEE IF USING SERIAL NUMBERS                   
*******  BNE   PUTPPSXX                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYREC     AGY/MED/CLT                                   
         LA    R7,KEY                                                           
         USING PPSERPPD,R7                                                      
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKNUM,DOUBLE+3   9'S COMPLEMENT OF SERIAL NUMBER              
*                                                                               
         DROP  R7                                                               
*                                                                               
         MVC   WORK+6(6),COMMAND      SAVE COMMAND                              
*                                                                               
*                                                                               
PUTPP9   MVC   KEY+27(4),DMDSKADD  MOVE DISK ADDRESS                            
         MVC   KEY+25(2),PBUYREC+27 MOVE CONTROL BYTES                          
         GOTO1 ADD                                                              
*                                                                               
         MVC   COMMAND(6),WORK+6     RESTORE COMMAND                            
*                                                                               
         BRAS  RE,UPDPSV           ADD DATE PASSIVE POINTERS                    
*                                                                               
PUTPPSXX DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO UPDATE DATE PASSIVE POINTERS                                    
*        HANDLES POINTER FOR                                                    
*              SPACE CLOSING DATE                                               
*              MATERIALS CLOSING DATE                                           
*              BILLABLE DATE                                                    
*              PAYABLE DATE                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UPDPSV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    DMDSKADD,DMDSKADD   DIE IF NO DISK ADDRESS                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,UPDPSVTB         ESTABLISH TABLE OF DATES                     
         USING UPDPSVTB,R2                                                      
*                                                                               
         L     R5,AIO             POINT TO PBUYREC                              
         USING PBUYRECD,R5                                                      
*                                                                               
         LA    R1,WORK                                                          
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
*                                                                               
UPPLOOP  DS    0H                                                               
*                                                                               
         CLI   UPPTPPID,X'FF'      DONE AT END OF TABLE                         
         BE    UPPDONE                                                          
*                                                                               
*        FIND DATE FOR PASSIVE IN NEW RECORD                                    
*                                                                               
         LR    R6,R5               POINT TO NEW BUY RECORD                      
         MVC   ELCODE,UPPTELID     SET ID OF NEEDED ELEMENT                     
*                                                                               
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BNZ   UPPCONT             SKIP IF ELEMENT NOT FOUND                    
*                                                                               
         LLC   RF,UPPTDISP         GET DISPLACEMENT OF DATE                     
         LA    R1,0(RF,R6)         POINT TO DATE FOR PASSIVE                    
         MVC   UPDPSVDT,0(R1)      SAVE NEW DATE                                
*                                                                               
*        CREATE PASSIVE FOR NEW RECORD                                          
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH PASSIVE KEY                        
         USING PBYPPSVD,R4                                                      
*                                                                               
         MVC   PBYPAGY,PBUYKAGY    SET AGENCY                                   
         MVC   PBYPMED,PBUYKMED    SET MEDIA                                    
         MVI   PBYPRCD,PBYPRIDQ    SET BASIC RECORD ID                          
         MVC   PBYPDTYP,UPPTPPID   SET DATE TYPE ID                             
         MVC   PBYPCLT,PBUYKCLT    SET CLIENT                                   
         MVC   PBYPPRD,PBUYKPRD    SET PRODUCT                                  
         MVC   PBYPEST,PBUYKEST    SET ESTIMATE                                 
         MVC   PBYPDTE,UPDPSVDT    SET DATE                                     
         MVC   PBYPDADR,DMDSKADD   SET DISK ADDRESS                             
*                                                                               
*        READ NEW PASSIVE                                                       
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERR TESTS                   
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTOR HIGH                READ FOR NEW PASSIVE                         
*                                                                               
         CLC   PBYPKEY,KEYSAVE     SKIP IF PASSIVE NOT FOUND                    
         BNE   UPPNEW10                                                         
*                                                                               
         MVC   PBYPCNTL,PBUYCNTL   SET CONTROL BYTES                            
*                                                                               
         GOTOR WRITE               RE-WRITE PASSIVE                             
*                                                                               
UPPNEW05 DS    0H                                                               
*                                                                               
         B     UPPNEWX                                                          
*                                                                               
UPPNEW10 DS    0H                  ADD NEW PASSIVE                              
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
*                                                                               
         MVC   PBYPCNTL,PBUYCNTL   SET CONTROL BYTES                            
         MVC   PBYPDISK,DMDSKADD   SET DISK ADDRESS                             
*                                                                               
         GOTOR ADD                 ADD NEW PASSIVE                              
*                                                                               
UPPNEWX  DS    0H                                                               
*                                                                               
UPPCONT  DS    0H                                                               
         LA    R2,UPPTENTL(R2)     BUMP TO NEXT TABLE ENTRY                     
         B     UPPLOOP                                                          
*                                                                               
UPPDONE  DS    0H                                                               
*                                                                               
         ST    R5,AIO              RESTORE IOAREA PTR                           
*                                                                               
UPDPSVX  DS    0H                  RESET PASS DELETES                           
         XIT1                                                                   
*                                                                               
*        TABLE FOR PROCESSING PASSIVE POINTERS FOR DATES                        
*                                                                               
         DS    0D                  ALIGNMENT                                    
         DC    CL8'UPDPSVTB'                                                    
*                                                                               
UPDPSVTB DS    0X                  TABLE OF PASSIVES TO BE UPDATED              
UPPTPPID DC    AL1(PBYPMCLQ)       DATE ID - MATERIALS CLOSING                  
UPPTELID DC    XL1'20'             ELEMENT ID                                   
UPPTDISP DC    AL1(PBDMDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
UPPTENTL EQU   *-UPDPSVTB          LENGTH OF TABLE ENTRY                        
*                                                                               
         DC    AL1(PBYPCLSQ)       DATE ID - SPACE     CLOSING                  
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDCDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    AL1(PBYPBLBQ)       DATE ID - BILLABLE DATE                      
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDBDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    AL1(PBYPPAYQ)       DATE ID - PAYABLE DATE                       
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDPDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         DROP  RB,R2,R4,R5                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO DELETE DATE PASSIVE POINTERS                                    
*        HANDLES POINTER FOR                                                    
*              SPACE CLOSING DATE                                               
*              MATERIALS CLOSING DATE                                           
*              BILLABLE DATE                                                    
*              PAYABLE DATE                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DELPSV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    DMDSKADD,DMDSKADD   DIE IF NO DISK ADDRESS                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,DELPSVTB         ESTABLISH TABLE OF DATES                     
         USING DELPSVTB,R2                                                      
*                                                                               
         L     R5,AIO             POINT TO PBUYREC                              
         USING PBUYRECD,R5                                                      
*                                                                               
         LA    R1,WORK                                                          
         ST    R1,AIO         SO WRITE DOESN'T DESTROY MY RECORD                
*                                                                               
DPPLOOP  DS    0H                                                               
*                                                                               
         CLI   DPPTPPID,X'FF'      DONE AT END OF TABLE                         
         BE    DPPDONE                                                          
*                                                                               
*        FIND DATE FOR PASSIVE IN NEW RECORD                                    
*                                                                               
         LR    R6,R5               POINT TO NEW BUY RECORD                      
         MVC   ELCODE,DPPTELID     SET ID OF NEEDED ELEMENT                     
*                                                                               
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BNZ   DPPCONT             SKIP IF ELEMENT NOT FOUND                    
*                                                                               
         LLC   RF,DPPTDISP         GET DISPLACEMENT OF DATE                     
         LA    R1,0(RF,R6)         POINT TO DATE FOR PASSIVE                    
         MVC   DELPSVDT,0(R1)      SAVE NEW DATE                                
*                                                                               
*        CREATE PASSIVE FOR OLD RECORD                                          
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH PASSIVE KEY                        
         USING PBYPPSVD,R4                                                      
*                                                                               
         MVC   PBYPAGY,PBUYKAGY    SET AGENCY                                   
         MVC   PBYPMED,PBUYKMED    SET MEDIA                                    
         MVI   PBYPRCD,PBYPRIDQ    SET BASIC RECORD ID                          
         MVC   PBYPDTYP,DPPTPPID   SET DATE TYPE ID                             
         MVC   PBYPCLT,PBUYKCLT    SET CLIENT                                   
         MVC   PBYPPRD,PBUYKPRD    SET PRODUCT                                  
         MVC   PBYPEST,PBUYKEST    SET ESTIMATE                                 
         MVC   PBYPDTE,DELPSVDT    SET DATE                                     
         MVC   PBYPDADR,DMDSKADD   SET DISK ADDRESS                             
*                                                                               
*        READ OLD PASSIVE                                                       
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERR TESTS                   
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTOR HIGH                READ FOR NEW PASSIVE                         
*                                                                               
         CLC   PBYPKEY,KEYSAVE     SKIP IF PASSIVE NOT FOUND                    
         BNE   DPPNEW10                                                         
*                                                                               
         MVC   PBYPCNTL,PBUYCNTL   SET CONTROL BYTES                            
         OI    PBYPCNTL,X'80'      SET DELETE BIT ON                            
*                                                                               
         GOTOR WRITE               RE-WRITE PASSIVE                             
*                                                                               
DPPNEW05 DS    0H                                                               
*                                                                               
         B     DPPNEWX                                                          
*                                                                               
DPPNEW10 DS    0H                  ADD NEW PASSIVE                              
*                                                                               
DPPNEWX  DS    0H                                                               
*                                                                               
DPPCONT  DS    0H                                                               
         LA    R2,DPPTENTL(R2)     BUMP TO NEXT TABLE ENTRY                     
         B     DPPLOOP                                                          
*                                                                               
DPPDONE  DS    0H                                                               
*                                                                               
         ST    R5,AIO              RESTORE IOAREA PTR                           
*                                                                               
DELPSVX  DS    0H                  RESET PASS DELETES                           
         XIT1                                                                   
*                                                                               
*        TABLE FOR PROCESSING PASSIVE POINTERS FOR DATES                        
*                                                                               
         DS    0D                  ALIGNMENT                                    
         DC    CL8'DELPSVTB'                                                    
*                                                                               
DELPSVTB DS    0X                  TABLE OF PASSIVES TO BE DELATED              
DPPTPPID DC    AL1(PBYPMCLQ)       DATE ID - MATERIALS CLOSING                  
DPPTELID DC    XL1'20'             ELEMENT ID                                   
DPPTDISP DC    AL1(PBDMDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
DPPTENTL EQU   *-DELPSVTB          LENGTH OF TABLE ENTRY                        
*                                                                               
         DC    AL1(PBYPCLSQ)       DATE ID - SPACE     CLOSING                  
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDCDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    AL1(PBYPBLBQ)       DATE ID - BILLABLE DATE                      
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDBDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    AL1(PBYPPAYQ)       DATE ID - PAYABLE DATE                       
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDPDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         DROP  RB,R2,R4,R5                                                      
*                                                                               
         EJECT                                                                  
******************************************************************              
**  THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK                               
**  WHICH ARE "PERSONAL ID"                                                     
*                                                                               
PID      NTR1  BASE=*,LABEL=*                                                   
         XC    SVSFMPID,SVSFMPID         PASSWORD ID NUMBER CLEARED             
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0                                          
*                                                                               
         DROP  RF                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'          CHECK IF SECRET CODE IS THERE             
         BZ    *+10                                                             
         MVC   SVSFMPID,FAPASSWD      SAVE PASSWORD ID NUMBER CLEARED           
         DROP  R1                                                               
         SPACE 1                                                                
         XIT1                                                                   
*                                                                               
*********************************************************                       
**THIS ROUTINE ADDS A7 ELEM FOR THE SOON DELETED REC                            
**                                                                              
*********************************************************                       
DELPID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK(PPIDLEQ),WORK                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PPIDELQ      'A7'                                         
         BAS   RE,GETEL                                                         
         BNE   DELPID20                                                         
PP       USING PPIDELD,WORK                                                     
         MVC   PP.PPIDELM(4),0(R6)             SAVE OLD VALUE                   
         GOTO1 VRECUP,DMCB,(1,AIO),(R6),0     DELETE A7                         
*                                                                               
DELPID20 DS    0H                                                               
         MVI   PP.PPIDELM,PPIDELQ      'A7'                                     
         MVI   PP.PPIDELL,PPIDLEQ      '0A'                                     
         MVC   PP.PPIDDEL,SVSFMPID     WHO DELETED                              
*                                                                               
         L     RE,ATWA             POINT TO TWA                                 
         USING TWAD,RE                                                          
         MVC   PP.PPUSRID,TWAUSRID     ADD CONNECT ID NUM                       
         DROP  RE                                                               
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   DELPID30                                                         
         OI    PP.PPIDPRG,PPIDCPYQ     "SFM BUY COPY"                           
         B     DELPID50                                                         
*                                                                               
DELPID30 DS    0H                                                               
         CLI   ACTNUM,ACTMOVE                                                   
         BNE   DELPID50                                                         
         OI    PP.PPIDPRG,PPIDMOVQ     "SFM BUY MOVE"                           
*                                                                               
DELPID50 DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'FF'        FIND E-O-R                                   
         BAS   RE,GETEL                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(1,AIO),WORK,(R6)   ADD A7                           
         XIT1                                                                   
*                                                                               
         DROP  PP                                                               
*                                                                               
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,KEY                                                           
         USING PBUYRECD,R6                                                      
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
         LA    R4,LKUPKEY                                                       
         USING LKKEYD,R4                                                        
*                                                                               
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=C'BC'    CLIENT LOCK                                    
         MVC   LOCKMED,PBUYKMED                                                 
         MVC   LOCKCLT,PBUYKCLT                                                 
*                                                                               
TSTLOK20 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK20                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCPROF+5,C'2'      SUB-CLIENT ?                                 
         BNE   TSTLOK28            NO                                           
*                                                                               
         MVC   LOCKCLT,SVCPROF+6   USE MASTER CLIENT                            
         DROP  R4                                                               
*                                                                               
TSTLOK24 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK24                                                         
*                                  CHECK CLIENT/PUB LOCK                        
TSTLOK28 DS    0H                                                               
*                                  CHECK CLIENT/PUB LOCK                        
         XC    LKUPKEY,LKUPKEY                                                  
         LA    R4,LKUPKEY                                                       
         USING LKKEYD,R4                                                        
*                                                                               
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=C'BP'    CLIENT/PUB LOCK                                
         MVC   LOCKMED,PBUYKMED                                                 
         MVC   LOCKCLT,PBUYKCLT                                                 
         MVC   LOCKPUB,PBUYKPUB                                                 
         XC    LOCKPUB,=4X'FF'     COMPLEMENT TO ELIMINATE X'00'                
         OC    BPUB,BPUB           SEE IF DOING ALL PUBS                        
         BNZ   TSTLOK30            NO - TEST FOR ONE PUB LOCK                   
         XC    LOCKPUB,LOCKPUB     CLEAR PUB NUMBER TO TEST FOR LOCK            
*                                    ON ANY PUB FOR THIS MED/CLT                
TSTLOK30 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK30                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCPROF+5,C'2'      SUB-CLIENT ?                                 
         BNE   TSTLOK40            NO                                           
*                                                                               
         MVC   LOCKCLT,SVCPROF+6   USE MASTER CLIENT                            
         DROP  R4                                                               
*                                                                               
TSTLOK34 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK34                                                         
*                                  ADD A PUB LOCK                               
TSTLOK40 DS    0H                                                               
*                                                                               
         B     TSTLKEQ             DO NOT ADD LOCK (12/20/01)                   
*                                                                               
*                                  ADD AN APPROPRIATE LOCK                      
         XC    LKUPKEY,LKUPKEY                                                  
         LA    R4,LKUPKEY                                                       
         USING LKKEYD,R4                                                        
*                                                                               
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=C'BP'      SET TO CLIENT/PUB LOCK                       
         MVC   LOCKMED,PBUYKMED                                                 
         MVC   LOCKCLT,PBUYKCLT                                                 
         MVC   LOCKPUB,PBUYKPUB                                                 
         XC    LOCKPUB,=4X'FF'     TO ELIMINATE X'00' FIELDS                    
         OC    BPUB,BPUB           SEE IF DOING ALL PUBS                        
         BNZ   TSTLOK50            NO - LOCK ONE PUB ONLY                       
         XC    LOCKPUB,LOCKPUB     CLEAR PUB                                    
         MVC   LOCKRTY,=C'BC'      SET TO CLIENT LOCK FOR ALL PUBS              
         DROP  R4                                                               
*                                                                               
TSTLOK50 L     RF,ACOMFACS         ***** ADD THE LOCK                           
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK50                                                         
         CLI   4(R1),0             ANY OTHER ERROR  SEND                        
         BNE   TSTLKNEQ            RECORD LOCKED MESSAGE                        
*                                                                               
TSTLKEQ  DS    0H                                                               
         MVI   LOCKSW,C'Y'         INDICATE LOCK TESTED AND SET                 
         CR    RB,RB                                                            
         B     TSTLKXIT            REC LOCKED BY ME - GO                        
TSTLKNEQ LTR   RB,RB               REC LOCKED - CANNOT START                    
TSTLKXIT XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*=================================================================*             
* ROUTINE TO UNLOCK KEY SET BY THIS APPLICATION                   *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*                                                                 *             
*  LKUPKEY SHOULD STILL CONTAIN SAME DATA USED TO SET THE LOCK    *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
UNLOCK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
UNLOCK2  L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKUNLKQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    UNLOCK2                                                          
*                                                                               
*   MAY NEED OTHER MESSAGES "OR" FOR CONDITIONAL ERRORS HERE                    
*                                                                               
UNLOKXIT XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*   DISPLAY "DATA LOCKED" MESSAGE ALONG WITH AN ACCOUNT OF WHAT HAS   *         
*   BEEN PROCESSED UP TO THE POINT OF THE LOCK ENCOUNTER              *         
*          ******  (NOT IN USE AS OF 05/30/01)  ******                *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
LK900    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
         CLI   ACTNUM,ACTMKLIV     TEST ACTION=MAKELIVE                         
         BE    LK940                                                            
*                                                                               
         LA    R2,SFMOUT1H                                                      
         ICM   R0,15,DELBUYS                                                    
         BZ    LK905                                                            
         MVC   8(16,R2),=C'999 BUYS DELETED'                                    
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         L     R0,DELDOLS                                                       
         EDIT  (R0),(15,25(R2)),2,FLOAT=$,ALIGN=LEFT                            
         OI    6(R2),X'80'         SET TO XMT                                   
         LA    R2,SFMOUT2H         ADVANCE OUTPUT POINTER                       
*                                                                               
LK905    DS    0H                                                               
         ICM   R0,15,ADDBUYS                                                    
         BZ    LK999                                                            
         MVC   8(14,R2),=C'999 BUYS ADDED'                                      
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         L     R0,ADDDOLS                                                       
         EDIT  (R0),(15,25(R2)),2,FLOAT=$,ALIGN=LEFT                            
         OI    6(R2),X'80'         SET TO XMT                                   
         B     LK999                                                            
         SPACE 1                                                                
LK940    DS    0H                  ACTION=MAKELIVE                              
         ICM   R0,15,LIVBUYS                                                    
         BZ    LK999                                                            
*                                                                               
         LA    R2,SFMOUT1H                                                      
         MVC   8(18,R2),=C'999 BUYS MADE LIVE'                                  
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         L     R0,LIVDOLS                                                       
         EDIT  (R0),(15,30(R2)),2,FLOAT=$,ALIGN=LEFT                            
         OI    6(R2),X'80'         SET TO XMT                                   
         B     LK999                                                            
*                                                                               
LK999    DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'** DATA LOCK - HIT ENTER TO CONTINUE **'          
*                                                                               
LKXX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CKMAX    NTR1  BASE=*,LABEL=*                                                   
************************************************************                    
* FIND AND CHECK ALL CONTRACTS FOR CLIENT                                       
************************************************************                    
*                                                                               
*        REGISTERS R8,R9,AND R3,RA ARE IN USE -                                 
*                                  DO NOT ALTER IN THIS ROUTINE                 
*                                                                               
         NI    DMINBTS,X'7F'       SET OFF READ FOR UPDATE                      
         MVI   CONSW,C'N'          SET NO RECORD READ                           
         MVI   ERROR,0                                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PCONRECD,R6                                                      
*                                                                               
         MVC   PCONKAGY,AGENCY                                                  
         MVC   PCONKMED,QMED                                                    
         MVI   PCONKRCD,X'10'                                                   
         MVC   PCONKCLT,QCLT                                                    
*                                                                               
         CLI   SVCPROF+5,C'2'       TEST SLAVE CLIENT                           
         BNE   *+10                                                             
         MVC   PCONKCLT,SVCPROF+6   READ MASTER CONTRACT                        
         MVC   PCONKPUB(6),BPUB                                                 
*                                                                               
         OC    SADVDATA,SADVDATA    CHK FOR ADV DATA                            
         BZ    CKMAX1                                                           
*                                                                               
         OC    SADVDATA,SADVDATA      NEW ADV SYSTEM                            
         BZ    CKMAX1              NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    CKMAX1                                                           
         CLC   SVAOR,AGENCY        SEE IF AM THE AOR                            
         BE    CKMAX1                                                           
*                                                                               
         TM    SVAORC,X'01'        TEST PUB LINK                                
         BZ    CKMAX0C                                                          
         OC    BPUB,BPUB           SEE IF ALL PUBS                              
         BZ    CKMAX0C                                                          
*                                                                               
         BAS   RE,FNDAPUB                                                       
*                                                                               
         MVC   KEY+7(6),SVADVPUB                                                
CKMAX0C  MVC   KEY(2),SVAOR        AOR                                          
         MVC   KEY+4(3),SVADV      ADV                                          
*                                                                               
*                                   MUST SWITCH TO AOR                          
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    CKMAX1                                                           
         XC    CONHEAD,CONHEAD                                                  
         OI    DMINBTS,X'80'                                                    
         MVC   CONHEAD(34),=C'** ADVERTISER SYSTEM NOT ACTIVE **'               
         GOTO1 ERREX2                                                           
*                                                                               
CKMAX1   GOTO1 HIGH                                                             
         B     CKMAX4                                                           
*                                                                               
CKMAX2   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CKMAX4   CLC   KEY(7),KEYSAVE     A/M/REC/CL                                    
         BNE   CKMAXX                                                           
         OC    BPUB,BPUB          SEE IF ALL PUBS                               
         BZ    CKMAX8                                                           
         CLC   KEY(13),KEYSAVE    A/M/REC/CLT/PUB                               
         BNE   CKMAXX                                                           
*                                                                               
CKMAX8   MVI   CONSW,C'Y'          SET FLAG FOR CONTRACT READ                   
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PCONRECD,R6                                                      
         L     R7,AIO1             POINT TO BUYREC                              
         LA    R7,(PBUYKDAT-PBUYKEY)(R7)                                        
         CLC   PCONSTRT(3),BEND    CONTRACT START AFTER PERIOD END              
         BH    CKMAX2                                                           
         CLC   PCONEND(3),BSTART   CONTRACT END BEFORE PEROID START             
         BL    CKMAX2                                                           
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKMAX2                                                           
*                                                                               
         USING PCATELEM,R6                                                      
         OC    PCATMAX,PCATMAX                                                  
         BNZ   CKMAXERR                                                         
         OC    PCATMAXZ,PCATMAXZ                                                
         BNZ   CKMAXERR                                                         
         B     CKMAX2              CONTINUE SEARCH                              
*                                                                               
CKMAXERR DS    0H                                                               
         OC    SADVDATA,SADVDATA      NEW ADV SYSTEM                            
         BZ    CKMAXE2             NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    CKMAXE2                                                          
         CLC   SVAOR,AGENCY        SEE IF AM THE AOR                            
         BE    CKMAXE2                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                  MUST BE ABLE TO SWITCH BACK                
*                                                                               
CKMAXE2  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         OI    DMINBTS,X'80'                                                    
         MVC   CONHEAD(54),=C'** MAX/ISSUE IN USE - MAKELIVE AND COPY AX        
               RE INVALID **'                                                   
         NI    SFMMEDH+4,X'DF'       UNVALIDATE MEDIA                           
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
CKMAXX   OI    DMINBTS,X'80'       RESET FOR READ FOR UPDATE                    
         OC    SADVDATA,SADVDATA      NEW ADV SYSTEM                            
         BZ    CKMAXX2             NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    CKMAXX2                                                          
         CLC   SVAOR,AGENCY        SEE IF AM THE AOR                            
         BE    CKMAXX2                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                  MUST BE ABLE TO SWITCH BACK                
*                                                                               
CKMAXX2  DS    0H                                                               
         XMOD1 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
FNDAPUB  NTR1                                                                   
         MVC   X(32),KEY                                                        
         XC    SVADVPUB,SVADVPUB                                                
         TM    SVAORC,X'01'         PUB LINK REQUIRED (NEW ADV)                 
         BZ    FNDA9                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'FE'                                                        
         MVC   KEY+1(1),QMED             MEDIA                                  
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+4(3),SVADV                                                   
         MVC   KEY+7(2),SVAOR                                                   
         MVC   KEY+9(6),BPUB                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         CLC   KEY(15),KEYSAVE                                                  
         BE    FNDA5                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'** ADVERTISER PUB LINK MISSING **'                
         LA    R2,SFMPUBH         CURSOR TO PUB                                 
         GOTO1 ERREX2                                                           
*                                                                               
FNDA5    DS    0H                                                               
         MVC   SVADVPUB,KEY+15          SAVE ADV PUB NUMBER                     
*                            NEEDED TO READ ADVERTISER CONTRACT                 
*                                                                               
FNDA9    DS    0H                                                               
         MVC   KEY(32),X                                                        
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        CHKINV - CHECK FOR ATTACHED INVOICE                          *         
*              RETURN NE CC IF THERE IS AN INVOICE                    *         
*              BUMP NUMBER OF BUYS BYPASSED BECAUSE OF THIS           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
CHKINV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        LOOK FOR INVOICE LINE ELEMENT IN BUY                                   
*                                                                               
         L     R6,AIO                                                           
         USING PBUYRECD,R6                                                      
         MVI   ELCODE,PBNVELQ      INVOICE LINE ELM ID                          
         BAS   RE,GETEL                                                         
         BE    CHKINVNE            ELEMENT FOUND - SET NE CC                    
*                                                                               
*        FIND INVOICE MATCHING STATUS ELEMENT                                   
*                                                                               
         L     R6,AIO                                                           
         USING PBUYRECD,R6                                                      
         MVI   ELCODE,PBMATELQ     INVOICE MATCH STATUS ELM ID                  
         BAS   RE,GETEL                                                         
         BNE   CHKINVOK            ELEMENT NOT FOUND - SET EQ CC                
*                                                                               
         USING PBMATELD,R6         ESTABLISH ELEMENT                            
*                                                                               
         CLI   PBMTSTAT,PBMTSNIQ   OKAY IF NO INVOICE STATUS                    
         BE    CHKINVOK                                                         
*                                                                               
CHKINVNE DS    0H                  SET NE CC                                    
*                                                                               
         LTR   RB,RB                                                            
         B     CHKINVX                                                          
*                                                                               
CHKINVOK DS    0H                  SET EQ CC                                    
         CR    RB,RB                                                            
         B     CHKINVX                                                          
*                                                                               
CHKINVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISALLOW PRODUCT CODE AAA AS A "TO" PRODUCT                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
CKTOAAA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'AAA',SVPRDT      IS 'TO' PRD CODE AAA ?                       
         BE    CKTOAAER            YES - INVALID                                
*                                                                               
         CLI   5(R2),0             TEST 'TO' PRD ENTERED                        
         BE    CKTOAAAX            NO - RETURN                                  
*                                                                               
         CLC   =C'AAA',8(R2)       IS 'TO' PRD CODE AAA ?                       
         BNE   CKTOAAAX            NO - RETURN                                  
*                                                                               
CKTOAAER DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(51),=C' ** MOVE/COPY TO PRODUCT CODE AAA IS NOT X        
               ALLOWED **'                                                      
         GOTO1 ERREX2                                                           
*                                                                               
CKTOAAAX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISALLOW ACTION IF USER SET UP FOR USE OF EIO IN ADBUYER     *         
***********************************************************************         
         DS    0D                                                               
CKEIO    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVKEY(25),KEY       SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING SCHRECD,R5                                                       
         L     R6,AIO              POINT TO CLIENT RECORD                       
         MVC   SCHKKEY(SCHKPRD-SCHKKEY),0(R6)       THRU CLIENT                 
         MVI   SCHKRCD,SCHKRCDQ    SETUP RECORD CODE                            
*                                                                               
         GOTOR HIGH                                                             
         CLC   SCHKKEY(SCHKPRD-SCHKKEY),KEYSAVE                                 
         BE    CKEIOGET            HAS CLIENT SETUP RECORD                      
         XC    KEY,KEY                                                          
         MVC   SCHKKEY(SCHKRCD-SCHKKEY),0(R6)   AGY/MEDIA                       
         MVI   SCHKRCD,SCHKRCDQ    SETUP RECORD CODE                            
         GOTOR HIGH                                                             
         CLC   SCHKKEY(SCHKPRD-SCHKKEY),KEYSAVE                                 
         BNE   CKEIOX              OKAY - NO EIO SETUP RECORD                   
         DROP  R5                                                               
*                                                                               
CKEIOGET DS    0H                                                               
         GOTOR GETREC                                                           
         L     R5,AIO                                                           
         LA    R5,33(R5)                                                        
         USING SCHHDRD,R5                                                       
         CLI   SCHHDRCD,SCHHDRQ    SETUP HEADER ELEMENT ?                       
         BNE   CKEIOX              NO - OKAY - NOT SET UP FOR EIO'S             
         CLI   SCHEIO,C'Y'         USE EIO'S ?                                  
         BNE   CKEIOX              NO - OKAY - NOT SET UP FOR EIO'S             
         DROP  R5                                                               
*                                                                               
CKEIOERR DS    0H                                                               
         XC    CONHEAD,CONHEAD X                                                
         MVC   CONHEAD(60),=C'*** COPY/MOVE NOT ALLOWED FOR CLIENT SET X        
               UP TO USE EIO ****'                                              
         GOTOR ERREX2                                                           
*                                                                               
CKEIOX   DS    0H                                                               
         MVC   KEY(25),SVKEY       RESTORE                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF3D                                                       
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
SVTODATA DS    0XL60                                                            
SVPRDT   DS    CL3                                                              
SVESTT   DS    XL2                                                              
SVSTATT  DS    XL1                                                              
SVRTYPT  DS    XL1                                                              
SVPERT   DS    XL6                                                              
         ORG   SVPERT                                                           
SVPERTST DS    XL3                                                              
SVPERTND DS    XL3                                                              
SVTALLO  DS    CL47                                                             
*                                                                               
SVFRDATA DS    0XL60                                                            
SVPRDF   DS    CL3                                                              
SVESTF   DS    XL2                                                              
SVSTATF  DS    XL1                                                              
SVRTYPF  DS    XL1                                                              
SVPERF   DS    CL6                                                              
         ORG   SVPERF                                                           
SVPERFST DS    XL3                                                              
SVPERFND DS    XL3                                                              
SVFALLO  DS    CL47                                                             
*                                                                               
         DS    CL5                 END OF SVFRDATA                              
*                                                                               
PTODAY   DS    CL2                 TODAY PACKED                                 
DELSW    DS    CL1                                                              
CONSW    DS    CL1                 C'Y' = CONTRACT HAS BEEN READ                
REALLO   DS    CL1                 C'Y' IF REALLOCATING ZZZ BUYS                
TOFROM   DS    XL1                 X'11' = PRD TO PRD                           
*                                  X'12' = PRD TO ZZZ                           
*                                  X'21' = ZZZ TO PRD                           
*                                  X'22' = ZZZ TO ZZZ                           
BYTE2    DS    XL1                                                              
*                                                                               
LKUPKEY  DS    XL16                FOR DATA LOCK HANDLING                       
LOCKSW   DS    XL1                 "Y" = LOCK ALREADY TESTED                    
*                                                                               
FROMADJ  DS    CL3                 TO COMPARE TO & FROM ADJ CODES               
TOADJ    DS    CL3                                                              
SAVERE   DS    A                                                                
SVMAXIO  DS    H                                                                
SVLSTKEY DS    XL25                                                             
*                                                                               
SVPRSTAT DS    XL1         SAVED "TO" PRODUCT STATUS BYTE                       
*                                                                               
SVCLTST  DS    CL1         SAVED CLIENT STATUS                                  
SVC2FAC  DS    PL5         SAVED CLIENT COST2 FACTOR                            
SVE2FAC  DS    PL5         COST2 FACTOR OF "TO" ESTIMATE                        
SVCFDTA  DS    CL3         SAVED CLIENT FREEZE-BY-DATE ELEMENT DATA             
*                            STATUS(1) AND YM DATE(2)                           
         DS    0F                                                               
BUYTOTS  DS    XL32                                                             
         ORG   BUYTOTS                                                          
ADDDOLS  DS    F                   GROSS DOLLARS ADDED                          
ADDBUYS  DS    F                   NUM BUYS ADDED                               
DELDOLS  DS    F                   GROSS DOLLARS DELETED                        
DELBUYS  DS    F                   NUM BUYS DELETED                             
LIVDOLS  DS    F                   GROSS DOLLARS MADE LIVE                      
LIVBUYS  DS    F                   NUM BUYS MADE LIVE                           
DELNOBYS DS    F                   NUM BUYS NOT DELETED                         
MVCNOBYS DS    F                   NUM BUYS NOT MOVED                           
         SPACE 2                                                                
*                                                                               
SADVDATA DS    0CL18              SAVED FROM CLIENT HEADER                      
SVAOR    DS    CL2                AGY OF RECORD                                 
SVADV    DS    CL3                ADVERTISER                                    
SVCEQU   DS    CL3                EQUATED ADV CLIENT CODE                       
SVADVST  DS    CL3                START DATE                                    
SVADVED  DS    CL3                END DATE                                      
SVAORSE  DS    CL1                AOR SE NUMBER                                 
SVAORC   DS    CL3                CONTROLS                                      
*                                 FIRST BYTE                                    
*                                 X'01' = PUB LINK                              
*                                 X'02' = CON LEVEL LOOK-UP ($CON)              
*                                 X'04' = CON RATE LOOK-UP  ($CON)              
*                                 X'08' = CON RATE LOOK-UP ($BUY)               
*                                 X'10' = ADV AUTO SCHEDULE CHECKING            
*                                                                               
*        END OF SADVDATA                                                        
*                                                                               
SVADVPUB DS    CL6                                                              
DOUBLE   DS    D                                                                
*                                                                               
X        DS    CL100                                                            
*                                                                               
SAVCLT   DS    XL3'00'                                                          
*                                                                               
UPDPSVDT DS    XL3                 DATE FOR PASSIVE                             
DELPSVDT DS    XL3                 DATE FOR PASSIVE                             
*                                                                               
PARS     DS    6F                                                               
         ORG   PARS                                                             
* USCAN TABLE DEFINITION                                                        
*                                                                               
UFRST    DS    C                                                                
UADDR    DS    AL3                                                              
USTRNG   DS    H                                                                
ULNGTH   DS    H                                                                
UVAL     DS    C                                                                
UEND     DS    AL3                                                              
USCN     DS    C                                                                
USTOP    DS    C                                                                
USCN1    DS    C                                                                
USCN2    DS    C                                                                
USCN3    DS    C                                                                
USCN4    DS    C                                                                
USCN5    DS    C                                                                
USCN6    DS    C                                                                
         ORG                                                                    
*                                                                               
*                                                                               
PVALUESD DSECT                                                                  
       ++INCLUDE PVALUES                                                        
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
         SPACE 2                                                                
       ++INCLUDE PBDELEM                                                        
         SPACE 2                                                                
       ++INCLUDE PPRELEM                                                        
         SPACE 2                                                                
       ++INCLUDE PPAYELEM                                                       
         SPACE 2                                                                
       ++INCLUDE PBILELEM                                                       
         SPACE 2                                                                
       ++INCLUDE PIOELEM                                                        
         SPACE 2                                                                
       ++INCLUDE PCHGELEM                                                       
         SPACE 2                                                                
       ++INCLUDE PPPPIDEL                                                       
         SPACE 2                                                                
       ++INCLUDE PPGENPBMAT                                                     
         SPACE 2                                                                
       ++INCLUDE PPGENPBNV                                                      
         SPACE 2                                                                
PCATELD  DSECT                                                                  
       ++INCLUDE PCATELEM                                                       
*                                                                               
       ++INCLUDE PPGENBYPP                                                      
*                                                                               
         EJECT                                                                  
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
PJOBRECD DSECT                                                                  
       ++INCLUDE PJOBREC                                                        
         EJECT                                                                  
PCONRECD DSECT                                                                  
       ++INCLUDE PCONREC                                                        
         SPACE 2                                                                
       ++INCLUDE PASRELEM                                                       
         EJECT                                                                  
*                                                                               
PPSERPPD DSECT                                                                  
       ++INCLUDE PPSERPP                                                        
PSERELED DSECT                                                                  
       ++INCLUDE PSERELEM                                                       
*                                                                               
       ++INCLUDE PPGENSCH                                                       
*                                                                               
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
        EJECT                                                                   
       ++INCLUDE DDCOMFACS                                                      
        EJECT                                                                   
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
*                                                                               
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                 BASE PUB ONLY                                
         DS    XL2                                                              
*                                                                               
       ++INCLUDE FATWA                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'164PRSFM03   02/25/15'                                      
         END                                                                    
