*          DATA SET SPREPR109  AT LEVEL 128 AS OF 11/09/11                      
*          DATA SET SPREPR102  AT LEVEL 053 AS OF 06/01/99                      
*   THIS IS A CORRECTION PROGRAM THAT EXTRACTS RTS BUYLINES FROM                
*          THE RECOVERY FILES, SORTS THEM, AND PRODUCES A LIST OF               
*          UNIQUE CONTRACT NUMBERS WITHIN REPS.  THE LIST MAY                   
*          THEN BE FED INTO A SCRIPT TO DRIVE THE RTS/ALL.                      
*                                                                               
*                                                                               
*PHASE SPR109A                       <<< WAS SPR102C                            
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE REGENBUC                                                               
*INCLUDE SORTER                                                                 
         TITLE 'SPREPR102 (SPR102) - REP -> SPOT BUY X-FER EXTRACT'             
*                                                                               
*        FOR ERROR CORRECTION ONLY:  THIS IS NOT THE PRODUCTION                 
*        VERSION OF THE SPR102                                                  
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR102 (SPR102) --- CORRECTION EXTRACT                         *          
*                          RECOVERY FILE EXTRACTION PHASE            *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* REQUEST CARD:                                                      *          
*                                                                    *          
* QOPT1/2 (COL 62/63)   =   REP SYSTEM NUMBER (IN HEX)               *          
* QOPT3   (COL 64)      =   'D' TAKE INPUT FROM A DISK FILE          *          
* QOPT4   (COL 65)      =   'M' MEANS MULTI-TAPE INPUT               *          
* QOPT4+1 (COL 66)      =    # OF TAPES IF COL 65 = 'M'              *          
* QUESTOR (COL 69)      =   TWO-CHARACTER POWER CODE OF SUBSIDIARY   *          
* QUESTOR+2 (COL 71)    =   ONE-CHARACTER CONTRACT TYPE CODE         *          
* ------------------------------------------------------------------ *          
*                                                                    *          
* WHILE NOT EOF ON RECOVERY FILE                                     *          
*     -TEST FOR A SPOTPAK MARKED BUY RECORD                          *          
*     -FORMAT AND RELEASE A SORT RECORD                              *          
* END WHILE                                                          *          
*                                                                    *          
* SORT                                                               *          
*                                                                    *          
* PRODUCE CONTROL REPORT                                             *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JUL02/03 (MRR) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*                                                                    *          
*                     **  END TOMBSTONE  **                          *          
**********************************************************************          
*                                                                               
SPR102   CSECT                                                                  
         NMOD1 0,SPR102,R9,R8,R7,RR=R2                                          
         ST    R2,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING SPWORKD,RC,RA                                                    
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAINLINE                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
*        MAIN LINE                                                              
*                                                                               
MAINLINE EQU   *                                                                
         BAS   RE,INIT                                                          
         BNZ   MAINERR                                                          
         BAS   RE,READRCV                                                       
         BNZ   MAINERR                                                          
         MVC   P+1(06),=C'COUNT='                                               
         EDIT  SORTRECS,(5,P+7)                                                 
         GOTO1 REPORT                                                           
         CLC   SORTRECS(4),=F'1'                                                
         BE    MAIN100                                                          
         MVC   P+1(20),=C'ENTERING SORT RETURN'                                 
         GOTO1 REPORT                                                           
         XC    SORTRECS,SORTRECS                                                
MAIN10   EQU   *                                                                
         XC    DM2,DM2                                                          
         GOTO1 ASORT,DMCB,=C'GET'                 SORT THE FILE                 
         ZICM  R6,DM2,4                                                         
         BZ    MAIN100                                                          
         CLC   SORTREC(10),0(R6)   SAME REP + CONTRACT?                         
         BE    MAIN10              YES - SKIP IT                                
         L     RF,SORTRECS                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SORTRECS                                                      
*                                                                               
         MVC   SORTREC,0(R6)                                                    
         MVC   P(2),SORTREC                                                     
         MVC   P+3(8),SORTREC+2                                                 
         EDIT  SORTRECS,(5,P+20)                                                
*                                                                               
         GOTO1 REPORT                                                           
         B     MAIN10                                                           
MAIN100  EQU   *                                                                
         GOTO1 ASORT,DMCB,=C'END'                                               
*                                                                               
*        END MAINLINE / FINISH RUN                                              
*                                                                               
MAINGOOD EQU   *                                                                
         MVC   P(21),=C'*** END OF REPORT ***'                                  
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
MAINERR  EQU   *                                                                
         MVC   P(40),=C'* * * ERROR ENCOUNTERED DURING RUN * * *'               
         GOTO1 REPORT                                                           
         MVC   P(60),RUNERROR                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=C'* * * RUN TERMINATED * * *'                             
         GOTO1 REPORT                                                           
MAINEXIT EQU   *                                                                
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        INIT --- SET INITIAL VALUES FOR THIS RUN                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*                                                                               
         L     RE,UTL                                                           
         MVC   SPOTSYS,4(RE)                                                    
*                                                                               
         LA    RE,IOAREA1                                                       
         ST    RE,AREC                                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'              FIND CONTROL FILE ID RECORD               
         MVC   WORK+23(02),RCORIGID   LOAD AGENCY CODE                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                     
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                NO  - SHOULD HAVE BEEN THERE...              
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND THE SYS AUTHORIZATION ELEMENT           
INIT0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   INIT0020            FOUND                                        
         CLI   2(R1),X'08'         IS IT 'REP SYSTEM'?                          
         BE    INIT0030                                                         
INIT0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   INIT0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
INIT0030 EQU   *                                                                
         MVC   REPSYS,3(R1)        LOAD REP SYSTEM NUMBER                       
         L     RE,UTL                                                           
         MVC   4(1,RE),REPSYS                                                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AREC,DMWORK             
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSYS                                                  
INIT0040 EQU   *                                                                
         MVI   DODISK,0                                                         
         OPEN  (RCVTAPE,(INPUT))                                                
         LTR   RF,RF                                                            
         BZ    INIT0070                                                         
         DC    H'0'                                                             
INIT0070 EQU   *                                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',=C'NCTFILE X'                     
*                                                                               
         L     RF,=V(SORTER)                                                    
         A     RF,RELO                                                          
         ST    RF,ASORT                                                         
*                                                                               
         GOTO1 ASORT,DMCB,SORTCARD,RECCARD         INIT THE SORT                
*                                                                               
         XC    SORTREC,SORTREC     CLEAR THE SORT RECORD                        
*                                                                               
*                                                                               
         MVC   SORTKEY(4),=X'FFFFFFFF'     SEND A 'EOF' REC OUT                 
         GOTO1 ASORT,DMCB,=C'PUT',SORTREC                                       
         L     RF,SORTRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,SORTRECS                                                      
*                                                                               
         XC    SORTREC,SORTREC                                                  
*                                                                               
*        INIT EXIT                                                              
*                                                                               
INIT0100 EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        READRCV --- READ AND PROCESS THE REP RECOVERY FILE                     
*                                                                               
READRCV  NTR1                                                                   
*                                                                               
RRCV10   EQU   *                                                                
         BAS   RE,GETRCV                                                        
         BZ    RRCV15                                                           
         MVC   P+1(19),=C'END OF FILE REACHED'                                  
         GOTO1 REPORT                                                           
         B     RRCVGOOD                 NON-ZERO IS END-OF-FILE                 
RRCV15   EQU   *                                                                
         GOTO1 EXTRDISP,DMCB,3                                                  
*                                                                               
         XC    DM3(4),DM3                                                       
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFILE'),(X'08',RCVDATA),      X        
               RR=RELO                                                          
         CLI   DM4,0                                                            
         BNE   RRCV10                                                           
         MVC   SAVEDM4,DM4         SAVE A(X'08' ELEMENT)                        
         GOTO1 EXTRDISP,DMCB,1                                                  
*                                                                               
*   X'08' ELEMENT FOUND.  NOW LOOK FOR FIRST BUY COMMENT ELEMENT.               
*      IF FIRST THREE CHARACTERS = 'CR=', SKIP THE BUY.                         
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFILE'),(X'04',RCVDATA),      X        
               RR=RELO                                                          
         CLI   DM4,0                                                            
         BNE   RRCV20                                                           
         L     R4,DM4              A(BUY COMMENT ELEMENT)                       
         CLC   =C'CR=',2(R4)       IS IT A CREDIT BUY?                          
         BE    RRCV10              YES - SKIP IT                                
RRCV20   EQU   *                                                                
*                                                                               
*    CHECK X'01' ELEMENT FOR N/A BUYLINE (INTEREP COMBO TYPE).                  
*      IF FOUND, SKIP THE BUY.                                                  
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFILE'),(X'01',RCVDATA),      X        
               RR=RELO                                                          
         CLI   DM4,0                                                            
         BNE   RRCV30                                                           
         L     R4,DM4              A(BUY DESCRIPTION ELEMENT)                   
         TM    RBUYCOMB-RBUYELEM(R4),X'80'                                      
*                                  IS IT AN 'N/A' BUYLINE?                      
         BO    RRCV10              YES - SKIP IT                                
RRCV30   EQU   *                                                                
         CLI   QUESTOR+2,C' '      CONTRACT TYPE FILTER ENTERED?                
         BNH   RRCV3020            NO  - DON'T CHECK                            
         BAS   RE,CONFILTR         YES - GET CONTRACT HEADER                    
         BNZ   RRCV10              NOT A MATCH: SKIP IT                         
RRCV3020 EQU   *                                                                
         L     R4,SAVEDM4          SET A(X'08' ELEMENT)                         
         USING RBUYSPEL,R4                                                      
         GOTO1 CKAGY,DMCB,RBUYSPAG                                              
         BNZ   RRCV10                                                           
         BAS   RE,CKCON                                                         
         BNZ   RRCV10                                                           
         OC    RBUYSPST,RBUYSPST   NO DATA IN ELEMENT?                          
         BZ    RRCV10               YES - SKIP THIS RECORD                      
         OC    RBUYSADV,RBUYSADV   NO DATA IN ELEMENT?                          
         BZ    RRCV10               YES - SKIP THIS RECORD                      
         OC    RBUYSPRD,RBUYSPRD   NO DATA IN ELEMENT?                          
         BZ    RRCV10               YES - SKIP THIS RECORD                      
         DROP  R4                                                               
*                                                                               
         LA    RE,SORTREC                                                       
         USING RBUYREC,R4                                                       
         LA    R4,RCVDATA                                                       
         MVC   SORTKREP(2),RBUYKREP                                             
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RBUYKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+28(4),WORK+15                                               
         PACK  WORK+28(1),WORK+18(1)   REVERSE THE COMPLIMENT                   
         PACK  WORK+29(1),WORK+17(1)                                            
         PACK  WORK+30(1),WORK+16(1)                                            
         PACK  WORK+31(1),WORK+15(1)                                            
*                                                                               
***      MVC   P+1(10),=C'BUYLINE = '                                           
***      GOTO1 HEXOUT,DMCB,RBUYKCON,P+15,4,=C'TOG'                              
***      GOTO1 HEXOUT,DMCB,WORK+28,P+25,4,=C'TOG'                               
***      GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+28,SORTKCON,4,=C'TOG'                           
         MVC   SORTKLIN(2),RBUYKMLN    INSERT REP BUYLINE LINE #                
         ZICM  R4,RDATE,3                                                       
         LCR   R4,R4                                                            
         STCM  R4,7,SORTKDAT       TAKE LAST THREE BYTES                        
         ZICM  R4,RTIME,4                                                       
         LCR   R4,R4                                                            
         SRA   R4,4                DROP LOW-ORDER NYBBLE                        
         LA    R4,1(R4)            ADD 1 TO ADJUST FOR COMPLEMENT               
*                                                                               
*  NOTE:  TIME IS A 'PACKED DECIMAL' FORMAT, WHICH TAKES BITS 4-27,             
*        AND IGNORES BITS 0-3 AND 28-31.  THE SHIFT PROPERLY ALIGNS             
*        THE THREE BYTES NEEDED FOR THE SORT FIELD.                             
*                                                                               
         STCM  R4,7,SORTKTIM       TAKE LAST THREE BYTES                        
*                                                                               
         DROP  R4                                                               
*                                                                               
***      MVC   P+1(05),=C'SORT='                                                
***      MVC   P+6(19),SORTREC                                                  
***      GOTO1 REPORT                                                           
*                                                                               
         GOTO1 ASORT,DMCB,=C'PUT',SORTREC                                       
         GOTO1 EXTRDISP,DMCB,2                                                  
         L     RF,SORTRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,SORTRECS                                                      
*                                                                               
         B     RRCV10                  GET NEXT RECOVERY RECORD                 
*                                                                               
RRCVGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     RRCVEXIT                                                         
RRCVBAD  EQU   *                                                                
         LA    R0,1                                                             
RRCVEXIT EQU   *                                                                
         MVC   P+1(08),=C'RRCVEXIT'                                             
         GOTO1 REPORT                                                           
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        CONFILTR:  COMPARE CONTRACT TYPE FILTER FROM QUESTOR+2 WITH            
*                   CONTYPE IN CONTRACT HEADER                                  
*                                                                               
CONFILTR NTR1                                                                   
         LA    R2,RCVDATA          SET A(DATA RECORD)                           
         USING RBUYREC,R2                                                       
*                                                                               
         XC    REPKEY,REPKEY                                                    
         MVI   REPKEY,X'8C'        SET KEYTYPE                                  
         MVC   REPKEY+21(2),RBUYKREP                                            
*                                      REVERSE CONTRACT NUMBER                  
         MVC   WORK+15(4),RBUYKCON     RETRIEVE THE CONTRACT NUMBER             
         MVC   REPKEY+23(4),WORK+15       PRIME THE VALUE                       
         PACK  REPKEY+23(1),WORK+18(1)    REVERSE THE COMPLIMENT                
         PACK  REPKEY+24(1),WORK+17(1)                                          
         PACK  REPKEY+25(1),WORK+16(1)                                          
         PACK  REPKEY+26(1),WORK+15(1)                                          
*&&DO                                                                           
*                                                                               
*   TEST DISPLAY OF CONTRACT NUMBER REVERSAL                                    
*                                                                               
         MVC   P(04),=C'BUY:'                                                   
         MVC   P+4(27),RBUYKEY                                                  
         GOTO1 REPORT                                                           
         MVC   P(04),=C'CON:'                                                   
         MVC   P+4(27),REPKEY                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     CFIL0900            EXIT CC OKAY                                 
*&&                                                                             
*                                                                               
         L     R4,AREC                                                          
         USING RCONREC,R4                                                       
*                                                                               
         CLC   RCONKCON,REPKEY+23  CONTRACT ALREADY READ?                       
         BE    CFIL0040            YES - DON'T REREAD                           
*                                                                               
         BAS   RE,REPHIGH                                                       
         CLC   REPKEY(27),REPKSAVE                                              
         BNE   CFIL0910            KEY NOT FOUND: EXIT ERROR                    
*                                                                               
         BAS   RE,REPGET                                                        
CFIL0040 EQU   *                                                                
         CLC   RCONTYPE,QUESTOR+2  CONTRACT TYPE = FILTER?                      
         BNE   CFIL0910            NO  - EXIT CC NOT ZERO                       
*                                                                               
         DROP  R4                                                               
*                                                                               
CFIL0900 EQU   *                                                                
         SR    R0,R0               SET CC ZERO:  OKAY                           
         B     CFIL0990            EXIT CC ZERO                                 
CFIL0910 EQU   *                                                                
         LTR   RB,RB               EXIT CC NOT ZERO: ERROR                      
CFIL0990 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        EXTRDISP:  DISPLAY EXTRACT INFORMATION FOR PROBLEM TRACKING.           
*                                                                               
EXTRDISP NTR1                                                                   
         B     EXTR0500            SKIP ROUTINE                                 
*                                                                               
         LA    R2,RCVDATA          SET A(DATA RECORD)                           
         USING RBUYREC,R2                                                       
         CLI   DMCB+3,1            DISPLAY INDICATOR = RECORD FOUND?            
         BNE   EXTR0020            NO                                           
         MVC   P+1(14),=C'BUY RECORD =  '                                       
         B     EXTR0120                                                         
EXTR0020 EQU   *                                                                
         CLI   DMCB+3,2            DISPLAY INDICATOR = RECORD SORTED?           
         BNE   EXTR0040            NO                                           
         MVC   P+1(14),=C'BUY TO SORT   '                                       
         B     EXTR0400                                                         
EXTR0040 EQU   *                                                                
         CLI   DMCB+3,3            DISPLAY INDICATOR = RECORD SORTED?           
         BNE   EXTR0060            NO                                           
         MVC   P+1(14),=C'INPUT RECORD  '                                       
         B     EXTR0120                                                         
EXTR0060 EQU   *                                                                
         DC    H'0'                                                             
EXTR0120 EQU   *                                                                
         ZAP   WORK(5),=P'99999999'                                             
         PACK  WORK+10(1),RBUYKCON+3(1)     INVERT THE SEQUENCE                 
         PACK  WORK+11(1),RBUYKCON+2(1)     OF THE DIGITS                       
         PACK  WORK+12(1),RBUYKCON+1(1)                                         
         PACK  WORK+13(1),RBUYKCON+0(1)     MOVE IN SIGN AND                    
         MVI   WORK+14,X'0C'                SHIFT ONE DECIMAL PLACE             
         SRP   WORK+10(5),64-1,0            TO RIGHT                            
         SP    WORK(5),WORK+10(5)           BEFORE SUBTRACTING FROM             
         SRP   WORK(5),1,0                  NINES AND SHIFTING 1 TO             
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),WORK(4)                                                 
         EDIT  (P5,DUB+3),(8,P+18)                                              
         EDIT  RBUYKMLN,(3,P+27)                                                
         MVI   P+30,C','                                                        
         EDIT  RBUYKLIN,(3,P+31)                                                
         MVC   P+35(2),RBUYKREP    INSERT REP OF RECORD                         
         MVI   P+37,C'/'                                                        
*                                                                               
         DROP  R2                                                               
*                                                                               
         L     R2,SAVEDM4          RELOAD A(X'08' ELEMENT)                      
         USING RBUYSPEL,R2                                                      
*                                                                               
         MVC   P+38(2),RBUYSPAG                                                 
         MVC   P+41(1),RBUYSPMD                                                 
         MVC   P+43(3),RBUYSPCL                                                 
         MVC   P+47(3),RBUYSPPD                                                 
         MVC   P+51(3),RBUYSPPP                                                 
         EDIT  RBUYSPES,(3,P+55)                                                
         MVC   P+59(5),RBUYSPST                                                 
         EDIT  RBUYSPL#,(3,P+63),ZERO=NOBLANK                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
EXTR0400 EQU   *                                                                
         GOTO1 REPORT                                                           
EXTR0500 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        GETRCV --- RETURN A BUY RECORD FROM THE RECOVERY FILE IN THE           
*                    RECOVERY RECORD AREA                                       
*                                                                               
GETRCV   NTR1                                                                   
*                                                                               
         CLI   DODISK,C'D'         RECOVERY FILE ON DISK?                       
         BE    GRCV1               YES                                          
*                                  NO  - FROM TAPE                              
         LA    RE,RCVREC           CLEAR THE RCVREC AREA                        
         LA    RF,RCVDATAX-RCVREC                                               
         A     RF,=F'4048'                                                      
*                                  EXTEND DOWN INTO SPACE USED FOR              
*                                     RECOVERY FILE FROM DISK                   
         XCEFL                                                                  
*                                                                               
         BAS   RE,GETTAPE                                                       
         BZ    GRCVGOOD                                                         
         B     GRCVBAD                                                          
*                                                                               
GRCV1    EQU   *                                                                
         OC    ARCVREC,ARCVREC            THIS IS ZERO ON FIRST CALL            
         BNZ   GRCV20                                                           
*                                                                               
GRCV10   EQU   *                                                                
         LA    RE,RCVBLK                                                        
         LR    RF,RE                                                            
         LA    RF,6(RF)                                                         
         ST    RF,ARCVREC                                                       
         L     RF,=A(RCVBLKX-RCVBLK)                                            
         XCEFL                                                                  
         GET   RCVIN,RCVBLK                                                     
         L     RF,RCVBLKS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBLKS                                                       
         L     R2,ARCVREC                                                       
         B     GRCV30                                                           
*                                                                               
GRCV20   EQU   *                                                                
         L     R2,ARCVREC                                                       
         ZICM  R3,0(R2),2                 GET RECORD LENGTH                     
         AR    R2,R3                      POINT TO THE NEW RECORD               
         OC    0(2,R2),0(R2)              ANYTHING THERE?                       
         BZ    GRCV10                     NOPE, GET NEXT BLOCK                  
         ST    R2,ARCVREC                                                       
*                                                                               
GRCV30   EQU   *                                                                
         L     RF,RCVRECS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVRECS                                                       
         CLI   RFILTY-RECVHDR+2(R2),X'82'    REP FILE?                          
         BNE   GRCV20                                                           
         CLI   RRECTY-RECVHDR+2(R2),X'01'    EXCLUDE 'COPY' RECORDS             
         BE    GRCV20                                                           
         L     RF,RCVFRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,RCVFRECS                                                      
         CLI   RCVDATA-RCVREC+2(R2),X'0B'    BUY RECORDS?                       
         BNE   GRCV20                        NO  - SKIP IT                      
         CLI   RCVDATA-RCVREC+3(R2),X'01'    YES - SHADOW BUY?                  
         BE    GRCV20                        YES - SKIP IT                      
         CLC   QUESTOR(2),SPACES             SPECIFIC SUBSIDIARY?               
         BNH   GRCV40                        NO  - DON'T FILTER                 
         CLC   RCVDATA-RCVREC+71(2,R2),QUESTOR                                  
*                                  YES - BUY FOR THIS REP?                      
         BNE   GRCV20                        NO  - SKIP IT                      
GRCV40   EQU   *                                                                
         L     RF,RCVBUYS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBUYS                                                       
*                                                                               
         ZICM  R1,0(R2),2                                                       
         LA    RE,2(R2)                                                         
         LA    RF,RCVREC                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
GRCVGOOD EQU   *                                                                
         B     TESTGOOD                                                         
GRCVBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 3                                                                
RRCVEOD  EQU   *                                                                
         B     GRCVBAD                                                          
         EJECT                                                                  
*                                                                               
*        GETTAPE --- GET A TAPE RECOVERY RECORD                                 
*                                                                               
GETTAPE  NTR1                                                                   
*                                                                               
GTAP10   EQU   *                                                                
         LA    RE,RCVREC                                                        
         LA    RF,RCVDATAX-RCVREC                                               
         A     RF,=F'4048'                                                      
*                                  EXTEND DOWN INTO SPACE USED FOR              
*                                     RECOVERY FILE FROM DISK                   
         XCEFL                                                                  
*                                                                               
GTAP20   EQU   *                                                                
         GET   RCVTAPE,RCVRECT                                                  
*                                                                               
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    GTAP10                                                           
         TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    GTAP10                                                           
*                                                                               
         L     RF,RCVBLKS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBLKS                                                       
         L     RF,RCVRECS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVRECS                                                       
*                                                                               
         CLI   RFILTY,X'82'                  REP FILE?                          
         BNE   GTAP10                                                           
         CLI   RRECTY,X'01'                  EXCLUDE 'COPY' RECORDS             
         BE    GTAP10                                                           
         L     RF,RCVFRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,RCVFRECS                                                      
         CLI   RCVDATA,X'0B'                 BUY RECORDS?                       
         BNE   GTAP10                        NO                                 
         CLI   RCVDATA+1,X'01'               SHADOW BUY?                        
         BE    GTAP10                        YES - SKIP IT                      
*                                                                               
         CLC   QUESTOR(2),SPACES             SPECIFIC SUBSIDIARY?               
         BNH   GTAP30                        NO  - DON'T FILTER                 
         CLC   RCVDATA+16(2),QUESTOR                                            
*                                  YES - BUY FOR THIS REP?                      
         BNE   GTAP10                        NO  - SKIP IT                      
GTAP30   EQU   *                                                                
*                                                                               
         L     RF,RCVBUYS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBUYS                                                       
*                                                                               
*        GETTAPE EXIT                                                           
*                                                                               
GTAPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
GTAPBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 3                                                                
RTAPEEOD EQU   *                                                                
         B     GTAPBAD                                                          
*                                                                               
*   TAPE END-OF-DATA:  SINGLE PASS BRINGS IN ONE OR MULTI TAPES                 
*                                                                               
*        CLI   QOPT4,C'M'          MULTI-INPUT SET?                             
*        BNE   GTAPBAD             NO  - GET OUT                                
*        ZIC   RF,TAPESCTR         YES - CHECK COUNTER                          
*        LTR   RF,RF               ANY TAPES LEFT?                              
*        BZ    GTAPBAD             NO  - FINISHED                               
*        BCTR  RF,0                YES - SUBTRACT 1 FROM COUNTER                
*        STC   RF,TAPESCTR         STORE IT BACK                                
*        MVC   P(30),=C'TAPE FINISHED:  ACQUIRING NEXT'                         
*        GOTO1 REPORT                                                           
*        CLOSE (RCVTAPE)           CLOSE CURRENT TAPE                           
*        OPEN  (RCVTAPE,(INPUT))   OPEN NEXT TAPE                               
*        B     GTAP20                                                           
         EJECT                                                                  
*                                                                               
*        CKAGY --- CHECK THIS REP'S AGENCY TO SEE IF THEY ARE ON THE            
*                   CURRENT SPOTPAK FILE                                        
*                                                                               
*         P1   =   A(REP'S AGY CODE)                                            
*                                                                               
*                                                                               
*        ROUTINE SETS CURAGY TO  A(AGYOK) TABLE ENTRY                           
*                                                                               
CKAGY    NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         XC    CURAGY,CURAGY                                                    
*                                                                               
         LA    R3,AGYOK                IS IT ON THE 'OK' LIST?                  
CAGY10   EQU   *                                                                
         ZICM  R0,0(R3),2                                                       
         BZ    CAGY100                                                          
         CLC   0(2,R2),0(R3)                                                    
         BNE   CAGY20                                                           
         ST    R3,CURAGY                                                        
         B     CAGYGOOD                                                         
CAGY20   EQU   *                                                                
         LA    R3,ATABEND-ATABCODE(R3)                                          
         B     CAGY10                                                           
*                                                                               
CAGY100  EQU   *                                                                
         LA    R3,AGYNOTOK             IS IT ON THE 'NOT OK' LIST               
CAGY110  EQU   *                                                                
         ZICM  R0,0(R3),2                                                       
         BZ    CAGY200                                                          
         CLC   0(2,R2),0(R3)                                                    
         BE    CAGYBAD                                                          
         LA    R3,2(R3)                                                         
         B     CAGY110                                                          
*                                                                               
CAGY200  EQU   *                       NOT ON ANY LIST, READ CTFILE             
         LA    R5,IOAREA1                                                       
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'5'              SYSTEM ACCESS RECORD                     
         MVC   23(2,R5),0(R2)                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5),0                   
         CLI   DMCB+8,0                                                         
         BE    CAGY210                                                          
         MVC   P(6),=C'AGENCY'                                                  
         MVC   P+8(2),0(R2)                                                     
         GOTO1 REPORT                                                           
         MVC   P(L'NOSYSID),NOSYSID    NO ID RECORD ERROR                       
         GOTO1 REPORT                                                           
         B     CAGYNOOK                                                         
CAGY210  EQU   *                                                                
         LA    R5,28(R5)               POINT TO THE START OF REC                
CAGY250  EQU   *                                                                
         CLI   0(R5),0                                                          
         BNE   CAGY260                                                          
         MVC   P(6),=C'AGENCY'                                                  
         MVC   P+8(2),0(R2)                                                     
         GOTO1 REPORT                                                           
         MVC   P(L'NOSPOT),NOSPOT      NOT CLEARED FOR SOT                      
         GOTO1 REPORT                                                           
         B     CAGYNOOK                                                         
CAGY260  EQU   *                                                                
         CLI   0(R5),X'21'             SYSTEM ELEMENT?                          
         BNE   CAGY270                                                          
         CLI   2(R5),X'02'             SPOT?                                    
         BE    CAGY300                                                          
CAGY270  EQU   *                                                                
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     CAGY250                                                          
CAGY300  EQU   *                                                                
         CLC   SPOTSYS,3(R5)                                                    
         BE    CAGYOK                 IT'S GOOD, ADD IT TO THE LIST             
*                                                                               
CAGYNOOK EQU   *                      ADD AGY CODE TO NOT OK LIST               
         LA    R3,AGYNOTOK                                                      
CAGYN10  EQU   *                                                                
         ZICM  RF,0(R3),2                                                       
         BZ    CAGYN20                                                          
         LA    R3,2(R3)                                                         
         B     CAGYN10                                                          
CAGYN20  EQU   *                                                                
         MVC   0(2,R3),0(R2)                                                    
         B     CAGYBAD                                                          
*                                                                               
CAGYOK   EQU   *                      ADD AGY CODE TO OK LIST                   
         LA    R3,AGYOK                                                         
CAGYO10  EQU   *                                                                
         ZICM  RF,0(R3),2                                                       
         BZ    CAGYO20                                                          
         LA    R3,ATABEND-ATABCODE(R3)                                          
         B     CAGYO10                                                          
CAGYO20  EQU   *                                                                
         MVC   0(2,R3),0(R2)                                                    
         ST    R3,CURAGY                                                        
*                                                                               
*        CKAGY EXIT                                                             
*                                                                               
CAGYGOOD EQU   *                                                                
         B     TESTGOOD                                                         
CAGYBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        CKCON --- PROCESS THIS BUY RECORD AGAINST THE TABLE WITH               
*                   REP CODES AND STARTING CONTRACT NUMBERS                     
*                                                                               
CKCON    NTR1                                                                   
*                                                                               
         LA    R6,REPTAB                                                        
         LA    R5,RCVDATA                                                       
         USING RBUYREC,R5                                                       
*                                                                               
CCON10   EQU   *                                                                
         OC    0(10,R6),0(R6)                                                   
         BZ    CCON100                                                          
         CLC   0(2,R6),RBUYKREP                                                 
         BE    CCON200                                                          
         LA    R6,10(R6)                                                        
         B     CCON10                                                           
*                                                                               
CCON100  EQU   *                                                                
         GOTO1 CKREP,DMCB,(R6)                                                  
         BNZ   CCONBAD                                                          
*                                                                               
CCON200  EQU   *                                                                
         OC    2(4,R6),2(R6)                STARTING NUM IN REP REC?            
         BZ    CCONGOOD                     NOPE                                
         ZAP   WORK(5),=P'99999999'                                             
         PACK  WORK+10(1),RBUYKCON+3(1)     INVERT THE SEQUENCE                 
         PACK  WORK+11(1),RBUYKCON+2(1)     OF THE DIGITS                       
         PACK  WORK+12(1),RBUYKCON+1(1)                                         
         PACK  WORK+13(1),RBUYKCON+0(1)     MOVE IN SIGN AND                    
         MVI   WORK+14,X'0C'                SHIFT ONE DECIMAL PLACE             
         SRP   WORK+10(5),64-1,0            TO RIGHT                            
         SP    WORK(5),WORK+10(5)           BEFORE SUBTRACTING FROM             
         SRP   WORK(5),1,0                  NINES AND SHIFTING 1 TO             
         CLC   WORK(4),2(R6)                                                    
         BL    CCONBAD                                                          
         DROP  R5                                                               
*                                                                               
*        CKCON EXIT                                                             
*                                                                               
CCONGOOD EQU   *                                                                
         B     TESTGOOD                                                         
CCONBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        CKREP --- GET THE REP RECORD AND THE CONTRACT NUMBER START             
*                                                                               
*        P1    =   A(ENTRY TO LOAD IN THE TABLE)                                
*                                                                               
CKREP    NTR1                                                                   
*                                                                               
         L     R6,0(R1)                                                         
*                                                                               
         LA    R5,RCVDATA                                                       
         USING RBUYREC,R5                                                       
*                                                                               
         XC    REPKEY,REPKEY                                                    
         MVI   REPKEY,X'01'                                                     
         MVC   REPKEY+25(2),RBUYKREP                                            
         BAS   RE,REPHIGH                                                       
         CLC   REPKEY(27),REPKSAVE                                              
         BNE   CREPBAD                                                          
         BAS   RE,REPGET                                                        
*                                                                               
         L     R4,AREC       *** FIND STANDARD S-E DAY TIME FOR REP ***         
         USING RREPREC,R4                                                       
         MVC   6(2,R6),=H'500'     DEFAULT S-E TIME DAY = 5A-459A               
         MVC   8(2,R6),=H'459'                                                  
         CLI   RREPPROF+4,C'Y'     REP PROFILE #5=Y??                           
         BNE   *+16                                                             
         MVC   6(2,R6),=H'600'     OPTIONAL S-E TIME DAY = 6A-559A              
         MVC   8(2,R6),=H'559'                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFILE '),(X'05',AREC),        X        
               RR=RELO                                                          
         CLI   DM4,0                                                            
         BNE   CREPBAD                                                          
         L     R4,DM4                                                           
         USING RREPSPOT,R4                                                      
         MVC   0(2,R6),RBUYKREP                                                 
         MVC   2(4,R6),RREPSPC#                                                 
         DROP  R4,R5                                                            
*                                                                               
*        CKCON EXIT                                                             
*                                                                               
CREPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
CREPBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        DOREPORT --- PRODUCE EXTRACT CONTROL REPORT                            
*                                                                               
DOREPORT NTR1                                                                   
*                                                                               
         EDIT  (4,RCVBLKS),(10,P),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+11(25),=C'RECOVERY BLOCKS WERE READ'                           
         GOTO1 REPORT                                                           
         EDIT  (4,RCVRECS),(10,P),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+11(26),=C'RECOVERY RECORDS WERE READ'                          
         GOTO1 REPORT                                                           
         EDIT  (4,RCVFRECS),(10,P),COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+11(34),=C'REPFILE RECOVERY RECORDS WERE READ'                  
         GOTO1 REPORT                                                           
         EDIT  (4,RCVBUYS),(10,P),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+11(21),=C'BUY RECORDS WERE READ'                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         EDIT  (4,SORTRECS),(10,P),COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+11(29),=C'RECORDS WERE SENT TO THE SORT'                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,AGYOK                                                         
         USING AGYTABD,R2                                                       
         SR    R4,R4                COUNT UP SORT RECS                          
         SR    R5,R5                COUNT UP SPOTS                              
         SR    R6,R6                COUNT UP DOLLARS                            
DREP10   EQU   *                                                                
         ZICM  RF,0(R2),2                                                       
         BZ    DREP100                                                          
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         BAS   RE,SETMSG                                                        
         MVC   PAGYCODE(2),ATABCODE                                             
         GOTO1 GETMYAGY,DMCB,ATABCODE,PAGYNAME                                  
         EDIT  (4,ATABRECS),(6,PNUMRECS),COMMAS=YES,ZERO=NOBLANK                
         EDIT  (4,ATABSPOT),(6,PSPOTS),COMMAS=YES,ZERO=NOBLANK                  
         EDIT  (4,ATABDOLS),(9,PDOLS),COMMAS=YES,ZERO=NOBLANK                   
         A     R4,ATABRECS                                                      
         A     R5,ATABSPOT                                                      
         A     R6,ATABDOLS                                                      
         DROP  R2,R3                                                            
         GOTO1 REPORT                                                           
         LA    R2,ATABEND-ATABCODE(R2)                                          
         B     DREP10                                                           
*                                                                               
DREP100  EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         BAS   RE,SETMSG                                                        
         MVC   PAGYCODE(36),=CL36'* * * RUN TOTALS * * *'                       
         EDIT  (R4),(6,PNUMRECS),COMMAS=YES,ZERO=NOBLANK                        
         EDIT  (R5),(6,PSPOTS),COMMAS=YES,ZERO=NOBLANK                          
         EDIT  (R6),(9,PDOLS),COMMAS=YES,ZERO=NOBLANK                           
         DROP  R3                                                               
         GOTO1 REPORT                                                           
*                                                                               
*        DOREPORT EXIT                                                          
*                                                                               
DREPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
DREPBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        SETMSG --- PUT TEXTS INTO THE PRINT LINE                               
*                                                                               
SETMSG   NTR1                                                                   
*                                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVC   PMSG1(L'PMSG1),=C'AGENCY'                                        
         MVI   PMSG2,C'-'                                                       
         MVC   PMSG3(L'PMSG3),=C'HAD'                                           
         MVC   PMSG4(L'PMSG4),=C'SORT RECORDS, THAT REPRESENTS'                 
         MVC   PMSG5(L'PMSG5),=C'SPOTS AND'                                     
         MVC   PMSG6(L'PMSG6),=C'DOLLARS'                                       
         DROP  R2                                                               
*                                                                               
SMSGGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        GETMYAGY --- GET THE AGENCY NAME                                       
*                                                                               
*        P1     =   A(AGENCY CODE)                                              
*        P2     =   A(AGENCY NAME)                                              
*                                                                               
GETMYAGY NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYHDR,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GAGYBAD                                                          
         GOTO1 GET                                                              
         L     R4,AREC                                                          
         MVC   0(20,R3),AGYNAME                                                 
         DROP  R4                                                               
*                                                                               
GAGYGOOD EQU   *                                                                
         B     TESTGOOD                                                         
GAGYBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        REPPAK COMMUNICATION WITH DATA MANAGER (DIRECTORY)                     
*                                                                               
         SPACE 3                                                                
REPREAD  MVC   COMMAND,DMREAD                                                   
         MVC   FILE,REPDIR                                                      
         B     REPDIRL                                                          
         SPACE 2                                                                
REPSEQ   MVC   COMMAND,DMRSEQ                                                   
         MVC   FILE,REPDIR                                                      
         B     REPDIRL                                                          
         SPACE 2                                                                
REPHIGH  MVC   COMMAND,DMRDHI                                                   
         MVC   FILE,REPDIR                                                      
         MVC   REPKSAVE,REPKEY                                                  
         B     REPDIRL                                                          
         SPACE 2                                                                
REPDIRL  NTR1                                                                   
         L     R3,UTL                                                           
         MVC   4(1,R3),REPSYS                                                   
         IC    R4,DMINBTS                                                       
         MVC   REPKSAVE,REPKEY                                                  
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,REPKSAVE,REPKEY                 
         MVC   4(1,R3),SPOTSYS                                                  
         B     DMCHECK                                                          
         SPACE 3                                                                
*                                                                               
*        REPPAK COMMUNICATION WITH DATA MANAGER (FILE)                          
*                                                                               
         SPACE 3                                                                
REPGET   MVC   COMMAND,GETREC                                                   
         MVC   FILE,REPFIL                                                      
         L     R3,UTL                                                           
         MVC   4(1,R3),REPSYS                                                   
         LA    R2,REPKEY+28                                                     
REPFILL  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,(R2),AREC,DMWORK,0              
         MVC   4(1,R3),SPOTSYS                                                  
         SPACE 3                                                                
DMCHECK  EQU   *                                                                
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         MVI   DMOUTBTS,X'FF'                                                   
         BNZ   DMERRS                                                           
         B     TESTGOOD                                                         
DMERRS   EQU   *                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
TESTBAD  EQU   *                                                                
         LA    R0,1                                                             
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
*        ERROR TEXTS                                                            
*                                                                               
NOSYSID  DC    C'THIS AGENCY CAN NOT BE FOUND IN THE CONTROL SYSTEM.'           
NOSPOT   DC    C'THIS AGENCY HAS NOT BEEN CLEARED FOR SPOTPAK.'                 
TAPEMSG  DC    CL60'REP TO SPOT (SR1) - ANY MORE TAPES?'                        
         EJECT                                                                  
*                                                                               
*        DCB'S                                                                  
*                                                                               
RCVIN    DCB   DDNAME=RCVIN,DSORG=PS,RECFM=U,                          X        
               BLKSIZE=9500,MACRF=GM,EODAD=RRCVEOD                              
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=V,LRECL=8200,             X        
               MACRF=GM,EODAD=RTAPEEOD                                          
*                                                                               
RCVOUT   DCB   DDNAME=RCVOUT,DSORG=PS,RECFM=FB,LRECL=1050,             X        
               BLKSIZE=1050,MACRF=PM                                            
         EJECT                                                                  
*                                                                               
*        LITERAL POOL                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SORT RECORDS, ET AL                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTREC  DS    0CL19                                                            
SORTKEY  DS    0CL19                                                            
SORTKREP DS    CL2   +00            REPPAK REP  CODE                            
SORTKCON DS    CL8   +02            REPPAK CONTRACT NUMBER                      
SORTKLIN DS    CL2   +10            REPPAK BUYLINE LINE #                       
SORTKDAT DS    CL3   +12                                                        
SORTKTIM DS    CL4   +15                                                        
SORTRECX EQU   *                                                                
         DS    0D                                                               
         DC    CL8'SORTCARD'                                                    
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=19'                                    
*                                                                               
*                                                                               
*        AGENCY TABLES                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'*AGYTAB*'                                                    
AGYTAB   EQU   *                                                                
AGYOK    DS    15CL16       AGENCIES THAT ARE ON CURRENT SPOT FILE              
         DS    CL2                                                              
AGYNOTOK DS    CL50         AGENCIES THAT ARE NOT ON CURRENT SPOT FILE          
         DS    CL2                                                              
AGYTABX  EQU   *                                                                
AGYTABL  EQU   AGYTABX-AGYTAB                                                   
*                                                                               
*                                                                               
*        IO AREA                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*IOAREA*'                                                    
IOAREA1  DS    2008C                                                            
IOAREA1X EQU   *                                                                
IOAREA1L EQU   IOAREA1X-IOAREA1                                                 
*                                                                               
*        LOCAL VARIABLES                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*LOCAL**'                                                    
CURAGY   DS    A            A(CURRENT AGY IN AGYTAB)                            
SORTRECS DS    F            NUMBER OF RECORDS SENT TO THE SORT                  
RCVBLKS  DS    F            NUMBER OF RCV BLOCKS READ                           
RCVRECS  DS    F            NUMBER OF RCV RECORDS READ                          
RCVFRECS DS    F            NUMBER OF RCV RECORDS READ FROM REPFILE             
RCVBUYS  DS    F            NUMBER OF RCV BUY RECORDS READ                      
ARCVREC  DS    F            A(CURRENT RCV RECORD IN BLOCK)                      
BBUCKETS DS    CL600        REP BUY RECORD BUCKET OUTPUT                        
RBLOCK   DS    CL16         REGENBUC ADDR BLOCK                                 
RUNERROR DS    CL60                                                             
CONSMSG  DS    CL60                                                             
DODISK   DS    X            IF NOT ZERO, TAKE INPUT FROM 9500 DISK BLK          
ELCODE   DS    X                                                                
SPOTSYS  DS    X            SPOT SYS NUMBER (TO AND FROM UTL)                   
REPSYS   DS    X            REP SYS NUMBER                                      
REPDIR   DC    CL8'REPDIR  '                                                    
REPFIL   DC    CL8'REPFIL  '                                                    
REPKEY   DS    CL32                                                             
REPKSAVE DS    CL32                                                             
REPTAB   DS    16CL10       REP CODE + STARTING CON NUMBER + STD DAY            
REPFLIST DC    CL8' REPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
TAPESCTR DC    XL1'00'             TAPE COUNTER IF MULTI-TAPE INPUT             
SAVEDM4  DS    F                   A(X'08' ELEMMENT)                            
*                                                                               
*        RECOVERY FILE RECORD AREA                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'*RCVREC*'                                                    
RCVRECT  DS    CL4          TAPE INPUT GOES HERE                                
RCVREC   EQU   *                                                                
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RCVDATA  EQU   *                                                                
         DS    2024C        WILL HOLD ONLY A REP SYSTEM RECORD                  
*                                     EXPANDED RECORD SIZES WILL BE             
*                                     HANDLED BY USING RCVBLK AREA              
RCVDATAX EQU   *                                                                
*                                                                               
*        RECOVERY FILE BLOCK AREA                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*RCVBLK*'                                                    
RCVBLK   EQU   *                                                                
         DS    9500C                                                            
RCVBLKX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* DSECT FOR AGENCY (OK) TABLE                                                   
*                                                                               
AGYTABD  DSECT                                                                  
ATABCODE DS    CL2                                                              
         DS    CL2                                                              
ATABRECS DS    CL4                                                              
ATABSPOT DS    CL4                                                              
ATABDOLS DS    CL4                                                              
ATABEND  DS    0CL1                                                             
         EJECT                                                                  
*                                                                               
* DSECT FOR PRINT LINE                                                          
*                                                                               
PLINED   DSECT                                                                  
PMSG1    DS    CL6         'AGENCY'                                             
         DS    CL1                                                              
PAGYCODE DS    CL2                                                              
PMSG2    DS    CL1         '-'                                                  
PAGYNAME DS    CL33                                                             
         DS    CL1                                                              
PMSG3    DS    CL3         'HAD'                                                
         DS    CL1                                                              
PNUMRECS DS    CL6                                                              
         DS    CL1                                                              
PMSG4    DS    CL29        'SORT RECORDS, THAT REPRESENTS'                      
         DS    CL1                                                              
PSPOTS   DS    CL6                                                              
         DS    CL1                                                              
PMSG5    DS    CL9         'SPOTS AND'                                          
         DS    CL1                                                              
PDOLS    DS    CL9                                                              
         DS    CL1                                                              
PMSG6    DS    CL7         'DOLLARS'                                            
*                                                                               
         EJECT                                                                  
RBUYRECD DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
RREPRECD DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
SPAGYD   DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'128SPREPR109 11/09/11'                                      
         END                                                                    
