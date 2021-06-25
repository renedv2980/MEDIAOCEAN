*          DATA SET RECNT5A    AT LEVEL 062 AS OF 03/02/11                      
*          DATA SET RECNT5A    AT LEVEL 051 AS OF 07/12/10                      
*PHASE T8025AA,+0                                                               
         TITLE 'T8025A - REP MON EDIT (UPLOAD VERSION)'                         
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT5A (T8025A) --- MON EDIT (UPLOAD VERSION)           *             
*                                                                 *             
* NOTE: USED RECNT52 AS TEMPLATE. IF RECNT52 CHANGES, WE MAY NEED *             
*       TO PROPAGATE SAME CHANGES TO THIS MODULE.                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 30JUL10 (SKU) --- ORIGINAL ROLLOUT                              *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8025A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T8025A,R9                                        
         LR    R8,RC                                                            
         USING MYWORKD,R8                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
MON      DS    0H                                                               
         MVI   CFLAG,0                                                          
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         BAS   RE,BLDBUCK          BUILD TOTAL BUCKETS                          
         BAS   RE,VALBUCK          GO VALIDATE BUCKETS                          
*                                                                               
         BAS   RE,ADJBUCK          GO ADJUST BUCKETS IF FLT SHORTENED           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,60     ADD/DISP MON DATA                            
         CLI   CFLAG,1             CHANGED?                                     
         BNE   MONX                                                             
         BAS   RE,UPDVER           UPDATE VERSION NUMBER AND UNCONFIRM          
         GOTO1 VDISMSG,DMCB,61     MON DATA CHANGED                             
         OI    RCONMODR+1,X'20'    SET MONTHLY DONE FLAG ON CON                 
         BAS   RE,TRUDATE          UPDATE TRUE ACTIVITY DATE                    
         BAS   RE,SARCOMS          PENDING -> SPL COMMENTS                      
         BAS   RE,ALTCAL           SET ALTERNATE CALENDAR BUCKETS               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
MONX     DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*******************************************************************             
*                                                                 *             
* IO3 IS THE STARTING BUCKET TOTAL   (BUILT IN DISBUCK)           *             
*    AREA+0(2)=YYMM                                               *             
*    AREA+2(4)=DOLLARS                                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
* BUILD TOTAL BUCKETS                                                           
BLDBUCK  NTR1                                                                   
         L     R4,AIO3                                                          
***>>>   LA    RF,L'IO3                                                         
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R4),(RF)                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
                                                                                
* IF BACK BILLING, CONVERT TO INVOICE BUCKETS                                   
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   *+8                                                              
         MVI   ELCODE,X'04'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   BBX                 NO BUCKETS YET                               
BB4      MVC   0(2,R4),2(R6)       SAVE BUCK YYMM                               
BB5      L     R1,6(R6)            R1=BUCK AMT                                  
         A     R1,2(R4)            ADD RUNNING TOTAL                            
         ST    R1,2(R4)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   BBX                                                              
         CLC   0(2,R4),2(R6)       SAME BDCST MON?                              
         BE    BB5                                                              
         LA    R4,6(R4)            NEXT TOTAL BUCKET                            
         B     BB4                                                              
BBX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
VALBUCK  NTR1                                                                   
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
* WORK HAS EBCDIC START AND WORK+6 HAS END DATE                                 
         GOTO1 VGTBROAD,DMCB,(1,WORK),TEMP,GETDAY,ADDAY                         
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),TEMP+12,GETDAY,ADDAY                    
*                                                                               
* TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                        
* TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,TEMP+6),(X'20',BCSTRTDT)                          
         GOTO1 DATCON,DMCB,(0,TEMP+18),(X'20',BCENDDT)                          
*                                                                               
*                                                                               
* IF CONTRACT WAS CREATED VIA REPPAK CONTRACT UPLOAD, PACING WILL BE            
* DAILY                                                                         
*                                                                               
         NI    MISCFLAG,X'FF'-MFRCU                                             
         LA    R6,RCONREC          CHECK IF RCU CONTRACT                        
         USING RCONRFEL,R6                                                      
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    MISCFLAG,MFRCU      FLAG RCU CONTRACT                            
*                                                                               
         LA    R4,MONTBL                                                        
         LA    R2,MO$MON1H         FIRST FLD HDR                                
         MVC   ROW#,=C'01'                                                      
VB10     DS    0H                  SET R4 TO FIRST CORRESPOND                   
         CLC   1(2,R4),10(R2)       W/FIRST MON ON SCREEN                       
         BE    VB13                                                             
         LA    R4,L'MONTBL(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   VB10                                                             
*                                                                               
         LA    R3,1017             B'CAST MONTH REQUIRED/INVALID                
         B     VBERROR                                                          
*                                                                               
VB13     DS    0H                                                               
         LR    RF,R2                                                            
         LA    RE,MO$ENDH                                                       
VB15     DS    0H                                                               
         CR    RF,RE                                                            
         BNL   VB20                                                             
         AHI   RF,MO$MLENQ                                                      
         CLC   8(4,R2),8(RF)       DUPLICATE BUCKET MONTH? ERROR!               
         BNE   VB15                                                             
         LA    R3,1016             DUPLICATE BUCKET FOUND                       
         LR    R2,RF                                                            
         B     VBERROR                                                          
*                                                                               
VB20     DS    0H                                                               
         L     R5,AIO3             R5 TO TOTAL BUCKETS                          
         LA    R3,1013             BUCKET MUST OVERLAP FLIGHT                   
         CLC   8(4,R2),BCSTRTDT    CHECK IF WITHIN FLIGHT B'CAST MONTHS         
         BL    VBERROR                                                          
         CLC   8(4,R2),BCENDDT                                                  
         BH    VBERROR                                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),8(R2)                                                    
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,BINDATE)                                 
         MVC   BYEAR,BINDATE                                                    
*                                                                               
         PACK  DUB,10(2,R2)                                                     
         CVB   R1,DUB                                                           
         STC   R1,BMONTH                                                        
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R3,1018             ACTIVITY DATE REQUIRED/INVALID               
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    VBERROR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,8(R2)),(2,ACTDATE)                                
         ST    R2,ACTFLD                                                        
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO MONEY FLD                              
*                                                                               
         LA    R3,1019             BOOKED DOLLARS REQUIRED/INVALID              
         CLI   5(R2),0                                                          
         BE    VBERROR                                                          
*                                                                               
* CK IF THIS BRDCST MON HAS A TOTAL BUCKET                                      
VB25     CLC   0(1,R4),1(R5)       SAME BRDCAST MON?  (BIN)                     
         BE    VB30                YES - SEE IF ANY BUCKET CHANGE               
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         CLI   0(R5),0                                                          
         BNE   VB25                                                             
*                                                                               
* IF NO PREVIOUS VALUE, THEN SEE IF ONE NOW                                     
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VB100               NO, CK NEXT FLD                              
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    VBERROR                                                          
         CLI   5(R2),9                                                          
         BNE   VBERROR                                                          
         GOTO1 VPACK               AMOUNT IN PENNIES                            
         ST    R0,BUCKET$                                                       
         B     VB50                                                             
*                                                                               
* NOW SEE IF VALUE ON SCREEN IS SAME AS IN TOTAL BUCKET                         
VB30     DS    0H                                                               
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    VBERROR                                                          
         CLI   5(R2),9                                                          
         BNE   VBERROR                                                          
         GOTO1 VPACK               AMOUNT IN PENNIES                            
         ST    R0,BUCKET$                                                       
*                                                                               
         CLC   BUCKET$,2(R5)                                                    
         BNE   VB110               BUILD NEW BUCKET                             
         CLC   =C'ACC-BB',CONBUY   NEED TO CHANGE EXISTING                      
         BE    VB110               ACTIVITY DATE FOR BACK BILLING               
*        LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     VB100                                                            
*                                                                               
* ADD NEW 03 (NO EXISTING 03'S FOR THIS MONTH)                                  
* OR 04'S FOR BACK BILLING                                                      
*                                                                               
VB50     DS    0H                                                               
         MVI   CFLAG,1             SET CHANGED                                  
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'030A'                                                 
                                                                                
* IF BACK BILLING, CONVERT TO INVOICE BUCKETS                                   
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   *+8                                                              
         MVI   WORK,X'04'                                                       
*                                                                               
         MVC   WORK+2(1),BYEAR     BROADCAST YY                                 
         MVC   WORK+3(1),BMONTH                                                 
         MVC   WORK+4(2),ACTDATE   ACTIVITY MONDATE                             
*        MVC   WORK+6(4),DMCB+4                                                 
         MVC   WORK+6(4),BUCKET$                                                
*                                                                               
VB60     DS    0H                                                               
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   VB90                                                             
*                                                                               
* FOR BACK BILLING, CHANGE ACTIVITY DATE TO THE QUARTER PREVIOUS TO             
* THE BUCKET MONTH                                                              
*                                                                               
         BAS   RE,BILLING                                                       
*                                                                               
VB90     DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                     
*                                                                               
* LOOP BACK FOR NEXT MONTH                                                      
VB100    DS    0H                                                               
         OI    4(R2),X'20'         SET PREVALID                                 
VB103    ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO NEXT BUCKET DATE FLD                   
         CLI   0(R2),0             LAST FIELD?                                  
         BE    VBX                                                              
         CLI   8(R2),0             IS FIELD IN USE?                             
         BNE   VB105               YES, KEEP GOING                              
*                                                                               
         LR    RF,R2                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0               RF TO ACTIVITY FLD                           
         CLI   8(RF),0             IS FIELD IN USE?                             
         BNE   VB105               YES, KEEP GOING                              
         ZIC   R0,0(RF)                                                         
         AR    RF,R0               R2 TO BUCKET FLD                             
         CLI   8(RF),0             IS FIELD IN USE?                             
         BNE   VB105               YES, KEEP GOING                              
         LR    R2,RF                                                            
         B     VB103                                                            
*                                                                               
VB105    DS    0H                                                               
*        LA    R4,L'MONTBL(R4)     NEXT TBL ENTRY                               
*        CLI   0(R4),X'FF'         LAST MONTH?                                  
*        BNE   VB20                                                             
         LA    R4,MONTBL           BACK TO FIRST MON                            
*                                                                               
         CLC   =C'09',ROW#                                                      
         BNE   VB108                                                            
         MVC   ROW#,=C'10'                                                      
         B     VB10                                                             
*                                                                               
VB108    DS    0H                                                               
         ZICM  RF,ROW#,2                                                        
         AHI   RF,1                                                             
         STCM  RF,3,ROW#                                                        
         B     VB10                                                             
*                                                                               
* FIRST SEE IF AN 03 EL EXISTS FOR THIS BDCST MON/ACTIVITY DATE,                
* IF NOT, JUST ADD NEW ELEMENT,                                                 
* ELSE CHANGE EXISTING EL FOR NEW TOTAL.                                        
VB110    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,3                                                         
                                                                                
* IF BACK BILLING, CONVERT TO INVOICE BUCKETS                                   
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   *+8                                                              
         MVI   ELCODE,X'04'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   VB140                                                            
*                                                                               
* IF BDCST MON FOUND, SEE IF ANY ACTIVITY THIS WEEK                             
VB120    DS    0H                                                               
         CLC   2(2,R6),0(R5)       MUST BE RIGHT BDCST MON                      
         BL    VB130                                                            
         BH    VB140               NO ACTIVITY THIS DATE FOR BDCST MON          
*                                                                               
         CLC   =C'ACC-BB',CONBUY   BACK BILLING, BACK UP                        
         BNE   VB125               ACTIVITY DATE                                
         MVC   WORK(10),0(R6)                                                   
         MVC   WORK+10(24),DMCB    SAVE OFF DMCB SINCE                          
         BAS   RE,BILLING          IT IS USED AFTER THIS BILLING CALL           
         MVC   4(2,R6),WORK+4                                                   
         MVC   DMCB(24),WORK+10                                                 
         B     VB150                                                            
*                                                                               
VB125    DS    0H                                                               
         LA    R3,1012                                                          
         CLC   4(2,R6),ACTDATE     HAVE RIGHT BDCST MON,ANY ACT THIS WK         
         BE    VB150               CHANGE EXISTING 03,DON'T ADD NEW ONE         
         BL    VB130                                                            
         L     R2,ACTFLD                                                        
         B     VBERROR             ERROR IF ACT DATE IS EARLIER                 
*                                                                               
VB130    BAS   RE,NEXTEL                                                        
         BE    VB120                                                            
*                                                                               
* NO ACTIVITY FOR THIS BROADCAST MONTH THIS DATE                                
VB140    L     R0,BUCKET$          $ IN FLD                                     
         S     R0,2(R5)                                                         
         ST    R0,BUCKET$                                                       
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     VB50                JUST ADD NEW 03                              
* CHANGE EXISTING 03 (OR 04)                                                    
VB150    DS    0H                                                               
         MVI   CFLAG,1             SET CHANGED                                  
         L     R1,2(R5)            CURRENT TOT BUCKET                           
         S     R1,6(R6)            SUBTRACT THIS WEEKS ACTIVITY                 
         L     R0,BUCKET$          NEW MONTH TOTAL                              
         SR    R0,R1                                                            
         ST    R0,6(R6)                                                         
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     VB100                                                            
*                                                                               
VBX      B     XIT                                                              
*                                                                               
VBERROR  DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         STCM  R3,3,DMCB+2                                                      
         GOTO1 GETTXT,DMCB,,,(C'E',0),(2,ROW#),(X'44',0)                        
         OI    6(R2),X'40'         PUT CURSOR HERE                              
         L     RD,BASERD           INSTANT STACK UNWIND                         
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
ADJBUCK  NTR1                                                                   
         OC    TWASVSDT,TWASVSDT                                                
         BZ    ADJBX                                                            
         CLC   RCONDATE,TWASVSDT   DID SCRIPT CHANGE FLIGHT DATES?              
         BE    ADJBX               NO EXIT                                      
         BAS   RE,BLDBUCK          BUILD TOTAL BUCKETS                          
         L     R5,AIO3             R5 TO TOTAL BUCKETS                          
         XC    WORK,WORK                                                        
*                                                                               
* GET BROADCAST YYMM OF CONTRACT START DATE PROVIDED IN SCRIPT                  
*                                                                               
         GOTO1 DATCON,DMCB,(3,TWASVSDT),(0,WORK)                                
         GOTO1 DATCON,DMCB,(3,TWASVEDT),(0,WORK+6)                              
         GOTO1 VGTBROAD,DMCB,(1,WORK),TEMP,GETDAY,ADDAY                         
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),TEMP+12,GETDAY,ADDAY                    
*                                                                               
* TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                        
* TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                         
*                                                                               
* IN EBCDIC                                                                     
         GOTO1 DATCON,DMCB,(0,TEMP+6),(X'20',BCSTRTDT)                          
         GOTO1 DATCON,DMCB,(0,TEMP+18),(X'20',BCENDDT)                          
* IN YMD BINARY                                                                 
         GOTO1 DATCON,DMCB,(0,TEMP+6),(3,BCSTDATE)                              
         GOTO1 DATCON,DMCB,(0,TEMP+18),(3,BCENDATE)                             
*                                                                               
ADJB10   DS    0H                                                               
         CLC   0(2,R5),BCSTDATE    BUCKET OUTSIDE OF NEW FLT?                   
         BL    ADJB50                                                           
         CLC   0(2,R5),BCENDATE    BUCKET OUTSIDE OF NEW FLT?                   
         BH    ADJB50                                                           
         B     ADJB100                                                          
*                                                                               
* CHECK IF BUCKET WAS PROVIDED ON SCREEN                                        
*                                                                               
ADJB50   DS    0H                                                               
         OC    2(4,R5),2(R5)       TOTAL IS $0 ALREADY? SKIP                    
         BZ    ADJB100                                                          
         LA    R2,MO$MON1H                                                      
*                                                                               
ADJB80   DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    ADJB90                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),8(R2)                                                    
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,BINDATE)                                 
         CLC   0(2,R5),BINDATE     FOUND ON SCREEN, NEXT BUCKET                 
         BE    ADJB100                                                          
*                                                                               
ADJB90   DS    0H                                                               
         LA    R2,MO$MON2H-MO$MON1H(R2)                                         
         LA    RE,MO$ENDH                                                       
         CR    R2,RE                                                            
         BNH   ADJB80                                                           
*                                                                               
* BUCKET NOT FOUND ON SCREEN AND FALLS OUTSIDE OF NEW FLIGHTS                   
* SET THIS BUCKET TO $0 TOTAL IN CONTRACT RECORD                                
*                                                                               
         MVI   CFLAG,1             SET CHANGED                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'030A'                                                 
         MVC   ELEM+2(2),0(R5)     BROADCAST YM                                 
*                                  ACTIVITY MONDATE                             
         GOTO1 DATCON,DMCB,(5,0),(2,ELEM+4)                                     
*                                                                               
* CHECK IF ACTIVITY TODAY. IF SO, DON'T POST EXTRA DELTA BUCKET                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(X'03',RCONREC),          X        
               (4,ELEM+2)                                                       
         CLI   DMCB+12,0                                                        
         BNE   ADJB95                                                           
         L     RF,DMCB+12                                                       
         MVC   BUCKET$,6(RF)       BUCKET FROM PREVIOUS UL OF SAME DAY          
         L     R1,BUCKET$                                                       
         MVC   BUCKET$,2(R5)       SUBTRACT TOTAL BUCKET                        
         L     RE,BUCKET$          TO GET DIFFERENCE                            
         SR    R1,RE                                                            
         ST    R1,BUCKET$                                                       
         MVC   6(4,RF),BUCKET$                                                  
         B     ADJB100                                                          
*                                                                               
* NO OTHER ACTIVITY FROM TODAY. ADD DELTA BUCKET                                
*                                                                               
ADJB95   DS    0H                                                               
         MVC   BUCKET$,2(R5)       NEGATE TOTAL BUCKET                          
         L     RE,BUCKET$          TO GET SUM OF $0                             
         LNR   RE,RE                                                            
         ST    RE,BUCKET$                                                       
         MVC   ELEM+6(4),BUCKET$                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELEM,0                     
*                                                                               
ADJB100  DS    0H                                                               
         LA    R5,6(R5)                                                         
         CLI   0(R5),0                                                          
         BNE   ADJB10                                                           
         B     ADJBX                                                            
*                                                                               
ADJBX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE TO SET ACTIVITY DATE TO PREVIOUS QUARTER FOR BACK BILLING             
* WORK HAS EITHER THE X'03' OR X'04' BUCKET ELEMENT                             
*                                                                               
BILLING  NTR1                                                                   
         XC    WORK3,WORK3         MONTH OF BUCKET DICTATES                     
         MVC   WORK2(2),WORK+2     WHICH MONTH TO BACK UP                       
         MVI   WORK2+2,15          THIS WILL ENSURE CORRECT MONTH               
         GOTO1 DATCON,DMCB,(3,WORK2),(0,WORK3)                                  
         XC    WORK2,WORK2                                                      
*                                                                               
         GOTO1 VGTBROAD,DMCB,(1,WORK3),WORK3+6,GETDAY,ADDAY                     
         GOTO1 DATCON,DMCB,(0,WORK3+12),(5,WORK3+18)                            
         MVC   WORK3+21(2),=C'15'                                               
         MVC   WORK3+26(6),=C'(- M)'                                            
*                                                                               
         TM    PROFILES+CNTBBILB,CNTBBILA                                       
         BZ    BILL05              PROFILE TO GO GET LAST MONTH                 
         MVI   WORK3+28,C'1'       INSTEAD OF LAST QUARTER?                     
         B     BILL25                                                           
*                                                                               
BILL05   DS    0H                  FIND BROADCAST MONTH LAST QUARTER            
         LA    R3,QTABLE           GET FIRST MONDAY OF PREVIOUS QUARTER         
BILL10   CLC   WORK3+18(3),0(R3)                                                
         BE    BILL20                                                           
         LA    R3,L'QTABLE(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   BILL10                                                           
         LA    R3,579                                                           
         B     ERROR                                                            
*                                                                               
BILL20   DS    0H                                                               
         MVC   WORK3+28(1),3(R3)                                                
*                                                                               
BILL25   DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CPERVAL                                                       
         DROP  RE                                                               
*                                                                               
         LA    R3,579                                                           
         GOTO1 (RF),DMCB,(14,WORK3+18),(0,WORK2)                                
         CLI   DMCB+4,1                                                         
         BE    ERROR                                                            
*                                  FIND START DATE OF BROADCAST MONTH           
PDATED   USING PERVALD,WORK2                                                    
         GOTO1 VGTBROAD,DMCB,(1,PDATED.PVALESTA),WORK3,GETDAY,ADDAY             
         DROP  PDATED              MOVE COMPRESSED DATE                         
*                                                                               
BILL30   DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK3),(2,WORK+4)                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
DISBUCK  NTR1                                                                   
         CLI   CFLAG,1             HAVE ANY BUCKETS CHANGED?                    
         BNE   *+8                                                              
         BAS   RE,BLDBUCK          IF SO, REBUILD TOTAL BUCKETS                 
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
* WORK HAS EBCDIC START AND WORK+6 HAS END DATE                                 
         GOTO1 VGTBROAD,DMCB,(1,WORK),TEMP,GETDAY,ADDAY                         
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),TEMP+12,GETDAY,ADDAY                    
*                                                                               
         GOTO1 DATCON,DMCB,(0,TEMP+6),(20,STRTDATE)  K START DATE               
         GOTO1 DATCON,DMCB,(0,TEMP+18),(20,ENDDATE)  K END DATE                 
*                                                                               
* TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                        
* TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                         
*                                                                               
         MVC   BCSTRTDT,TEMP+6                                                  
         MVC   BCENDDT,TEMP+18                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(0,TEMP+6),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(0,TEMP+18),(3,WORK+3)                               
* WORK NOW HAS START BROADCAST MONTH & YEAR AND WORK+3 HAS END                  
* BROADCAST MONTH AND YEAR (YMD - BINARY)                                       
*                                                                               
         L     R5,AIO3             R5 TO OLD TOTAL BUCKETS                      
         MVC   YEAR,STRTDATE+2     K START YEAR                                 
         MVC   BYEAR,WORK          SAVE BIN YEAR                                
         ZIC   R4,WORK+1           START MON                                    
         ZIC   R3,WORK+4           END MON                                      
         CLC   WORK(1),WORK+3      SAME YEAR?                                   
         BE    *+8                                                              
         LA    R3,12(R3)           ADJUST FOR NEXT YEAR                         
         SR    R3,R4                                                            
         LA    R3,1(R3)            NUM MONTHS                                   
         BCTR  R4,0                MONTHS ARE 0 RELATIVE                        
         MH    R4,=Y(L'MONTBL)     R4 NOW AT STARTING MONTH                     
         LA    R4,MONTBL(R4)       IT HELPS TO POINT IT AT THE TABLE            
         LA    R2,MO$MON1H         FIRST FLD HDR                                
DB10     DS    0H                                                               
         MVC   8(3,R2),1(R4)       MMM                                          
         MVI   11(R2),C'/'                                                      
         MVC   12(2,R2),YEAR       YY                                           
         OI    4(R2),X'20'         SET PRE-VALID ON MON FLD                     
         ZIC   R0,0(R2)            R2 TO INPUT FLD                              
         AR    R2,R0                                                            
         OI    4(R2),X'20'         SET PREVALID BIT                             
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT                             
*                                                                               
* DISPLAY $ (IF ANY)                                                            
         CLC   0(1,R5),BYEAR       THIS YEAR?                                   
         BNE   DB20                                                             
         CLC   1(1,R5),0(R4)       THIS MONTH?                                  
         BNE   DB20                                                             
         EDIT  (4,2(R5)),(10,8(R2)),2                                           
         OI    6(R2),X'80'         XMIT FLD                                     
         LA    R5,6(R5)            SET NEXT TOTAL BUCKET                        
DB20     ZIC   R0,0(R2)            R2 TO NEXT DATE FIELD                        
         AR    R2,R0                                                            
         LA    R4,L'MONTBL(R4)     NEXT MON                                     
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   DB30                NO - CONTINUE                                
         LA    R4,MONTBL           BACK TO START OF TBL                         
         MVC   YEAR,ENDDATE+2      K END YEAR (EBCDIC)                          
         ZIC   R1,BYEAR                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BYEAR                                                         
DB30     BCT   R3,DB10                                                          
DBX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* UPDATE VERSION NUMBER AND UNCONFIRM CONTRACT AND DISPLAY NEW VER NO           
UPDVER   NTR1                                                                   
         CLC   RCONMODD,TODAY                                                   
         BE    *+8                                                              
         OI    TAREQ,X'01'                                                      
*         MVC   RCONMODD,TODAY                                                  
*                                                                               
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
                                                                                
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    UV10                                                             
         DROP  R6                                                               
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),WORK                                
         BNZ   ERROR                                                            
                                                                                
UV10     DS    0H                                                               
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    UV20                                                             
         SPACE 1                                                                
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
UV20     OI    RCONCONF,X'80'      NOT CONFIRMED                                
*                                                                               
* NOW DISPLAY NEW VERSION NUMBER                                                
         FOUT  CONMODH,MYSPACES,11                                              
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    UV50                                                             
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED, SHOW MOD NUM.                     
         BO    UV50                                                             
         SPACE 1                                                                
         ZIC   R3,1(R6)                                                         
         AR    R6,R3               GET NEXT ELEMENT                             
         SPACE 1                                                                
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    UV25                                                             
         DC    H'0',C'MISSING X20 - SEND ELEM'                                  
         SPACE 1                                                                
         USING RCONSEND,R6                                                      
UV25     CLC   RCONSRV,RCONSSV                                                  
         BH    UV30                                                             
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     UV40                                                             
UV30     EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
UV40     MVC   CONMOD(7),=C'WIP VER'                                            
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BZ    XIT                                                              
         MVC   CONMOD(7),=C'ESL VER'                                            
         DROP  R6                                                               
         B     XIT                                                              
         SPACE 1                                                                
UV50     CLI   RCONMOD,0           K MOD NUM                                    
         BE    XIT                                                              
         MVC   CONMOD(7),=C'MOD NUM'                                            
         EDIT  (1,RCONMOD),(3,CONMOD+8),ALIGN=LEFT                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*                                                                               
TRUDATE  NTR1                                                                   
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,RCONDATE),(3,TDATELT+5)                           
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 VADDELEM,DMCB,RCONREC,TDATELT                                    
TDAT0040 EQU   *                                                                
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         XIT1                                                                   
       EJECT                                                                    
*********************************************************************           
* ALTCAL  - SETS ALTERNATE CALENDAR BUCKETS, IF NEEDED              *           
*********************************************************************           
ALTCAL   NTR1                                                                   
         TM    TWASTAOP,X'20'      ALTERNATE CALENDAR STATION?                  
         BNO   ALCA0060            NO  - FINISHED                               
*                                  YES - DROP X'53'/X'54' ELTS                  
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'53',RCONREC),0,0                
*                                  DELETE OLD ESTIMATE ELTS FOR MON             
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'54',RCONREC),0,0                
*                                  DELETE OLD INVOICE  ELTS FOR MON             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
*                                                                               
         CLC   =C'ACC-BB',CONBUY   BACK BILLING?                                
         BNE   *+8                 NO                                           
         MVI   ELCODE,X'04'        YES - COPY X'04' -> X'54'                    
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   ALCA0060            NO BUCKETS AT ALL                            
         B     ALCA0040            PROCESS BUCKET FOUND                         
ALCA0020 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   ALCA0060            BUCKETS FINISHED                             
ALCA0040 EQU   *                                                                
         MVC   WORK(10),0(R6)      MOVE BUCKET TO WORK AREA                     
         OI    WORK,X'50'          SET ALTERNATE CALENDAR BUCKET                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                     
*                                  ADD BUCKET TO CONTRACT                       
         B     ALCA0020            GO BACK FOR NEXT BUCKET                      
ALCA0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* SARCOMS - CHANGES PENDING COMMENTS TO SPL COMMENTS                *           
*********************************************************************           
SARCOMS  NTR1                                                                   
SARCM2   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   SARCMX                                                           
*                                                                               
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R6)                                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),(R6)                                  
*                                                                               
         LA    R6,RCONELEM                                                      
SARCM4   CLI   0(R6),0                                                          
         BE    SARCM6                                                           
         CLI   0(R6),X'07'                                                      
         BH    SARCM6                                                           
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     SARCM4                                                           
*                                                                               
SARCM6   MVI   WORK2,X'07'                                                      
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),WORK2,(R6)                            
         B     SARCM2                                                           
*                                                                               
SARCMX   B     XIT                                                              
         EJECT                                                                  
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
XIT      XIT1                                                                   
         DROP  R7                                                               
MONTBL   DS    0CL3                                                             
         DC    X'01',C'01'                                                      
         DC    X'02',C'02'                                                      
         DC    X'03',C'03'                                                      
         DC    X'04',C'04'                                                      
         DC    X'05',C'05'                                                      
         DC    X'06',C'06'                                                      
         DC    X'07',C'07'                                                      
         DC    X'08',C'08'                                                      
         DC    X'09',C'09'                                                      
         DC    X'0A',C'10'                                                      
         DC    X'0B',C'11'                                                      
         DC    X'0C',C'12'                                                      
         DC    X'FF'                                                            
*                                                                               
QTABLE   DS    0CL4                                                             
         DC    C'JAN',C'3'                                                      
         DC    C'FEB',C'4'                                                      
         DC    C'MAR',C'5'                                                      
         DC    C'APR',C'3'                                                      
         DC    C'MAY',C'4'                                                      
         DC    C'JUN',C'5'                                                      
         DC    C'JUL',C'3'                                                      
         DC    C'AUG',C'4'                                                      
         DC    C'SEP',C'5'                                                      
         DC    C'OCT',C'3'                                                      
         DC    C'NOV',C'4'                                                      
         DC    C'DEC',C'5'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTEAD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
MYWORKD  DSECT                                                                  
YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                         
DATE     DS    CL2                 EBCDIC - FOR DISPLAY                         
BYEAR    DS    X                   BINARY YEAR                                  
BMONTH   DS    X                   BINARY MONTH                                 
BCSTRTDT DS    CL6                 BROADCAST END YYMMDD OF FLIGHT START         
BCENDDT  DS    CL6                 BROADCAST END YYMMDD OF FLIGHT END           
BCSTDATE DS    XL3                 BROADCAST END YYMMDD OF FLIGHT START         
BCENDATE DS    XL3                 BROADCAST END YYMMDD OF FLIGHT END           
CFLAG    DS    X                   CHANGED FLAG                                 
CMBNDX   DS    XL1                 COMBO COUNTER                                
MYWORK   DS    XL20                                                             
SVCKEY   DS    CL27                SAVED CONTRACT KEY                           
SVKNUM   DS    F                   SAVED ORIGINAL TWACNUM                       
BUCKET$  DS    F                                                                
ACTFLD   DS    A                                                                
SVCMB17  DS    XL36                SAVED 17 ELEMENT                             
STRTDATE DS    CL8                 YYYYMMDD DATCON TYPE 20                      
ENDDATE  DS    CL8                 YYYYMMDD DATCON TYPE 20                      
OUTDATE  DS    CL6                                                              
BINDATE  DS    XL3                                                              
ACTDATE  DS    XL2                                                              
MISCFLAG DS    X                                                                
MFRCU    EQU   X'80'               CONTRACT CREATED BY UPLOAD                   
MO$MLENQ EQU   MO$MON2H-MO$MON1H   LENGTH OF ONE ROW FOR MO$                    
ROW#     DS    CL2                                                              
ELEM     DS    XL255                                                            
MYWORKX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062RECNT5A   03/02/11'                                      
         END                                                                    
