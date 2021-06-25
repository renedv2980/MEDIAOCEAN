*          DATA SET RECNT57    AT LEVEL 145 AS OF 12/26/07                      
*PHASE T80257A,+0                                                               
         TITLE 'T80257 - REP PACE DISPLAY/EDIT'                                 
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT52 (T80257) --- PACE DISPLAY/EDIT                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 26DEC07 (HQ ) --- FIX END OF SCREEN DUMP                        *             
*                                                                 *             
* 19FEB97 (DBU) --- USED RECNT52 (MON ACTION) AS A MODEL          *             
*                   NEW PACE ACTION                               *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80257   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80257,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
PACE     DS    0H                                                               
*                                                                               
*                                                                               
* RECORD FULL?                                                                  
*                                                                               
         LA    R2,PCEESINH                                                      
         SR    R8,R8                                                            
         ICM   R8,3,RCONLEN                                                     
         AH    R8,=H'120'                                                       
         C     R8,=AL4(L'IO2)                                                   
         BNH   *+12                                                             
         LA    R3,668              RECORD FULL, CREATE NEW ONE                  
         B     ERROR                                                            
*                                                                               
         MVI   CFLAG,0                                                          
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         BAS   RE,BLDBUCK          BUILD TOTAL BUCKETS                          
         BAS   RE,VALBUCK          GO VALIDATE BUCKETS                          
         TM    PROFILES+CNTMEOCB,CNTMEOCA                                       
*                                  MO PARALLEL IN USE?                          
         BO    PACE0010            YES - DON'T PROTECT THE FIELDS               
         LA    R2,PCEMCOHH         NO  - SET A(MO CON HEADER)                   
         OI    1(R2),X'0C'                                                      
         FOUT  (R2)                                                             
         LA    R2,PCEMCONH         SET A(MO CON NUMBER)                         
         OI    1(R2),X'20'         TURN ON PROTECT                              
         FOUT  (R2)                                                             
PACE0010 EQU   *                                                                
         BAS   RE,DISBUCK          GO DISPLAY BUCKETS                           
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,87     ADD/DISP PACE DATA                           
         CLI   CFLAG,1             CHANGED?                                     
         BNE   PACE0020                                                         
         BAS   RE,UPDVER           UPDATE VERSION NUMBER AND UNCONFIRM          
         GOTO1 VDISMSG,DMCB,88     PACE DATA CHANGED                            
         OI    RCONMODR+1,X'20'    SET MONTHLY DONE FLAG ON CON                 
         GOTO1 VPUTREC,DMCB,RCONREC                                             
PACE0020 DS    0H                                                               
*                                                                               
* RECORD FULL?                                                                  
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,RCONLEN                                                     
         AH    R8,=H'120'                                                       
         C     R8,=AL4(L'IO2)                                                   
         BNH   PACEX                                                            
         GOTO1 VDISMSG,DMCB,90     WARNING - RECORD FULL                        
*                                                                               
PACEX    DS    0H                                                               
         B     EXXMOD              GO TO HELL                                   
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
         LR    RE,R4                                                            
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'        DEFAULT - INVOICE                            
                                                                                
* IF ESTIMATE CONVERT TO ESTIMATE BUCKETS                                       
         CLI   PCEESIN,C'E'                                                     
         BNE   *+8                                                              
         MVI   ELCODE,X'03'                                                     
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
* DON'T VALIDATE IF FIRST TIME THROUGH                                          
* READ CAREFULLY: IF NOT PREVIOUSLY VALIDATED, SKIP VALIDATE!!!                 
* REASON: THE MONTH FLDS ARE PROTECTED, BUT AREN'T ON THE SCREEN UNTIL          
*         THE FIRST DISPLAY IS DONE, AT WHICH TIME ITS VALID BITS ARE           
*         SET.                                                                  
*                                                                               
         TM    PCESTRTH+4,X'20'    PRE-VALID?                                   
         BZ    VALI0900            NO - SKIP VALIDATE                           
*                                                                               
* VALIDATE MEDIA OCEAN PARALLEL CONTRACT NUMBER                                 
         TM    PROFILES+CNTMEOCB,CNTMEOCA                                       
*                                  MO PARALLEL IN USE?                          
         BNO   VALI0020            NO                                           
         GOTO1  HELLO,DMCB,(C'D',=C'REPFILE'),(X'A8',RCONREC),0,0               
*                                  YES - DELETE PARALLEL MO ELEMENT             
         LA    R2,PCEMCONH         MEDIA OCEAN CONTRACT #                       
         CLI   5(R2),0             NUMBER ENTERED?                              
         BE    VALI0020            NO                                           
*                                                                               
*   ANY VALIDATION CRITERIA?                                                    
*                                                                               
         MVC   NEWA8CON,PCEMCON                                                 
         OC    NEWA8CON,MYSPACES                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,NEWA8ELT,0                 
VALI0020 EQU   *                                                                
*                                                                               
* VALIDATE ESTIMATE/INVOICE                                                     
         LA    R2,PCEESINH         ESTIMATE/INVOICE                             
         CLI   5(R2),0             DEFAULT USED (INVOICE)?                      
         BE    VALI0040            YES                                          
         CLI   8(R2),C'E'          ESTIMATE?                                    
         BE    VALI0040                                                         
         CLI   8(R2),C'I'          INVOICE?                                     
         BE    VALI0040                                                         
         B     INFLD               ERROR - INVALID FIELD                        
VALI0040 EQU   *                                                                
* VALIDATE DATE FIELD                                                           
         LA    R2,PCEACDTH                                                      
*                                                                               
*              GET TODAY'S DATE                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(X'20',DUB)                                    
         MVC   SVTDATE,DUB         SAVE TODAYS DATE                             
         LA    R8,DATE                                                          
         BAS   RE,GETMONDY         GET MONDAY                                   
*                                                                               
         CLI   5(R2),0             ANY ACTIVITY DATE?                           
         BE    VALI0100            NO - USE DEFAULT, TODAY'S DATE               
*                                                                               
*              GET ACTIVITY DATE                                                
*                                                                               
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8                                                      
         GOTO1 CDATVAL,DMCB,(0,8(R2)),BLOCK                                     
         DROP  R8                                                               
         OC    DMCB(4),DMCB                                                     
         BNZ   VALI0060                                                         
         LA    R3,13                                                            
         B     ERROR                                                            
VALI0060 EQU   *                                                                
         MVC   DUB(6),BLOCK                                                     
         LA    R8,DATE2                                                         
         BAS   RE,GETMONDY         GET MONDAY, AND STORE IN DATE2               
         CLC   DATE,DATE2          ACT. DATE > THAN CURRENT MONDAY?             
         BNL   VALI0080            NO                                           
         LA    R3,665              FUTURE DATES ARE NOT VALID ACT. DATE         
         B     ERROR                                                            
VALI0080 EQU   *                                                                
         MVC   DATE,DATE2          USE NOT DEFAULT DATE                         
VALI0100 EQU   *                                                                
*                                                                               
* GET CURRENT BROADCAST MONTH                                                   
*                                                                               
         GOTO1 VGTBROAD,DMCB,(1,SVTDATE),(X'20',BRDATE),GETDAY,ADDAY            
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(0,BRDATE+6),(X'20',BRDATE+6)                        
         PACK  DUB,BRDATE+6(2)          GET YEAR                                
         CVB   R1,DUB                                                           
         STC   R1,BRYEAR                                                        
         PACK  DUB,BRDATE+8(2)        GET MONTH                                 
         CVB   R1,DUB                                                           
         STC   R1,BRMONTH                                                       
*                                                                               
* VALIDATE TOTAL/CHANGE FIELD                                                   
         LA    R2,PCEACDFH         TOTAL/CHANGE                                 
         CLI   5(R2),0             DEFAULT USED (TOTAL)?                        
         BE    VALI0120            YES                                          
         CLI   8(R2),C'T'          TOTAL?                                       
         BE    VALI0120                                                         
         CLI   8(R2),C'C'          CHANGE?                                      
         BE    VALI0120                                                         
         B     INFLD               ERROR - INVALID FIELD                        
VALI0120 EQU   *                                                                
         L     R5,AIO3             R5 TO TOTAL BUCKETS                          
         LA    R4,MONTBL                                                        
         LA    R2,PCESTRTH         FIRST FLD HDR                                
VALI1040 DS    0H                  SET R4 TO FIRST CORRESPOND                   
         CLC   1(3,R4),8(R2)       W/FIRST MONTH ON SCREEN                      
         BE    VALI0160                                                         
         LA    R4,L'MONTBL(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   VALI1040                                                         
         DC    H'0'                NO MONTH?                                    
VALI0160 DS    0H                                                               
         PACK  DUB,11(2,R2)        GET YEAR                                     
         CVB   R1,DUB                                                           
         STC   R1,BYEAR                                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO MONEY FLD                              
*                                                                               
* VALIDATE MONEY FIELD                                                          
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VALI0460            NO, NEXT FLD                                 
         LA    RE,PCELAST          END OF SCREEN?                               
         CR    R2,RE                                                            
         BNL   VALI0900            EXIT                                         
*                                                                               
* PACE MM/YY IS ALLOWED TILL CURRENT BROADCAST MONTH                            
* ONLY FOR INVOICED BUCKETS                                                     
*                                                                               
         CLI   PCEESIN,C'E'                                                     
         BE    VALI0200                                                         
         CLC   BYEAR,BRYEAR        BEYOND CURRENT BROADCAST YEAR?               
         BH    VALI0180                                                         
         BL    VALI0200                                                         
         CLC   0(1,R4),BRMONTH     BEYOND CURRENT BROADCAST MONTH?              
         BL    *+12                                                             
VALI0180 EQU   *                                                                
         LA    R3,667                                                           
         B     ERROR                                                            
*                                                                               
VALI0200 EQU   *                                                                
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   VALI0220                                                         
         LA    R3,3                NON-NUMERIC INPUT FLD                        
         B     ERROR                                                            
*                                                                               
VALI0220 EQU   *                                                                
         MVI   CFLAG,1             SET CHANGED                                  
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'040A'     DEFAULT - INVOICE                           
         CLI   PCEESIN,C'E'        ESTIMATE?                                    
         BNE   *+8                                                              
         MVI   WORK,X'03'                                                       
         MVC   WORK+2(1),BYEAR     BROADCAST YY                                 
         MVC   WORK+3(1),0(R4)     MM                                           
*                                                                               
*   CHANGE YEAR FORMAT:                                                         
         MVC   DMWORK(4),DMCB+4    SAVE DOLLAR AMOUNT                           
*                                                                               
         MVC   WORK+39(2),WORK+2   MOVE YYMM                                    
         MVI   WORK+41,1           SET DD TO 1                                  
         GOTO1 DATCON,DMCB,(3,WORK+39),(0,WORK+42)                              
         GOTO1 DATCON,DMCB,(0,WORK+42),(3,WORK+39)                              
         MVC   WORK+2(1),WORK+39                                                
*   YEAR FORMAT NOW 2000 = X'64'                                                
*                                                                               
         MVC   WORK+4(2),DATE      ACTIVITY DATE                                
         MVC   AMOUNT(4),DMWORK    NEW AMOUNT                                   
         LA    R6,RCONREC                                                       
         MVC   ELCODE(1),WORK                                                   
         LA    R3,662                                                           
         BAS   RE,GETEL                                                         
*                                                                               
* FIND IF INV  OR EST ELEMENT ALREADY EXISTS                                    
* IF IT DOES EXIST THAN RECALCULATE TOTAL FOR THIS PERIOD                       
* MOREOVER, PREVENT FROM MAKING CHANGES TO PREVIOUSLY ENTERED WEEKS             
*                                                                               
VALI0240 EQU   *                                                                
         BNE   VALI0320            NO MORE WEEKS TO CHECK                       
VALI0260 EQU   *                                                                
         CLC   WORK+2(2),2(R6)     SAME MONTH?                                  
         BNE   VALI0280                                                         
         CLC   WORK+4(2),4(R6)     SUBSEQUENT WEEK EXIST?                       
         BH    VALI0280            CHECK NEXT                                   
         BE    VALI0300                                                         
         LA    R2,PCEACDTH                                                      
         B     ERROR                                                            
VALI0280 EQU   *                                                                
         BAS   RE,NEXTEL           NEXT ELEMENT                                 
         B     VALI0240                                                         
*                                                                               
VALI0300 EQU   *                                                                
         MVC   OLDAMNT(4),6(R6)    SAVE PREV DELTA                              
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),(R6)                                  
         OI    MYFLAG,RECALC       ELEMENT EXIST - RECALCULATE                  
         CLC   0(1,R6),ELCODE      NEXT ELEMENT - SAME TYPE?                    
         BNE   VALI0320            NO                                           
         B     VALI0260                                                         
*                                                                               
VALI0320 EQU   *                                                                
         CLI   PCEACDF,C'C'        TOTAL OR CHANGE?                             
         BNE   VALI0360            TOTAL                                        
VALI0340 EQU   *                                                                
         MVC   WORK+6(4),AMOUNT    CHANGE AMOUNT                                
         B     VALI0440            ADD ELEMENT                                  
*                                                                               
VALI0360 EQU   *                   NEW TOTAL - ADD DIFFERENCE                   
         CLI   1(R5),0             END OF TABLE?                                
         BE    VALI0340            YES - NO TOTAL FOR THIS MONTH                
         CLC   0(1,R4),1(R5)       SAME BROADCAST MONTH?                        
         BE    VALI0380            YES                                          
         LA    R5,6(R5)                                                         
         B     VALI0360                                                         
* TOTAL FOUND - PERFORM DIFFERENCE                                              
VALI0380 EQU   *                                                                
         TM    MYFLAG,RECALC       DUPLICATE ENTRY - RECALCULATE?               
         BZ    VALI0400            NO - PERFORM STANDARD CALCULATION            
         L     R8,2(R5)            LOAD OLD TOTAL                               
         S     R8,AMOUNT           DELTA  = (OLD TOTAL-NEW TOTAL)               
         L     R0,OLDAMNT          OLD BUCKET AMOUNT                            
         SR    R0,R8               NEW BUCKET AMOUNT                            
         NI    MYFLAG,X'FF'-RECALC                                              
         B     VALI0420                                                         
VALI0400 EQU   *                                                                
         L     R0,AMOUNT                                                        
         S     R0,2(R5)            CALCULATE THE DIFFERENCE                     
VALI0420 EQU   *                                                                
         STCM  R0,15,WORK+6        STORE THE DIFFERENCE                         
         LTR   R0,R0                                                            
         BZ    VALI0460                                                         
VALI0440 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                     
         L     R5,AIO3             RESET R5 TO TOTAL BUCKETS                    
VALI0460 EQU   *                                                                
         OI    4(R2),X'20'         SET PREVALID                                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO NEXT BUCKET FLD                        
         CLI   8(R2),0             IS FIELD IN USE?                             
         BNH   VALI0900            NO, NO MORE FIELDS                           
         LA    RE,PCELAST          END OF SCREEN?                               
         CR    R2,RE                                                            
         BNL   VALI0900            EXIT                                         
*                                                                               
         LA    R4,MONTBL           BACK TO FIRST MON                            
         B     VALI1040                                                         
*                                                                               
VALI0900 B     XIT                                                              
         EJECT                                                                  
*                                                                               
* GET DAY OF WEEK, GET 2-BYTE MONDAY WEEK DAY                                   
*                                                                               
GETMONDY NTR1                                                                   
*                                                                               
*              FIND DAY OF WEEK                                                 
*                                                                               
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),=3C' '                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R6,DMCB             DAY OF WEEK                                  
         BCTR  R6,R0                                                            
*                                                                               
*   DAILY PACING ADJUSTMENT                                                     
*                                                                               
         TM    TWAFLAGS,X'08'      DAILY PACING USER?                           
         BO    GMON0020            YES                                          
         LNR   R6,R6               BACK UP TO MONDAY                            
*              GET CURRENT MONDAY                                               
         B     GMON0040                                                         
GMON0020 EQU   *                                                                
         SR    R6,R6               DAILY:  NO ADJUSTMENT TO MONDAY              
GMON0040 EQU   *                                                                
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R6)                                      
*              GET 2-BYTE CURRENT MONDAY-WEEK DATE                              
         GOTO1 DATCON,DMCB,DMCB+12,(2,(R8))                                     
         XIT1                                                                   
*                                                                               
DISBUCK  NTR1                                                                   
         TM    PROFILES+CNTMEOCB,CNTMEOCA                                       
*                                  MO PARALLEL IN USE?                          
         BNO   DISB0020            NO                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A8'        LOOK FOR MEDIA OCEAN PARALLEL                
                                                                                
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   DISB0020            NO PARALLEL ELEMENT                          
         USING RCONMOEL,R6                                                      
         MVC   PCEMCON,RCONMOC#    INSERT CONTRACT NUMBER ON SCREEN             
         DROP  R6                                                               
         FOUT  PCEMCOHH                                                         
DISB0020 EQU   *                                                                
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
* TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                        
* TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                         
         GOTO1 DATCON,DMCB,(0,TEMP+6),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(3,WORK),(X'20',TEMP+6)                              
         GOTO1 DATCON,DMCB,(0,TEMP+18),(3,WORK+3)                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(X'20',TEMP+18)                           
* WORK NOW HAS START BROADCAST MONTH & YEAR AND WORK+3 HAS END                  
* BROADCAST MONTH AND YEAR (YMD - BINARY)                                       
*                                                                               
         L     R5,AIO3             R5 TO OLD TOTAL BUCKETS                      
         MVC   YEAR,TEMP+6         K START YEAR                                 
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
         LA    R2,PCESTRTH         FIRST FLD HDR                                
DISB0040 DS    0H                                                               
         MVC   8(3,R2),1(R4)       MMM                                          
         MVC   11(2,R2),YEAR       YY                                           
         OI    4(R2),X'20'         SET PRE-VALID ON MON FLD                     
         ZIC   R0,0(R2)            R2 TO INPUT FLD                              
         AR    R2,R0                                                            
         OI    4(R2),X'20'         SET PREVALID BIT                             
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT                             
*                                                                               
DISB0060 ZIC   R0,0(R2)            R2 TO NEXT DATE FIELD                        
         AR    R2,R0                                                            
         LA    R4,L'MONTBL(R4)     NEXT MON                                     
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   DISB0080            NO - CONTINUE                                
         LA    R4,MONTBL           BACK TO START OF TBL                         
         MVC   YEAR,TEMP+18        K END YEAR (EBCDIC)                          
         ZIC   R1,BYEAR                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BYEAR                                                         
DISB0080 BCT   R3,DISB0040                                                      
DISB0900 EQU   *                                                                
         B     XIT                                                              
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
XIT      XIT1                                                                   
         DROP  R7                                                               
*                                                                               
INFLD    LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
MONTBL   DS    0CL4                                                             
         DC    X'01',C'JAN'                                                     
         DC    X'02',C'FEB'                                                     
         DC    X'03',C'MAR'                                                     
         DC    X'04',C'APR'                                                     
         DC    X'05',C'MAY'                                                     
         DC    X'06',C'JUN'                                                     
         DC    X'07',C'JUL'                                                     
         DC    X'08',C'AUG'                                                     
         DC    X'09',C'SEP'                                                     
         DC    X'0A',C'OCT'                                                     
         DC    X'0B',C'NOV'                                                     
         DC    X'0C',C'DEC'                                                     
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
YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                         
DATE     DS    CL2                 EBCDIC                                       
DATE2    DS    CL2                 EBCDIC                                       
SVTDATE  DS    CL6                 SAVED TODAYS DATE                            
BRDATE   DS    CL12                BROADCAST ST/END DATE                        
BRYEAR   DS    X                   BINARY BROADCAST YEAR                        
BRMONTH  DS    X                   BINARY BROADCAST MONTH                       
BYEAR    DS    X                   BINARY YEAR                                  
CFLAG    DS    X                   CHANGED FLAG                                 
CMBNDX   DS    XL1                 COMBO COUNTER                                
MYWORK   DS    XL20                                                             
SVCKEY   DS    CL27                SAVED CONTRACT KEY                           
SVKNUM   DS    F                   SAVED ORIGINAL TWACNUM                       
SVCMB17  DS    XL36                SAVED 17 ELEMENT                             
BLOCK    DS    CL56                USED BY PERVAL                               
AMOUNT   DS    CL4                 CURRENT AMOUNT                               
OLDAMNT  DS    CL4                 OLD AMOUNT                                   
MYFLAG   DS    X                                                                
RECALC   EQU   X'80'               ELEMENT EXISTS                               
NEWA8ELT DC    X'A820'                                                          
         DC    X'01'               ELEMENT VERSION NUMBER                       
NEWA8CON DC    XL10'00'            MO CONTRACT #                                
         DC    XL19'00'            SPARE                                        
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTD8D                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145RECNT57   12/26/07'                                      
         END                                                                    
