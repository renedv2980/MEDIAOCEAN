*          DATA SET RECNT5C    AT LEVEL 048 AS OF 12/15/10                      
*          DATA SET RECNT5C    AT LEVEL 145 AS OF 12/26/07                      
*PHASE T8025CA,+0                                                               
         TITLE 'T8025C - REP #MON DISPLAY/EDIT'                                 
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT5C (T8025C) --- #MON DISPLAY/EDIT                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 12NOV10 (KUI) --- USED RECNT57 AS MODEL                         *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8025C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8025C,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         MVI   CFLAG,0                                                          
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         BAS   RE,BLDBUCK          BUILD TOTAL BUCKETS                          
         BAS   RE,VALBUCK          GO VALIDATE BUCKETS                          
         BAS   RE,DISBUCK          GO DISPLAY BUCKETS                           
*                                                                               
*&&DO                                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BNE   MAIN10                                                           
         USING RCONCUEL,R6                                                      
         CLI   RCONCUCT,0          UPLOADED AT LEAST ONCE SKIP UPDATE           
         BNE   EXXMOD                                                           
         DROP  R6                                                               
*&&                                                                             
*                                                                               
MAIN10   DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,60     ADD/DISP MON DATA                            
         CLI   CFLAG,1             CHANGED?                                     
         BNE   EXXMOD                                                           
         GOTO1 VDISMSG,DMCB,61     MON DATA CHANGED                             
*                                                                               
         OI    RCONMODR+1,X'20'    SET MONTHLY DONE FLAG ON CON                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONRFEL,R6                                                      
         OI    RCONRF1,X'04'       MARK #MON INVOKED                            
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BE    MAIN50                                                           
*                                                                               
         XC    WORK,WORK                                                        
RCUAD    USING RCONCUEL,WORK                                                    
         MVI   RCUAD.RCONCUCD,X'3C'                                             
         MVI   RCUAD.RCONCULN,RCONCULQ                                          
         MVC   RCUAD.RCONCUMA,DATE       ACTIVITY DATE                          
         MVC   RCUAD.RCONCUW#,RC$TONM                                           
         OC    RCUAD.RCONCUW#,MYSPACES                                          
                                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         DROP  RCUAD                                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN50   DS    0H                                                               
         USING RCONCUEL,R6                                                      
         GOTO1 DATCON,DMCB,(5,0),(2,RCONCUMD)                                   
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,RCONCUMT                                                    
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         MVC   RCONTRF,RC$TONM     SAVE OFF TRAFFIC ORDER NUMBER                
         OC    RCONTRF,MYSPACES                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
* UPDATE PASSIVE SCRIPT KEY                                                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
RCUKD    USING RCONCUTP,KEY                                                     
         MVI   RCUKD.RCONCUTP,X'AE'                                             
         MVC   RCUKD.RCONCURP,REPALPHA                                          
         MVC   RCUKD.RCONCUST,RCONKSTA                                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
MAIN60   DS    0H                                                               
         CLC   KEY(RCONCUT#-RCONCUTP),KEYSAVE                                   
         BNE   MAIN80                                                           
*                                                                               
         CLC   RCUKD.RCONCUC#,RCONKCON                                          
         BNE   MAIN70                                                           
*                                                                               
         USING RCONCUEL,R6                                                      
         CLC   RCUKD.RCONCUT#,RCONCUW#   SAME TRAFFIC ORDER NUMBER?             
         BE    MAINX                                                            
         DROP  R6                                                               
*                                                                               
         OI    KEY+27,X'80'        DELETE OLD TRAFFIC# PASSIVE                  
         GOTO1 VWRITE                                                           
*                                                                               
MAIN70   DS    0H                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         B     MAIN60                                                           
*                                                                               
* ADD SCRIPT PASSIVE KEY                                                        
*                                                                               
MAIN80   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   RCUKD.RCONCUTP,X'AE'                                             
         MVC   RCUKD.RCONCURP,REPALPHA                                          
         MVC   RCUKD.RCONCUST,RCONKSTA                                          
         MVC   RCUKD.RCONCUC#,RCONKCON                                          
*                                                                               
         USING RCONCUEL,R6                                                      
         MVC   RCUKD.RCONCUT#,RCONCUW#   TRAFFIC ORDER NUMBER                   
         DROP  RCUKD,R6                                                         
*                                                                               
         MVC   KEY+28(4),TWAKADDR  SAVE DISK ADDR                               
*                                                                               
         GOTO1 VADD                                                             
*                                                                               
MAINX    DS    0H                                                               
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
         MVI   ELCODE,X'03'                                                     
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
*&&DO                                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BNE   VALI0040                                                         
         USING RCONCUEL,R6                                                      
         CLI   RCONCUCT,0          UPLOADED AT LEAST ONCE SKIP UPDATE           
         BNE   VALIX                                                            
         DROP  R6                                                               
*&&                                                                             
*                                                                               
VALI0040 EQU   *                                                                
* DON'T VALIDATE IF FIRST TIME THROUGH                                          
* READ CAREFULLY: IF NOT PREVIOUSLY VALIDATED, SKIP VALIDATE!!!                 
* REASON: THE MONTH FLDS ARE PROTECTED, BUT AREN'T ON THE SCREEN UNTIL          
*         THE FIRST DISPLAY IS DONE, AT WHICH TIME ITS VALID BITS ARE           
*         SET.                                                                  
*                                                                               
         TM    RC$STRTH+4,X'20'    PRE-VALID?                                   
         BZ    VALIX                                                            
*                                                                               
* VALIDATE DATE FIELD                                                           
*                                                                               
         LA    R2,RC$ACDTH                                                      
         LA    R3,1                                                             
         CLI   5(R2),0             ANY ACTIVITY DATE?                           
         BE    ERROR               MISSING INPUT                                
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
*                                                                               
* VALIDATE TRAFFIC ORDER NUMBER FIELD                                           
*                                                                               
VALI0060 EQU   *                                                                
         LA    R2,RC$TONMH         TRAFFIC ORDER NUMBER FIELD REQUIRED          
         LA    R3,1014                                                          
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
*                                                                               
         XC    KEY,KEY                                                          
RCUKD    USING RCONCUTP,KEY                                                     
         MVI   RCUKD.RCONCUTP,X'AE'                                             
         MVC   RCUKD.RCONCURP,REPALPHA                                          
         MVC   RCUKD.RCONCUST,RCONKSTA                                          
         MVC   RCUKD.RCONCUT#,8(R2)      TRAFFIC ORDER NUMBER                   
         DROP  RCUKD                                                            
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
         CLC   KEY(23),KEYSAVE                                                  
         BNE   VALI0070                                                         
*                                                                               
         ZAP   WORK+30(5),=P'0'                                                 
         MVO   WORK+30(5),KEY+23(4)                                             
         ZAP   WORK+20(5),=P'99999999'                                          
         SP    WORK+20(5),WORK+30(5)                                            
         CLC   RCONKCON,KEY+23                                                  
         BE    VALI0070                                                         
         LA    R3,1023                                                          
         GOTO1 HEXOUT,DMCB,KEY+23,WORK,4                                        
         XC    DMCB(24),DMCB                                                    
         STCM  R3,3,DMCB+2                                                      
         GOTO1 GETTXT,DMCB,,,(C'E',0),(8,WORK),(X'44',0)                        
         OI    6(R2),X'40'         PUT CURSOR HERE                              
         L     RD,BASERD           INSTANT STACK UNWIND                         
         B     EXXMOD                                                           
*                                                                               
*                                                                               
* REMOVE PREVIOUS BUCKETS                                                       
*                                                                               
VALI0070 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0                
*                                                                               
         GOTO1 DATCON,DMCB,BLOCK,(2,DATE)                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BNE   VALI0080                                                         
         USING RCONCUEL,R6                                                      
         MVC   RCONCUMA,DATE       ACTIVITY DATE                                
         MVC   RCONCUW#,RC$TONM                                                 
         OC    RCONCUW#,MYSPACES                                                
         DROP  R6                                                               
*                                                                               
VALI0080 EQU   *                                                                
         L     R5,AIO3             R5 TO TOTAL BUCKETS                          
         LA    R4,MONTBL                                                        
         LA    R2,RC$STRTH         FIRST FLD HDR                                
VALI0100 DS    0H                  SET R4 TO FIRST CORRESPOND                   
         CLC   1(3,R4),8(R2)       W/FIRST MONTH ON SCREEN                      
         BE    VALI0160                                                         
         LA    R4,L'MONTBL(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   VALI0100                                                         
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
         LA    RE,RC$LAST          END OF SCREEN?                               
         CR    R2,RE                                                            
         BNL   VALI0900            EXIT                                         
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
         MVC   WORK(2),=X'030A'                                                 
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
*                                                                               
         MVC   WORK+6(4),AMOUNT    CHANGE AMOUNT                                
         B     VALI0440            ADD ELEMENT                                  
*                                                                               
VALI0440 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                     
         L     R5,AIO3             RESET R5 TO TOTAL BUCKETS                    
VALI0460 EQU   *                                                                
         OI    4(R2),X'20'         SET PREVALID                                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO NEXT BUCKET FLD                        
         CLI   8(R2),0             IS FIELD IN USE?                             
         BNH   VALI0900            NO, NO MORE FIELDS                           
         LA    RE,RC$LAST          END OF SCREEN?                               
         CR    R2,RE                                                            
         BNL   VALI0900            EXIT                                         
*                                                                               
         LA    R4,MONTBL           BACK TO FIRST MON                            
         B     VALI0100                                                         
*                                                                               
VALI0900 DS    0H                                                               
         CLI   CFLAG,1                                                          
         BE    VALIX                                                            
         LA    R2,RC$MONYH         FIRST FLD HDR                                
         LA    R3,1024                                                          
         B     ERROR                                                            
*                                                                               
VALIX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DISBUCK  NTR1                                                                   
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISB0020                                                         
         USING RCONCUEL,R6                                                      
         MVC   RC$TONM,RCONCUW#                                                 
         CLI   RCONCULN,RCONCULQ                                                
         BL    DISB0020                                                         
         GOTO1 DATCON,DMCB,(2,RCONCUMA),(5,RC$ACDT)                             
         DROP  R6                                                               
*                                                                               
DISB0020 DS    0H                                                               
         OI    RC$TONMH+4,X'20'    SET VALID                                    
         OI    RC$TONMH+6,X'80'    XMIT FLD                                     
         OI    RC$ACDTH+4,X'20'    SET VALID                                    
         OI    RC$ACDTH+6,X'80'    XMIT FLD                                     
*                                                                               
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
         LA    R2,RC$STRTH         FIRST FLD HDR                                
DISB0040 DS    0H                                                               
         MVC   8(3,R2),1(R4)       MMM                                          
         MVC   11(2,R2),YEAR       YY                                           
         OI    4(R2),X'20'         SET PRE-VALID ON MON FLD                     
         ZIC   R0,0(R2)            R2 TO INPUT FLD                              
         AR    R2,R0                                                            
         OI    4(R2),X'20'         SET PREVALID BIT                             
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT                             
*                                                                               
* DISPLAY $ (IF ANY)                                                            
         CLC   0(1,R5),BYEAR       THIS YEAR?                                   
         BNE   DISB0060                                                         
         CLC   1(1,R5),0(R4)       THIS MONTH?                                  
         BNE   DISB0060                                                         
         EDIT  (4,2(R5)),(10,8(R2)),2                                           
         OI    6(R2),X'80'         XMIT FLD                                     
         LA    R5,6(R5)            SET NEXT TOTAL BUCKET                        
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
OLDTRF#  DS    CL10                                                             
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
       ++INCLUDE RECNTC9D                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048RECNT5C   12/15/10'                                      
         END                                                                    
