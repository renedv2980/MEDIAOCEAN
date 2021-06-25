*          DATA SET RECNT7C    AT LEVEL 133 AS OF 08/23/02                      
*PHASE T8027CA                                                                  
*INCLUDE RETIMVAL                                                               
*INCLUDE REDAYVAL                                                               
         TITLE 'T8027C - REPPAK SPORTS BUY EDIT'                                
***********************************************************************         
*                                                                     *         
*     RECNT7C (T8027C) --- CONTRACT SPORTS BUY EDIT                   *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
* 23AUG02 SKU RATE FIELD BUG FIX                                      *         
* 10AUG00 RHV DOB                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T8027C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8027C,R9,R8                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
***>     OI    SBSDATEH+1,X'20'    PROTECT FLIGHT FIELD                         
***>     TM    TWASTREO,X'80'      IS THIS STEREO?                              
***>     BZ    *+8                 NO                                           
***>     NI    SBSDATEH+1,X'FF'-X'20'   YES, UNPROTECT FLIGHT                   
*                                                                               
         MVI   STAT2,0             CLEAR OUT STATUS BYTE                        
         LA    R2,CONBACTH                                                      
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  K DISK ADDR                                  
         MVI   UPDATE,C'Y'                                                      
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         SPACE 1                                                                
         LA    R2,CONBACTH                                                      
         LA    R3,BACERR                                                        
         TM    RCONCNTL,X'01'      COMPRESSED CONTRACT                          
         BO    ERROR                                                            
         SPACE 1                                                                
         TM    RCONMODR+1,X'80'    IF 'ACE' THERE ARE SOME                      
         BZ    BUED0040            SPECIAL TESTS TO BE DONE                     
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING RCONSEND,R6                                                      
*                                                                               
* CAN'T MAKE CHANGES IF OTHER SIDE IS IN PROCESS OF MAKING CHANGES              
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    BUED0030                                                         
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         TM    RCONSENF,X'10'      X'10'=STA. VERS. NOT ADVANCED                
         BZ    ERROR                                                            
         B     BUED0040                                                         
         DROP  R6                                                               
*                                                                               
BUED0030 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING RCONSEND,R6                                                      
         LA    R3,168              LATEST REP VERSION NOT YET SENT              
         TM    RCONSENF,X'20'      X'20'=REP VERS. NOT ADVANCED                 
         BZ    ERROR                                                            
         DROP  R6                                                               
*                                                                               
BUED0040 DS    0H                                                               
         CLC   BUYACT(3),=C'DEL'  DELETE?                                       
         BE    BUED0050                                                         
         CLC   BUYACT,=C'CAN'      CANCEL?                                      
         BNE   BUED0055                                                         
*                                                                               
BUED0050 DS    0H                                                               
         CLC   =C'DELDDS',CONBACT                                               
         BNE   BUED0053                                                         
         LA    R3,12               INVALID ACTION                               
         CLI   TWAOFFC,C'*'        DDS ONLY                                     
         BNE   ERROR                                                            
*                                                                               
BUED0053 DS    0H                                                               
         GOTO1 =A(DELBUY),DMCB,(RC),RR=Y                                        
         B     EXXMOD                                                           
*                                                                               
BUED0055 DS    0H                                                               
         CLC   CONADV(3),=C'GEN'   DON'T ALLOW BUYS WHEN ADV=GEN &              
         BNE   BUED0060                                                         
         CLC   CONCAT,=C'ZZ'       CATEGORY=ZZ (I.E. GENERAL AVAILS)            
         BNE   BUED0060                                                         
         LA    R3,192                                                           
         B     ERROR                                                            
         SPACE 1                                                                
*              MUST BE NEW BUY                                                  
         SPACE 1                                                                
BUED0060 EQU   *                                                                
         MVI   ECFORMAT,C' '       CLEAR EC FORMAT                              
         CLI   RCONKSTA+4,C' '     TV STATION?                                  
         BE    BUED0160            YES                                          
         CLI   RCONKSTA+4,C'L'     TV STATION?                                  
         BE    BUED0160            YES                                          
*                                                                               
BUED0080 EQU   *                   FORCE BOP BEFORE BUYS ON NEW CONT.           
         CLC   BUYACT(3),=C'CHA'                                                
         BE    BUED0140                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    BUED0140                                                         
         BAS   RE,GETSTAT                                                       
         B     BUED0120                                                         
         EJECT                                                                  
*                                                                               
GETSTAT  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           BUILD STATION KEY AND GET RECORD             
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         SPACE                                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GEST0100                                                         
         DC    H'0'                                                             
GEST0100 GOTO1 VGETREC,DMCB,IOAREA                                              
         XIT1                                                                   
*                                                                               
BUED0120 EQU   *                                                                
         TM    RSTASTAT,X'80'      BOP CHECK OVERRIDE                           
         BO    BUED0140            YES                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    BUED0140            FOUND BOP                                    
         BAS   RE,ISTAKOV                                                       
         BZ    BUED0140                                                         
         LA    R3,BOPERR                                                        
         B     ERROR                                                            
*                                                                               
BOPERR   EQU   173                 MISSING BOP DATA                             
*                                                                               
BUED0140 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0200            NO EPL ELEMENT                               
         USING RCONSPEL,R6                                                      
         CLC   RCONSPAM(3),=C'CAN' NO BUYS ALLOWED ON CANCELLED                 
         BNE   BUED0200            CONTRACTS                                    
         LA    R3,190                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
BUED0160 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0180            NO SAR ELEMENT                               
*                                                                               
         USING RSAREL,R6                                                        
         OC    RSARRDT,RSARRDT     CONTRACT RESOLVED?                           
         BNZ   BUED0180            YES - FIELD CONTAINS DATE                    
*                                                                               
         MVC   RSARRDT,TODAY       NO  - RESOLVE CONTRACT                       
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     BUED0180                                                         
         DROP  R6                                                               
*                                                                               
BUED0180 DC    0H                                                               
         BAS   RE,GETSTAT          GET STATION FOR EC FORMAT                    
         MVC   ECFORMAT,RSTATRAF   SAVE EC FORMAT                               
         CLI   TWAACCS,C'$'        IF STATION, GO DIRECTLY TO GET               
         BE    CHGBUY              BUY RECORD, THEN TO T80216                   
*                                                                               
BUED0200 CLC   BUYACT(3),=C'CHA'  CHANGE?                                       
         BE    CHGBUY                                                           
*                                                                               
         MVI   RBUYLEN+1,77        ELEM LENGTH (34 + 43)                        
         MVC   RBUYCODE(2),=X'012B'     BUY DESC ELEM CODE + LEN                
*                                                                               
* MARK BUYREC WITH CORRECT SPORTS BUY FLAG                                      
*                                                                               
         GOTO1 (RFGETTAB,VREPFACS),DMCB,('RTSPORTS',0)                          
         BE    *+6                                                              
         DC    H'0'                          MUST BE THERE                      
         L     R2,0(R1)                                                         
         MVC   HALF,0(R2)                    TABLE ENTRY LENGTH                 
         LA    R2,4(R2)                      START OF TABLE                     
BUED0210 DS    0H                                                               
         CLI   0(R2),X'FF'                   BUYACT NOT MATCHED                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BUYACT,0(R2)                                                     
         BE    BUED0220                                                         
         AH    R2,HALF                                                          
         B     BUED0210                                                         
*                                                                               
BUED0220 DS    0H                                                               
         MVI   RBUYSFG,X'40'       SPORTS FLAG                                  
         MVC   RBUYSTYP,67(R2)     SPORTS TYPE INDICATOR                        
*                                                                               
* SET OFF PREVIOUSLY VALID BITS FOR ADD (IN CASE ERROR)                         
*                                                                               
         LA    R2,SBSDATEH                                                      
         LA    R3,SBSLAST                                                       
         SR    RE,RE                                                            
*                                                                               
         NI    4(R2),X'DF'                                                      
         IC    RE,0(R2)            LEN                                          
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3                                                            
         BL    *-14                                                             
         FOUT  CONBNUMH,MYSPACES,8                                              
         NI    CONBNUMH+4,X'DF'    TURN OFF VALID BITS IN CASE OF ERROR         
         B     DATEDIT                                                          
         EJECT                                                                  
********************************************************************            
* CHECK IF CONTRACT IS A TAKEOVER CONTRACT                                      
********************************************************************            
ISTAKOV  NTR1                                                                   
         XC    WORK2,WORK2                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         USING RCONCMEL,R6                                                      
         ZIC   R1,RCONCMLN                                                      
         SH    R1,=H'3'                                                         
         BM    TKOVNO                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONCMNT                                                
         DROP  R6                                                               
*                                                                               
         CLC   =C'C=TO',WORK2      COMMET MUST BE C=TOXXXX-X                    
         BNE   TKOVNO              WHERE XXXX-X IS THE STATION                  
         MVC   WORK(6),WORK2+4                                                  
                                                                                
         CLC   =C'-TV',CONSTA+4                                                 
         BE    TKOV20                                                           
         CLC   =C'-TV',CONSTA+3    CHECK INCASE OF 3 LETTER CALLS               
         BE    TKOV30                                                           
         CLC   =C'-C',WORK2+7      CHECK IF COMBO                               
         BE    TKOV50                                                           
         CLC   =C'-C',WORK2+8                                                   
         BE    TKOV50                                                           
                                                                                
* COMPARE FOR RADIO                                                             
         CLI   WORK+5,C' '         CHECK INCASE OF 3 LETTER CALLS               
         BNE   TKOV10                                                           
         MVI   WORK+5,C'M'                                                      
                                                                                
TKOV10   DS    0H                                                               
         CLC   WORK(6),CONSTA                                                   
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* COMPARE FOR TV                                                                
TKOV20   DS    0H                  TV CAN BE SPECIFIED AS                       
         CLC   WORK(4),CONSTA      XXXX OR XXXX-T                               
         BNE   TKOVNO                                                           
         CLC   WORK+4(2),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+4                                                    
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* AND 3 LETTER CALLS                                                            
TKOV30   DS    0H                                                               
         CLC   WORK(3),CONSTA                                                   
         BNE   TKOVNO                                                           
         CLC   WORK+3(3),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+3                                                    
         BE    TKOVYES                                                          
         B     TKOVNO                                                           
                                                                                
* CHECK FOR COMBO TAKEOVER                                                      
TKOV50   DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
                                                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TKOVNO                                                           
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         CLI   WORK+3,C'-'         TAKEOVER COMMENT STATION MUST MATCH          
         BE    TKOV60              PARENT STATION                               
         CLC   RSTACS(4),WORK                                                   
         BNE   TKOVNO                                                           
                                                                                
TKOV60   DS    0H                  INCASE PARENT'S CALL IS 3 LETTERS            
         CLC   RSTACS(3),WORK                                                   
         BE    TKOVYES                                                          
         DROP  R6                                                               
                                                                                
TKOVNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
TKOVYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* CHANGE BUY                                                                    
*                                                                               
CHGBUY   DC    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWABADDR  BUY DISK ADDR                                
         DROP  RF                                                               
*                                                                               
         TM    STAT2,X'40'         UPDATE ALREADY IN PROGRESS?                  
         BO    CHGB0025            YES - SKIP RE-READ                           
*                                                                               
         LA    R2,SBSDATEH                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    RF,IOAREA                                                        
*                                                                               
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC  MOVE TO BUY AREA                   
*        LA    R6,RBUYREC                                                       
*        LA    R6,1000(R6)                                                      
         L     R6,AIO2                                                          
         GOTO1 VMOVEREC,DMCB,IOAREA,(R6)                                        
*                                                                               
*   IF STATION, ALL FIELDS ARE PROTECTED EXCEPT ORDER COMMENT FIELD,            
*   THEREFORE, EDIT ONLY ORDER COMMENT FIELD                                    
*                                                                               
CHGB0025 DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    BUYED2                                                           
*                                                                               
*              IF EITHER COMMENT CHANGES REVALIDATE BOTH                        
*                                                                               
         TM    SBSCMT1H+4,X'20'    COMMENT 1 CHANGE?                            
         BO    *+8                                                              
         NI    SBSCMT2H+4,X'DF'    COMMENT 2                                    
*                                                                               
         TM    SBSCMT2H+4,X'20'    COMMENT 2 CHANGE?                            
         BO    *+8                                                              
         NI    SBSCMT1H+4,X'DF'    COMMENT 1                                    
*                                                                               
* EDIT DATE FIELD                                                               
*                                                                               
DATEDIT  DS    0H                                                               
         LA    R2,SBSDATEH                                                      
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    BUYED2                                                           
*                                                                               
* RE-VALIDATE RATE FIELD IF DATES CHANGED                                       
*                                                                               
         NI    SBSCOSTH+4,X'FF'-X'20'                                           
*                                                                               
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
*                                                                               
* +/- WEEK(S) TO CURRENT DATES?                                                 
*                                                                               
***>     GOTO1 =A(DTADDSUB),DMCB,(RC),RR=Y                                      
*                                                                               
* DELETE OLD '02' & '03' ELEMS                                                  
*                                                                               
         GOTO1 VDELELEM,DMCB,(3,RBUYREC)                                        
         GOTO1 VDELELEM,DMCB,(2,RBUYREC)                                        
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
*                                                                               
         MVC   WORK2(2),=X'030B'   DATE ELEM CODE + LEN                         
*                                                                               
         MVC   WORK3(2),=X'0209'   ELEM CODE + LENGTH                           
         MVI   WORK3+8,1           WEIGHTING FACTOR                             
         MVC   WORK3+4(4),=X'FFFFFFFF'                                          
*                                                                               
*              GET LOWEST START DAY AND HIGHEST END DAY IN DAY ELEMENTS         
***>     SR    R5,R5                                                            
***>     ZIC   R4,RBUYSTED                                                      
***>     SRDL  R4,4                                                             
***>     SRL   R5,28                                                            
***>     STM   R4,R5,DMCB+16                                                    
*                                                                               
         XC    WORK+24(6),WORK+24  FOR CONSECUTIVE TEST                         
*                                                                               
* EDIT START DATE                                                               
*                                                                               
         LA    R7,SBSDATEH+7     FOR SCAN (BACKUP 1 BYTE)                       
         ST    R7,DMCB+12                                                       
*                                                                               
STARTED  MVC   DMCB(4),DMCB+12                                                  
         LA    R3,SDTERR                                                        
         GOTO1 SCAN,DMCB,,SBSDATEH  SCAN FOR NEXT DATE FIELD                    
*                                                                               
         CLI   DMCB,0              NONE?                                        
         BNE   DATE50                                                           
*                                                                               
         OC    WORK+24(6),WORK+24  NO DATES GIVEN?                              
         BZ    ERROR                                                            
         B     DATE240                                                          
*                                                                               
DATE50   L     R5,DMCB             FIELD ADDR                                   
         MVC   DMCB+12(4),DMCB     SAVE LEN + ADDR FOR SCAN                     
         CLC   0(2,R5),=C'S-'      K START INDICATOR                            
         BNE   DATE75                                                           
*&&DO                                                                           
*              DETERMINE DATE IN FIRST WEEK OF CONTRACT                         
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
*                                                                               
         CLC   FULL(3),MYSPACES    VALID K START DATE?                          
         BNE   *+6                                                              
         DC    H'0'                K ERROR                                      
*                                                                               
         ZIC   RE,DMCB             DAY OF WEEK                                  
         L     R4,DMCB+16          BUY START DAY                                
         SR    R4,RE                                                            
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            NEXT WEEK                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4) GET 4ST DATE                        
*&&                                                                             
         MVC   WORK+12(6),WORK     USE FLIGHT START DATE                        
*                                                                               
         LA    R5,2(R5)            NEXT FIELD                                   
         B     DATE150                                                          
*                                                                               
DATE75   DS    0H                  EDIT START DATE                              
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+12                                     
*                                                                               
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
*                                                                               
         LA    R5,1(R7,R5)         NEXT FIELD                                   
*                                                                               
         MVC   WORK+12(2),WORK     K START YEAR                                 
         CLC   WORK+14(4),WORK+2   BUY MMDD V K MMDD                            
         BNL   DATE100                                                          
*                                                                               
DATE90   CLC   WORK(2),WORK+6      K START AND END YEARS SAME?                  
         BE    ERROR                                                            
         MVC   WORK+12(2),WORK+6   USE K END YEAR                               
*                                                                               
DATE100  DS    0H                                                               
***>     GOTO1 GETDAY,DMCB,WORK+12,FULL VALIDATE START DAY                      
***>     CLC   FULL(3),MYSPACES                                                 
***>     BE    ERROR                                                            
***>     LA    R3,SDYERR                                                        
***>     ZIC   R4,DMCB             START DAY                                    
***>     C     R4,DMCB+16          SAME AS 1ST DAY?                             
***>     BE    DATE150                                                          
*                                                                               
***>     CLC   WORK+12(2),WORK+6   BUY YEAR SAME AS END YEAR                    
***>     BE    ERROR                                                            
***>     B     DATE90              FOR CONTRACTS MORE THAN 1 CALENDER           
*                                                                               
DATE150  XC    WORK2+8(2),WORK2+8                                               
         CLC   WORK+12(6),WORK+6   BUY START V K END                            
         BNH   *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
         CLC   WORK+12(6),WORK+24  BUY START DATE V LAST ELEM END DATE          
         BH    *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
*                                                                               
         CLI   0(R5),C'E'          -E INDICATOR?                                
         BNE   DATE175                                                          
         MVC   WORK+18(6),WORK+6   USE FLT END DATE                             
*                                                                               
         LA    R5,1(R5)                                                         
         B     DATE200                                                          
*                                                                               
DATE175  EQU   *                   EDIT END DATE                                
         CLI   0(R5),C'*'          NO END DATE?                                 
***>     BE    DATE177                                                          
         BE    DATE176                                                          
*                                                                               
         LR    R6,R5                                                            
         BCTR  R6,R0                                                            
         CLI   0(R6),C'*'                                                       
         BE    DATE176                                                          
         CLI   0(R6),C'('                                                       
         BE    DATE176                                                          
         CLI   0(R6),0                                                          
         BE    DATE176                                                          
         CLI   0(R6),C' '                                                       
         BNE   DATE180                                                          
*                                                                               
DATE176  DS    0H                  NO END DATE GIVEN                            
         MVC   WORK+18(6),WORK+12  1 DAY (START=END)                            
         B     DATE210                                                          
*                                                                               
DATE180  GOTO1 DATVAL,DMCB,(1,(R5)),WORK+18 END DATE                            
*                                                                               
         L     RE,DMCB             LENGTH                                       
         LTR   RE,RE                                                            
         BNZ   DATE199A                                                         
*                                                                               
* CHECK FOR END WEEKS OPTION                                                    
         LA    R4,1                                                             
         CLI   1(R5),C'W'          WEEKS IND?                                   
         BE    DATE193                                                          
         LA    R4,2                                                             
         CLI   2(R5),C'W'                                                       
         BE    *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
* W HAS BEEN ENTERED - PACK WEEKS                                               
DATE193  LR    R7,R5                                                            
         LR    R3,R5                                                            
         LA    R5,1(R4,R5)         END OF FIELD                                 
         LR    R6,R4                                                            
*                                                                               
DATE195  CLI   0(R7),X'F0'         NUMERIC?                                     
         BNL   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         CLI   0(R7),X'F9'                                                      
         BNH   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         LA    R7,1(R7)                                                         
         BCT   R4,DATE195                                                       
* NUMERIC WEEKS ENTERED                                                         
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R3)                                                      
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         MVC   WORK+30(6),WORK+12   START DATE                                  
         MVC   WORK+18(6),WORK+12  START TO END                                 
         OI    WORK2+8,X'80'       EVERY WEEK                                   
         LA    R3,7                                                             
***>>    BCTR  R4,R0               NUMBER OF WEEKS                              
         LTR   R4,R4                                                            
         BZ    DATE199                                                          
*                                                                               
* TEST FOR ALTERNATE WEEKS                                                      
         CLI   0(R5),C'A'                                                       
         BNE   DATE198                                                          
         OI    WORK2+8,X'40'                                                    
         NI    WORK2+8,X'7F'                                                    
         LA    R5,1(R5)                                                         
         LA    R3,14                                                            
* GET NEXT WEEK                                                                 
DATE198  GOTO1 ADDAY,DMCB,WORK+30,WORK+18,(R3)                                  
         MVC   WORK+30(6),WORK+18                                               
         BCT   R4,DATE198          GET NUMBER OF WEEKS-1                        
         GOTO1 ADDAY,DMCB,WORK+30,WORK+18,-1                                    
         MVC   WORK+30(6),WORK+18                                               
*                                                                               
* GET DAY SPAN FOR WEEK                                                         
DATE199  DS    0H                                                               
***      L     R6,DMCB+20          END DAY OF WEEK                              
***      C     R6,DMCB+16          END V START DAY                              
***      BNL   *+8                                                              
***      LA    R6,7(R6)            OUT OF WEEK ROTATOR                          
***      S     R6,DMCB+16          GET DAY SPAN                                 
***      GOTO1 ADDAY,(R1),WORK+30,WORK+18,(R6)                                  
         B     DATE215                                                          
*                                                                               
* END DATE IS VALID MONTH-DAY                                                   
DATE199A MVC   WORK+18(2),WORK+6   K END YEAR                                   
         CLC   WORK+20(4),WORK+8   BUY END MMDD V K END MMDD                    
         BNH   *+10                                                             
         MVC   WORK+18(2),WORK     MOVE K START YEAR                            
*                                                                               
         LA    R5,0(RE,R5)         FIELD END                                    
*                                                                               
DATE200  CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DATE210                                                          
         LA    R5,1(R5)                                                         
         OI    WORK2+8,X'40'                                                    
         B     *+8                                                              
DATE210  OI    WORK2+8,X'80'       EVERY WEEK                                   
DATE215  LA    R3,EDTERR                                                        
         CLC   WORK+18(6),WORK+6   BUY END V K END DATE                         
         BH    ERROR                                                            
*                                                                               
         CLC   WORK+18(6),WORK+12  BY END V BUY START                           
         BL    ERROR                                                            
*                                                                               
         MVC   WORK+24(6),WORK+18  SAVE BUY END DATE FOR CONSECUTIVE            
*                                  TEST                                         
* NOW GET TOTAL WEEKS                                                           
DATE230  SR    R7,R7               CTR                                          
         MVC   WORK+30(6),WORK+12  START                                        
*                                                                               
         LA    R3,7                                                             
         TM    WORK2+8,X'40'       ALT?                                         
         BZ    *+8                                                              
         LA    R3,14                                                            
DATE235  LA    R7,1(R7)                                                         
         GOTO1 ADDAY,DMCB,WORK+30,WORK+36,(R3)                                  
         MVC   WORK+30(6),WORK+36                                               
*                                                                               
         CLC   WORK+30(6),WORK+18  PAST END?                                    
         BNH   DATE235                                                          
*                                                                               
         STC   R7,WORK2+10                                                      
*              CONVERT DATES FOR BUYREC                                         
         GOTO1 DATCON,DMCB,WORK+12,(3,WORK2+2)    START DATE                    
*                                                                               
         GOTO1 (RF),(R1),WORK+18,(3,WORK2+5)      END DATE                      
*                                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2  ADD 03 ELEM TO BUYREC               
*                                                                               
         GOTO1 GETDAY,DMCB,WORK+12,FULL  FIGURE START & END DAYS                
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,0(R1)            START DAY                                    
         GOTO1 GETDAY,DMCB,WORK+18,FULL                                         
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R5,0(R1)            END                                          
*                                                                               
         STC   R5,WORK3+2                                                       
         LR    RF,R4                                                            
         SLL   RF,4                                                             
         STC   RF,BYTE                                                          
         OC    WORK3+2(1),BYTE     START-END DAY INDICATOR                      
*                                                                               
         MVC   HALF(1),RBUYSTED                                                 
         NI    HALF,X'FF'-X'0F'    ONLY START DAY BITS                          
*                                                                               
         STC   R5,HALF+1                                                        
         NI    RBUYSTED,X'FF'-X'F0' ONLY END DAY BITS                           
         CLC   HALF+1(1),RBUYSTED                                               
         BL    *+10                                                             
         MVC   RBUYSTED,HALF+1     HIGHEST END DAY                              
*                                                                               
         CLI   HALF,0                                                           
         BE    *+20                                                             
         CLC   HALF(1),BYTE                                                     
         BH    *+10                                                             
         MVC   BYTE,HALF           LOWEST START DAY                             
         OC    RBUYSTED,BYTE       TOGETHER NOW                                 
*                                                                               
** THE FOLLOWING IS A SMALL BUT COMPLICATED ALGORITHM TO GENERATE THE           
** DAY OF WEEK INDICATOR FLAG BYTE                                              
         CR    R4,R5               OOW?                                         
         BNH   *+8                 NO                                           
         LA    R5,7(R5)            YES                                          
         SR    R5,R4               DAYS DIFF                                    
         LA    R5,1(R5)            =DAYS INCLUSIVE                              
         CHI   R5,7                CAN'T BE MORE THAN A 7 DAY WEEK!             
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   FULL(3),=X'884000'  **THIS IS AN INSTRUCTION FOR 'EX'**          
         STC   R4,FULL+3           **IT IS A SRL INSTRUCTION        **          
         SR    R4,R4                                                            
         SRL   R4,1                SLIDE BITS BY 1                              
         O     R4,=X'00008000'     TURN ON ANOTHER BIT                          
         BCT   R5,*-8              FOR TOTAL NUMBER OF DAYS INCLUSIVE           
         EX    R0,FULL             SHIFT TO THE START DAY                       
         LR    R5,R4               SAVE A COPY OF R4                            
         ICM   R4,2,=X'00'                                                      
         SRL   R4,1                                                             
         SRL   R5,8                                                             
         OR    R4,R5               CASE OF OOW, WE USED 2 BYTES                 
         STC   R4,WORK3+3          DAY INDICATORS                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK3  ADD 02 ELEM TO BUYREC               
*                                                                               
***>     XC    WORK,WORK                                                        
         MVC   DMCB(24),DMCB2      RESTORE                                      
         B     STARTED             SEE IF ANOTHER DATE ENTRY                    
*                                                                               
DATE240  EQU   *                                                                
BUYED2   DS    0H                  THIS WAS CNT16                               
         MVI   BSTATUS,0           CLEAR OUT STATUS BYTE                        
*                                                                               
*  FOR STATION, DEAL WITH ORDER COMMMENT                                        
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   ADDSPOT                                                          
         SPACE 1                                                                
         LA    R4,2                BCT THROUGH 2 COMMENT LINES                  
         LA    R2,SBSOCM1H                                                      
         TM    4(R2),X'20'         HAS 1ST LINE CHANGED                         
         BZ    STA5                                                             
         LA    R2,SBSOCM2H                                                      
         TM    4(R2),X'20'         HAS 2ND LINE CHANGED                         
         BO    EXXMOD              NEITHER CHANGED, GET OUT                     
         LA    R2,SBSOCM1H         OTHERWISE, REDO BOTH LINES                   
         SPACE 1                                                                
STA5     OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)  YES, DELETE OLD                   
         SPACE 1                                                                
         CLI   5(R2),0            IF EITHER COMMENT LINE IS                     
         BNE   STA7                BLANK, DON'T ADD BLANK ELEMENT               
         LA    R2,SBSOCM2H                                                      
         LA    R4,1                ONLY BCT ONCE                                
         CLI   5(R2),0                                                          
         BE    STA10                                                            
         SPACE 1                                                                
STA7     CLC   8(3,R2),=C'#DS'     STATION NOT ALLOWED TO USE                   
         BNE   *+12                'DON'T SEND' FEATURE                         
         LA    R3,2                INVALID INPUT FLD                            
         B     ERROR                                                            
         XC    WORK2(100),WORK2    AND REBUILD NEW ELEMENT                      
         MVI   WORK2,X'84'                                                      
*   WORK2+2 IS ZERO TO DESIGNATE STATION BUY ORDER COMMENT                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+3(0),8(R2)                                                 
         LA    RE,4(RE)            TOTAL LENGTH                                 
         STC   RE,WORK2+1                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         SPACE 1                                                                
         LA    R2,SBSOCM2H         LOOK AT 2ND COMMENT LINE                     
         CLI   5(R2),0                                                          
         BE    STA10                                                            
         BCT   R4,STA7                                                          
         SPACE 1                                                                
* UNCONFIRM CONTRACT AND UPDATE VERSION NUMBER IF NECESSARY                     
*                                                                               
STA10    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THERE SHOULD ALREADY BE A '1F'               
                                                                                
         USING RCONXEL,R6                                                       
         OI    RCONCONF,X'80'      TURN ON NOT CONFIRMED                        
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    STA11                                                            
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'       TURN ON CONFIRMED PREVIOUSLY                
         DROP  R6                                                               
                                                                                
STA11    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    STA12                                                            
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
                                                                                
STA12    DS    0H                                                               
         TM    RCONSENF,X'10'      ON MEANS STA VERSION NOT ADVANCED            
         BZ    STA20                                                            
*                                                                               
* ADVANCE STATION VERSION AND UPDATE VERSION DATES                              
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    STA15                                                            
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
STA15    DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'S',RCONREC)                                     
         BNZ   ERROR                                                            
                                                                                
STA20    DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSSV          STORE VERSION NUMBER IN BUY                  
         DROP  R6                                                               
         STC   R1,RBUYVER                                                       
         OI    TAREQ,X'01'         T/A REQ INDICATOR- TO PUT CONTRACT           
         MVI   BYTE,C'O'           UPDATE BUY CHANGE INDICATOR                  
         BAS   RE,ADDCODE                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
ADDSPOT  DS    0H                                                               
*                                                                               
         LA    R3,265                                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    ADDSP000            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    ADDSP010            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
ADDSP000 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   TYPEDIT                                                          
*                                                                               
ADDSP010 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'        X'08' ELEMENT MUST BE UNIQUE                 
         BAS   RE,GETEL                                                         
         BE    TYPEDIT                                                          
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
*                                                                               
         CLI   TWASPES,0                                                        
         BNE   ADDSP10                                                          
                                                                                
* PROFILE TO ALLOW SPOTPAK INTERFACE DATA                                       
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    TYPEDIT             IF OFF, SKIP SPOTPAK INTERFACE ADD           
         B     ERROR                                                            
                                                                                
ADDSP10  DS    0H                                                               
         XC    WORK2,WORK2                                                      
WK2      USING RBUYSPEL,WORK2                                                   
         MVC   WK2.RBUYSPCD(2),=X'0830'                                         
         MVC   WK2.RBUYSPAG,TWASPAG    SPOTPAK AGENCY POWER CODE                
         MVC   WK2.RBUYSPMD,TWASPMD    SPOTPAK MEDIA CODE                       
         MVC   WK2.RBUYSPCL,TWASPCL    SPOTPAK CLIENT CODE                      
         MVC   WK2.RBUYSPPD,TWASPPD    SPOTPAK PRODUCT CODE                     
         MVC   WK2.RBUYSPES,TWASPES    SPOTPAK ESTIMATE NUMBER                  
         MVC   WK2.RBUYSPPP,TWASPPP    SPOTPAK PIGGY PRODUCT CODE               
         MVC   WK2.RBUYSPP1,TWASPP1    SPOTPAK PRODUCT 1 SPLIT                  
         MVC   WK2.RBUYSPP2,TWASPP2    SPOTPAK PRODUCT 2 SPLIT                  
         MVC   WK2.RBUYSPST,RCONKSTA   STATION CALL LETTERS                     
         MVC   WK2.RBUYSADV,RCONKADV   REPPAK ADVERTISER CODE                   
         MVC   WK2.RBUYSPRD,RCONPRD    REPPAK PRODUCT CODE                      
         DROP  WK2                                                              
         DROP  RF                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2   ADD SPOTPAK INTERFACE ELEM         
         B     TYPEDIT                                                          
*                                                                               
TYPEDIT  DS    0H                                                               
RATEDIT  DS    0H                                                               
         MVC   RBUYKPLN,=3X'FF'    NO PLANS FOR SPORTS BUYS                     
         MVI   RBUYNW,1            NPW ALWAYS 1 FOR SPORTS BUYS                 
*                                                                               
         LA    R2,SBSCOSTH         RATE EDIT                                    
         TM    4(R2),X'20'                                                      
         BO    DESCEDIT                                                         
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R3,RATERR                                                        
         XC    RBUYCOMB,RBUYCOMB   DEFAULT BUY TO NON-COMBO                     
         XC    RBUYCOS,RBUYCOS                                                  
         CLI   5(R2),0                                                          
         BE    ADD10                                                            
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
         GOTO1 CASHVAL,DMCB,8(R2)                                               
         CLI   DMCB,X'FF'          ERROR?                                       
         BE    ERROR                                                            
***>     OC    DMCB+4(4),DMCB+4                                                 
***>     BZ    ADD10               0 COST                                       
         SR    R5,R5                                                            
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE AN 03 ELEM BY NOW!                 
RATED10  ZIC   RF,RBUYDTWK-RBUYDTEL(R6)                                         
         AR    R5,RF                                                            
         BAS   RE,NEXTEL                                                        
         BE    RATED10                                                          
         SR    R2,R2               PREP FOR DIVISION                            
         L     R3,DMCB+4                                                        
         ST    R3,RBUYTCOS         ALSO SAVE ORIGINAL ENTERED COST              
         LTR   R3,R3                                                            
         BZ    *+6                                                              
         DR    R2,R5                                                            
         ST    R3,RBUYCOS                                                       
*                                                                               
ADD10    DS    0H                                                               
         CLC   BUYACT(3),=C'CHA'  CHANGE?                                       
         BE    DESCEDIT                                                         
* GET LINE NUMBER FOR NEW BUY                                                   
         MVI   RBUYKTYP,X'0B'      BUY KEY TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  RF                                                               
         OI    DMINBTS,X'08'       GET DELETED RECORDS                          
*              FIND NEXT LINE NUMBER                                            
         XC    HALF,HALF           LINE NUMBER                                  
         MVC   KEY(22),RBUYREC                                                  
         XC    KEY+22(10),KEY+22                                                
         GOTO1 VHIGH                                                            
*                                                                               
ADD50    CLC   KEY(22),KEYSAVE                                                  
         BNE   ADD100                                                           
         CLI   KEY+26,255          PLANREC?                                     
         BE    ADD75                                                            
         CLC   HALF+1(1),KEY+26                                                 
         BNL   *+10                                                             
         MVC   HALF+1(1),KEY+26    HIGHEST LINE NUMBER SO FAR                   
         SPACE 1                                                                
ADD75    OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         B     ADD50                                                            
*                                                                               
ADD100   LH    RE,HALF             LAST LINE NUMBER                             
         LA    RE,1(RE)                                                         
         CH    RE,=H'254'                                                       
         BNH   *+16                                                             
         LA    R2,CONBACTH                                                      
         LA    R3,MAXERR                                                        
         B     ERROR                                                            
*                                                                               
         STC   RE,RBUYKLIN         BUY LINE NUMBER                              
         STC   RE,RBUYKMLN         IN CASE NO MAKE-GOOD                         
         NI    DMINBTS,X'F7'       TURN OFF DELETE PASS                         
         EJECT                                                                  
*                                                                               
* DESCRIPTIVE EDIT                                                              
*                                                                               
DESCEDIT LA    R2,SBSDCF1H                                                      
         TM    4(R2),X'20'                                                      
         BO    DESCE50                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VDELELEM,DMCB,(X'81',RBUYREC)                                    
*                                                                               
         MVI   WORK2,X'81'         ELEM CODE                                    
         CLI   5(R2),0             ENTRY?                                       
         BE    DESCE50                                                          
         MVC   WORK2+2(L'SBSDCF1),8(R2)                                         
         IC    RE,5(R2)            LEN OF INPUT                                 
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          LEN                                          
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
*                                                                               
DESCE50  LA    R2,SBSDCF2H                                                      
         TM    4(R2),X'20'                                                      
         BO    COMEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VDELELEM,DMCB,(X'82',RBUYREC)                                    
*                                                                               
         MVI   WORK2,X'82'         ELEM CODE                                    
         CLI   5(R2),0             ENTRY?                                       
         BE    COMEDIT                                                          
         MVC   WORK2+2(L'SBSDCF1),8(R2)                                         
         IC    RE,5(R2)            LEN OF INPUT                                 
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          LEN                                          
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
*                                                                               
* COMMENTS EDIT                                                                 
*                                                                               
COMEDIT  LA    R2,SBSCMT1H                                                      
         TM    4(R2),X'20'                                                      
         BO    ORDCOM                                                           
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
*                                                                               
         GOTO1 VDELELEM,DMCB,(4,RBUYREC)                                        
*                                                                               
         MVC   RBUYKMLN,RBUYKLIN   FOR CHANGE IF MG REF ELIMINATED              
         MVI   WORK2,4             ELEM CODE                                    
         CLI   5(R2),0             ENTRY?                                       
         BE    COED1720                                                         
         LA    R3,880                                                           
         CLC   =C'MG=',8(R2)       MG= NOT ALLOWED                              
         BE    ERROR                                                            
         CLC   =C'CR=',8(R2)       CR= NOT ALLOWED                              
         BE    ERROR                                                            
         MVC   WORK2+2(L'SBSCMT1),8(R2)                                         
         IC    RE,5(R2)            LEN OF INPUT                                 
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          LEN                                          
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
*                                  ADD COMMENT 1                                
COED1720 EQU   *                                                                
         LA    R2,SBSCMT2H                                                      
         LA    R3,MGLERR                                                        
         CLI   5(R2),0             ENTRY?                                       
         BE    ORDCOM                                                           
         MVC   WORK2+2(L'SBSCMT2),8(R2)                                         
         IC    RE,5(R2)            LEN                                          
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          LEN                                          
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
*                                  ADD COMMENT 2                                
*                                                                               
*  NOTE: A NEW BUY ORDER COMMENT IS REQUIRED UNLESS THE CONTRACT HAS            
*        NEVER BEEN SENT (VERSION 1) OR UNLESS THE ONLY CHANGE MADE             
*        WAS A CHANGE TO PROGRAM TYPE (STAT2 NOT X'80')                         
*                                                                               
*  ****  SKIP BUY ORDER COMMENT VALIDATION IF ACTION IS MAKEGOOD APPLY          
*        SINCE NO ORDER COMMENTS WILL BE PASSED                                 
*                                                                               
*        A BUY LINE CAN BE DESIGNATED 'DON'T SEND' IF                           
*         A. IT'S AN ACE OR GRAPHNET CONTRACT AND                               
*         B. IT'S NOT VERSION 1 AND                                             
*         C. THE CHARACTERS IN QUOTES '#DS' ARE THE 1ST 3 CHARACTERS            
*            ON THE 1ST REP BUY ORDER COMMENT LINE AND                          
*         D. THE STATION IS DESIGNATED OK FOR 'DON'T SEND' BUYLINES             
*                                                                               
ORDCOM   DS    0H                                                               
         TM    STAT2,X'80'         CHANGES MADE TO REQ NEW ORD CMT?             
         BZ    ORDCOMD             NO                                           
         TM    PROFILES+CNTBOCMB,CNTBOCMA  PROF#35?                             
         BZ    ORDCOM2             NO - ORD CMT REQ                             
*                                                                               
         XC    RSTAREC(32),RSTAREC CHECK STAREAC FOR OVERRIDE                   
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   ORDCOMD             NO ELEM - OCM NOT REQ                        
         TM    RSTAOPTA,X'08'      STA OPT #14?                                 
         BO    ORDCOM2             YES OCM REQUIRED                             
         DROP  R6                                                               
*                                                                               
ORDCOMD  DS    0H                                                               
         LA    R2,SBSOCM1H         NO                                           
         TM    4(R2),X'20'         HAVE ORD CMTS CHANGED                        
         BZ    ORDCOM1                                                          
         LA    R2,SBSOCM2H                                                      
         TM    4(R2),X'20'                                                      
         BO    ENDCOM                                                           
*                                                                               
ORDCOM1  OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
ORDCOM2  LA    R2,SBSOCM1H         BUY ORDER COMMENT                            
         LA    R4,2                BCT THROUGH 2 COMMENT LINES                  
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ORDCOM7                                                          
         CLI   5(R2),0             NO INPUT ON 1ST LINE                         
         BNE   ORDCOM3A                                                         
*                                                                               
         LA    R2,SBSOCM2H         LOOK AT 2ND LINE                             
         LA    R4,1                                                             
         CLI   5(R2),0                                                          
         BNE   ORDCOM3                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ORDCOM2A                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
ORDCOM2A OC    RCONSRDT,RCONSRDT   HAS CONTRACT EVER BEEN SENT?                 
         BNZ   ORDCOM2C            YES                                          
         TM    RCONSENF,X'01'      TAKEOVER CONTRACT?                           
         BO    ORDCOM2C                                                         
         DROP  R6                                                               
                                                                                
         GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)                                    
*                                  DELETE ANY ORDCOMS PRESENT                   
         B     ENDCOM                                                           
ORDCOM2C EQU   *                                                                
         TM    PROFILES+CNTBOCMB,CNTBOCMA  PROF#35?                             
         BZ    ORDCOM2D            NO - ORD CMT REQ                             
*                                                                               
         XC    RSTAREC(32),RSTAREC CHECK STAREAC FOR OVERRIDE                   
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   ENDCOM              NO ELEM - OCM NOT REQ                        
         TM    RSTAOPTA,X'08'      STA OPT #14?                                 
         BZ    ENDCOM              NO OCM REQUIRED                              
         DROP  R6                                                               
ORDCOM2D DS    0H                                                               
         LA    R3,1                MISSING INP FLD                              
         LA    R2,SBSOCM1H                                                      
         B     ERROR                                                            
*                                                                               
ORDCOM3  EQU   *                                                                
         LA    R2,SBSOCM1H         RESET A(1ST LINE INPUT)                      
ORDCOM3A TM    4(R2),X'20'         1ST LINE INPUT THIS TIME?                    
         BZ    ORDCOM7             YES                                          
         LA    R2,SBSOCM2H                                                      
         TM    4(R2),X'20'         2ND LINE INPUT THIS TIME?                    
         BO    ORDCOM5                                                          
         CLI   5(R2),0             IS THERE 2ND LINE?                           
         BE    ORDCOM5                                                          
         LA    R2,SBSOCM1H       ONE INPUT THIS TIME, REDO BOTH LINES           
         B     ORDCOM7                                                          
         SPACE 1                                                                
ORDCOM5  LA    R3,129            NEW COMMENT REQUIRED                           
         LA    R2,SBSOCM1H                                                      
         B     ERROR                                                            
         SPACE 1                                                                
ORDCOM7  CLC   8(3,R2),=C'#DS'     'DON'T SEND' LINE NOT ALLOWED ON             
         BNE   ORDCOM7K                                                         
         TM    RCONMODR+1,X'C0'          NON ACE OR GRAPHNET                    
         BZ    ORDCOM8                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONSTAT,X'02'           OR IF STATION NOT TURNED ON             
         BZ    ORDCOM8                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ORDCOM7A                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
ORDCOM7A OC    RCONSRDT,RCONSRDT         OR VERSION 1                           
         BZ    ORDCOM8K                                                         
         SPACE 1                                                                
         CLI   5(R2),3             AND MUST INPUT MORE THAN JUST #DS            
         BH    ORDCOM7K                                                         
         LA    R3,135              COMMENT REQUIRED FOR #DS                     
         B     ERROR                                                            
         SPACE 1                                                                
ORDCOM7K LA    R2,SBSOCM2H                                                      
         CLC   8(3,R2),=C'#DS'     AND NOT ALLOWED ON LINE 2                    
         BE    ORDCOM8P                                                         
         LA    R2,SBSOCM1H                                                      
         B     ORDCOM9                                                          
         DROP  R6                                                               
ORDCOM8  LA    R3,133              NOT VALID FOR CONTRACT                       
         B     ERROR                                                            
ORDCOM8K LA    R3,134              NOT VALID FOR VERSION 1                      
         B     ERROR                                                            
ORDCOM8P LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
         SPACE 1                                                                
ORDCOM9  GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)    DELETE OLD                      
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BO    ORDCOM11                                                         
         CLI   5(R2),0             NON ACE/GRAPH DOESN'T REQUIRE                
         BNE   ORDCOM11              COMMENT                                    
         LA    R2,SBSOCM2H         2ND LINE                                     
         LA    R4,1                                                             
         CLI   5(R2),0             IS THERE 2ND LINE?                           
         BE    ENDCOM                                                           
         SPACE 1                                                                
ORDCOM11 XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         MVI   WORK2,X'84'                                                      
         OI    WORK2+2,X'80'       REP COMMENT                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+3(0),8(R2)                                                 
         LA    RE,4(RE)            GET TOTAL LENGTH (1 FROM BCTR + 2)           
         STC   RE,WORK2+1                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         SPACE 1                                                                
         LA    R2,SBSOCM2H         2ND COMMENT LINE IS ALWAYS OPTIONAL          
         CLI   5(R2),0                                                          
         BE    ENDCOM                                                           
         BCT   R4,ORDCOM11                                                      
         SPACE 1                                                                
ENDCOM   DS    0H                                                               
*                                                                               
         TITLE 'T80216 REPPAK EDIT END'                                         
ENDEDIT  DS    0H                                                               
         OI    CONBACTH+4,X'20'                                                 
         OI    CONBNUMH+4,X'20'                                                 
         LA    R2,CONBACTH         CURSOR                                       
* PUT RBUYNW IN 03 ELEMENTS (EFF DATES) IF NO OVERRIDE                          
*     AND NOT 'DARE' ORDER (EFF DATES CAN'T BE CHANGED FOR THEM)                
         MVI   ELCODE,X'1D'        DARE ELEMENT?                                
         BAS   RE,GETEL                                                         
         BNE   END080              NO  - PROCEED                                
         LA    R6,RCONREC          YES - CHECK FOR 'CONFIRMED'                  
         MVI   ELCODE,X'1F'        EXTENDED DESCRIPTION ELEMENT?                
         BAS   RE,GETEL                                                         
         BNE   END200              NOT FOUND - NEW DARE ORDER.                  
         TM    RCONCONF-RCONXEL(R6),X'E0'                                       
*                                  ANY 'CONFIRMED' FLAG ON?                     
         BZ    END200              NO  - TREAT AS DARE ORDER                    
*                                  YES - UPDATE # SPOTS/WEEK                    
*                                                                               
*   NOTE - THIS TEST MAY HAVE TO BE CHANGED FOR ORDERS WHICH ARE                
*        CONFIRMED/UNCONFIRMED FOR DARE.                                        
*                                                                               
END080   EQU   *                                                                
         LA    R6,RBUYREC                                                       
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   END200                                                           
END100   DS    0H                                                               
         TM    8(R6),X'01'                                                      
         BO    *+10                                                             
         MVC   9(1,R6),RBUYNW                                                   
         BAS   RE,NEXTEL                                                        
         BE    END100                                                           
END200   CLC   BUYACT(3),=C'CHA'                                                
         BNE   ADDBUY                                                           
***>     CLC   BUYACT(3),=C'BUX'   SPECIAL NO BUCKET ACTION                     
***>     BE    ADDBUY                                                           
*                                                                               
*                                  MISSED ELTS NAKED BY CHANGE?                 
         GOTO1 VMOVEREC,DMCB,RBUYREC,IOAREA                                     
*&&DO                                                                           
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'        MISSED SPOT ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    MDAY0040                                                         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'07'        CREDIT ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   MDAY0160            NOT FOUND - CHECK X'66' ELTS                 
*                                  STILL COVERED BY DAYS/DATES?                 
*                                                                               
MDAY0040 MVC   WORK+3(3),2(R6)     MISSED DATE                                  
         BAS   RE,CKMISS           CHECK IF EXPOSED                             
         L     R3,DMCB             NUMBER OF SPOTS IN WEEK                      
         LTR   R3,R3                                                            
         BM    MDAY0080                                                         
         CLI   TEMP,X'FF'                                                       
         BNE   MDAY0120                                                         
MDAY0080 LA    R3,MG1ERR           MUST DELETE MAKE-GOODS                       
         B     ERROR                                                            
*                                                                               
MDAY0120 LR    R7,R6                                                            
         ZIC   R4,1(R6)                                                         
         AR    R6,R4               NEXT ELEM                                    
         CLI   0(R6),6             MISSED?                                      
         BE    *+12                                                             
         CLI   0(R6),7                                                          
         BNE   MDAY0160                                                         
         CLC   2(3,R6),2(R7)                                                    
         BE    MDAY0120            IF SAME MISSED ELEM DATE - SKIP              
         B     MDAY0040                                                         
MDAY0160 EQU   *                                                                
         MVI   HALF2,X'56'           SET MG SPLIT ELEMENT CODE                  
MDAY0170 EQU   *                                                                
         LA    R6,IOAREA                                                        
         MVC   ELCODE(1),HALF2     MAKEGOOD OFFER OR SPLIT ELT                  
         BAS   RE,GETEL                                                         
         BNE   MDAY0320            NOT FOUND - FINISHED                         
*                                  STILL COVERED BY DAYS/DATES?                 
MDAY0200 DS    0H                                                               
         MVC   WORK+3(3),2(R6)     MISSED DATE                                  
         CLI   0(R6),X'66'                                                      
         BNE   MDAY0210                                                         
         MVC   WORK+3(3),7(R6)                                                  
*                                  STILL COVERED BY DAYS/DATES?                 
MDAY0210 DS    0H                                                               
         BAS   RE,CKMISS           CHECK IF EXPOSED                             
         L     R3,DMCB             NUMBER OF SPOTS IN WEEK                      
         LTR   R3,R3                                                            
         BM    MDAY0240            MAY NOT LEAVE MINUS NUMBER                   
         BZ    MDAY0280            ZERO SPOTS PERMITTED:                        
*                                     SPLIT MAY HAVE REDUCED NUMBER TO          
*                                     ZERO LEGALLY                              
         CLI   TEMP,X'FF'                                                       
         BNE   MDAY0280                                                         
MDAY0240 EQU   *                                                                
         LA    R3,MG1ERR                                                        
         CLI   HALF2,X'56'         MG SPLIT IN PROGRESS?                        
         BE    ERROR               YES - USE MAKEGOOD ERROR                     
         LA    R3,MGOERR           NO  - MUST DELETE MAKE-GOOD OFFERS           
         B     ERROR                                                            
*                                                                               
MGOERR   EQU   458                                                              
*                                                                               
MDAY0280 LR    R7,R6                                                            
         ZIC   R4,1(R6)                                                         
         AR    R6,R4               NEXT ELEM                                    
         CLC   0(1,R6),HALF2       MAKEGOOD OFFER OR SPLIT ELT?                 
         BNE   MDAY0320                                                         
         CLI   0(R6),X'66'                                                      
         BE    MDAY0290                                                         
         CLC   2(3,R6),2(R7)       FOR ELEMENT X'56'                            
         BE    MDAY0280            IF SAME MISSED ELEM DATE - SKIP              
         B     MDAY0200                                                         
*                                                                               
MDAY0290 DS    0H                                                               
         CLC   7(3,R6),7(R7)       FOR ELEMENT X'66'                            
         BE    MDAY0280            IF SAME MISSED ELEM DATE - SKIP              
         B     MDAY0200                                                         
*                                  ALL MISSED ELTS COVERED                      
MDAY0320 EQU   *                                                                
         CLI   HALF2,X'66'         MG OFFER FINISHED?                           
         BE    MDAY0340            YES - FINISHED                               
         MVI   HALF2,X'66'         NO  - GO BACK AND DO IT                      
         B     MDAY0170                                                         
*&&                                                                             
MDAY0340 EQU   *                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET-DON'T CARE IF                   
         BNZ   MDAY0360            BUY CREATED TODAY                            
         SPACE 1                                                                
         CLC   RBUYCREA,TODAY      BUY CREATED TODAY?                           
         BE    END300                                                           
*                                  ANALYZE CHANGE                               
MDAY0360 DS    0H                                                               
*        LA    R6,RBUYREC                                                       
*        LA    R6,1000(R6)                                                      
         L     R6,AIO2                                                          
*                                  SET PARAMETER LIST TO VCHECKEL               
         L     RF,VCHECKEL         ELEMENT COMPARE ROUTINE IN BASE              
         GOTO1 (RF),DMCB,RBUYREC,(R6),BYTE                                      
*                                  CHECK DAY                                    
         MVI   DMCB,2              DAY ELEM CODE                                
         MVI   BYTE,C'D'           DAY CHG CODE                                 
         BASR  RE,RF               CHECK CHANGE                                 
         BAS   RE,ADDCODE                                                       
*                                  CHECK TIME                                   
         MVI   BYTE,C'T'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK EFF. DATES                             
         MVI   DMCB,3                                                           
         MVI   BYTE,C'E'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK COMMENTS                               
         MVI   DMCB,4                                                           
         MVI   BYTE,C'Z'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK MAKE-GOOD CHANGE                       
         MVI   DMCB,5                                                           
         MVI   BYTE,C'M'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK ORDER COMMENT CHANGE                   
         MVI   DMCB,X'84'                                                       
         MVI   BYTE,C'O'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK SPORTS BUY DESCRIPTIVE CHANGE          
         MVI   DMCB,X'81'                                                       
         MVI   BYTE,C'V'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
         MVI   DMCB,X'82'          BOTH FIELDS                                  
         MVI   BYTE,C'V'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK LENGTH                                 
         LA    R5,RBUYDUR                                                       
         L     R6,AIO2                                                          
         LA    R6,RBUYDUR-RBUYREC(R6)                                           
         MVI   BYTE,C'L'                                                        
         CLC   0(2,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK SPOTS/PER/WEEK                         
         LA    R5,RBUYNW                                                        
         L     R6,AIO2                                                          
         LA    R6,RBUYNW-RBUYREC(R6)                                            
         MVI   BYTE,C'S'                                                        
         CLC   0(1,R5),0(R6)                                                    
         BE    MDAY0520                                                         
         BAS   RE,ADDCODE                                                       
*                                  CHANGE NPW IN 03 DATE ELEMENTS               
*                                     IF = TO OLD NPW                           
         LA    R3,RBUYELEM                                                      
         SR    R4,R4                                                            
MDAY0400 CLI   0(R3),0                                                          
         BE    MDAY0520                                                         
         CLI   0(R3),3                                                          
         BE    MDAY0480                                                         
*                                                                               
MDAY0440 IC    R4,1(R3)            NEXT ELEM                                    
         AR    R3,R4                                                            
         B     MDAY0400                                                         
*                                                                               
MDAY0480 CLC   9(1,R3),0(R6)       SAME AS OLD NPW?                             
         BNE   MDAY0440            IF SO NO CHANGE                              
         MVC   9(1,R3),RBUYNW                                                   
         B     MDAY0440                                                         
*                                                                               
*                                  CHECK RATE                                   
MDAY0520 LA    R5,RBUYCOS                                                       
         L     R6,AIO2                                                          
         LA    R6,RBUYCOS-RBUYREC(R6)                                           
         MVI   BYTE,C'R'                                                        
         CLC   0(4,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK PLAN, CLASS OR SECTION                 
         MVI   BYTE,C'P'                                                        
         LA    R5,RBUYKPLN         PLAN                                         
         L     R6,AIO2                                                          
         LA    R6,RBUYKPLN-RBUYREC(R6)                                          
         CLC   0(3,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
         LA    R5,RBUYSEC          SECTION                                      
         L     R6,AIO2                                                          
         LA    R6,RBUYSEC-RBUYREC(R6)                                           
         CLC   0(3,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
         LA    R5,RBUYCLS          CLASS                                        
         L     R6,AIO2                                                          
         LA    R6,RBUYCLS-RBUYREC(R6)                                           
         CLC   0(3,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
         B     END300                                                           
         EJECT                                                                  
*                                  ADD BUY                                      
*                                  UPDATE CHANGE INDICATORS                     
ADDBUY   MVC   RBUYCREA,TODAY      CREATION DATE                                
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET CONTRACT                        
         BNZ   *+12                                                             
*                                                                               
         CLI   RCONMOD,X'FF'       -1?                                          
         BE    ADD150                                                           
         CLI   RBUYKLIN,1          LINE 1?                                      
         BNE   ADD150                                                           
         CLC   RCONCREA,TODAY                                                   
         BE    *+14                                                             
         OI    TAREQ,1             T/A REQ IND                                  
         MVC   RCONCREA,TODAY      LINE 1 SHOULD NOT BUMP K MOD                 
*                                                                               
         OI    RCONMODR,X'20'+X'10' X'20'=LINE 1 ADDED                          
*                                   X'10'=NOT PENDING/BUYLINE(S) WERE           
*                                         ADDED AT ONE POINT.  THIS BIT         
*                                         DOES NOT GET RESET.                   
         B     *+8                                                              
ADD150   BAS   RE,BUMPNUM          BUMP MODIFICATION NUMBER IN K                
         MVC   RBUYKMOD,RCONMOD                                                 
         TM    RCONMODR+1,X'C0'    IF ACE/GRAPHNET SKIP THIS CODE               
         BNZ   ADD160              (X'FF' IS VALID MOD #)                       
         SPACE 1                                                                
         CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RBUYKMOD,0                                                       
ADD160   MVC   RBUYCHGI,=C'A '     ADD IND                                      
*                                                                               
         LR    RF,RA               SPECIAL IF COMBO K                           
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          WE WANT TO ADD THE EXTRA DESCRIPTION         
         BE    ADD170              ONLY ONCE                                    
         CLI   TWACMBPT,1                                                       
         BH    ADD180                                                           
         DROP  RF                                                               
*                                                                               
ADD170   DS    0H                                                               
*                                  ADD EXTRA DESCRIPTION ELEMENT X'10'          
         GOTO1 =A(ADDEXTRA),DMCB,(RC),RR=Y                                      
*                                                                               
ADD180   DS    0H                                                               
*                                                                               
         CLI   RCONKSTA+4,C'F'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
         CLI   RCONKSTA+4,C'A'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
         CLI   RCONKSTA+4,C'C'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
*                                                                               
         GOTO1 =A(SARCOMS),RR=Y                                                 
         B     END300                                                           
         SPACE 4                                                                
* ROUTINE TO BUMP CONTRACT MODIFICATION MUMBER                                  
         SPACE 1                                                                
BUMPNUM  OI    RCONMODR,X'40'      BUY CHANGE IND                               
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BCR   8,RE                                                             
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BNZ   BUMP5                                                            
*                                                                               
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BCR   8,RE                                                             
* UPDATE MODIFICATION NUMBER                                                    
BUMP5    OI    TAREQ,1             T/A REQ IND                                  
         NI    RCONMODR,X'5F'      NO K HEADLINE CHANGE                         
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET- DON'T BUMP MOD NUMBER          
         BNZ   BUMP10                                                           
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,RCONMOD          MOD NUMBER                                   
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
BUMP10   MVC   RCONMODD,TODAY                                                   
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*  UPDATE REP VERSION NUMBER                                                    
*                                                                               
END300   DS    0H                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    END500                                                           
         TM    STAT2,X'80'         BUY CHANGED-UP VER/MOD                       
         BO    END325                                                           
         OI    BYTE4,X'02'         NO, BUT STILL DO PUTREC                      
         B     END500                                                           
         SPACE 1                                                                
END325   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    END330                                                           
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
END330   DS    0H                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    END350                                                           
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATE                                   
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    END340                                                           
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
END340   DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC)                                     
         BNZ   ERROR                                                            
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
                                                                                
END350   DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSRV         STORE REP VERSION NO. IN BUY                  
         DROP  R6                                                               
         STC   R1,RBUYVER          INSERT REP VERSION #                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    END400                                                           
         SPACE 1                                                                
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
END400   OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         SPACE 1                                                                
END500   B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD CODE TO RBUYCHGI  (CODE IN BYTE)                               
***********************************************************************         
ADDCODE  NTR1                                                                   
         CLI   BYTE,0                                                           
         BE    ADDCXIT                                                          
         BAS   RE,BUMPNUM          BUMP K NUMBER                                
         MVC   RBUYKMOD,RCONMOD    K MODIFICATION NUMBER                        
         SPACE 1                                                                
*                                                                               
* FOR ACE/GRAPHNET -  THE VERSION, NOT THE DAY, IS WHAT MATTERS                 
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ADDC50                                                           
         CLI   TWAACCS,C'$'        STATION ONLY HAS 1 CHG CODE, SO IT           
         BE    ADDC75              DOESN'T MATTER IF UPDATED ALREADY            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ADDC10                                                           
         DC    H'0'                                                             
ADDC10   TM    4(R6),X'20'         REP VERSION NOT ADVANCED                     
         BNO   ADDC30                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
         SPACE 1                                                                
ADDC30   CLC   5(1,R6),RBUYVER     COMPARE CONTRACT TO BUY VERSION              
         BE    ADDC35                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
ADDC35   CLI   RBUYCHGI,C'A'       IF ADDED THIS VERSION,                       
         BE    ADDCXIT             DON'T CARE ABOUT OTHER CHANGES               
         B     ADDC100                                                          
         SPACE 1                                                                
*  DO SOME TESTS FOR NON ACE/GRAPHNET CONTRACTS                                 
         SPACE 1                                                                
ADDC50   CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RBUYKMOD,0                                                       
         CLC   TODAY,RBUYCHGD                                                   
         BE    ADDC100                                                          
ADDC75   MVC   RBUYCHGD,TODAY                                                   
         MVC   RBUYCHGI(1),BYTE    CHANGE CODE                                  
         MVI   RBUYCHGI+1,C' '                                                  
*                                                                               
ADDCXIT  XIT1                                                                   
* BUY ALREADY CHANGED TODAY                                                     
ADDC100  CLI   RBUYCHGI,C' '                                                    
         BNE   *+14                                                             
         MVC   RBUYCHGI(1),BYTE    FIRST                                        
         B     ADDCXIT                                                          
         CLI   RBUYCHGI,C'*'                                                    
         BE    ADDCXIT                                                          
         CLC   RBUYCHGI(1),BYTE                                                 
         BE    ADDCXIT                                                          
         CLI   RBUYCHGI+1,C' '                                                  
         BNE   *+14                                                             
         MVC   RBUYCHGI+1(1),BYTE  2D                                           
         B     ADDCXIT                                                          
         CLC   RBUYCHGI+1(1),BYTE                                               
         BE    ADDCXIT                                                          
         MVC   RBUYCHGI,=C'* '     MORE THAN 2                                  
         B     ADDCXIT                                                          
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE UPDATES THE MOD CODE IN THE TARGET MISSED LINE FOR A             
* MAKEGOOD. THE TARGET MISSED BUY IS IN 'IOAREA'. ROUTINE IS LIFTED             
* FROM ADDCODE.                                                                 
**********************************************************************          
MGADDCDE NTR1                                                                   
IOA      USING RBUYREC,IOAREA                                                   
         MVC   IOA.RBUYKMOD,RCONMOD K MOD NUM                                   
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    MGADDC20                                                         
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* TAKE THE LATEST/HIGHEST VERSION NUMBER                                        
         TM    RCONSENF,X'20'                                                   
         BZ    MGADDC10                                                         
*                                                                               
         ZIC   R1,RCONSRV          NEXT REP VERSION NUMBER                      
         LA    R1,2(R1)            SHOULD BE THE GREATER OF                     
         STC   R1,IOA.RBUYVER      REP VERSION NUMBER + 2                       
         CLC   IOA.RBUYVER,RCONSSV                                              
         BH    MGADDC30            OR                                           
         ZIC   R1,RCONSSV                                                       
         LA    R1,1(R1)            STA VERSION NUMBER + 1                       
         STC   R1,IOA.RBUYVER                                                   
         B     MGADDC30                                                         
                                                                                
MGADDC10 DS    0H                  REP VERSION ADVANCED                         
         CLC   RCONSRV,IOA.RBUYVER                                              
         BE    MGADDC40                                                         
         MVC   IOA.RBUYVER,RCONSRV                                              
         B     MGADDC30                                                         
         DROP  R6                                                               
*                                                                               
*  DO SOME TESTS FOR NON ACE/GRAPHNET CONTRACTS                                 
*                                                                               
MGADDC20 DS    0H                                                               
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BE    MGADDC30                                                         
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BE    MGADDC30                                                         
* UPDATE MODIFICATION NUMBER IN TARGET MISSED LINE FOR NON-ACE/GRAPH            
* CONTRACT MOD NUMBER WILL BE UPDATED LATER BY ADDCODE                          
         ZIC   R1,IOA.RBUYKMOD                                                  
         LA    R1,1(R1)                                                         
         STC   R1,IOA.RBUYKMOD                                                  
         CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   IOA.RBUYKMOD,0                                                   
         CLC   TODAY,IOA.RBUYCHGD                                               
         BE    MGADDC40                                                         
                                                                                
MGADDC30 MVC   IOA.RBUYCHGD,TODAY                                               
         MVC   IOA.RBUYCHGI,=C'M ' MAKEGOOD CHANGE CODE                         
         B     MGADDCX                                                          
*                                                                               
* BUY ALREADY CHANGED TODAY                                                     
MGADDC40 CLI   IOA.RBUYCHGI,C' '                                                
         BNE   MGADDC50                                                         
         MVI   IOA.RBUYCHGI,C'M'                                                
         B     MGADDCX                                                          
                                                                                
MGADDC50 DS    0H                                                               
         CLI   IOA.RBUYCHGI,C'*'                                                
         BE    MGADDCX                                                          
         CLI   IOA.RBUYCHGI,C'M'                                                
         BE    MGADDCX                                                          
         CLI   IOA.RBUYCHGI+1,C' '                                              
         BNE   MGADDC60                                                         
         MVI   IOA.RBUYCHGI+1,C'M'                                              
         B     MGADDCX                                                          
                                                                                
MGADDC60 DS    0H                                                               
         CLI   IOA.RBUYCHGI+1,C'M'                                              
         BE    MGADDCX                                                          
         MVC   IOA.RBUYCHGI,=C'* ' MORE THAN 2                                  
*                                                                               
MGADDCX  XIT1                                                                   
         DROP  IOA                                                              
         TITLE 'SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS'                
*        P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                        
*        P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR. FOUND            
*        AN ASTERISK DELIMITS SUB-FIELDS                                        
SCAN     NTR1                                                                   
*                                                                               
         L     R2,4(R1)       FIELD HEADER                                      
         L     R3,0(R1)       LAST FIELD                                        
*                                                                               
         ZIC   R4,0(R1)       LENGTH OF PREVIOUS FIELD                          
         LA    R3,1(R4,R3)    NEW SCAN PLUS DELIMITER                           
         ST    R3,0(R1)       LAST FIELD                                        
         SR    R5,R5          LENGTH COUNTER                                    
         IC    R4,5(R2)       TOTAL LENGTH OF INPUT                             
         LA    R2,8(R4,R2)    TOTAL FIELD END                                   
         MVI   4(R1),0        ELIM. STOP INDICATOR                              
FIELD25  CR    R3,R2          END OF INPUT?                                     
         BL    FIELD100                                                         
FIELD50  STC   R5,0(R1)       NEW FIELD LENGTH                                  
         XIT1                                                                   
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   *+12                                                             
         MVI   4(R1),C'*'                                                       
         B     FIELD50                                                          
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
         EJECT                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTC7D                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
*********************************************************************           
* SARCOMS - CHANGES PENDING COMMENTS TO SPL COMMENTS                *           
*********************************************************************           
T80215   CSECT                                                                  
         DS    0H                                                               
SARCOMS  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*SARCOMS'                                                    
*                                                                               
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
SARCMX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         CSECT                                                                  
***********************************************************************         
* DELETE BUY                                                                    
***********************************************************************         
DELBUY   NMOD1 0,*DELB*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    DELB0040                                                         
*                                                                               
*  IF ACE/GRAPHNET, CAN ONLY DELETE IF BUYLINE HAS NEVER BEEN SENT, AND         
*  CAN ONLY CANCEL IF BUYLINE HAS PREVIOUSLY BEEN SENT                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            GET SEND ELEMENT                             
         BE    DELB0020                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
DELB0020 MVC   SVKVER,RCONSRV      SAVE K REP VER                               
         MVC   SVKSENF,RCONSENF     SAVE SEND INFO                              
         DROP  R6                                                               
*                                                                               
DELB0040 LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWABADDR                                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    DELB0120                                                         
*                                                                               
* ACTION CANCEL                                                                 
*                                                                               
         CLC   BUYACT,=C'CAN'                                                   
         BNE   DELB0080                                                         
         LA    R3,198              BUYLINE HAS BEEN SENT                        
         CLI   RBUYCHGI,C'A'       OK TO CANCEL IF MOD CODE <> 'A'              
         BNE   DELB0120                                                         
         CLC   RBUYVER,SVKVER      IF MOD CODE='A', CHECK BUY VERSION           
         BNE   DELB0120            AGAINST K VERSION                            
*                                  OK TO CANCEL SINCE UNEQUAL VERSIONS          
*                                  IMPLIES K WAS SENT                           
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DELB0060                                                         
         TM    SVKSENF,X'20'        SENT BY REP                                 
         BZ    ERROR                                                            
         B     DELB0120                                                         
*                                                                               
DELB0060 DS    0H                                                               
         TM    SVKSENF,X'10'                                                    
         BZ    ERROR                                                            
         B     DELB0120                                                         
*                                                                               
* ACTION DELETE                                                                 
*                                                                               
DELB0080 DS    0H                                                               
         CLC   =C'DELDDS',CONBACT                                               
         BE    DELB0180                                                         
* PROFILE ALLOWS DELETE OF ANY BUYLINE NOT TRANSFERRED TO SPOT                  
         TM    PROFILES+CNTBDELB,CNTBDELA                                       
         BNZ   DELB0120            PROF ON, SKIP TO SPOT XFER CHECK             
*                                                                               
         LA    R3,197                                                           
*        CLI   RBUYCHGI,C'A'       CHANGE MODE MUST BE 'A'                      
*        BNE   ERROR                                                            
         CLC   RBUYVER,SVKVER      BUYLINE CAN BE DELETED ONLY                  
         BL    ERROR               IF IT HAS NEVER BEEN SENT                    
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DELB0100                                                         
         TM    SVKSENF,X'20'        SENT BY REP                                 
         BO    ERROR                                                            
         B     DELB0120                                                         
*                                                                               
DELB0100 DS    0H                                                               
         TM    SVKSENF,X'10'                                                    
         BO    ERROR                                                            
*                                                                               
* MAKE SURE BUY HASN'T BEEN XFERRED TO SPOTPAK                                  
DELB0120 DS    0H                                                               
*                                                                               
* REMOVE RTS CHECK                                                              
*                                                                               
*&&DO                                                                           
         CLC   =C'CAN',BUYACT      CAN?                                         
         BE    DELB0180            OK                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    DELB0140            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    DELB0160            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
DELB0140 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   DELB0180                                                         
DELB0160 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   DELB0180                                                         
         USING RBUYSPEL,R6                                                      
         CLI   RBUYSPL#,0          HAS BEEN XFERRED?                            
         BE    DELB0180            NO, OKAY TO DEL                              
         DROP  R6                                                               
         LA    R3,269              CAN'T DEL BUY XFER'D TO SPOT                 
         B     ERROR                                                            
*&&                                                                             
*                                                                               
* CHECK FOR MISSED ELEMENTS                                                     
DELB0180 LA    R5,RBUYELEM                                                      
         SR    R4,R4                                                            
         LA    R3,MG1ERR           NO MASTER DELETE ERROR                       
DELB0200 CLI   0(R5),0             LAST?                                        
         BE    DELB0280                                                         
         CLI   0(R5),5             MAKE-GOOD?                                   
         BE    ERROR                                                            
         CLI   0(R5),6             MISSED?                                      
         BE    ERROR                                                            
         CLI   0(R5),7             MISSED CREDIT ISSUED                         
         BE    ERROR                                                            
         CLI   0(R5),X'56'         MISSED (NEW SPLIT 03 VERSION)?               
         BNE   DELB0220                                                         
         EDIT  (1,5(R5)),(3,WORK2),ALIGN=LEFT                                   
         LA    R3,652              MUST DELETE MAKEGOOD BUYLINE FIRST           
         B     DELB0240                                                         
*                                                                               
DELB0220 EQU   *                                                                
         CLI   0(R5),X'66'         MAKEGOOD OFFER ELEMENT?                      
         BNE   DELB0260                                                         
         MVC   WORK2(2),2(R5)                                                   
         LA    R0,2                                                             
         LA    R3,653              MUST DELETE MAKEGOOD OFFER FIRST             
*                                                                               
DELB0240 EQU   *                                                                
         LA    RF,WORK2            BUFFER FOR VERSION NUMBER                    
         ST    RF,DMCB+12                                                       
         STC   R0,DMCB+12          INDICATE SIZE OF VERSION NUMBER              
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    CONBACTH+6,X'40'    FORCE CURSOR TO ACTION                       
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
*                                                                               
DELB0260 EQU   *                                                                
         IC    R4,1(R5)            NEXT ELEM                                    
         AR    R5,R4                                                            
         B     DELB0200                                                         
*                                                                               
DELB0280 DS    0H                                                               
         CLC   BUYACT,=C'CAN'      CANCELLED?                                   
         BNE   DELB0285            NO - DELETE IT                               
         TM    PROFILES+CNTKCANB,CNTKCANA  KEEP CANCELLED BUYS?                 
         BO    DELB0290                    YES - SKIP DELETE                    
DELB0285 DS    0H                                                               
         OI    RBUYCNTL,X'80'      DELETE BIT                                   
*                                                                               
DELB0290 DS    0H                                                               
*        LA    R6,RBUYREC                                                       
*        LA    R6,1000(R6)                                                      
         L     R6,AIO2                                                          
         GOTO1 VMOVEREC,DMCB,RBUYREC,(R6)    SAVE OLD BUYREC                    
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD                                                            
         CLC   BUYACT,=C'CAN'      CANCEL?                                      
         BNE   DELB0295            NO                                           
         TM    PROFILES+CNTKCANB,CNTKCANA  KEEP CANCELLED BUYS?                 
         BO    *+8                                                              
DELB0295 DS    0H                                                               
         OI    KEY+27,X'80'        DELETE                                       
         GOTO1 VWRITE                                                           
* UPDATE PASSIVE KEY                                                            
         GOTO1 VLOAD,DMCB,(X'19',0),RBUYREC                                     
*                                                                               
         MVC   RBUYCHGD,TODAY                                                   
         MVC   RBUYCHGI,=C'C '     CANCEL INDICATOR                             
         BAS   RE,BUMPNUM          BUMP K MOD NUMBER                            
*                                                                               
         MVC   RBUYKMOD,RCONMOD                                                 
         CLC   BUYACT,=C'CAN'                                                   
         BE    *+10                                                             
         MVC   RBUYCHGI,=C'X '                                                  
         MVC   BUYACT,=C'DEL'                                                   
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE OR GRAPHNET                              
         BZ    DELB0380                                                         
*                                                                               
*  UPDATE REP VERSION NUMBER AND MARK UNCONFIRMED                               
*                                                                               
DELB0300 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    DELB0320                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
DELB0320 DS    0H                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    DELB0340                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    DELB0330                                                         
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
DELB0330 DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC)                                     
         BNZ   ERROR                                                            
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
                                                                                
DELB0340 DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSRV          STORE REP VERSION NO. IN BUY                 
         DROP R6                                                                
         STC   R1,RBUYVER          STORE REP VERSION NO. IN BUY                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    DELB0360                                                         
         SPACE 1                                                                
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
DELB0360 OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         SPACE 1                                                                
DELB0380 GOTO1 VMOVEREC,DMCB,RBUYREC,IOAREA                                     
         GOTO1 VPUTREC,DMCB,IOAREA                                              
***>     BAS   RE,ECXDAY           EC BIAS CROSS DAY RECONTRUCT                 
         B     EXXMOD                                                           
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* IF EC/BIAS, READ ALL BUY RECORDS AND RECONSTRUCT CROSS DAY EXCLUSION          
***********************************************************************         
ECXDAY   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        EC BIAS?                                     
         BNE   ECXDAYX                                                          
         DROP  RF                                                               
                                                                                
         XC    WORK2,WORK2         WILL HOLD CROSS DAY                          
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(22),IOAREA      READ ALL BUYS UNDER THIS CONTRACT            
         GOTO1 VHIGH                                                            
         B     ECXDAY15                                                         
                                                                                
ECXDAY10 DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
ECXDAY15 DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   ECXDAY30                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ECXDAY10                                                         
                                                                                
         USING RBUYDYEL,R6                                                      
         ZIC   RF,RBUYDYIN         START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,RBUYDAYS         DAYS                                         
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   ECXDAY20                                                         
                                                                                
         USING RCONCCEL,R6                                                      
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    ECXDAY20                                                         
         LA    R2,BUYDAYSH                                                      
         LA    R3,399                                                           
         B     ERROR               CANNOT CROSS CROSS DAY DEFAULT               
         DROP  R6                                                               
                                                                                
ECXDAY20 DS    0H                                                               
         OC    WORK2(1),WORK       COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK2,X'7F'         ERROR IF ALL ON                              
         BNO   ECXDAY10            GET NEXT BUY RECORD                          
         LA    R2,BUYDAYSH                                                      
         B     ERROR                                                            
                                                                                
ECXDAY30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
*                                                                               
* DO WE HAVE TO PUT IN A CHECK TO SEE IF THE EC BIAS X'13' ELEMENT              
* BE ADDED TO A CONTRACT BEFORE A CERTAIN (RELEASE?) DATE?                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   ECXDAY40                                                         
         USING RCONCCEL,R6         UPDATE CROSS DAY                             
         MVC   RCONCCCD,WORK2                                                   
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     ECXDAY50                                                         
         DROP  R6                                                               
                                                                                
ECXDAY40 DS    0H                  NO BIAS EC ELEMENT, CONSTRUCT ONE            
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL1                                                
         MVC   RCONCCCD,WORK2      DAYS IN CROSS DAY FOR ALL BUYS               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
ECXDAY50 DS    0H                  REFRESH CROSS DAY INDICATORS                 
         MVC   BUYXDAY,=7C'.'      CLEAR                                        
         ZIC   RF,WORK2            CROSS DAY BYTE                               
         SLL   RF,25                                                            
         LA    RE,BUYXDAY                                                       
         LA    R1,C'1'                                                          
                                                                                
ECXDAY60 DS    0H                                                               
         LTR   RF,RF                                                            
         BNM   ECXDAY70            IF DAY BIT IS ON                             
         STC   R1,0(RE)            PUT CORRESPONDING NUMBER THERE               
                                                                                
ECXDAY70 DS    0H                                                               
         SLL   RF,1                NEXT DAY BIT                                 
         LA    RE,1(RE)            NEXT DAY DISPLAY POSITION                    
         LA    R1,1(R1)            NEXT EBCDIC DISPLAY NUMBER                   
         LA    R0,BUYXDAY+7        CHECK IF WE'VE LOOKED AT ALL 7 DAYS          
         CR    RE,R0                                                            
         BNH   ECXDAY60                                                         
         OI    BUYXDAYH+6,X'80'    XMIT IT                                      
                                                                                
ECXDAYX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ADD EXTRA DESCRIPTION ELEMENT X'80'                                           
***********************************************************************         
ADDEXTRA NMOD1 0,*ADEX*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         XC    WORK,WORK                                                        
WKD      USING RBUYXXEL,WORK                                                    
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         MVC   WKD.RBUYXXMD,RCONMOD    CONTRACT MOD# AT BUY CREATION            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDEXT10                                                         
         USING RCONSEND,R6                                                      
         MVC   WKD.RBUYXXVR,RCONSRV    CONTRACT VER# AT BUY CREATION            
         TM    RCONSENF,X'20'                                                   
         BZ    ADDEXT10                                                         
         ZIC   R1,WKD.RBUYXXVR         REP VERSION NOT ADVANCED                 
         LA    R1,2(R1)                ADVANCE VERSION MANUALLY                 
         STC   R1,WKD.RBUYXXVR                                                  
         DROP  R6,WKD                                                           
*                                                                               
ADDEXT10 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK                                       
*                                                                               
ADDEXTX  DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'133RECNT7C   08/23/02'                                      
         END                                                                    
