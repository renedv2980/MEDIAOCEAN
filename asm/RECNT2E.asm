*          DATA SET RECNT2E    AT LEVEL 205 AS OF 02/13/12                      
*PHASE T8022EA                                                                  
*INCLUDE OUTDAY                                                                 
         TITLE 'T8022E - REPPAK EXPANDED SINGLE BUY DISPLAY'                    
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT2E (T8022E) --- EXPANDED SINGLE BUY DISPLAY         *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
* 13FEB12 SKU FOXLOC DEMO CHECK                                   *             
* 22OCT07 SKU SKIP CHECK FOR CBS LOCAL MARKET                     *             
* 05MAR06 BU  DD0   BUYACT = DISPLAY MOD TRAP ELEMENTS            *             
* 23OCT02 SKU OPEN ALL FIELDS IF RADIO EDI                        *             
* 10JUN02 SKU DEMO RATING SUPPORT                                 *             
* 18MAR02 SKU SUPPORT REPLACEMENT OFFER DISPLAY                   *             
* 19SEP01 BU  DISPLAY BUYLINE CODE IF PROFILE ON                  *             
* 26JUL01 BU  ACTIVATE BUYLINE CODE ENTRY FIELD ON PROFILE        *             
* 25JUL01 BU  NEW FIELDS ON SCREEN                                *             
* 26JAN01 RHV PGM FIELD                                           *             
* 10OCT00 SKU FIX MOD CODE REDISPLAY BUG                          *             
* 25MAY00 BU  TRADE PROCESSING                                    *             
* 14MAR00 SKU NEW ENTRY                                           *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8022E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8022E,R9                                                      
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         L     R4,4(R1)            JUST DISPLAY HEADER INFO AND EXIT?           
*                                                                               
         BAS   RE,DISPCON                                                       
*                                                                               
         GOTOR CKSTPROF                                                         
         BNE   MAIN10                                                           
         GOTO1 =A(DISPDEMO),DMCB,(RC),RR=Y                                      
*                                                                               
MAIN10   DS    0H                                                               
         CLC   =C'HEAD',0(R4)      OH ONE GAVE ME HEAD?                         
         BE    EXXMOD              ALL DONE, EXIT                               
*                                                                               
         TM    PROFILES+CNTFOXSB,CNTFOXSA                                       
*                                  DISPLAY BUYLINE CODE FIELD?                  
         BZ    DISP000             NO                                           
         NI    BUYBCODH+1,X'FF'-X'0C'                                           
*                                  YES - DISPLAY LABEL FIELD                    
         OI    BUYBCODH+6,X'80'    TRANSMIT FIELD                               
         NI    BUYSCODH+1,X'FF'-X'FC'                                           
*                                  DISPLAY CODE FIELD, PERMIT I/P               
         OI    BUYSCODH+6,X'80'    TRANSMIT FIELD                               
DISP000  EQU   *                                                                
         OI    BUYFLTH+1,X'20'     PROTECT FLIGHT FIELD                         
         OI    BUYFLTH+6,X'80'     TRANSMIT FIELD                               
         TM    TWASTREO,X'80'      IS THIS STEREO?                              
         BZ    *+8                 NO                                           
         NI    BUYFLTH+1,X'FF'-X'20'    YES, UNPROTECT FLIGHT                   
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DISP0               YES                                          
         GOTO1 =A(CHKDARE),DMCB,(RC),RR=Y                                       
         B     DISP4                                                            
DISP0    EQU   *                                                                
         LA    R4,BUYDAYSH         TURN OFF PROTECTED BITS TO FOUT              
         NI    1(R4),X'DF'         SPACES. TURN ON AGAIN AT END                 
         LA    R4,BUYTIMSH                                                      
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYLENH                                                       
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYDTESH                                                      
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYDTE2H                                                      
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYCLSH                                                       
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYSECH                                                       
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYPLNH                                                       
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYTYPH                                                       
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYNPWH                                                       
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYRATEH                                                      
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYCOM1H                                                      
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYCOM2H                                                      
         NI    1(R4),X'DF'                                                      
         LA    R4,BUYPGMH                                                       
         NI    1(R4),X'DF'                                                      
*                                                                               
         LA    R3,15*3             15 MISSED ENTRIES WITH 3 FIELDS EACH         
         LA    R4,BUYMD1H                                                       
DISP3    NI    1(R4),X'DF'                                                      
         OI    4(R4),X'20'         SET PREVIOUSLY VALIDATED                     
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   R3,DISP3                                                         
*                                                                               
DISP4    DS    0H                                                               
         LR    RF,RA               FOR COMBO K'S, DON'T CLEAR COMBO             
         AH    RF,=Y(TWAWORKQ)       RATE FIELDS FOR ACTION BUY/CHA             
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    DISP10                                                           
         DROP  RF                                                               
         CLC   =C'BU',BUYACT                                                    
         BE    DISP8                                                            
         CLC   =C'CH',BUYACT                                                    
         BNE   DISP10                                                           
*                                                                               
DISP8    GOTO1 VFOUTBLK,DMCB,BUYDAYSH,BUYRATE,0                                 
         GOTO1 VFOUTBLK,DMCB,BUYCOM1H,BUYLAST,0                                 
         B     DISP12                                                           
*                                                                               
DISP10   GOTO1 VFOUTBLK,DMCB,BUYDAYSH,BUYLAST,0                                 
         TM    TWAFLAGS,TWAFLPTQ   IF PATTERN/NOTATION DISPLAY                  
         BZ    DISP12              CLEAR THESE FIELDS AS WELL                   
         XC    BUYCLS,BUYCLS                                                    
         OI    BUYCLSH+6,X'80'                                                  
         XC    BUYNOT,BUYNOT                                                    
         OI    BUYNOTH+6,X'80'                                                  
         XC    BUYUPT,BUYUPT                                                    
         OI    BUYUPTH+6,X'80'                                                  
DISP12   DS    0H                                                               
         CLC   =C'BU',BUYACT                                                    
         BE    DISP50                                                           
         CLC   =C'CH',BUYACT                                                    
         BE    DISP50                                                           
         SPACE 1                                                                
DISP16   LA    R2,BUYBNUMH                                                      
         LA    R3,BUYERR                                                        
*                                                                               
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,11         BUY REC TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*                                                                               
         CH    R0,=H'254'                                                       
         BH    ERROR                                                            
         MVC   KEY,RBUYREC                                                      
         STC   R0,RBUYKLIN         LINE NUMBER                                  
*                                                                               
         GOTO1 VHIGH                                                            
         B     DISP21                                                           
DISP20   GOTO1 VSEQ                                                             
DISP21   CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   ERROR                                                            
         CLC   KEY+26(1),RBUYKLIN                                               
         BNE   DISP20                                                           
* REPDIR REC FOUND                                                              
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWABADDR,KEY+28     SAVE DISK ADDR                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         LA    RF,BUYCOMUH                                                      
         MVC   BUYCOMU(07),=C'       '                                          
         NI    1(RF),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
*                                                                               
         LA    R2,BUYCOMUH                                                      
         TM    RBUYFLG2,X'02'      TRADE BUY FLAG ON?                           
         BNO   DISP40              NO                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OI    TWACMTRD,X'40'      YES - SET 'TRADE BUY ENCOUNTERED'            
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   BUYCOMU(07),=C'*TRADE*'                                          
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
DISP40   EQU   *                                                                
         OI    BUYCOMUH+6,X'80'    SET FIELD TO TRANSMIT                        
*                                                                               
*                                                                               
*              GET 1ST DAY-TIME ELEMENT                                         
DISP50   DS    0H                                                               
         CLI   RBUYCHGI,C'C'       DON'T DISPLAY CANCELLED LINES                
         BE    ERROR                                                            
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,BUYDAYS                                                       
         LA    R7,BUYTIMS                                                       
         SR    R8,R8                                                            
         IC    R8,BUYDAYSH                                                      
         LA    R8,BUYDAYSH-1(R8)   DAY FIELD END-1                              
         SR    R5,R5                                                            
         IC    R5,BUYTIMSH                                                      
         LA    R5,BUYTIMSH-1(R5)   TIME FIELD END-1                             
DISPDYTM DS    0H                                                               
         MVC   WORK,MYSPACES                                                    
         CLC   =X'FFFF',2(R6)      KATZ CONVERTED BUY                           
*                                     CONTAINING ERROR?                         
         BNE   DISP0080            NO                                           
         BAS   RE,KZDAYERR         YES - RETRIEVE DAY ERROR ELT                 
         B     DISP0090            SKIP REST OF DISPLAY                         
*              DISPLAY DAY-TIME FIELDS                                          
DISP0080 EQU   *                                                                
         GOTO1 =V(OUTDAY),DMCB,3(R6),2(R6),WORK,RR=YES                          
         LA    RE,WORK                                                          
         SR    R4,R4                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
*              ADD DAY LENGTH FIELD                                             
         LA    RE,0(R4,R3)                                                      
         CR    RE,R8               ROOM LEFT?                                   
         BH    DISP100                                                          
         STC   R4,BYTE                                                          
         BCTR  R4,R0                                                            
         EX    R4,*+8              MOVE DAY                                     
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
*                                                                               
DISP0090 EQU   *                                                                
*              TIME                                                             
         CLC   =X'FFFF',4(R6)      CONVERTED TIME IN ERROR?                     
         BNE   DISP0095            NO                                           
         BAS   RE,KZTIMERR         YES - RETRIEVE TIME ERROR ELT                
         B     DISP0099                                                         
DISP0095 EQU   *                                                                
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTO1 CALLOV,DMCB,0       GET ADDRESS OF UNTIME                        
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),4(R6),WORK                                             
         LA    RE,WORK             GET TIME LEN                                 
         SR    R2,R2                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
         LA    RE,0(R2,R7)                                                      
         CR    RE,R5               ROOM LEFT?                                   
         BH    DISP100                                                          
         STC   R2,BYTE2                                                         
         BCTR  R2,R0                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK                                                     
DISP0099 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DISP100                                                          
         ZIC   R2,BYTE             DAY LENGTH                                   
         LA    R3,0(R2,R3)         NEXT DAY                                     
         ZIC   R2,BYTE2            TIME LARGER                                  
         LA    R7,0(R2,R7)         NEXT TIME                                    
         MVI   0(R3),C'*'          FIELD SEPARATOR                              
         MVI   0(R7),C'*'                                                       
         LA    R3,1(R3)                                                         
         LA    R7,1(R7)                                                         
         B     DISPDYTM                                                         
         EJECT                                                                  
*                                                                               
*   RETRIEVE THE X'22' ELEMENT, AND INSERT ITS CONTENTS TO THE                  
*        DAY FIELD ON THE SCREEN.  IF THIS ROUTINE IS REFERENCED,               
*        THE BUYLINE WILL ONLY CONTAIN A SINGLE X'02' ELEMENT,                  
*        BECAUSE IT WAS CONVERTED FROM KATZ THAT WAY.                           
*        R6 --> X'02' ELEMENT IN THE RECORD.                                    
*                                                                               
KZDAYERR NTR1                                                                   
KZDA0020 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R6),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                ERROR RECORD MUST HAVE X'22'                 
         CLI   0(R6),X'22'         ERROR ELEMENT?                               
         BNE   KZDA0020            NO  - GO BACK FOR NEXT                       
         MVC   0(10,R3),2(R6)      YES - MOVE DATA TO FIELD                     
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RETRIEVE THE X'32' ELEMENT, AND INSERT ITS CONTENTS TO THE                  
*        TIME FIELD ON THE SCREEN.  IF THIS ROUTINE IS REFERENCED,              
*        THE BUYLINE WILL ONLY CONTAIN A SINGLE X'02' ELEMENT,                  
*        BECAUSE IT WAS CONVERTED FROM KATZ THAT WAY.                           
*        R6 --> X'02' ELEMENT IN THE RECORD.                                    
*                                                                               
KZTIMERR NTR1                                                                   
KZTM0020 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R6),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                ERROR RECORD MUST HAVE X'32'                 
         CLI   0(R6),X'32'         ERROR ELEMENT?                               
         BNE   KZTM0020            NO  - GO BACK FOR NEXT                       
         MVC   0(11,R7),2(R6)      YES - MOVE DATA TO FIELD                     
         XIT1                                                                   
         EJECT                                                                  
*              LENGTH                                                           
DISP100  DS    0H                                                               
         MVC   HALF,RBUYDUR                                                     
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,BUYLEN),ALIGN=LEFT                                   
         STC   R0,BUYLENH+5        NEW OUTPUT LENGTH.  THIS ACTUALLY            
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LR    RE,R0                                                            
         LA    RE,BUYLEN(RE)                                                    
         TM    RBUYDUR,X'80'       MINUTES?                                     
         BZ    DISP102                                                          
         MVI   0(RE),C'M'                                                       
*                                                                               
         ZIC   RF,BUYLENH+5        ADJUST FOR NEW LENGTH                        
         LA    RF,1(RF)                                                         
         STC   RF,BUYLENH+5                                                     
DISP102  EQU   *                                                                
         GOTOR CKSTPROF                                                         
         BNE   DISP102B                                                         
         GOTO1 =A(DISPDEMO),DMCB,(RC),RR=Y                                      
*                                                                               
DISP102B EQU   *                                                                
         XC    BUYSCOD,BUYSCOD     CLEAR BUYLINE CODE FIELD                     
**       LR    RF,RA                                                            
**       AH    RF,=Y(TWAWORKQ)                                                  
**       USING TWAWORK,RF                                                       
                                                                                
         TM    PROFILES+CNTFOXSB,CNTFOXSA                                       
*                                  DISPLAY BUYLINE CODE FIELD?                  
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'5F'        LOOK FOR SPECIAL BUY CODE ELT                
         BAS   RE,GETEL                                                         
         BNE   DISP103             NOT FOUND                                    
         MVC   BUYSCOD(3),RBYSCDBC-RBYSCDEL(R6)                                 
DISP103  EQU   *                                                                
                                                                                
*                                                                               
* DISPLAY FLIGHT & MOD CODES IF STEREO IS ACTIVE                                
         TM    TWASTREO,X'80'      IS STEREO ACTIVE?                            
         BZ    DISP105             NO, SKIP                                     
*                                                                               
         EDIT  RBUYFLT,BUYFLT,ALIGN=LEFT   DISPLAY FLIGHT CODE                  
         OI    BUYFLTH+6,X'80'             TRANSMIT FIELD                       
*                                                                               
         MVC   BUYMOD,MYSPACES                                                  
         BAS   RE,PBBY             DISPLAY MOD CODE                             
         OI    BUYMODH+6,X'80'     TRANSMIT FIELD                               
         B     DISP105             GO ON                                        
         EJECT                                                                  
*                                                                               
* PBBY -- THIS SUBROUTINE WAS LIFTED FROM REGENPBBY AND IS USED                 
*         TO FIND THE MOD CODES                                                 
*                                                                               
PBBY     NTR1                                                                   
         TM    RCONMODR+1,X'C0'    X'C0'=ACE/GRAPHNET CONTRACT                  
         BZ    PBBY0100                                                         
         SPACE 1                                                                
         SR    R4,R4                                                            
         LA    R1,RCONELEM                                                      
PBBY0030 IC    R4,1(R1)            FIND X'1F' ELEMENT                           
         AR    R1,R4                                                            
         CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEMENT SHOULD BE THERE                      
         CLI   0(R1),X'1F'                                                      
         BNE   PBBY0030                                                         
         SPACE 1                                                                
         TM    6(R1),X'80'         X'80'=NOT CONFIRMED                          
         BZ    PBBY0060                                                         
         SPACE 1                                                                
         IC    R4,1(R1)                                                         
         AR    R1,R4               GET X'20' (SEND) ELEMENT                     
         CLI   0(R1),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         CLI   5(R1),1             DON'T PRINT CHANGES WHEN                     
         BNE   PBBY0040            REP VERSION IS 1 AND                         
         CLI   14(R1),0            STATION VERSION IS 0                         
         BE    PBBYEXIT                                                         
         SPACE 1                                                                
PBBY0040 CLC   5(1,R1),14(R1)      COMPARE REP TO STATION VERSION               
         BH    PBBY0050  THEN COMPARE BUY VERSION TO HIGHER OF THE TWO          
*                        & DON'T PRINT CHANGES FOR OLD VERSIONS                 
         CLC   RBUYVER,14(R1)      STATION VERSION                              
         BE    PBBY0110                                                         
         TM    RBUYCNTL,X'80'      IF CANCELLED                                 
         BO    PBBYEXIT            DON'T PRINT LINE AT ALL                      
         B     PBBYEXIT            OTHERWISE, DON'T PRINT MOD CODE              
         SPACE 1                                                                
PBBY0050 CLC   RBUYVER,5(R1)       REP VERSION                                  
         BE    PBBY0110                                                         
         TM    RBUYCNTL,X'80'      IF CANCELLED                                 
         BO    PBBYEXIT            DON'T PRINT LINE AT ALL                      
         B     PBBYEXIT            OTHERWISE, DON'T PRINT MOD CODE              
         SPACE 1                                                                
* CONFIRMED -  ONLY PRINT CHANGES FROM LAST MOD NUMBER, AND DON'T               
*              PRINT CHANGE CODE O (ORDER COMMENT)                              
         SPACE 1                                                                
PBBY0060 DS    0H                                                               
*                                                                               
         CLI   RCONMOD,0           DON'T PRINT CHANGES FOR MOD 0                
         BE    PBBY0070                                                         
*        CLI   RBUYKMOD,0          DON'T PRINT CHANGES FOR MOD 0                
*        BE    PBBY0070                                                         
*                                                                               
         ZIC   R4,RCONMOD                                                       
         BCTR  R4,0                GET LAST MOD NUMBER                          
         ZIC   R1,RBUYKMOD                                                      
         CR    R1,R4                                                            
         BE    PBBY0080                                                         
PBBY0070 TM    RBUYCNTL,X'80'      IF CANCELLED                                 
         BO    PBBYEXIT            DON'T PRINT LINE AT ALL                      
         B     PBBYEXIT            OTHERWISE, DON'T PRINT MOD CODE              
         SPACE 1                                                                
PBBY0080 DS    0H                                                               
         LA    R4,BUYMOD                                                        
         CLI   RBUYCHGI,C'O'       ORDER COMMENT                                
         BE    PBBY0090                                                         
         MVC   0(1,R4),RBUYCHGI                                                 
         LA    R4,1(R4)                                                         
         MVI   0(R4),C' '          MOVE BLANK TO 2ND CHARACTER                  
         SPACE 1                                                                
PBBY0090 CLI   RBUYCHGI+1,C'O'     ORDER COMMENT                                
         BE    PBBYEXIT                                                         
         CLI   RBUYCHGI+1,C' '     BLANK                                        
         BE    PBBYEXIT                                                         
         MVC   0(1,R4),RBUYCHGI+1                                               
         OC    BUYMOD,=C'  '       SPACES                                       
         B     PBBYEXIT                                                         
         SPACE 1                                                                
* FOR NON ACE/GRAPHNET CONTRACTS                                                
         SPACE 1                                                                
PBBY0100 CLI   RCONMOD,0           K MOD NUM                                    
         BE    PBBYEXIT                                                         
         CLI   RCONMOD,X'FF'                                                    
         BE    PBBYEXIT                                                         
         CLI   RBUYKMOD,X'FF'                                                   
         BE    PBBYEXIT                                                         
         CLI   RBUYKMOD,0          BUY K MOD NUM                                
         BE    PBBYEXIT                                                         
         CLC   RBUYKMOD,RCONMOD                                                 
         BL    PBBYEXIT                                                         
*                                                                               
PBBY0110 MVC   BUYMOD,RBUYCHGI                                                  
PBBYEXIT XIT1                                                                   
         EJECT                                                                  
*                                                                               
* DISPLAY CROSS DAY                                                             
DISP105  DS    0H                  CHECK PROFILE, OR IF DDS, DISPLAY            
         XC    BUYXDAY,BUYXDAY     CLEAR                                        
         LR    RF,RA               CROSS DAYS                                   
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    DISP106                                                          
                                                                                
         TM    PROFILES+CNTXDAYB,CNTXDAYA                                       
         BZ    DISP110             PROFILE SET TO USE THIS?                     
                                                                                
DISP106  DS    0H                                                               
         CLI   TWAECON,C'B'        IS IT EC/BIAS?                               
         BNE   DISP110                                                          
         DROP  RF                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'        EC BIAS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DISP110                                                          
         USING RCONCCEL,R6                                                      
                                                                                
         MVC   BUYXDAY,=7C'.'      CLEAR                                        
         ZIC   RF,RCONCCCD         CROSS DAY BYTE                               
         SLL   RF,25                                                            
         LA    RE,BUYXDAY                                                       
         LA    R1,C'1'                                                          
                                                                                
DISP107  DS    0H                                                               
         LTR   RF,RF                                                            
         BNM   DISP108             IF DAY BIT IS ON                             
         STC   R1,0(RE)            PUT CORRESPONDING NUMBER THERE               
                                                                                
DISP108  DS    0H                                                               
         SLL   RF,1                NEXT DAY BIT                                 
         LA    RE,1(RE)            NEXT DAY DISPLAY POSITION                    
         LA    R1,1(R1)            NEXT EBCDIC DISPLAY NUMBER                   
         LA    R0,BUYXDAY+7        CHECK IF WE'VE LOOKED AT ALL 7 DAYS          
         CR    RE,R0                                                            
         BNH   DISP107                                                          
                                                                                
DISP110  DS    0H                                                               
         OI    BUYXDAYH+6,X'80'    XMIT IT                                      
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY EFFECTIVE DATES                                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,WORK2            OUTPUT                                       
         MVI   WORK2,C' '                                                       
*                                                                               
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R8,WORK2+60         OUTPUT END                                   
*                                                                               
         LA    R5,WORK2+120                                                     
DISPDTES LA    R3,WORK+20                                                       
*              DISPLAY DATES                                                    
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R3))     START DATE                    
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R3,5(R3)                                                         
         B     DISP150                                                          
*                                                                               
         MVI   5(R3),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R3))      END DATE                      
*                                                                               
         LA    R3,11(R3)                                                        
*                                                                               
DISP150  TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
* DISPLAY NPW IF NOT = RBUYNW OR IF OVERRIDE FLAG SET                           
         TM    8(R6),X'01'         OVERRIDE FLAG SET?                           
         BO    DISP155             YES - SHOW SPOTS                             
         CLC   RBUYNW,9(R6)        NO  - CHECK NUMBER PER WEEK                  
*                                     THIS IS REDUNDANT                         
         BE    DISP160                                                          
DISP155  EQU   *                                                                
         MVI   0(R3),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R3)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R3),C'0'                                                       
         LA    R3,2(R3)                                                         
         MVI   2(R3),C' '                                                       
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
DISP160  LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM DISPLAY LEN                         
         LR    RF,R7                                                            
         AR    RF,R3               OUTPUT PTR                                   
* CHECK IF ROOM IN FIRST LINE                                                   
         CR    RF,R8               WORK2+60                                     
         BNH   DISP162                                                          
* FIRST LINE EXCEEDED - START AT BUYDTE2                                        
         LA    R8,500(R8)          ELIM. FIRST TEST                             
         LA    R7,WORK2+60         START 2D LINE                                
         CLI   WORK2+60,C'*'                                                    
         BNE   DISP164                                                          
         LA    R7,1(R7)                                                         
         B     DISP164                                                          
*                                                                               
DISP162  CR    RF,R5               WORK2+120                                    
         BH    DISP175             DOESN'T FIT                                  
*                                                                               
DISP164  BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK+20                                                  
         MVC   WORK+20(20),MYSPACES                                             
         LA    R7,1(R3,R7)         OUTPUT PTR                                   
         BAS   RE,NEXTEL                                                        
         BNE   DISP200                                                          
*                                                                               
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         B     DISPDTES                                                         
* DATES DON'T FIT - TRY TO COMPRESS - DISPLAY WEEKS AS END DATE                 
DISP175  LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R3,WORK2                                                         
*                                                                               
DISP180  GOTO1 DATCON,(R1),(3,2(R6)),(4,(R3))                                   
         LA    R7,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   DISP182                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  R7,R0                                                            
DISP182  AR    R3,R7                                                            
* TEST 1 WEEK                                                                   
         CLI   10(R6),1            1 WEEK?                                      
         BE    DISP185                                                          
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
* DISPLAY WEEKS                                                                 
         EDIT  (1,10(R6)),(2,(R3)),ALIGN=LEFT                                   
         CLI   10(R6),9                                                         
         BH    *+16                                                             
         MVI   1(R3),C'W'                                                       
         LA    R3,2(R3)                                                         
         B     *+12                                                             
         MVI   2(R3),C'W'                                                       
         LA    R3,3(R3)                                                         
         TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
* NUMBER PER WEEK                                                               
DISP185  CLC   RBUYNW,9(R6)        SAME FOR BUY?                                
         BE    DISP190                                                          
* DISPLAY NPW                                                                   
         MVI   0(R3),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R3)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R3),C'0'                                                       
         LA    R3,2(R3)                                                         
         MVI   2(R3),C' '                                                       
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
* GET NEXT ELEM                                                                 
DISP190  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DISP200                                                          
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         B     DISP180                                                          
DISP200  MVC   BUYDTES,WORK2       DATES                                        
         CLI   WORK2+60,C' '                                                    
         BE    *+10                                                             
         MVC   BUYDTE2,WORK2+60    MOVE 2D LINE                                 
         EJECT                                                                  
*                                                                               
*              DPT-CLS-SEC-PLN-TYPE-NPW-RATE                                    
         MVC   BUYDPT(L'RBUYDPT),RBUYDPT     DAYPART                            
*                                                                               
CLSPTN   DS    0H                                                               
         TM    TWAFLAGS,TWAFLPTQ  PROFILE FOR PATTERN?                          
         BO    CLPT10                                                           
         MVC   BUYCLS(L'RBUYCLS),RBUYCLS     NO, SHOW CLASS                     
         B     CLPTEND             DONE WITH CLASS/PATTERN                      
*                                                                               
CLPT10   LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CLPTEND             NO ELEMENT, DONE                             
         USING RBUYPTEL,R6                                                      
         MVC   BUYCLS(L'RBUYPTPT),RBUYPTPT   SHOW PATTERN                       
         MVC   BUYNOT(L'RBUYPTNT),RBUYPTNT     SHOW NOTATION                    
         OI    BUYNOTH+6,X'80'                                                  
         MVI   BUYUPT,C'N'         'USE PATTERN TIMES'                          
         TM    RBUYPTFL,X'80'                                                   
         BZ    *+8                                                              
         MVI   BUYUPT,C'Y'                                                      
         OI    BUYUPTH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'01'        GET '01' ELEM BACK                           
         BAS   RE,GETEL                                                         
*                                                                               
CLPTEND  DS    0H                                                               
         MVC   BUYSEC(L'RBUYSEC),RBUYSEC  SECTION                               
*                                                                               
         MVC   BUYPLN(L'RBUYKPLN),RBUYKPLN   PLAN                               
         CLC   BUYPLN(3),=3X'FF'   NO PLAN?                                     
         BNE   *+10                                                             
         MVC   BUYPLN,MYSPACES                                                  
*                                                                               
         MVC   BUYTYP(L'RBUYTYP),RBUYTYP   PROGRAM TYPE                         
*                                                                               
* FOR DDS TERMINAL ONLY, DISPLAY DARE LINK IF ANY EXISTS                        
*                                                                               
         LR    RF,RA               CROSS DAYS                                   
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DISP203                                                          
         OC    RBUYAGBL,RBUYAGBL                                                
         BZ    DISP203                                                          
         EDIT  RBUYAGBL,(2,BUYTYP),ALIGN=LEFT                                   
         DROP  RF                                                               
*                                                                               
DISP203  DS    0H                                                               
*                                                                               
         LR    RF,RA               IF COMBO, CHECK FOR BUYS WITH                
         AH    RF,=Y(TWAWORKQ)     N/A RATES                                    
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    DISP205                                                          
         DROP  RF                                                               
*                                                                               
         TM    RBUYCOMB,X'80'      IF N/A COMBO BUY FLAG,                       
         BZ    DISP205             NUMBER PER WEEK IS SAVED AT RBUYCOMB         
         MVC   WORK+17(1),RBUYCOMB SINCE BUY HAS 0 SPOTS                        
         NI    WORK+17,X'FF'-X'80' EDIT USES FIRST 17 BYTES OF WORK             
         EDIT  (1,WORK+17),(3,BUYNPW),ALIGN=LEFT                                
         STC   R0,BUYNPWH+5        NEW OUTPUT LENGTH.  THIS ACTUALLY            
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         CLI   WORK+17,0                                                        
         BNE   DISP207                                                          
         B     DISP206                                                          
*                                                                               
DISP205  DS    0H                                                               
         EDIT  (1,RBUYNW),(3,BUYNPW),ALIGN=LEFT        NUMBER PER WEEK          
         STC   R0,BUYNPWH+5        NEW OUTPUT LENGTH.  THIS ACTUALLY            
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         CLI   RBUYNW,0                                                         
         BNE   DISP207                                                          
DISP206  MVI   BUYNPW,C'0'                                                      
         MVI   BUYNPWH+5,1         ADJUST LENGTH FOR REST OF COMBO              
*                                                                               
*** COMBO                                                                       
DISP207  LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    DISP210                                                          
         DROP  RF                                                               
         BAS   RE,COMBO            DISPLAY COMBO RATE FIELDS                    
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWACMTRD,X'40'      TRADE BUY ENCOUNTERED?                       
         BNO   DISP215             NO                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R2,BUYCOMUH         YES - SET 'COMMENT FIELD FLAG'               
         MVC   BUYCOMU(07),=C'*TRADE*'                                          
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         OI    BUYCOMUH+6,X'80'    SET FIELD TO TRANSMIT                        
         B     DISP215                                                          
*** COMBO                                                                       
*                                                                               
DISP210  DS    0H                                                               
         EDIT  (4,RBUYCOS),(9,BUYRATE),2,ALIGN=LEFT,FLOAT=-      RATE           
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   DISP215             NO                                           
         LA    RF,BUYRATE                                                       
         AR    RF,R0               ADD LENGTH OF OUTPUT                         
         MVI   0(RF),C'T'          INSERT 'TRADE' INDICATOR                     
*                                                                               
         EJECT                                                                  
DISP215  DS    0H                                                               
*                                                                               
*              DISPLAY COMMENTS                                                 
*              GET 1ST COMMENT                                                  
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP220                                                          
*                                                                               
*********************************************************************           
*********************************************************************           
*********************************************************************           
* NEW EXTENDED BUY SCREEN WILL NO LONGER DISPLAY MG= COMMENT IN                 
* THE COMMENT FIELDS. TARGET MISSED DATES WILL BE DISPLAY AT THE                
* BOTTOM OF THE SCREEN. THE CODE BELOW WILL BE MODIFIED TO REFLECT THIS         
*********************************************************************           
*********************************************************************           
*********************************************************************           
*                                                                               
* FOR MAKEGOOD OFFER APPLY, THE X'04' ELEMENT CAN EXCEED THE ACTUAL             
* SCREEN SIZE (BUY CMT 1) TO DISPLAY THE MG= REFERENCE. THEREFORE,              
* WE'LL NEED TO CONTINUE THE MG= REFERENCE LINE TO THE SECOND CMT LINE          
*                                                                               
         USING RBUYCMEL,R6                                                      
DISP214  DS    0H                                                               
         CLC   =C'MG=',RBUYCMNT                                                 
         BNE   DISP218                                                          
*        BAS   RE,MAKEGOOD                                                      
*        B     DISP220                                                          
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DISP220                                                          
*                                                                               
DISP218  DS    0H                                                               
         ZIC   R4,1(R6)                                                         
         AHI   R4,-3                                                            
         CHI   R4,60                                                            
         BL    *+8                                                              
         LA    R4,59               SET AT MAX FIELD LENGTH                      
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   BUYCOM1(0),RBUYCMNT                                              
DISP219  BAS   RE,NEXTEL                                                        
         BNE   DISP220                                                          
*                                                                               
         ZIC   R4,RBUYCMLN                                                      
         SH    R4,=H'3'                                                         
         CH    R4,=H'60'                                                        
         BL    *+8                                                              
         LA    R4,59               SET AT MAX FIELD LENGTH                      
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   BUYCOM2(0),RBUYCMNT                                              
         DROP  R6                                                               
*                                                                               
         MVC   BUYORDC,MYSPACES    IN CASE THERE ARE NO COMMENTS                
         MVC   BUYORD2,MYSPACES                                                 
*    GET BUY ORDER COMMENT ELEMENT                                              
DISP220  EQU   *                                                                
         CLC   =C'DD0',BUYACT      DISPLAY TRAPMOD ELTS?                        
         BNE   DISP225             NO                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DISP225             NO                                           
         GOTO1 =A(TRAPMODS),RR=Y                                                
DISP225  EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'84'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP250                                                          
         CLI   1(R6),4                                                          
         BL    DISP230             SKIP IF NOTHING TO PRINT                     
         ZIC   R4,1(R6)                                                         
         SH    R4,=H'4'                                                         
*                                                                               
         CH    R4,=H'60'                                                        
         BL    *+8                                                              
         LA    R4,59               SET AT MAX FIELD LENGTH                      
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   BUYORDC(0),3(R6)                                                 
DISP230  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DISP250                                                          
         CLI   1(R6),4                                                          
         BL    DISP250             SKIP IF NOTHING TO PRINT                     
         ZIC   R4,1(R6)                                                         
         SH    R4,=H'4'                                                         
*                                                                               
         CH    R4,=H'60'                                                        
         BL    *+8                                                              
         LA    R4,59               SET AT MAX FIELD LENGTH                      
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   BUYORD2(0),3(R6)                                                 
*                                                                               
DISP250  DS    0H                                                               
         XC    BUYPGM,BUYPGM                                                    
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISPXIT                                                          
         USING RBUYPGEL,R6                                                      
         ZIC   R1,RBUYPGLN                                                      
         AHI   R1,-3                                                            
         CHI   R1,L'BUYPGM-1                                                    
         BNH   *+6                                                              
         DC    H'0'                PROGRAM DOESN'T FIT                          
         EX    R1,*+4                                                           
         MVC   BUYPGM(0),RBUYPGM                                                
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
DISPXIT  DC    0H'0'                                                            
*                                                                               
* DISPLAY TARGET MISSED INFO                                                    
*                                                                               
         LA    R6,RBUYREC                                                       
         USING RBUYCMEL,R6                                                      
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISPX05                                                          
         CLC   =C'CR=',RBUYCMNT    CREDIT CANNOT BE MIXED WITH MG               
         BE    DISPX08                                                          
         DROP  R6                                                               
*                                                                               
DISPX05  DS    0H                                                               
         GOTO1 =A(DISPMISS),RR=Y                                                
         OI    BUYBACTH+6,X'40'    FORCE CURSOR HERE                            
*                                                                               
DISPX08  DS    0H                                                               
         LR    RF,RA               FOR COMBO ORDERS, DON'T REFRESH              
         AH    RF,=Y(TWAWORKQ)     THE SCREEN UNTIL THE LAST PASS               
         USING TWAWORK,RF          THIS ENSURES THE BUYLINE AND THE             
         CLI   TWACOMBO,0          CONTRACT RECORDS WILL BE IN SYNCH            
         BE    DISPX10                                                          
         CLC   TWACMBPT,TWACOMBO                                                
         BL    DISPX15                                                          
         DROP  RF                                                               
*                                                                               
DISPX10  DS    0H                                                               
         LA    R4,BUYDAYSH         PREPARE TO SET ALL VALID BITS ON             
         LA    R5,BUYLAST                                                       
         SR    RE,RE                                                            
         OI    4(R4),X'20'         VALID BIT                                    
         IC    RE,0(R4)            FIELD LEN                                    
         LA    R4,0(RE,R4)         NEXT FIELD                                   
         CR    R4,R5                                                            
         BL    *-14                                                             
*                                                                               
DISPX15  DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         XC    TWALSTKY,TWALSTKY                                                
         XC    TWALSTPL,TWALSTPL                                                
         DROP  RF                                                               
         FOUT  BUYBACTH,MYSPACES,8                                              
         OI    BUYBACTH+4,X'20'                                                 
         OI    BUYBNUMH+4,X'20'                                                 
*                                                                               
DISPX20  GOTO1 VFOUTBLK,DMCB,BUYDAYSH,BUYLAST,1                                 
         SPACE 1                                                                
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   DISPX60                                                          
*                                                                               
         LA    R4,BUYDAYSH         SET PROTECTED BITS ON ALL FIELDS             
         LA    R5,BUYLAST          EXCEPT BUY ORDER COMMENTS                    
*                                                                               
DISPX40  DS    0H                                                               
         OI    1(R4),X'20'         PROTECTED BIT                                
         OI    6(R4),X'80'         XMIT                                         
*                                                                               
DISPX50  DS    0H                                                               
         ZIC   RF,0(R4)            FIELD LEN                                    
         AR    R4,RF               NEXT FIELD                                   
*                                                                               
         LA    RF,BUYSECH                                                       
         CR    R4,RF               LEAVE SECTION UNPROTECTED                    
         BE    DISPX50                                                          
         LA    RF,BUYORDCH                                                      
         CR    R4,RF               LEAVE ORD CMT UNPROTECTED                    
         BE    DISPX50                                                          
         LA    RF,BUYORD2H                                                      
         CR    R4,RF               LEAVE ORD CMT UNPROTECTED                    
         BE    DISPX50                                                          
*                                                                               
         CR    R4,R5                                                            
         BL    DISPX40                                                          
*                                                                               
DISPX60  DS    0H                                                               
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    DISPX70                                                          
         NI    BUYCLSH+1,X'FF'-X'20'    UNPROTECT PATTERN                       
         NI    BUYNOTH+1,X'FF'-X'20'    UNPROTECT NOTATION                      
         NI    BUYUPTH+1,X'FF'-X'20'    UNPROTECT USE PATERN                    
*                                                                               
DISPX70  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*&&DO                                                                           
**********************************************************************          
* PROCESS MAKEGOOD REFERENCE LINE                                               
**********************************************************************          
MAKEGOOD NTR1                                                                   
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   MGX                                                              
         USING RBUYCMEL,R6                                                      
                                                                                
         MVC   BUYCOM1(L'BUYCOM1),RBUYCMNT                                      
*                                  MOVE ADDITIONAL DATES TO NEXT LINE           
         LA    R3,BUYCOM1+L'BUYCOM1-1                                           
         LA    R4,L'BUYCOM1                                                     
MG10     CLI   0(R3),C','                                                       
         BE    MG20                                                             
         MVI   0(R3),C' '                                                       
         BCTR  R3,0                                                             
         BCT   R4,MG10                                                          
         DC    H'0'                                                             
*                                                                               
MG20     DS    0H                  FLAG CONTINUATION                            
         MVI   0(R3),C'+'                                                       
         LA    R3,2(R4,R6)                                                      
*                                                                               
         ZIC   R1,RBUYCMLN                                                      
         SR    R1,R4                                                            
         SH    R1,=H'3'            OVERHEAD + 1                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   BUYCOM2(0),0(R3)                                                 
*                                                                               
MGX      DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
*&&                                                                             
**********************************************************************          
* COMBO BUY RATES DISPLAY                                                       
**********************************************************************          
COMBO    NTR1                                                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         NI    TWACMTRD,X'FF'-X'40'                                             
*                                  TURN OFF 'TRADE ENCOUNTERED'                 
         CLC   =C'BUY',BUYACT      BUY ACTION?                                  
         BNE   COMBO03                                                          
         LA    RF,BUYCOMUH                                                      
         MVC   BUYCOMU(07),=C'       '                                          
COMBO03  EQU   *                                                                
*                                                                               
         CLC   =C'BU',BUYACT       COMBO BUY ADD HAS NO EXISTING                
         BE    COMBOX              RATES TO DISPLAY, SO EXIT                    
         CLC   =C'CH',BUYACT       DON'T REFRESH RATE FOR ACT CHA SINCE         
         BE    COMBOX              WE NEED TO VALIDATE ALL RATES                
*                                                                               
COMBO05  LA    R2,TWACMBC1         GET NEXT COMBO K#                            
         LA    R4,BUYCBC1H         RATE DISPLAY FIELD                           
         B     COMBO20                                                          
*                                                                               
COMBO10  DS    0H                                                               
         ZIC   RF,0(R4)            BUMP TO NEXT RATE FIELD                      
         AR    R4,RF                                                            
*                                                                               
         LA    R2,9(R2)            BUMP TO NEXT COMBO K#                        
         OC    0(4,R2),0(R2)       GET BUYLINE IF CONTRACT EXISTS               
         BZ    COMBOX                                                           
*                                                                               
COMBO20  DS    0H                                                               
         ZAP   WORK(5),=P'0'       CHANGE FROM PWOS TO PWS                      
         MVO   WORK(5),0(4,R2)                                                  
         ZAP   WORK+10(5),=P'99999999' GET 9'S COMPLEMENT                       
         SP    WORK+10(5),WORK(5)                                               
         MVO   WORK(5),WORK+10(5)  CHANGE TO PWOS                               
*                                                                               
         MVC   KEY(27),RBUYREC     NUMBER                                       
         PACK  KEY+18(1),WORK+3(1) REVERSE THE COMPLIMENT                       
         PACK  KEY+19(1),WORK+2(1)                                              
         PACK  KEY+20(1),WORK+1(1)                                              
         PACK  KEY+21(1),WORK(1)                                                
*                                                                               
         OI    DMINBTS,X'08'       GET BUY KEY                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO4             GET ADDRESSIBILITY TO IO4                    
         OI    DMINBTS,X'08'       GET BUY RECORD                               
         GOTO1 VGETREC,DMCB,(R5)                                                
         TM    RCONCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    37(R5),X'80'        TM RBUYCOMB,X'80'                            
         BZ    COMBO50             IF COMBO BUY PLACE HOLDER                    
         MVC   8(2,R4),=C'NA'      DISPLAY "NA" INSTEAD OF 0 DOLLARS            
         B     COMBO60             (IT MEANS THIS BUYLINE HAS 0 SPOTS)          
*                                                                               
COMBO50  EDIT  (4,38(R5)),(9,8(R4)),2,ALIGN=LEFT,FLOAT=-      RATE              
DIE2     EQU   *                                                                
         TM    RBUYFLG2-RBUYREC(R5),X'02'  TRADE BUY?                           
         BNO   COMBO60             NO                                           
         LA    RF,8(R4)                                                         
         AR    RF,R0               ADD LENGTH OF OUTPUT                         
         MVI   0(RF),C'T'          INSERT 'TRADE' INDICATOR                     
         OI    TWACMTRD,X'40'      SET 'TRADE ENCOUNTERED'                      
*                                                                               
COMBO60  EQU   *                                                                
*                                                                               
*   TEST DIE 2                                                                  
***      MVC   DIE2,=X'0000'                                                    
*   TEST DIE 2 END                                                              
*                                                                               
         LA    RF,BUYCBC4H                                                      
         CR    R4,RF                                                            
         BL    COMBO10             LOOP UNTIL DONE                              
*                                                                               
COMBOX   XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY ABBREVIATED CONTRACT HEADER INFO                                      
*                                                                               
DISPCON  NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         MVC   BUYTYPE(L'RCONTYPE),RCONTYPE                                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISCON05                                                         
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED, SHOW MOD NUM.                     
         BO    DISCON30                                                         
*                                                                               
         ZIC   R3,1(R6)                                                         
         AR    R6,R3               GET NEXT ELEMENT                             
*                                                                               
DISCON05 DS    0H                                                               
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BNE   DISCON35                                                         
         USING RCONSEND,R6                                                      
                                                                                
DISCON10 DS    0H                                                               
         OI    CONMODH+6,X'80'     XMIT FIELD                                   
*                                                                               
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    DISCON15                                                         
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     DISCON20                                                         
DISCON15 EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
                                                                                
DISCON20 DS    0H                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET?                                    
         BZ    DISCON25                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RADIO EDI USES WIP                           
         BAS   RE,GETEL                                                         
         BNE   DISCON23                                                         
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'                                                   
         BO    DISCON25                                                         
         DROP  R6                                                               
*                                                                               
DISCON23 DS    0H                                                               
         MVC   CONMOD(7),=C'ESL VER'                                            
         B     DISCON35                                                         
                                                                                
DISCON25 DS    0H                  DISPLAY WORK IN PROGRESS                     
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISCON35                                                         
         USING RCONSEND,R6                                                      
*                                                                               
         MVC   CONMOD+4(3),=C'VER'                                              
         TM    RCONSENF,X'10'+X'20' SHOW IF WIP                                 
         BO    DISCON35                                                         
         MVC   CONMOD(3),=C'WIP'                                                
         B     DISCON35                                                         
         DROP  R6                                                               
                                                                                
DISCON30 CLI   RCONMOD,0           K MOD NUM                                    
         BE    DISCON35                                                         
         MVC   CONMOD(7),=C'MOD NUM'                                            
         EDIT  (1,RCONMOD),(3,CONMOD+8),ALIGN=LEFT                              
*                                                                               
DISCON35 DS    0H                                                               
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,BUYSTA                         
*                                                                               
         MVC   BUYSAL,RCONSAL                                                   
*                                                                               
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),REPALPHA                                               
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),BUYHDTE,0,WORK            
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,BUYAGY,WORK2,WORK              
         MVC   BUYAGYN,WORK2                                                    
*                                                                               
         MVC   BUYADV,RCONKADV                                                  
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKADV,WORK2,0,WORK                   
         MVC   BUYADVN,WORK2                                                    
*                                                                               
*&&DO                                                                           
         CLC   RCONPRD,MYSPACES    PRODUCT CODE?                                
         BE    DISCON40                                                         
*                                                                               
         MVC   BUYPRD(2),=C'C='                                                 
         MVC   BUYPRD+2(3),RCONPRD                                              
         MVI   BUYPRD+5,0                                                       
         MVC   BUYPRD+6(9),TWAPRDNM                                             
         B     DISCON60                                                         
*                                                                               
*              FIND PRODUCT ELEMENT                                             
*                                                                               
DISCON40 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISCON60                                                         
         MVC   BUYPRD,2(R6)        PRODUCT EXPANSION                            
*                                                                               
* DISPLAY ESTIMATE NUMBER                                                       
*                                                                               
DISCON60 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISCONX             NO EI CODES                                  
         USING RCONIEL,R6                                                       
*                                                                               
         MVC   BUYEST,RCONXEST     DEFAULT USING EXPANDED EST #                 
*                                                                               
         OC    BUYEST,MYSPACES     IF NO EXPANDED ESTIMATE NUMBER FOUND         
         CLC   BUYEST,MYSPACES     USE OLD FORMAT                               
         BNE   DISCONX                                                          
         MVC   BUYEST(L'RCONIEST),RCONIEST                                      
         DROP  R6                                                               
*&&                                                                             
*                                                                               
DISCONX DS     0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTDED                                                       
         EJECT                                                                  
*********************************************************************           
* DISPLAY MISSED DATE/SPT/TARGET LINE INFO AT BOTTOM OF SCREEN      *           
*********************************************************************           
T8022E   CSECT                                                                  
DISPDEMO NMOD1 DDEMODX-DDEMOD,*DDEMO*,CLEAR=YES                                 
         LR    R5,RC                                                            
         USING DDEMOD,R5                                                        
         L     RC,0(R1)                                                         
*                                                                               
         OI    BUYDVALH+1,X'20'                                                 
         CLI   RCONKSTA+4,C'A'     RADIO SHOULDN'T SEE DEMO                     
         BE    DDEMOX                                                           
         CLI   RCONKSTA+4,C'F'                                                  
         BE    DDEMOX                                                           
*                                                                               
         XC    BUYDCAT,BUYDCAT                                                  
         XC    BUYDVAL,BUYDVAL                                                  
* HAN    NI    BUYDVALH+1,X'FF'-X'20'                                           
         MVI   DEMOFLGS,0                                                       
         XC    WORK2,WORK2                                                      
*                                                                               
         LA    R6,RCONREC          USE AGENCY DEMO IF PRESENT                   
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEMO03                                                          
         CLI   1(R6),18            IGNORE OLD FORMAT AGY DEMO                   
         BNE   DDEMO03                                                          
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'0E'        DISPLAY USER OVERRIDE                        
         BAS   RE,GETEL                                                         
         BE    DDEMO60                                                          
         OI    BUYDVALH+1,X'20'    PROTECT FIELD AND EXIT                       
*                                                                               
         B     DDEMOX                                                           
*                                                                               
DDEMO03  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEMO05                                                          
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'20'       LOCAL INDICATOR SET?                         
         BZ    DDEMO05                                                          
         CLC   =C'CB',REPALPHA     SKIP CHECK FOR CBS                           
         BE    DDEMO05                                                          
         CLC   =C'FF',REPALPHA     SKIP CHECK FOR FOXLOC                        
         BE    DDEMO05                                                          
         OI    BUYDVALH+1,X'20'                                                 
         B     DDEMOX              YES, PROTECT FIELD AND EXIT                  
         DROP  R6                                                               
*                                                                               
DDEMO05  DS    0H                                                               
*                                                                               
*&&DO                                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        EDI ORDER?                                   
         BAS   RE,GETEL                                                         
         BNE   DDEMO10                                                          
*                                                                               
         TM    PROFILES+CNTDMOVB,CNTDMOVA                                       
         BZ    DDEMO10             REP CAN OVERRIDE DEMO?                       
         OI    BUYDVALH+1,X'20'    NO,PROTECT FIELD                             
*&&                                                                             
DDEMO10  DS    0H                                                               
         GOTO1 =A(CKAGDEM),RR=Y                                                 
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEMO13                                                          
*                                                                               
         OI    DEMOFLGS,X'80'      DARE AGENCY DEMO PRESENT                     
*                                                                               
DDEMO13  DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'0E'                                                     
         BAS   RE,GETEL                                                         
         BE    DDEMO60                                                          
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEMO15                                                          
*                                                                               
         CLI   1(R6),18            IGNORE OLD DEMO                              
         BE    DDEMO14                                                          
         CLI   1(R6),34                                                         
         BNE   DDEMO60                                                          
*                                                                               
DDEMO14  DS    0H                                                               
         OI    BUYDVALH+1,X'20'    NO,PROTECT FIELD                             
         B     DDEMOX                                                           
*                                                                               
DDEMO15  DS    0H                                                               
*                                                                               
* AGENCY DID NOT SEND OVER ANY DEMOS. ALLOW REP TO INPUT DEMOS                  
*                                                                               
* HAN    NI    BUYDVALH+1,X'FF'-X'20'                                           
*                                                                               
         OI    DEMOFLGS,X'40'      NO VALUES PRESENT                            
         LA    R6,RCONREC          USE AGENCY DEMO IF PRESENT                   
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEMO20                                                          
         CLI   1(R6),18            IGNORE OLD FORMAT AGY DEMO                   
         BE    DDEMO20                                                          
         USING RCONDDEL,R6                                                      
         MVC   WORK2(3),RCONDDCT                                                
         CLC   WORK2(3),MYSPACES                                                
         BNE   DDEMO120                                                         
*                                                                               
* NO AGENCY DEMO PRESENT, USE PENDING DEMO IF PRESENT                           
*                                                                               
DDEMO20  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    DDEMO30                                                          
         USING RSARXEL,R6                                                       
*                                                                               
DDEMO25  DS    0H                                                               
         MVC   BUYDCAT(9),=C'*NO DEMO*'                                         
         OI    BUYDVALH+1,X'20'    NO DEMOS, PROTECT FIELD                      
         B     DDEMOX                                                           
*                                                                               
DDEMO30  DS    0H                                                               
         MVC   WORK2(3),RSARXDEM   DEFAULT                                      
         LA    R3,RSARXDEM                                                      
         LA    R4,8                                                             
*                                                                               
DDEMO40  DS    0H                                                               
         TM    0(R3),X'40'         PRIMARY?                                     
         BO    DDEMO50                                                          
         AHI   R3,3                                                             
         BCT   R4,DDEMO40                                                       
         OC    WORK2(3),WORK2      NO DEMO?                                     
         BZ    DDEMO25                                                          
         B     DDEMO120                                                         
*                                                                               
DDEMO50  DS    0H                                                               
         MVC   WORK2(3),0(R3)      PRIMARY FOUND                                
         B     DDEMO120                                                         
*                                                                               
DDEMO60  DS    0H                                                               
         USING RBUYDMEL,R6                                                      
*                                                                               
         SR    R2,R2                                                            
         ZIC   R3,RBUYDMLN                                                      
         SHI   R3,2                                                             
         LA    RE,L'RBUYDMCV                                                    
         DR    R2,RE               R3 HAS NUMBER OF DEMOS TO CHECK              
*                                                                               
         LA    R4,RBUYDMCT         CHECK FOR PRIMARY DEMO, IF ANY               
*                                  ELSE TAKE FIRST DEMO AS PRIMARY              
         MVC   WORK2(3),RBUYDMCT   AS DEFAULT                                   
         CLC   3(4,R4),=AL4(-1)                                                 
         BE    DDEMO70             SKIP DISPLAY FOR -1 (NO DEMO)                
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),3(4,R4)                                                  
         EDIT  (P5,WORK),(8,BUYDVAL),1,ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
DDEMO70  DS    0H                                                               
         CLI   0(R4),C'('          USER DEFINED?                                
         BE    DDEMO75                                                          
         TM    0(R4),X'40'         PRIMARY?                                     
         BO    DDEMO80                                                          
DDEMO75  DS    0H                                                               
         AHI   R4,L'RBUYDMCV                                                    
         BCT   R3,DDEMO70                                                       
         LA    R4,RBUYDMCT                                                      
         B     DDEMO90                                                          
         DROP  R6                                                               
*                                                                               
DDEMO80  DS    0H                                                               
         MVC   WORK2(3),0(R4)                                                   
*                                                                               
         XC    BUYDVAL,BUYDVAL                                                  
         CLC   3(4,R4),=AL4(-1)                                                 
         BE    DDEMO90             SKIP DISPLAY FOR -1 (NO DEMO)                
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),3(4,R4)                                                  
         EDIT  (P5,WORK),(8,BUYDVAL),1,ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
DDEMO90  DS    0H                                                               
         MVI   BUYDCAT+9,C' '                                                   
         CLC   3(4,R4),7(R4)                                                    
         BE    DDEMO100                                                         
         CLC   7(4,R4),=AL4(-1)    NO PREVIOUS VALUE                            
         BE    DDEMO100                                                         
         MVI   BUYDCAT+9,C'*'                                                   
*                                                                               
DDEMO100 DS    0H                  DEMO IS NOT PROVIDE BY AGENCY (-1)           
         CLC   3(4,R4),=AL4(-1)    OR IS ZERO, ALLOW REP CHANGES                
         BE    DDEMO120                                                         
         CLI   0(R6),RBUYRDCQ      REP DEMO PRESENT?                            
         BNE   DDEMO120            YES, REP HAS ALREADY OVERRIDDEN              
*        OC    3(4,R4),3(R4)       AGENCY DEMO, UNPROTECT FIELD                 
*        BNZ   DDEMO120                                                         
*                                                                               
         NI    BUYDVALH+1,X'FF'-X'20'                                           
*                                                                               
DDEMO120 DS    0H                                                               
         CLI   WORK2+1,C'T'        FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK2+1,C'I'                                                     
*                                                                               
         LA    R3,DDEMOIO                                                       
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000AE0',0                                      
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DEMCON,0(R1)                                                     
*                                                                               
         GOTO1 DEMCON,DMCB,(1,WORK2),(2,WORK),(0,DBLOCKD)                       
         DROP  R3                                                               
*                                                                               
         MVC   BUYDCAT(7),WORK                                                  
         MVI   BUYDCAT+7,C':'                                                   
         TM    DEMOFLGS,X'40'                                                   
         BO    DDEMOX                                                           
*                                                                               
DDEMO130 DS    0H                                                               
         MVI   BUYDCAT+8,C'A'                                                   
         CLI   0(R6),X'0D'                                                      
         BE    DDEMO140                                                         
         MVI   BUYDCAT+8,C'R'                                                   
         TM    DEMOFLGS,X'80'                                                   
         BZ    DDEMO140                                                         
         MVI   BUYDCAT+9,C'*'                                                   
*                                                                               
DDEMO140 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEMO145                                                         
         CLI   1(R6),18            OLD FORMAT DEMO                              
         BNE   DDEMOX                                                           
DDEMO145 DS    0H                                                               
         LA    R6,RCONREC          DEFAULT SHOW R IF DARE                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEMO150                                                         
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'                                                   
         BO    DDEMOX                                                           
*                                                                               
DDEMO150 DS    0H                                                               
         MVI   BUYDCAT+8,C' '                                                   
*                                                                               
DDEMOX   DS    0H                                                               
         OI    BUYDCATH+6,X'80'                                                 
         OI    BUYDVALH+6,X'80'                                                 
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* LOCAL STORAGE AREA                                                            
*                                                                               
DDEMOD   DSECT                                                                  
DEMOFLGS DS    X                                                                
* X'80' = DARE AGENCY DEMO PRESENT                                              
* X'40' = DISPLAY CATEGORY AND EXIT                                             
DDEMOIO  DS    XL512                                                            
DDEMODX  EQU   *                                                                
         EJECT                                                                  
*********************************************************************           
* OVERRIDE BUY COMMENT WITH TRAPMOD ELEMENT DISPLAY                 *           
*********************************************************************           
T8022E   CSECT                                                                  
TRAPMODS NTR1  BASE=*,LABEL=*                                                   
         XC    BUYCOM1,BUYCOM1     CLEAR OUT BUYCOM1                            
         LA    R2,RBUYELEM         SET A(DESCRIPTOR ELEMENT)                    
         LA    R3,BUYCOM1                                                       
         LA    R4,8                SET MAX DISPLAY                              
TMOD0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    TMOD0900            YES - FINISHED                               
         CLI   0(R2),X'D0'         TRAPMOD ELEMENT?                             
         BNE   TMOD0080            NO                                           
         EDIT  (1,2(R2)),(3,0(R3)) EDIT VERSION NUMBER                          
         MVC   4(2,R3),3(R2)       INSERT MOD CODE(S)                           
         ZIC   RF,1(R2)                                                         
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         LA    R3,7(R3)            BUMP TO NEXT SPACE IN BUYCOM1                
         BCT   R4,TMOD0020         GO BACK FOR NEXT                             
         B     TMOD0900            ONLY DO 8                                    
TMOD0080 EQU   *                                                                
         ZIC   RF,1(R2)                                                         
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     TMOD0020                                                         
TMOD0900 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHK AGENCY DEMO AND PROTECT/UNPROTECT  DEMO FIELD                 *           
*********************************************************************           
T8022E   CSECT                                                                  
CKAGDEM  NTR1  BASE=*,LABEL=*                                                   
         NI    BUYDVALH+1,X'FF'-X'20'                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        EDI ORDER?                                   
         BAS   RE,GETEL                                                         
         BNE   CDEMO10                                                          
*                                                                               
         TM    PROFILES+CNTDMOVB,CNTDMOVA                                       
         BZ    CDEMO10             REP CAN OVERRIDE DEMO?                       
         OI    BUYDVALH+1,X'20'    NO,PROTECT FIELD                             
*                                                                               
CDEMO10  DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL                                                         
         BNE   CDEMO105            UNPROTECT FIELD                              
         CLI   1(R6),18            IGNORE OLD DEMO                              
         BE    CDEMOX                                                           
         CLI   1(R6),34                                                         
         BE    CDEMOX                                                           
*                                                                               
CDEMO60  DS    0H                                                               
         USING RBUYDMEL,R6                                                      
*                                                                               
         SR    R2,R2                                                            
         ZIC   R3,RBUYDMLN                                                      
         SHI   R3,2                                                             
         LA    RE,L'RBUYDMCV                                                    
         DR    R2,RE               R3 HAS NUMBER OF DEMOS TO CHECK              
*                                                                               
         LA    R4,RBUYDMCT         CHECK FOR PRIMARY DEMO, IF ANY               
*                                  ELSE TAKE FIRST DEMO AS PRIMARY              
CDEMO70  DS    0H                                                               
         CLI   0(R4),C'('          USER DEFINED?                                
         BE    CDEMO75                                                          
         TM    0(R4),X'40'         PRIMARY?                                     
         BO    CDEMO100                                                         
CDEMO75  DS    0H                                                               
         AHI   R4,L'RBUYDMCV                                                    
         BCT   R3,CDEMO70                                                       
         LA    R4,RBUYDMCT                                                      
         DROP  R6                                                               
*                                                                               
CDEMO100 DS    0H                  DEMO IS NOT PROVIDE BY AGENCY (-1)           
         CLC   3(4,R4),=AL4(-1)    OR IS ZERO, ALLOW REP CHANGES                
         BE    CDEMO105                                                         
         OC    3(4,R4),3(R4)                                                    
         BNZ   CDEMOX                                                           
CDEMO105 DS    0H                                                               
         NI    BUYDVALH+1,X'FF'-X'20'                                           
*                                                                               
CDEMOX   DS    0H                                                               
         OI    BUYDVALH+6,X'80'                                                 
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* CHECK STATION PROFILES TO SEE IF WE NEED TO DISPLAY DEMO OR NOT               
*                                                                               
CKSTPROF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   CKSTPRY                                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         TM    PROFILES+CNTDEMOB,CNTDEMOA                                       
         BO    CKST0100            DISALLOW STATION TO SEE DEMO                 
*                                                                               
         TM    TWASTAOB,X'02'                                                   
         BO    CKSTPRN             DO NOT PROCESS DEMO                          
         B     CKSTPRY                                                          
CKST0100 DS    0H                                                               
         TM    TWASTAOB,X'02'                                                   
         BZ    CKSTPRN                                                          
         DROP  RF                                                               
*                                                                               
CKSTPRY  SR    RC,RC                                                            
CKSTPRN  LTR   RC,RC                                                            
CKSTPRX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
* DISPLAY MISSED DATE/SPT/TARGET LINE INFO AT BOTTOM OF SCREEN      *           
*********************************************************************           
T8022E   CSECT                                                                  
DISPMISS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    BUYMSD1H+6,X'80'                                                 
         OI    BUYMSD2H+6,X'80'                                                 
         OI    BUYMSD3H+6,X'80'                                                 
*                                                                               
         MVC   BUYMSD1(12),=C'Missed Dates'                                     
         MVC   BUYMSD2(12),=C'Missed Dates'                                     
         MVC   BUYMSD3(12),=C'Missed Dates'                                     
         TM    RBUYRTS,X'02'                                                    
         BZ    DISPMS10                                                         
         MVC   BUYMSD1(12),=C'NA Dates    '                                     
         MVC   BUYMSD2(12),=C'NA Dates    '                                     
         MVC   BUYMSD3(12),=C'NA Dates    '                                     
*                                                                               
DISPMS10 DS    0H                                                               
         LA    R6,RBUYELEM                                                      
DISPMS20 CLI   0(R6),X'05'                                                      
         BE    DISPMS30                                                         
         BH    DISPMSX                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   DISPMS20                                                         
         B     DISPMSX                                                          
*                                                                               
DISPMS30 DS    0H                                                               
         USING RBUYMGEL,R6                                                      
         LA    R2,BUYMD1H                                                       
*                                                                               
DISPMS40 DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,RBUYMGD1),(4,8(R2))    START DATE                 
         OC    RBUYMGD2,RBUYMGD2                                                
         BZ    DISPMS50                                                         
         MVI   13(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RBUYMGD2),(4,14(R2))   END DATE                   
*                                                                               
DISPMS50 DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         EDIT  RBUYMGSP,(3,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         EDIT  RBUYMGLI,(3,8(R2)),ALIGN=LEFT                                    
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'05'                                                      
         BE    DISPMS40                                                         
*                                                                               
DISPMSX  DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHKDARE CHECKS FOR DARE ORDERS.  IF FOUND, LIMITS FIELD CHANGES   *           
* FOR NON-TV DARE ORDERS, ALL FIELDS ARE OPEN FOR CHANGES                       
*********************************************************************           
CHKDARE  NMOD1 0,*CKDA*                                                         
         L     RC,0(R1)                                                         
         BAS   RE,CLRPROTS         CLEAR ALL PROTECTION BITS                    
*                                                                               
         CLI   RCONKSTA+4,C'A'     RADIO?                                       
         BE    CHDA0160                                                         
         CLI   RCONKSTA+4,C'F'                                                  
         BE    CHDA0160                                                         
*                                                                               
         LA    R6,RCONREC          LOOK FOR SEND ELEMENT                        
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL2                                                        
         BNE   CHDA0060                                                         
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20' CONFIRMED/PREVIOUSLY CONFIRMED?             
         BZ    CHDA0060            YES - MUST BE UNCONFIRMED                    
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BNE   CHDA0160                                                         
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CHDA0160            PROTECT PRODUCT FIELD FOR                    
         TM    RCONDRF2,X'01'      EXIT IF XML                                  
         BO    CHDA0160                                                         
         OC    RCONDRRV,RCONDRRV   DON'T PROTECT FOR DARE REVISION              
         BNZ   CHDA0020                                                         
         TM    RCONDRF2,X'80'+X'10' POOL/VARIOUS                                
         BZ    CHDA0160                                                         
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BO    CHDA0160                                                         
         B     CHDA0110                                                         
*                                                                               
* FOR REVISION, CHECK IF APPROVAL DATE/TIME IS MORE CURRENT THAN THE            
* LAST CONFIRMATION DATE. IF SO, WE NEED TO LOCK THE BUY                        
* (IF AGENCY RESENDS ORDER,) CHECK IF DELIVERY DATE IS MORE RECENT              
* THAN THE APPROVAL DATE                                                        
*                                                                               
CHDA0020 EQU   *                                                                
         CLC   RCONDRDD(4),RCONDRDA                                             
         BH    CHDA0160            SENT/RESENT NEWER, DON'T PROTECT FLD         
CHDA0030 EQU   *                                                                
         MVC   WORK(4),RCONDRDA                                                 
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC          LOOK FOR CONFIRMATION ELEMENT                
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL2                                                        
         BNE   CHDA0110            NOT FOUND, LOCK BUY                          
         USING RMODELEM,R6                                                      
         CLC   RMODEL1D,WORK                                                    
         BH    CHDA0160                                                         
         BL    CHDA0110                                                         
         GOTO1 HEXIN,DMCB,RMODEL1T,WORK+4,4                                     
         CLC   WORK+4(2),WORK+2                                                 
         BL    CHDA0110                                                         
         B     CHDA0160                                                         
         DROP  R6                                                               
*                                                                               
CHDA0060 EQU   *                                                                
         LA    R6,RCONREC          LOOK FOR DARE ELEMENT                        
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BNE   CHDA0160                                                         
         USING RCONDREL,R6                                                      
*                                                                               
         OC    RCONDRLK,RCONDRLK   FOR DARE PURGE *TEMP*                        
         BZ    CHDA0160                                                         
         TM    RCONDRF2,X'01'      EXIT IF XML                                  
         BO    CHDA0160                                                         
*                                                                               
* hq     CLI   RCONDRFG,0          ANYTHING SET IN STATUS BYTE?                 
* hq     BE    CHDA0160            NO  - NOT DARE CONTRACT                      
* KATZ EDI ORDER? OR ONE SHOT ? OR TAKEOVER??                                   
         TM    RCONDRFG,X'04'+X'02'+X'01'                                       
         BNZ   CHDA0160            YES, DON'T TREAT AS DARE CONTRACT            
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CHDA0110                                                         
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BO    CHDA0160                                                         
         DROP  R6                                                               
*                                                                               
CHDA0110 EQU   *                                                                
         LA    R2,FIELDTBL         SET A(SCREEN FIELD TABLE)                    
CHDA0120 EQU   *                                                                
         CLI   0(R2),X'FF'         TABLE DELIMITER REACHED?                     
         BE    CHDA0150            YES - RESTRICTED FIELDS                      
*                                     PROTECTED FOR DARE ORDERS                 
         TM    4(R2),X'80'         FIELD ALLOWS CHANGE?                         
         BO    CHDA0140            YES - GO TO NEXT FIELD                       
         MVC   DUB(4),0(R2)        NO  - GET A(FIELD HDR)                       
         L     R3,DUB                                                           
         LA    RF,TWAD             SET ADDRESSABILITY                           
         AR    R3,RF                                                            
         ZIC   R1,0(R3)                                                         
         TM    1(R3),X'02'         EXTEND FIELD HEADER PRESENT?                 
         BZ    *+8                 NO                                           
         SH    R1,=H'8'            YES, SUBTRACT 8 MORE                         
         SH    R1,=H'9'            MINUS FIELD HEADER +1 FOR EX                 
         BNP   CHDA0130            NEGATIVE? SHOULD NEVER HAPPEN                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR FIELD                                  
                                                                                
CHDA0130 DS    0H                                                               
         OI    1(R3),X'20'         SET FIELD TO 'PROTECTED'                     
         OI    6(R3),X'80'         SET FIELD TO 'TRANSMIT'                      
CHDA0140 EQU   *                                                                
         LA    R2,5(R2)            BUMP TO NEXT ENTRY                           
         B     CHDA0120            GO BACK FOR NEXT                             
CHDA0150 EQU   *                   RETURN WITH CC= ZERO                         
*        BAS   RE,PROTMISS         PROCTECT MISS DATE FIELDS                    
*                                                                               
CHDA0160 EQU   *                   RETURN WITH CC= ZERO                         
         SR    R0,R0               SET CC = ZERO                                
CHDA0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CLRPROTS:  CLEARS ALL PROTECTION BITS FROM FIELDS IF PREVIOUSLY             
*        SET.                                                                   
*                                                                               
CLRPROTS NTR1                                                                   
         LA    R2,FIELDTBL         SET A(SCREEN FIELD TABLE)                    
CLPR0020 EQU   *                                                                
         CLI   0(R2),X'FF'         TABLE DELIMITER REACHED?                     
         BE    CLPR0040            YES -                                        
         MVC   DUB(4),0(R2)        NO  - GET A(FIELD HEADER)                    
         L     R3,DUB                                                           
         LA    RF,TWAD             SET ADDRESSABILITY                           
         AR    R3,RF                                                            
         NI    1(R3),X'DF'         TURN OFF PROTECT BIT                         
         OI    6(R3),X'80'         TURN ON TRANSMIT BIT                         
         LA    R2,5(R2)            BUMP TO NEXT ENTRY                           
         B     CLPR0020            GO BACK FOR NEXT                             
CLPR0040 EQU   *                                                                
*                                                                               
         LA    R4,15*3                                                          
         LA    R2,BUYMD1H                                                       
*                                                                               
CLPR0045 DS    0H                                                               
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT BIT                         
         OI    6(R2),X'80'         SET FIELD TO 'TRANSMIT'                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,CLPR0045                                                      
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          COMBO CONTRACT?                              
         DROP  RF                                                               
         BE    CLPR0050            NO - SKIP                                    
         OI    BUYPLNH+1,X'20'     YES - PROTECT PLAN FIELD                     
         OI    BUYPLNLH+1,X'0C'      HIDE LABEL                                 
         B     *+8                                                              
CLPR0050 NI    BUYPLNLH+1,X'FF'-X'0C' DISPLAY LABEL                             
         OI    BUYPLNLH+6,X'80'                                                 
*                                                                               
CLPRX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
* PROTECT MISSED DATE FIELDS                                                    
*                                                                               
PROTMISS NTR1                                                                   
*                                                                               
         LA    R4,15*3                                                          
         LA    R2,BUYMD1H                                                       
*                                                                               
PMISS10  DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         TM    1(R2),X'02'         EXTEND FIELD HEADER PRESENT?                 
         BZ    *+8                 NO                                           
         SHI   R1,8                YES, SUBTRACT 8 MORE                         
         SHI   R1,9                MINUS FIELD HEADER +1 FOR EX                 
         BNP   PMISS20             NEGATIVE? SHOULD NEVER HAPPEN                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
PMISS20  OI    1(R2),X'20'         SET FIELD TO 'PROTECTED'                     
         OI    6(R2),X'80'         SET FIELD TO 'TRANSMIT'                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,PMISS10                                                       
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         GETEL2 R6,34,ELCODE                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CONTRACT SCREEN FIELD TABLE CONTAINS ADDRESS OF EACH SCREEN                 
*      FIELD HEADER, AND A FLAG BYTE.  IF FIELD CAN BE CHANGED,                 
*      FLAG BYTE IS SET TO X'80'.                                               
*                                                                               
FIELDTBL DC    AL4(BUYDAYSH-TWAD),X'00'                                         
         DC    AL4(BUYTIMSH-TWAD),X'00'                                         
         DC    AL4(BUYLENH-TWAD),X'00'                                          
         DC    AL4(BUYDTESH-TWAD),X'00'                                         
         DC    AL4(BUYDTE2H-TWAD),X'00'                                         
         DC    AL4(BUYTYPH-TWAD),X'80'                                          
         DC    AL4(BUYDPTH-TWAD),X'80'                                          
         DC    AL4(BUYCLSH-TWAD),X'80'                                          
         DC    AL4(BUYSECH-TWAD),X'80'                                          
         DC    AL4(BUYPLNH-TWAD),X'80'                                          
         DC    AL4(BUYNPWH-TWAD),X'00'                                          
         DC    AL4(BUYRATEH-TWAD),X'00'                                         
         DC    AL4(BUYCBC1H-TWAD),X'80'                                         
         DC    AL4(BUYCBC2H-TWAD),X'80'                                         
         DC    AL4(BUYCBC3H-TWAD),X'80'                                         
         DC    AL4(BUYCBC4H-TWAD),X'80'                                         
         DC    X'FF'               DELIMITER                                    
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'205RECNT2E   02/13/12'                                      
         END                                                                    
