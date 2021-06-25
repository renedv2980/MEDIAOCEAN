*          DATA SET RECNT25    AT LEVEL 190 AS OF 06/18/08                      
*PHASE T80225A                                                                  
*INCLUDE OUTDAY                                                                 
         TITLE 'T80225 - REPPAK SINGLE BUY DISPLAY'                             
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT25 (T80225) --- SINGLE BUY DISPLAY                  *             
*                                                                 *             
* --------------------------------------------------------------- *             
* REFER TO RECNTHIST FOR PAST HISTORY:                            *             
*                                                                 *             
* 25NOV02 SKU OPEN ALL FIELDS FOR RADIO EDI ORDERS                *             
* 25JUL01 BU  NEW FIELDS ON SCREEN                                *             
* 18JAN01 RHV PROGRAM FIELD                                       *             
* 23MAY00 BU  TRADE PROCESSING: COMBO                             *             
* 20MAY00 BU  TRADE PROCESSING                                    *             
* 08MAY00 SKU MULTI-MAKEGOOD USAGE CHECK                          *             
* 27MAR00 SKU CANCEL INDICATOR BUG FIX                            *             
* 01MAR99 AST DISPLAY PATTERN INSTEAD OF CLASS                    *             
* 04JAN99 RHV HANDLE NON-DELETED CANCELLED BUYS                   *             
* 13FEB98 SKU DISPLAY DARE REVISION AGENCY BUY LINK               *             
* 12NOV97 SKU FIX MAKEGOOD OFFER APPLY BUG                        *             
* 08OCT97 SKU UNPROTECT FIELDS FOR DARE REVISION ORDERS           *             
* 23JUL97 SKU 4K CONTRACT SUPPORT                                 *             
* 10JUL97 SKU UNPROTECT FIELDS FOR TAKEOVER CONTRACTS             *             
* 13JUN97 SKU RELAX DARE CHECK FOR REMOVED DARE ORDERS            *             
* 04APR97 SKU EXTEND FIELD LOCK FOR DARE POOL/VARIOUS ORDERS      *             
* 01APR96 SKU PREVENT COMMENTS FROM OVERFLOWING SCREEN FIELDS     *             
* 09MAR96 SKU STEREO SUPPORT                                      *             
* 06FEB96 SKU KATZ EDI DARE ORDERS HAVE FULL ACCESS               *             
* 13DEC95 SKU 2K CONTRACT SUPPORT                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80225   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80225                                                         
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         OI    BUYFLTH+1,X'20'     PROTECT FLIGHT FIELD                         
         OI    BUYFLTH+6,X'80'     TRANSMIT FIELD                               
         TM    TWASTREO,X'80'      IS THIS STEREO?                              
         BZ    *+8                 NO                                           
         NI    BUYFLTH+1,X'FF'-X'20'    YES, UNPROTECT FLIGHT                   
*                                                                               
         LA    R6,RCONREC          LOOK FOR RANDOM FLAG ELEMENT                 
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   MAIN0010                                                         
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       'TRADE' CONTRACT?                            
         BNO   MAIN0010            NO  -                                        
         OI    BUYFLAGS,X'80'      YES - SET INDICATOR                          
         DROP  R6                                                               
MAIN0010 EQU   *                                                                
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
DISP4    DS    0H                                                               
         LR    RF,RA               FOR COMBO K'S, DON'T CLEAR COMBO             
         AH    RF,=Y(TWAWORKQ)       RATE FIELDS FOR ACTION BUY/CHA             
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    DISP10                                                           
         DROP  RF                                                               
         CLC   =C'BUY',BUYACT                                                   
         BE    DISP8                                                            
         CLC   =C'CHA',BUYACT                                                   
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
         LA    R2,CONBNUMH                                                      
         LA    R3,BUYERR                                                        
*                                                                               
         CLC   BUYACT,=C'BUY'                                                   
         BE    DISP50                                                           
         CLC   BUYACT,=C'BUX'      SPECIAL NO BUCKET ACTION                     
         BE    DISP50                                                           
         CLC   BUYACT,=C'CHA'                                                   
         BE    DISP50                                                           
         CLC   BUYACT,=C'CHX'      SPECIAL NO BUCKET ACTION                     
         BE    DISP50                                                           
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
*                                                                               
*   CLEAR SCREEN *TRADE* DISPLAY                                                
*                                                                               
***      LR    RF,RA                                                            
***      AH    RF,=Y(TWAWORKQ)                                                  
***      USING TWAWORK,RF                                                       
*                                                                               
***      TM    TWACMTRD,X'40'      TRADE BUY ENCOUNTERED HERE?                  
*                                                                               
***      DROP  RF                                                               
*                                                                               
***      BO    DISP40              YES - DON'T CLEAR FIELD                      
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
         GOTO1 =A(CHECKMG),RR=Y    CHECK IF MULTI-MAKEGOOD IN USE               
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
         SR    R9,R9                                                            
         IC    R9,BUYTIMSH                                                      
         LA    R9,BUYTIMSH-1(R9)   TIME FIELD END-1                             
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
         CR    RE,R9               ROOM LEFT?                                   
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
*          DATA SET RECNT25B   AT LEVEL 092 AS OF 02/06/96                      
         SPACE 3                                                                
*                                                                               
* DISPLAY FLIGHT & MOD CODES IF STEREO IS ACTIVE                                
DISP102  TM    TWASTREO,X'80'      IS STEREO ACTIVE?                            
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
         LA    R9,WORK2+120                                                     
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
DISP162  CR    RF,R9               WORK2+120                                    
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
         GOTO1 =A(COMBO),RR=Y      DISPLAY COMBO RATE FIELDS                    
*                                                                               
         TM    BUYFLAGS,X'80'      TRADE BUY?                                   
         BO    DISP207A            YES                                          
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWACMTRD,X'40'      TRADE BUY ENCOUNTERED?                       
         BNO   DISP215             NO                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
DISP207A EQU   *                                                                
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
         ZIC   R4,1(R6)                                                         
         SH    R4,=H'3'                                                         
         CH    R4,=H'60'                                                        
         BL    DISP218                                                          
*                                                                               
* FOR MAKEGOOD OFFER APPLY, THE X'04' ELEMENT CAN EXCEED THE ACTUAL             
* SCREEN SIZE (BUY CMT 1) TO DISPLAY THE MG= REFERENCE. THEREFORE,              
* WE'LL NEED TO CONTINUE THE MG= REFERENCE LINE TO THE SECOND CMT LINE          
*                                                                               
         USING RBUYCMEL,R6                                                      
         CLC   =C'MG=',RBUYCMNT                                                 
         BNE   DISP216                                                          
         BAS   RE,MAKEGOOD                                                      
         B     DISP220                                                          
*                                                                               
DISP216  DS    0H                                                               
         LA    R4,59               SET AT MAX FIELD LENGTH                      
*                                                                               
DISP218  DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   BUYCOM1(0),RBUYCMNT                                              
         BAS   RE,NEXTEL                                                        
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
DISP220  LA    R6,RBUYREC                                                       
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
         B     DISP250                                                          
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
         FOUT  CONBACTH,MYSPACES,8                                              
         OI    CONBACTH+4,X'20'                                                 
         OI    CONBNUMH+4,X'20'                                                 
*                                                                               
DISPX20  GOTO1 VFOUTBLK,DMCB,BUYDAYSH,BUYLAST,1                                 
         SPACE 1                                                                
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   DISPX60                                                          
*                                                                               
         LA    R4,BUYDAYSH         SET PROTECTED BITS ON ALL FIELDS             
         LA    R5,BUYORDCH         EXCEPT BUY ORDER COMMENTS                    
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
*                                                                               
         BE    DISPX50                                                          
         CR    R4,R5                                                            
         BL    DISPX40                                                          
*                                                                               
         OI    BUYPGMH+1,X'20'     ALSO PROGRAM FIELD                           
         OI    BUYPGMH+6,X'80'     XMIT                                         
*                                                                               
DISPX60  DS    0H                                                               
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    DISPX70                                                          
         NI    BUYCLSH+1,X'FF'-X'20'    UNPROTECT PATTERN                       
DISPX70  B     EXXMOD                                                           
         EJECT                                                                  
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
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
*                                                                               
       ++INCLUDE RECNTFED                                                       
         EJECT                                                                  
***********************************************************************         
* CHECK IF MULTI-MAKEGOOD IN USE                                                
***********************************************************************         
         CSECT                                                                  
CHECKMG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWAACCS,C'$'        REP ONLY                                     
         BE    CMGX                                                             
*                                                                               
         LA    R6,RBUYELEM                                                      
*                                                                               
CMG05    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    CMGX                                                             
         CLI   0(R6),X'05'                                                      
         BE    CMG10                                                            
         BH    CMGX                                                             
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     CMG05                                                            
*                                                                               
CMG10    DS    0H                                                               
CURRENTD USING RBUYMGEL,R3                                                      
NEXTD    USING RBUYMGEL,R6                                                      
         LR    R3,R6                                                            
*                                                                               
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'05'                                                      
         BNE   CMGX                                                             
*                                                                               
         CLC   CURRENTD.RBUYMGLI,NEXTD.RBUYMGLI                                 
         BE    CMG10                                                            
         LA    R3,863                                                           
         LA    R2,CONBACTH                                                      
         GOTO1 VERROR                                                           
*                                                                               
CMGX     DS    0H                                                               
         XIT1                                                                   
         DROP  CURRENTD,NEXTD                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**>>>                                                                           
**********************************************************************          
* COMBO BUY RATES DISPLAY                                                       
**********************************************************************          
COMBO    NTR1  BASE=*,LABEL=*                                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         NI    TWACMTRD,X'FF'-X'40'                                             
*                                  TURN OFF 'TRADE ENCOUNTERED'                 
         CLC   =C'BUY',BUYACT                                                   
         BNE   COMB0040                                                         
         TM    TWACMTRD,X'80'      *TRADE* SET BY UPDATE?                       
         BNO   COMB0020            NO                                           
         NI    TWACMTRD,X'FF'-X'80'                                             
*                                  YES - TURN OFF FLAG,                         
*                                     DON'T CLEAR BANNER                        
         B     COMB0040                                                         
COMB0020 EQU   *                                                                
         LA    RF,BUYCOMUH                                                      
         MVC   BUYCOMU(07),=C'       '                                          
COMB0040 EQU   *                                                                
         CLC   =C'BUY',BUYACT      COMBO BUY ADD HAS NO EXISTING                
         BE    COMB0160            RATES TO DISPLAY, SO EXIT                    
         CLC   =C'CHA',BUYACT      DON'T REFRESH RATE FOR ACT CHA SINCE         
         BE    COMB0160            WE NEED TO VALIDATE ALL RATES                
*                                                                               
COMB0060 LA    R2,TWACMBC1         GET NEXT COMBO K#                            
         LA    R4,BUYCBC1H         RATE DISPLAY FIELD                           
         B     COMB0100                                                         
*                                                                               
COMB0080 DS    0H                                                               
         ZIC   RF,0(R4)            BUMP TO NEXT RATE FIELD                      
         AR    R4,RF                                                            
*                                                                               
         LA    R2,9(R2)            BUMP TO NEXT COMBO K#                        
         OC    0(4,R2),0(R2)       GET BUYLINE IF CONTRACT EXISTS               
         BZ    COMB0160                                                         
*                                                                               
COMB0100 DS    0H                                                               
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
*                                                                               
*   NEED TO SET A FLAG IF ONE OF THE COMBO BUYS IS A 'TRADE' BUY                
*        TO REDISPLAY THE INDICATOR ON THE COMMENT LINE                         
*                                                                               
         TM    RCONCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    37(R5),X'80'        TM RBUYCOMB,X'80'                            
         BZ    COMB0120            IF COMBO BUY PLACE HOLDER                    
         MVC   8(2,R4),=C'NA'      DISPLAY "NA" INSTEAD OF 0 DOLLARS            
         B     COMB0140            (IT MEANS THIS BUYLINE HAS 0 SPOTS)          
*                                                                               
COMB0120 EDIT  (4,38(R5)),(9,8(R4)),2,ALIGN=LEFT,FLOAT=-      RATE              
         TM    RBUYFLG2-RBUYREC(R5),X'02'  TRADE BUY?                           
         BNO   COMB0140            NO                                           
         LA    RF,8(R4)                                                         
         AR    RF,R0               ADD LENGTH OF OUTPUT                         
         MVI   0(RF),C'T'          INSERT 'TRADE' INDICATOR                     
         OI    TWACMTRD,X'40'      SET 'TRADE ENCOUNTERED'                      
*                                                                               
COMB0140 EQU   *                                                                
*                                                                               
         LA    RF,BUYCBC4H                                                      
         CR    R4,RF                                                            
         BL    COMB0080            LOOP UNTIL DONE                              
*                                                                               
COMB0160 XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**>>>                                                                           
*********************************************************************           
* CHKDARE CHECKS FOR DARE ORDERS.  IF FOUND, LIMITS FIELD CHANGES   *           
*********************************************************************           
CHKDARE  NMOD1 0,*CKDA*                                                         
         L     RC,0(R1)                                                         
         BAS   RE,CLRPROTS         CLEAR ALL PROTECTION BITS                    
*                                                                               
         CLI   RCONKSTA+4,C'A'     RADIO?                                       
         BE    CHDA0160                                                         
         CLI   RCONKSTA+4,C'F'     RADIO?                                       
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
         CLI   RCONDRFG,0          ANYTHING SET IN STATUS BYTE?                 
         BE    CHDA0160            NO  - NOT DARE CONTRACT                      
* KATZ EDI ORDER? OR ONE SHOT ? OR TAKEOVER??                                   
         TM    RCONDRFG,X'04'+X'02'+X'01'                                       
         BNZ   CHDA0160            YES, DON'T TREAT AS DARE CONTRACT            
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CHDA0110                                                         
         TM    RCONDRF2,X'01'      EXIT IF XML                                  
         BO    CHDA0160                                                         
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BO    CHDA0160                                                         
         DROP  R6                                                               
*                                                                               
CHDA0110 EQU   *                                                                
         LA    R2,FIELDTBL         SET A(SCREEN FIELD TABLE)                    
CHDA0120 EQU   *                                                                
         CLI   0(R2),X'FF'         TABLE DELIMITER REACHED?                     
         BE    CHDA0160            YES - RESTRICTED FIELDS                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'190RECNT25   06/18/08'                                      
         END                                                                    
