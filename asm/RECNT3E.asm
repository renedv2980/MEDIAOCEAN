*          DATA SET RECNT3E    AT LEVEL 071 AS OF 05/01/02                      
*PHASE T8023CA,+0                                                               
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE RETIMVAL                                                               
         TITLE 'T8023C - REPPAK EXPANDED TD/TC BUY SCREEN'                      
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT3C (T8023C) --- EXPANDED TD/TC  BUY DISPLAY         *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
*                                                                 *             
* 26JAN01 BU  NEW ENTRY                                           *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8023C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8023C,R9                                                      
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
*   RE-USE LOCALVAR WORKSPACE FOR THIS MODULE.                                  
*                                                                               
         LA    R8,A03ELT           SET A(WORKSPACE)                             
         USING LVARDSCT,R8                                                      
*                                                                               
         L     RE,AFACILS                                                       
         L     RF,0(RE)            A(FATIOB)                                    
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
*                                                                               
         CLI   PFKEY,2             PFKEY 2 PRESSED?                             
         BNE   MAIN0100            NO  - IGNORE ANY OTHER SETTING               
         LA    R3,INVLINE          SET INVALID LINE ERROR MSG                   
         BAS   RE,CHKCURSR         YES - CHECK CURSOR POSITION                  
         BNZ   CURSERR             EXIT WITH ERROR                              
         SR    R0,R0                                                            
         L     R1,FULL             SET RELATIVE LINE NUMBER FOUND               
         LA    RE,COLLAST          SET A(END OF SCREEN)                         
         A     RE,=F'8000'         MOVE DOWN 8K TO AKEYAREA                     
*                                     (AKEYAREA NOT SET YET)                    
         LA    RF,32               SET SIZE OF KEY SLOT                         
         MR    R0,RF               CALCULATE DISPLACEMENT                       
         AR    RE,R1               ADD DISPLACE TO A(KEYAREA)                   
         OC    0(32,RE),0(RE)      ANYTHING IN TABLE SLOT?                      
         BNZ   MAIN0020            YES                                          
CURSERR  EQU   *                                                                
         OI    COLBACTH+1,X'01'    TURN ON MODIFIED BIT                         
         OI    COLBACTH+6,X'80'    TURN ON TRANSMIT BIT                         
         B     ERROR                                                            
MAIN0020 EQU   *                                                                
         ST    RE,FULL             SET A(KEY OF BUY TO DISPLAY)                 
         BAS   RE,SWAPCON          OKAY TO SWAP TO CONTRACT                     
         XIT1                                                                   
MAIN0100 EQU   *                                                                
*                                                                               
         L     R4,4(R1)            JUST DISPLAY HEADER INFO AND EXIT?           
*                                                                               
         BAS   RE,DISPCON                                                       
*                                                                               
*                                                                               
**       OI    BUYFLTH+1,X'20'     PROTECT FLIGHT FIELD                         
**       OI    BUYFLTH+6,X'80'     TRANSMIT FIELD                               
**       TM    TWASTREO,X'80'      IS THIS STEREO?                              
**       BZ    *+8                 NO                                           
**       NI    BUYFLTH+1,X'FF'-X'20'    YES, UNPROTECT FLIGHT                   
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DISP0               YES                                          
         LA    R3,404              SET ERROR:  THIS IS WRONG ERROR              
         B     EXIT                                                             
DISP0    EQU   *                                                                
         CLC   =C'HEAD',0(R4)      HEADLINE ONLY DISPLAY?                       
         BE    EXXMOD              ALL DONE, EXIT                               
         CLC   =C'DISP',0(R4)      DISPLAY BUYLINES?                            
         BE    DISP0020            YES - EDIT INPUT                             
         BAS   RE,EDITIPUT         NO  - EDIT AND UPDATE                        
         B     EXXMOD                                                           
         EJECT                                                                  
DISP0020 EQU   *                                                                
         BAS   RE,CLERSCRN         DISPLAY: CLEAR SCREEN                        
         BAS   RE,CLERTWAK         DISPLAY: CLEAR KEYS SAVED IN TWA             
*                                     PLUS RECORD ELT STORAGE                   
         BAS   RE,GETBUYS          YES - RETRIEVE BUYS AND FORMAT               
         BZ    DISP0040            CC ZERO = MORE BUYS                          
         LA    R3,53               CC NOT ZERO = NO MORE BUYS                   
         B     DISP0060                                                         
DISP0040 EQU   *                                                                
         LA    R3,56               CC ZERO = MORE BUYS                          
DISP0060 EQU   *                                                                
*                                                                               
         GOTO1 VFOUTBLK,DMCB,COLTYPEH,COLLAST,1                                 
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   GETBUYS:  DRIVER FOR DISPLAY.                                               
*        1.  DETERMINES IF BUYS EXIST TO BE DISPLAYED.                          
*        2.  RETRIEVES AND FORMATS BUY ON SCREEN.                               
*        3.  IF SCREEN FULL, STORES LAST BUY NUMBER IN TWA                      
*        4.  STORES IN TWA KEY AND D/A OF EACH DISPLAYED BUY                    
*        5.  IF BUY MAKEGOOD, OPENS PROTECTED FIELDS                            
*        6.  IF BUY NOT M/G, SHUTS UNPROTECTED FIELDS, SETS TO PZ               
*                                                                               
GETBUYS  NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    HALF2,HALF2         CLEAR START NUMBER                           
         CLC   COLBNUM(4),=C'NEXT'                                              
         BNE   GBUY0010            NO                                           
         XC    COLBNUM,COLBNUM                                                  
         LA    R2,COLBNUMH                                                      
         FOUT  (R2)                CLEAR 'NEXT' FLAG                            
         MVC   KEY,TWALSTKY        YES - GET LAST KEY DISPLAYED                 
         GOTO1 VHIGH               RETRIEVE LAST KEY DISPLAYED                  
         B     GBUY0040            READ NEXT SEQUENTIAL KEY                     
GBUY0010 EQU   *                                                                
         LA    R2,COLBNUMH                                                      
         GOTO1 VPACK                                                            
         STC   R0,HALF2            SAVE START NUMBER                            
GBUY0020 EQU   *                                                                
         XC    KEY,KEY                                                          
*                                                                               
         MVI   KEY,X'0B'           BUILD BUY KEY                                
         MVC   KEY+RBUYKREP-RBUYREC(2),REPALPHA                                 
*                                  INSERT REP CODE                              
         MVC   KEY+RBUYKCON-RBUYREC(4),TWACNUM                                  
*                                  INSERT CONTRACT NUMBER                       
         GOTO1 VHIGH                                                            
         B     GBUY0060                                                         
GBUY0040 EQU   *                                                                
         GOTO1 VSEQ                                                             
GBUY0060 EQU   *                                                                
         CLC   KEYSAVE(22),KEY     BUY FOR DISPLAYED CON?                       
         BNE   GBUY0900            NO  - NO MORE BUYS: FINISHED                 
         CLI   HALF2,0             ANY START NUMBER?                            
         BZ    GBUY0070            NO                                           
         CLC   HALF2(1),KEY+26     BUY KEY = START KEY?                         
         BNE   GBUY0040            NO  - GO BACK FOR NEXT                       
         MVI   HALF2,0             YES - ONLY DO THIS ONCE                      
GBUY0070 EQU   *                                                                
         ZIC   RF,SCRNCTR          INCREMENT SCREEN COUNT                       
         LA    RF,1(RF)                                                         
         STC   RF,SCRNCTR                                                       
         BAS   RE,FORMSCRN         BUY FOUND:  PUT ON SCREEN                    
         BAS   RE,STOREKEY         SAVE KEY IN TWA SPACE.                       
         BAS   RE,TABLELTS         STORE EXISTING ELEMENTS                      
         MVC   TWALSTKY,KEY        SAVE KEY FOR NEXT SCREEN                     
*                                                                               
         DROP  R4                                                               
*                                                                               
         CLI   SCRNCTR,5           SCREEN FULL?                                 
         BE    GBUY0100            YES - SCREEN FULL                            
*                                                                               
         B     GBUY0040            NO  - GO BACK FOR NEXT                       
GBUY0100 EQU   *                                                                
         MVC   COLBNUM(4),=C'NEXT'                                              
         LA    R2,COLBNUMH                                                      
         FOUT  (R2)                                                             
         SR    RF,RF               SET CC = ZERO:  MORE BUYS                    
         B     GBUY0920            EXIT                                         
GBUY0900 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  NO MORE BUYS               
GBUY0920 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FORMSCRN.                                                                   
*        1.  RETRIEVE BUY RECORD                                                
*        2.  DETERMINE NEXT SCREEN SLOT                                         
*        3.  FORMAT DATA FROM BUY TO SCREEN                                     
*        4.  IF M/G, OPEN FIELDS                                                
*                                                                               
FORMSCRN NTR1                                                                   
         OI    DMINBTS,X'08'       RETRIEVE THE BUY RECORD                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         ZIC   R1,SCRNCTR          DETERMINE SCREEN POSITION                    
         BCTR  R1,0                MAKE ZERO RELATIVE                           
         LA    RF,COLLIN2H-COLLINH SET L(ONE BUYLINE ON SCREEN)                 
         SR    RE,RE                                                            
         MR    RE,R1               SET DISPLACEMENT TO SCREEN LINE              
         LA    R5,COLLINH          SET A(FIRST LINE)                            
         AR    R5,RF                                                            
*                                                                               
         USING COLLINH,R5                                                       
         EDIT  RBUYKLIN,(2,COLLIN)                                              
         LA    R2,COLLINH                                                       
         FOUT  (R2)                                                             
         GOTO1 DAYTIME,DMCB,(R5)   DISPLAY DAY/TIME INFORMATION                 
         GOTO1 EFFDATE,DMCB,(R5)   DISPLAY EFFECTIVE DATE INFORMATION           
         GOTO1 SPOTLEN,DMCB,(R5)   DISPLAY SPOT LENGTH INFORMATION              
         GOTO1 MAKEGOOD,DMCB,(R5)  DISPLAY MAKEGOOD INFORMATION                 
         GOTO1 ELT40DIS,DMCB,(R5)  DISPLAY BUY DATA INFORMATION                 
         GOTO1 ELT41DIS,DMCB,(R5)  DISPLAY M/G DATA INFORMATION                 
         EDIT  RBUYCOS,(12,COLRATE),2                                           
         LA    R2,COLRATEH                                                      
         FOUT  (R2)                                                             
         DROP  R5                                                               
         XIT1                                                                   
         EJECT                                                                  
DAYTIME  NTR1                                                                   
*                                                                               
         L     R5,0(R1)            RESET A(SCREEN LINE IN USE)                  
         USING COLLINH,R5                                                       
         LA    R6,RBUYREC          RETRIEVE FIRST DAY/TIME ELEMENT              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE PRESENT                              
         DC    H'0'                                                             
         GOTO1 =V(OUTDAY),DMCB,3(R6),2(R6),COLDAYS,RR=Y                         
         GOTO1 =V(UNTIME),DMCB,4(R6),COLTIMS,RR=Y                               
         BAS   RE,NEXTEL           MORE THAN ONE DAY/TIME?                      
         BNE   DATI0100            NO                                           
         MVI   COLDAYS+12,C'+'     YES                                          
DATI0100 EQU   *                                                                
         LA    R2,COLDAYSH                                                      
         FOUT  (R2)                                                             
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
EFFDATE  NTR1                                                                   
         L     R5,0(R1)            RESET A(SCREEN LINE IN USE)                  
         USING COLLINH,R5                                                       
         LA    R6,RBUYREC          RETRIEVE FIRST EFF DATE ELEMENT              
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE PRESENT                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,2(R6)),(5,COLDTES)                                
         OC    5(3,R6),5(R6)       ANY END DATE?                                
         BZ    EFFD0040            NO  - NOTHING TO DISPLAY                     
         CLC   2(3,R6),5(R6)       END DATE = START DATE?                       
         BE    EFFD0040            YES - DON'T DISPLAY                          
         MVI   COLDTES+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,5(R6)),(5,COLDTES+9)                              
EFFD0040 EQU   *                                                                
         BAS   RE,NEXTEL           MORE THAN EFF DATE ELEMENT?                  
         BNE   EFFD0100            NO                                           
         MVI   COLDTES+19,C'+'     YES                                          
EFFD0100 EQU   *                                                                
         DROP  R5                                                               
         XIT1                                                                   
         EJECT                                                                  
SPOTLEN  NTR1                                                                   
         L     R5,0(R1)            RESET A(SCREEN LINE IN USE)                  
         USING COLLINH,R5                                                       
         MVC   HALF,RBUYDUR                                                     
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,COLLEN),ALIGN=LEFT                                   
         TM    RBUYDUR,X'80'       MINUTES?                                     
         BZ    SPOL0020                                                         
         MVI   0(RE),C'M'                                                       
SPOL0020 EQU   *                                                                
*                                                                               
         DROP  R5                                                               
         XIT1                                                                   
         EJECT                                                                  
MAKEGOOD NTR1                                                                   
         L     R5,0(R1)            RESET A(SCREEN LINE IN USE)                  
         USING COLLINH,R5                                                       
         CLC   RBUYKMLN,RBUYKLIN   MAKEGOOD LINE?                               
         BE    MAKE0100            NO  - REGULAR                                
         LA    R2,COLMG1H          YES - TURN ON ALL FIELDS                     
         MVI   BYTE,0              CLEAR FLIP-FLOP                              
         LA    R3,12               SET LOOP CONTROL: 12 FIELDS                  
MAKE0020 EQU   *                                                                
         NI    1(R2),X'FF'-X'0C'   TURN OFF LOW INTENSITY                       
         CLI   BYTE,0              HEADER FIELD?                                
         BE    MAKE0040            YES - FIELD LABEL DISPLAYED                  
         NI    1(R2),X'FF'-X'20'   NO  - DATA: TURN OFF PROTECT                 
MAKE0040 EQU   *                                                                
         XI    BYTE,X'01'          FLIP-FLOP HEADER/DATA FIELD                  
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         BCT   R3,MAKE0020         GO BACK FOR NEXT FIELD                       
MAKE0100 EQU   *                                                                
         DROP  R5                                                               
         XIT1                                                                   
         EJECT                                                                  
STOREKEY NTR1                                                                   
         ZIC   RF,SCRNCTR          GET POSITION NUMBER                          
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SR    RE,RE                                                            
         LA    R2,32               LOAD KEY SIZE                                
         MR    RE,R2                                                            
         L     R1,AKEYAREA                                                      
         AR    R1,RF               DISPLACE TO KEY STORAGE                      
         MVC   0(32,R1),KEY        SAVE KEY IN STORAGE                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ELT40DIS:  RETRIEVE AND DISPLAY DATA FROM X'40' ELEMENT.                    
*                                                                               
ELT40DIS NTR1                                                                   
         L     R5,0(R1)            RESET A(SCREEN LINE IN USE)                  
         USING COLLINH,R5                                                       
         LA    R6,RBUYREC          RETRIEVE 40 ELEMENT                          
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   ELT40900            NOT FOUND                                    
         USING RBUYTDEL,R6                                                      
         LA    R2,COLGRPH                                                       
         CLC   RBUYTDGP,=X'FFFF'   SET TO BLANK?                                
         BE    ELT40020            YES                                          
         EDIT  RBUYTDGP,(3,COLGRP),ALIGN=LEFT,ZERO=NOBLANK                      
ELT40020 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLTCODH                                                      
         CLC   RBUYTDTP,=X'FF'     SET TO BLANK?                                
         BE    ELT40040            YES                                          
         EDIT  RBUYTDTP,(1,COLTCOD),ALIGN=LEFT,ZERO=NOBLANK                     
ELT40040 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLPRODH         (WON'T FIND BLANKS)                          
         CLC   RBUYTDPS,=X'FFFF'   SET TO BLANK?                                
         BE    ELT40060            YES                                          
         EDIT  RBUYTDPS,(4,COLPROD),ALIGN=LEFT,ZERO=NOBLANK                     
ELT40060 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLADVRH                                                      
         CLC   RBUYTDAS,=X'FFFF'   SET TO BLANK?                                
         BE    ELT40080            YES                                          
         EDIT  RBUYTDAS,(4,COLADVR),ALIGN=LEFT,ZERO=NOBLANK                     
ELT40080 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLSECH                                                       
         CLC   RBUYTDS#,=X'FF'   SET TO BLANK?                                  
         BE    ELT40100            YES                                          
         EDIT  RBUYTDS#,(1,COLSEC),ALIGN=LEFT,ZERO=NOBLANK                      
ELT40100 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLCLSH                                                       
         CLC   RBUYTDCL,=X'FFFF' SET TO BLANK?                                  
         BE    ELT40120            YES                                          
         MVC   COLCLS(2),RBUYTDCL                                               
ELT40120 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLCLSH                                                       
         CLC   RBUYTDST(4),=X'FFFFFFFF' SET TO BLANK?                           
         BE    ELT40140            YES                                          
         GOTO1 =V(UNTIME),DMCB,RBUYTDST,COLTIME,RR=Y                            
ELT40140 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLPLUH                                                       
         CLC   RBUYTDPU,=X'FF'   SET TO BLANK?                                  
         BE    ELT40160            YES                                          
         EDIT  RBUYTDPU,(2,COLPLU),ALIGN=LEFT,ZERO=NOBLANK                      
ELT40160 EQU   *                                                                
         FOUT  (R2)                                                             
ELT40900 EQU   *                                                                
         XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
*   ELT41DIS:  RETRIEVE AND DISPLAY DATA FROM X'41' ELEMENT.                    
*                                                                               
ELT41DIS NTR1                                                                   
         L     R5,0(R1)            RESET A(SCREEN LINE IN USE)                  
         USING COLLINH,R5                                                       
         LA    R6,RBUYREC          RETRIEVE 41 ELEMENT                          
         MVI   ELCODE,X'41'                                                     
         BAS   RE,GETEL                                                         
         BNE   ELT41900            NOT FOUND                                    
         USING RBUYTCEL,R6                                                      
         LA    R2,COLMG#H                                                       
         CLC   RBUYTCDT,=X'FFFFFF' SET TO BLANK?                                
         BE    ELT41020            YES                                          
         GOTO1 DATCON,DMCB,(3,RBUYTCDT),(5,COLMG#)                              
ELT41020 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLCN#H                                                       
         CLC   RBUYTCC#,=X'FFFFFFFF' SET TO BLANK?                              
         BE    ELT41040            YES                                          
         EDIT  RBUYTCC#,(8,COLCN#),ALIGN=LEFT,ZERO=NOBLANK                      
ELT41040 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLLN#H                                                       
         CLC   RBUYTCCL,=X'FFFF'   SET TO BLANK?                                
         BE    ELT41060            YES                                          
         MVC   COLLN#(2),RBUYTCCL                                               
ELT41060 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLML#H                                                       
         CLC   RBUYTCML,=X'FFFF'   SET TO BLANK?                                
         BE    ELT41080            YES                                          
         MVC   COLML#(2),RBUYTCML                                               
ELT41080 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLSL#H                                                       
         CLC   RBUYTCMS,=X'FF'     SET TO BLANK?                                
         BE    ELT41100            YES                                          
         EDIT  RBUYTCMS,(2,COLSL#),ALIGN=LEFT,ZERO=NOBLANK                      
ELT41100 EQU   *                                                                
         FOUT  (R2)                                                             
         LA    R2,COLOP#H                                                       
         CLC   RBUYTCOP,=X'FF'     SET TO BLANK?                                
         BE    ELT41120            YES                                          
         MVC   COLOP#(1),RBUYTCOP  NO                                           
ELT41120 EQU   *                                                                
         FOUT  (R2)                                                             
ELT41900 EQU   *                                                                
         XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
*   TABLELTS.  FOR EACH CONTRACT SHOWN, THE 40 AND 41 ELTS ARE                  
*        SAVED IN TABLES.  EACH TABLE ENTRY WILL CONTAIN THE                    
*        ORIGINAL RECORD ELEMENT, AND, AS VALIDATION IS DONE,                   
*        A NEW ELEMENT.  THE TABLE ENTRIES ARE SET AS A PAIR OF                 
*        ELEMENTS, THE FIRST BEING THE ORIGINAL, THE SECOND                     
*        THE NEW ENTRY.  DURING THE UPDATE PHASE, THE OLD AND                   
*        NEW WILL BE COMPARED.  IF A DIFFERENCE IS FOUND, THE                   
*        RECORD WILL BE UPDATED.  OTHERWISE, NO CHANGE WILL TAKE                
*        PLACE.                                                                 
*                                                                               
TABLELTS NTR1                                                                   
         LA    R6,RBUYREC          RETRIEVE 40 ELEMENT                          
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   TABL0060            NOT FOUND                                    
         ZIC   RF,SCRNCTR          GET POSITION NUMBER                          
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SR    RE,RE                                                            
         LA    R2,SZ40TAB          LOAD X'40' ELT SIZE (40 CHARS)               
         MR    RE,R2                                                            
         L     R1,AGRPAREA         SET A(TABLE AREA)                            
         AR    R1,RF               DISPLACE TO 40  STORAGE                      
         MVC   0(SZ40ELT,R1),0(R6) SAVE ELT IN STORAGE                          
*                                    (20 CHARS)                                 
TABL0060 EQU   *                                                                
         LA    R6,RBUYREC          RETRIEVE 41 ELEMENT                          
         MVI   ELCODE,X'41'                                                     
         BAS   RE,GETEL                                                         
         BNE   TABL0080            NOT FOUND                                    
         ZIC   RF,SCRNCTR          GET POSITION NUMBER                          
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SR    RE,RE                                                            
         LA    R2,SZ41TAB          LOAD X'41' ELT SIZE (32 CHARS)               
         MR    RE,R2                                                            
         L     R1,AMGAREA          SET A(TABLE AREA)                            
         AR    R1,RF               DISPLACE TO 41  STORAGE                      
         MVC   0(SZ41ELT,R1),0(R6) SAVE ELT IN STORAGE                          
*                                    (16 CHARS)                                 
TABL0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
**>>>>>>                                                                        
                                                                                
* BUMP DOWN SCREEN OUT LINES TO DETERMINE IF CURSOR ON VALID LINE               
* RETURN SCREEN LINE ADDR IN FULL                                               
* R3 USED FOR CURSOR ADDR  R4 SCREEN OUT LINE ADDR                              
                                                                                
CHKCURSR NTR1                                                                   
         L     R2,ATIOB                                                         
         USING TIOBD,R2                                                         
         LH    R3,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R3,RA               RA=TWA                                       
         LA    R4,COLPKEYH         END OF SCREEN                                
         CR    R3,R4                                                            
         BNL   CCNO                CURSOR > THAN LAST OUT LINE                  
         LA    R4,COLLINH          FIRST OUT LINE ON SCRREN                     
         CR    R3,R4                                                            
         BL    CCNO                                                             
                                                                                
         SR    R3,R4               CALC # OF SET OF LINES ON SCRN               
*                                  A(CURSOR) - A(1ST LINE)                      
         SR    R2,R2               CLEAR REG PAIR FOR DIVIDE                    
         LA    RF,COLLIN2H-COLLINH SET SIZE OF SET OF LINES                     
         DR    R2,RF                                                            
*                                                                               
*   R3 WILL CONTAIN ZERO-RELATIVE LINE NUMBER ON SCREEN                         
*                                                                               
CC20     EQU   *                                                                
         ST    R3,FULL             SET SCREEN LINE NUMBER                       
CCYES    EQU   *                                                                
         SR    R3,R3                                                            
CCNO     EQU   *                                                                
         LTR   R3,R3                                                            
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
*                                                                               
SWAPCON  NTR1                                                                   
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         L     R2,FULL             SET A(KEY OF BUYLINE TO DISPLAY)             
*---------------------*                                                         
* GET CONTRACT NUMBER *                                                         
*---------------------*                                                         
         LA    RF,CONCNUMH         SET A(CONTRACT INPUT)                        
*                                                                               
         MVC   WORK(8),SPACES                                                   
         ZIC   RE,5(RF)            GET LENGTH OF INPUT                          
         BCTR  RE,0                BACK OFF 1                                   
         EX    RE,SWAPEX02         MOVE CON# BY LENGTH                          
         B     SWAP0020                                                         
SWAPEX02 MVC   WORK(0),CONCNUM                                                  
SWAP0020 EQU   *                                                                
         BAS   R1,CHKNUM                                                        
         LA    R3,CONCNUM                                                       
         B     SWAP0040                                                         
                                                                                
********************************************                                    
* BUG CATCHER                                                                   
CHKNUM   LA    RE,WORK                                                          
         LA    RF,8                                                             
CHKNUM0  CLI   0(RE),X'40'                                                      
         BE    CHKNUM3                                                          
         CLI   0(RE),X'F0'                                                      
         BL    CURSERR                                                          
         CLI   0(RE),X'F9'                                                      
         BH    CURSERR                                                          
CHKNUM3  LA    RE,1(RE)                                                         
         BCT   RF,CHKNUM0                                                       
         BR    R1                                                               
***********************************************                                 
                                                                                
SWAP0040 DS    0H                                                               
*                                                                               
*   DMWORK IS BEING USED TO SET UP THE CONTROL ELEMENTS BECAUSE                 
*        WORK IS REFERENCED BY THE EDIT MACRO.                                  
*                                                                               
         XC    DMWORK,DMWORK                                                    
         LA    RE,DMWORK                                                        
         USING GLCONNUM,RE                                                      
         LA    RF,7                MOVE OUT MAX OF EIGHT CHARS                  
         EX    RF,SWAPEX04                                                      
         B     SWAP0060                                                         
SWAPEX04 MVC   GLCONNUM(0),0(R3)                                                
SWAP0060 EQU   *                                                                
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DIS'                                               
         USING RBUYREC,R2                                                       
         EDIT  RBUYKLIN,(3,GLCONBN),ALIGN=LEFT                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',DMWORK,GLCONLNQ,GLRKACT                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
*        LA    R2,RISMESSH                                                      
*        ZIC   R1,0(R2)            BUMP TO SERVICE REQUEST FIELD                
*        AR    R2,R1                                                            
*        OI    6(R2),X'C0'         XMIT FIELD                                   
*        ZIC   R1,0(R2)                                                         
*        S     R1,=F'8'            GET LENGTH OF FIELD                          
*        BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        XC    8(0,R2),8(R2)                                                    
*        MVC   8(10,R2),=C'+C,SV     '                                          
*                                                                               
         XC    DMWORK,DMWORK                                                    
         LA    R1,DMWORK                                                        
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 CGLOBBER,DMCB,=C'PUTD',DMWORK,14,GLVXCTL                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STGX     XIT1                                                                   
         B     EXIT                                                             
         DROP  R5                                                               
**>>>>>>                                                                        
EDITIPUT NTR1                                                                   
         BAS   RE,SETTWAK          SET A(WORKSPACES)                            
         BAS   RE,VALSCRN          VALIDATE INPUT ON SCREEN                     
*                                                                               
*  IF ERROR HAS BEEN FOUND, VALSCRN EXITS DIRECTLY VIA 'ERROR'.                 
*  IF VALSCRN RETURNS HERE: EACH RECORD ON THE SCREEN HAS HAD                   
*        AN X'40' ELEMENT BUILT, WHICH IS PRESENT IN THE TABLE                  
*        ADDRESSED BY AGRPAREA.  EACH RECORD WHICH IS A MAKEGOOD                
*        HAS HAD AN X'41' ELEMENT BUILT, WHICH IS PRESENT IN THE                
*        TABLE ADDRESSED BY AMGAREA.                                            
*        EACH TABLE IS POSITIONAL, AND CORRESPONDS WITH THE MAXIMUM             
*        FIVE KEYS PRESENT IN THE TABLE ADDRESSED BY AKEYAREA.                  
*        EACH KEY CORRESPONDS TO A SCREEN ENTRY, AND ALSO CONTAINS              
*        THAT RECORD'S DISK ADDRESS.                                            
*                                                                               
         BAS   RE,UPDATREC         UPDATE ALL RECORDS.                          
EPUT0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
VALSCRN  NTR1                                                                   
         L     R7,AKEYAREA         SET A(KEYS ON SCREEN)                        
         LA    R4,0                SET SCREEN COUNTER, ZERO RELATIVE            
VALS0020 EQU   *                                                                
         OC    0(32,R7),0(R7)      ANY ENTRY IN SLOT?                           
         BZ    VALS0900            NO  - FINISHED W/EDIT                        
*                                  THERE IS ROOM FOR 6+ ENTRIES IN THIS         
*                                    TABLE.  ONLY 5 SLOTS WILL EVER             
*                                    BE FILLED.  THE SIXTH SLOT ACTS AS         
*                                    TABLE DELIMITER SO CODE WILL NEVER         
*                                    RUN PAST TABLE END.                        
         LA    RF,COLLIN2H-COLLINH                                              
         SR    RE,RE                                                            
         MR    RE,R4               CALCULATE DISPLACEMENT TO LINE               
         LA    R5,COLLINH          SET A(FIRST SCREEN LINE)                     
         AR    R5,RF               ADD DISPLACEMENT                             
         USING COLLINH,R5          SET USING FOR FIELD                          
         MVI   BLD40ELT,X'FF'      CLEAR FIELD TO ALL X'FF'                     
         MVC   BLD40ELT+1(LBLD4ELT-1),BLD40ELT                                  
         MVC   BLD40ELT(2),=X'4014'                                             
         MVC   BLD41ELT(2),=X'4110'                                             
*                                                                               
*        VALIDATE EACH FIELD ON SET OF LINES                                    
*        CONCURRENTLY, BUILD NEW ELEMENTS FOR EACH RECORD, SO                   
*              THAT ANOTHER PASS DOESN'T HAVE TO BE MADE TO                     
*              BUILD DATA                                                       
*                                                                               
         LA    R3,BADGROUP         SET ERROR MESSAGE                            
         LA    R2,COLGRPH          GROUP CODE                                   
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0060            NO  - FIELD DEFAULTS TO X'FF'                
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0040            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0040 EQU   *                                                                
         CH    R0,=H'999'                                                       
         BH    ERROR                                                            
         STCM  R0,3,BLD40ELT+RBUYTDGP-RBUYTDEL                                  
VALS0060 EQU   *                                                                
*                                                                               
         LA    R3,BADTYSEC         SET ERROR MESSAGE                            
         LA    R2,COLTCODH         TYPE CODE                                    
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0100            NO  - FIELD DEFAULTS TO X'FF'                
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0080            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0080 EQU   *                                                                
         CH    R0,=H'9'                                                         
         BH    ERROR                                                            
         STCM  R0,1,BLD40ELT+RBUYTDTP-RBUYTDEL                                  
VALS0100 EQU   *                                                                
*                                                                               
         LA    R3,BADSEP           SET ERROR MESSAGE                            
         LA    R2,COLPRODH         PRODUCT CODE                                 
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0160            NO  - FIELD DEFAULTS TO X'FF'                
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0120            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0120 EQU   *                                                                
         CH    R0,=H'999'          000-099 OR 999 OKAY                          
         BE    VALS0140                                                         
         CH    R0,=H'99'                                                        
         BH    ERROR                                                            
VALS0140 EQU   *                                                                
         STCM  R0,3,BLD40ELT+RBUYTDPS-RBUYTDEL                                  
VALS0160 EQU   *                                                                
*                                                                               
         LA    R3,BADSEP           SET ERROR MESSAGE                            
         LA    R2,COLADVRH         ADVERT  CODE                                 
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0220            NO  - FIELD DEFAULTS TO X'FF'                
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0180            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0180 EQU   *                                                                
         CH    R0,=H'999'          000-099 OR 999 OKAY                          
         BE    VALS0200                                                         
         CH    R0,=H'99'                                                        
         BH    ERROR                                                            
VALS0200 EQU   *                                                                
         STCM  R0,3,BLD40ELT+RBUYTDAS-RBUYTDEL                                  
VALS0220 EQU   *                                                                
*                                                                               
         LA    R3,BADTYSEC         SET ERROR MESSAGE                            
         LA    R2,COLSECH          SECTION CODE                                 
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0260            NO  - FIELD DEFAULTS TO X'FF'                
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0240            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0240 EQU   *                                                                
         CH    R0,=H'09'           0-9 OKAY                                     
         BH    ERROR                                                            
         STCM  R0,1,BLD40ELT+RBUYTDS#-RBUYTDEL                                  
VALS0260 EQU   *                                                                
*                                                                               
*        CLASS:  NO EDITS:  'OR' IN SPACES                                      
*                                                                               
         MVC   BLD40ELT+RBUYTDCL-RBUYTDEL(2),COLCLS                             
         OC    BLD40ELT+RBUYTDCL-RBUYTDEL(2),=C'  '                             
*                                                                               
*   TIME VALIDATIONS                                                            
*                                                                               
         LA    R2,COLTIMEH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BZ    VALS0270            NO  - NO TIME ENTERED                        
         ZIC   RF,5(R2)            YES - GET L(INPUT)                           
         STC   RF,DMCB             STORE L(INPUT)                               
         LA    R2,COLTIME          SET A(TIME FIELD INPUT)                      
         STCM  R2,7,DMCB+1         SET A(INPUT) IN P1                           
         LA    R3,BADTIME          SET ERROR MESSAGE                            
         LA    R2,COLTIMEH         RESET CURSOR POSITION                        
         GOTO1 =V(RETIMVAL),DMCB,,WORK,RR=Y                                     
         CLI   DMCB,X'FF'          ERROR ON TIME?                               
         BE    ERROR               YES                                          
         MVC   BLD40ELT+RBUYTDST-RBUYTDEL(2),WORK                               
         MVC   BLD40ELT+RBUYTDET-RBUYTDEL(2),WORK+2                             
VALS0270 EQU   *                                                                
         LA    R3,BADPLOT          SET ERROR MESSAGE                            
         LA    R2,COLPLUH          PLOT UNIT                                    
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0320            NO  - FIELD DEFAULTS TO X'FF'                
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0280            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0280 EQU   *                                                                
         CH    R0,=H'99'                                                        
         BE    VALS0300                                                         
         CH    R0,=H'04'           0-4, 99 OKAY                                 
         BH    ERROR                                                            
VALS0300 EQU   *                                                                
         STCM  R0,1,BLD40ELT+RBUYTDPU-RBUYTDEL                                  
VALS0320 EQU   *                                                                
*                                                                               
         BAS   RE,SET40ELT         STORE BUYLINE'S X'40'                        
*                                                                               
         BAS   RE,CHEKMG           DETAIL LINE IN PROGRESS?                     
         BZ    VALS0640            YES -                                        
         LA    R2,COLMG#H          MAKEGOOD DATE                                
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0340            NO  - FIELD DEFAULTS TO X'FF'                
         CLI   5(R2),6             YES - SIX CHAR DATE ENTERED?                 
         BNE   VALS0330            NO                                           
         MVC   WORK+24(4),COLMG#+2                                              
         MVC   WORK+28(2),COLMG#                                                
*                                  DATE ENTERED: ASSUMED YYMMDD                 
*                                  RESEQUENCED TO MMDDYY FOR                    
*                                     DATVAL VALIDATION                         
         MVC   COLMG#(6),WORK+24                                                
VALS0330 EQU   *                                                                
         LA    R3,13               SET INVALID DATE MESSAGE                     
         GOTO1 DATVAL,DMCB,(0,COLMG#),WORK+12                                   
         L     RF,DMCB                                                          
         LTR   RF,RF                                                            
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,BLD41ELT+RBUYTCDT-RBUYTCEL)           
VALS0340 EQU   *                                                                
         LA    R2,COLCN#H          CSI CON #                                    
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0380            NO  - FIELD DEFAULTS TO X'FF'                
         LA    R3,BADCSI#          SET CON CSI # MESSAGE                        
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0360            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0360 EQU   *                                                                
         CH    R0,=H'9999'         0-9999  OKAY                                 
         BH    ERROR                                                            
         STCM  R0,15,BLD41ELT+RBUYTCC#-RBUYTCEL                                 
VALS0380 EQU   *                                                                
         LA    R2,COLLN#H          CSI LINE#                                    
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0420            NO  - FIELD DEFAULTS TO X'FF'                
         LA    R3,BADCSILN         SET CON CSI # MESSAGE                        
         TM    4(R2),X'04'         FIELD ALPHABETIC?                            
         BNO   ERROR               NO  - ERROR                                  
         OC    8(2,R2),=X'4040'    OR IN SPACES                                 
         MVC   BLD41ELT+RBUYTCCL-RBUYTCEL(2),8(R2)                              
VALS0420 EQU   *                                                                
         LA    R2,COLML#H          MASTER LINE#                                 
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0460            NO  - FIELD DEFAULTS TO X'FF'                
         LA    R3,BADCSILN         SET CON CSI # MESSAGE                        
         TM    4(R2),X'04'         FIELD ALPHABETIC?                            
         BNO   ERROR               NO  - ERROR                                  
         OC    8(2,R2),=X'4040'    OR IN SPACES                                 
         MVC   BLD41ELT+RBUYTCML-RBUYTCEL(2),8(R2)                              
VALS0460 EQU   *                                                                
         LA    R2,COLSL#H          SUB LINE#                                    
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0500            NO  - FIELD DEFAULTS TO X'FF'                
         LA    R3,BADSUBL          SET SUBLINE # MESSAGE                        
         GOTO1 VPACK               PACK FIELD                                   
         BNZ   VALS0480            OKAY                                         
         TM    4(R2),X'08'         NOT NUMERIC?                                 
         BZ    ERROR                                                            
VALS0480 EQU   *                                                                
         CH    R0,=H'98'           00-98 PERMITTED                              
         BH    ERROR                                                            
         STCM  R0,1,BLD41ELT+RBUYTCMS-RBUYTCEL                                  
VALS0500 EQU   *                                                                
         LA    R2,COLOP#H          OPEN FLAG FIELD                              
         LA    R3,BADOPEN          SET OPEN FLAG MESSAGE                        
         CLI   5(R2),0             ANY FIELD ENTRY?                             
         BE    VALS0540            NO  - FIELD DEFAULTS TO X'FF'                
         CLI   5(R2),1             MORE THAN ONE CHARACTER?                     
         BH    ERROR               YES                                          
         CLI   COLOP#,C'X'         VALID                                        
         BE    VALS0520                                                         
         CLI   COLOP#,C'Y'         VALID                                        
         BE    VALS0520                                                         
         CLI   COLOP#,C'N'         VALID                                        
         BE    VALS0520                                                         
         CLI   COLOP#,C' '         VALID                                        
         BE    VALS0540                                                         
         B     ERROR               NOT X,Y,Z, OR SPACE                          
VALS0520 EQU   *                                                                
         MVC   BLD41ELT+RBUYTCOP-RBUYTCEL(1),COLOP#                             
VALS0540 EQU   *                                                                
         LA    RF,BLD41ELT                                                      
*                                                                               
         BAS   RE,SET41ELT         STORE BUYLINE'S X'40'                        
*                                                                               
VALS0640 EQU   *                                                                
         LA    R4,1(R4)            BUMP TO NEXT SCREEN SET OF LINES             
         LA    R7,32(R7)           BUMP TO NEXT TABLE ENTRY                     
         B     VALS0020                                                         
*                                                                               
VALS0900 EQU   *                                                                
         XIT1                                                                   
BADTIME  EQU   84                  TIME ENTERED INVALID                         
BADGROUP EQU   882                 GROUP CODE INVALID AS ENTERED                
BADTYSEC EQU   883                 TYPE OR SECTION INVALID                      
BADSEP   EQU   884                 PROD/ADVERTISER SEPARATOR INVALID            
BADPLOT  EQU   885                 PLOT UNIT INVALID                            
BADCSI#  EQU   886                 CSI CON # INVALID                            
BADOPEN  EQU   887                 OPEN FLAG INVALID                            
BADSUBL  EQU   888                 MASTER CSI LINE INVALID                      
BADCSILN EQU   889                 SUBLINE # INVALID                            
INVLINE  EQU   890                 CURSOR NOT IN VALID POSITION                 
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*   CHEKMG:  FIND KEY IN KEYAREA BASED ON RELATIVE NUMBER ON                    
*        SCREEN.  CHECK MASTER VS DETAIL LINE NUMBER IN KEY.                    
*        IF NOT MG (NOT EQUAL), PASS BACK CC NOT ZERO.                          
CHEKMG   NTR1                                                                   
         LA    RF,32               SET A(KEY SIZE)                              
         SR    RE,RE                                                            
         MR    RE,R4               CALCULATE DISPLACEMENT                       
         L     RE,AKEYAREA                                                      
         AR    RF,RE               DISPLACE TO KEY                              
         USING RBUYREC,RF                                                       
         CLC   RBUYKMLN,RBUYKLIN   MASTER = DETAIL LINE?                        
         BE    CKMG0040            DETAIL LINE:  EQUAL                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         LTR   RB,RB               M/G LINE: NOT EQUAL                          
         B     CKMG0100                                                         
CKMG0040 EQU   *                                                                
         SR    R0,R0                                                            
CKMG0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SET40ELT:  INSERT 40 ELEMENT BUILT INTO WORK AREA FOR LATER USE             
*        IF FULL VALIDATION IS CLEAN.                                           
*        R4 CONTAINS ZERO RELATIVE COUNTER OF SCREEN POSITION                   
*        CURRENTLY IN USE.                                                      
*                                                                               
SET40ELT NTR1                                                                   
         LA    RF,SZ40TAB          CALCULATE DISPLACEMENT INTO TABLE            
         SR    RE,RE                                                            
         MR    RE,R4               L(TABLE SLOT) * REC # (ZERO REL)             
         L     RE,AGRPAREA         SET A(X'40' ELT TABLE)                       
         AR    RE,RF               ADD DISPLACEMENT                             
         MVC   DSPNEW40(SZ40ELT,RE),BLD40ELT                                    
*                                  MOVE ELT TO STORAGE IN SECOND                
*                                     SLOT OF PAIR                              
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SET41ELT:  INSERT 41 ELEMENT BUILT INTO WORK AREA FOR LATER USE             
*        IF FULL VALIDATION IS CLEAN.                                           
*        R4 CONTAINS ZERO RELATIVE COUNTER OF SCREEN POSITION                   
*        CURRENTLY IN USE.                                                      
*                                                                               
SET41ELT NTR1                                                                   
         LA    RF,SZ41TAB          CALCULATE DISPLACEMENT INTO TABLE            
         SR    RE,RE                                                            
         MR    RE,R4               L(TABLE SLOT) * REC # (ZERO REL)             
         L     RE,AMGAREA          SET A(X'41' ELT TABLE)                       
         AR    RE,RF               ADD DISPLACEMENT                             
         MVC   DSPNEW41(SZ41ELT,RE),BLD41ELT                                    
*                                  MOVE ELT TO STORAGE IN SECOND                
*                                     SLOT OF PAIR                              
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   UPDATREC:  PROCESS EACH KEY AT AKEYAREA.  DELETE OLD ELTS,                  
*        INSERT NEW ONES, AND REWRITE THE RECORD(S).                            
*        AT THIS TIME, ALL RECORDS ARE UPDATED.  THIS IS SIMPLER                
*        THAN CHECKING MYRIAD FIELD BITS TO DETERMINE WHICH ONES                
*        HAVE 'CHANGED'.  THE NUMBER OF READ/WRITES IS MINIMAL.                 
*                                                                               
UPDATREC NTR1                                                                   
         LA    R4,0                SET LOOP CONTROL                             
UPDA0020 EQU   *                                                                
         XC    AGRPCHG,AGRPCHG     CLEAR CHANGES FLAGS                          
         XC    AMGCHG,AMGCHG                                                    
         LA    RF,32               SET A(KEY SIZE)                              
         SR    RE,RE                                                            
         MR    RE,R4               CALCULATE DISPLACEMENT                       
         L     RE,AKEYAREA                                                      
         AR    RF,RE               DISPLACE TO KEY                              
*                                                                               
***      CH    R4,=H'0'                                                         
***      BNE   *+6                                                              
***      DC    H'0'                DUMP                                         
*   TEST                                                                        
         OC    0(32,RF),0(RF)      EMPTY SLOT?                                  
         BZ    UPDA0900            YES - FINISHED                               
         MVC   DUB(4),28(RF)       NO  - UNLOAD KEY OF RECORD                   
         L     R1,AGRPAREA         CHECK FOR X'40' ELT CHANGE                   
         LA    RF,SZ40TAB          SET A(TABLE ENTRY FOR X'40')                 
         SR    RE,RE                                                            
         MR    RE,R4               CALCULATE DISPLACEMENT                       
         L     RE,AGRPAREA                                                      
         AR    RF,RE               DISPLACE TO KEY                              
         CLC   0(SZ40ELT,RF),DSPNEW40(RF)                                       
*                                  OLD VS NEW ELTS:  SAME?                      
         BE    UPDA0040            YES - NO UPDATE FOR X'40'                    
         ST    RF,AGRPCHG          NO  - SAVE A(X'40' TO BE ADDED)              
UPDA0040 EQU   *                                                                
         L     R1,AMGAREA          CHECK FOR X'41' ELT CHANGE                   
         LA    RF,SZ41TAB          SET A(TABLE ENTRY FOR X'41')                 
         SR    RE,RE                                                            
         MR    RE,R4               CALCULATE DISPLACEMENT                       
         L     RE,AMGAREA                                                       
         AR    RF,RE               DISPLACE TO KEY                              
         CLC   0(SZ41ELT,RF),DSPNEW41(RF)                                       
*                                  OLD VS NEW ELTS:  SAME?                      
         BE    UPDA0060            YES - NO UPDATE FOR X'41'                    
         ST    RF,AMGCHG           NO  - SAVE A(X'41' TO BE ADDED)              
UPDA0060 EQU   *                                                                
         OC    AGRPCHG(8),AGRPCHG  ANY CHANGES NEEDED?                          
         BZ    UPDA0200            NO                                           
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),DUB       YES - RETRIEVE BUY RECORD                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         OC    AGRPCHG,AGRPCHG     X'40' CHANGE?                                
         BZ    UPDA0080            NO                                           
         GOTO1 VDELELEM,DMCB,(X'40',RBUYREC)                                    
*                                  DELETE ANY OLD X'40' ELEMENT                 
         L     R2,AGRPCHG          SET A(40 ELT STORAGE)                        
         LA    R2,DSPNEW40(R2)     BUMP TO NEW 40 ELT                           
         GOTO1 VADDELEM,DMCB,RBUYREC,(R2)                                       
UPDA0080 EQU   *                                                                
         OC    AMGCHG,AMGCHG       X'41' CHANGE?                                
         BZ    UPDA0100            NO                                           
         GOTO1 VDELELEM,DMCB,(X'41',RBUYREC)                                    
*                                  DELETE ANY OLD X'41' ELEMENT                 
         L     R2,AMGCHG           SET A(41 ELT STORAGE)                        
         LA    R2,DSPNEW41(R2)     BUMP TO NEW 41 ELT                           
         GOTO1 VADDELEM,DMCB,RBUYREC,(R2)                                       
UPDA0100 EQU   *                                                                
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
UPDA0200 EQU   *                                                                
         LA    R4,1(R4)            INCREMENT LOOP CONTROL                       
         B     UPDA0020            GO BACK FOR NEXT                             
*                                                                               
*        THERE IS ROOM IN THE KEY TABLE FOR 6+ ENTRIES.  ONLY FIVE              
*        APPEAR ON THE SCREEN.  THE SIXTH SERVES AS A TABLE DELIM-              
*        ITER OF ZERO, AND KEEPS THE LOOP FROM RUNNING AWAY.                    
*                                                                               
UPDA0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SETTWAK:  SET A(KEYS IN TWA).                                               
*                                                                               
SETTWAK  NTR1                                                                   
         LA    RF,COLLAST          SET A(END OF SCREEN)                         
         A     RF,=F'8000'         MOVE DOWN 8K                                 
         ST    RF,AKEYAREA         SAVE THE ADDRESS                             
         LA    RF,200(RF)          LEAVE SPACE FOR GROUP ELTS                   
         ST    RF,AGRPAREA         SAVE THE ADDRESS                             
         LA    RF,200(RF)          LEAVE SPACE FOR M/G ELTS                     
         ST    RF,AMGAREA          SAVE THE ADDRESS                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CLERTWAK:  SET A(KEYS IN TWA), CLEAR OUT SPACE.                             
*                                                                               
CLERTWAK NTR1                                                                   
         LA    RF,COLLAST          SET A(END OF SCREEN)                         
         A     RF,=F'8000'         MOVE DOWN 8K                                 
         ST    RF,AKEYAREA         SAVE THE ADDRESS                             
         XC    0(200,RF),0(RF)     CLEAR OUT 200 BYTES                          
         LA    RF,200(RF)          LEAVE SPACE FOR GROUP ELTS                   
         ST    RF,AGRPAREA         SAVE THE ADDRESS                             
         XC    0(200,RF),0(RF)     CLEAR OUT 200 BYTES                          
         LA    RF,200(RF)          LEAVE SPACE FOR M/G ELTS                     
         ST    RF,AMGAREA          SAVE THE ADDRESS                             
         XC    0(200,RF),0(RF)     CLEAR OUT 200 BYTES                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CLERSCRN:  CYCLE THROUGH ALL FIVE LINES, ERASE ANY DATA, TURN               
*        OFF MAKEGOOD LINE                                                      
*                                                                               
CLERSCRN NTR1                                                                   
         LA    R1,5                SET LOOP CONTROL                             
         LA    R5,COLLINH          SET A(FIRST LINE)                            
         USING COLLINH,R5                                                       
CLER0020 EQU   *                                                                
         XC    COLLIN,COLLIN                                                    
         LA    R2,COLLINH                                                       
         FOUT  (R2)                                                             
         XC    COLDAYS,COLDAYS                                                  
         LA    R2,COLDAYSH                                                      
         FOUT  (R2)                                                             
         XC    COLTIMS,COLTIMS                                                  
         LA    R2,COLTIMSH                                                      
         FOUT  (R2)                                                             
         XC    COLLEN,COLLEN                                                    
         LA    R2,COLLENH                                                       
         FOUT  (R2)                                                             
         XC    COLRATE,COLRATE                                                  
         LA    R2,COLRATEH                                                      
         FOUT  (R2)                                                             
         XC    COLDTES,COLDTES                                                  
         LA    R2,COLDTESH                                                      
         FOUT  (R2)                                                             
         XC    COLGRP,COLGRP                                                    
         LA    R2,COLGRPH                                                       
         FOUT  (R2)                                                             
         XC    COLTCOD,COLTCOD                                                  
         LA    R2,COLTCODH                                                      
         FOUT  (R2)                                                             
         XC    COLPROD,COLPROD                                                  
         LA    R2,COLPRODH                                                      
         FOUT  (R2)                                                             
         XC    COLADVR,COLADVR                                                  
         LA    R2,COLADVRH                                                      
         FOUT  (R2)                                                             
         XC    COLSEC,COLSEC                                                    
         LA    R2,COLSECH                                                       
         FOUT  (R2)                                                             
         XC    COLCLS,COLCLS                                                    
         LA    R2,COLCLSH                                                       
         FOUT  (R2)                                                             
         XC    COLTIME,COLTIME                                                  
         LA    R2,COLTIMEH                                                      
         FOUT  (R2)                                                             
         XC    COLPLU,COLPLU                                                    
         LA    R2,COLPLUH                                                       
         FOUT  (R2)                                                             
         XC    COLMG#,COLMG#                                                    
         XC    COLCN#,COLCN#                                                    
         XC    COLLN#,COLLN#                                                    
         XC    COLML#,COLML#                                                    
         XC    COLSL#,COLSL#                                                    
         XC    COLOP#,COLOP#                                                    
         BAS   RE,PROTECT          TURN ON PROTECT FLAGS                        
         LA    RF,COLLIN2H-COLLINH SET L(ONE BUYLINE ON SCREEN)                 
         AR    R5,RF               BUMP TO NEXT LINE                            
         BCT   R1,CLER0020         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROTECT:  MAKEGOOD LINE MUST BE CLOSED DOWN                                 
*                                                                               
PROTECT  NTR1                                                                   
         LA    R2,COLMG1H                                                       
         MVI   BYTE,0              CLEAR FLIP-FLOP                              
         LA    R3,12               SET LOOP CONTROL: 12 FIELDS                  
PROT0020 EQU   *                                                                
         OI    1(R2),X'0C'         TURN ON  LOW INTENSITY                       
         CLI   BYTE,0              HEADER FIELD?                                
         BE    PROT0040            YES - FIELD LABEL DISPLAYED                  
         OI    1(R2),X'20'         NO  - DATA: TURN ON  PROTECT                 
PROT0040 EQU   *                                                                
         XI    BYTE,X'01'          FLIP-FLOP HEADER/DATA FIELD                  
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         BCT   R3,PROT0020         GO BACK FOR NEXT FIELD                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* DISPLAY ABBREVIATED CONTRACT HEADER INFO                                      
*                                                                               
DISPCON  NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         MVC   COLTYPE(L'RCONTYPE),RCONTYPE                                     
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
         MVC   CONMOD(7),=C'ESL VER'                                            
         B     DISCON35                                                         
                                                                                
DISCON25 DS    0H                  DISPLAY WORK IN PROGRESS                     
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
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,COLSTA                         
*                                                                               
         MVC   COLSAL,RCONSAL                                                   
*                                                                               
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),REPALPHA                                               
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),COLHDTE,0,WORK            
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,COLAGY,WORK2,WORK              
         MVC   COLAGYN,WORK2                                                    
*                                                                               
         MVC   COLADV,RCONKADV                                                  
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKADV,WORK2,0,WORK                   
         MVC   COLADVN,WORK2                                                    
*                                                                               
*                                                                               
DISCONX DS     0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
*   THESE KEYS ARE KEPT LOCAL FOR ALIGNMENT, PREVENT MOD FROM BEING             
*        CORE RESIDENT                                                          
*                                                                               
AKEYAREA DS    A                                                                
AGRPAREA DS    A                                                                
AMGAREA  DS    A                                                                
AGRPCHG  DS    A                                                                
AMGCHG   DS    A                                                                
ATIOB    DS    A                                                                
DSPNEW40 EQU   20                                                               
DSPNEW41 EQU   16                                                               
SZ40TAB  EQU   40                                                               
SZ41TAB  EQU   32                                                               
SZ40ELT  EQU   20                                                               
SZ41ELT  EQU   16                                                               
SPACES   DC    CL16'                '                                           
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTC8D                                                       
         EJECT                                                                  
LVARDSCT DSECT                                                                  
PFKEY    DS    X                   KEY PRESSED                                  
SCRNCTR  DS    X                   ITEMS ON SCREEN                              
BLD40ELT DS    XL24                BUILD AREA FOR X'40' BUY ELT                 
BLD41ELT DS    XL16                BUILD AREA FOR X'41' BUY ELT                 
LBLD4ELT EQU   *-BLD40ELT          L(BLD40/41 ELTS)                             
*                                                                               
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLCON                                                        
*                                                                               
*   CONTRACT SCREEN FIELD TABLE CONTAINS ADDRESS OF EACH SCREEN                 
*      FIELD HEADER, AND A FLAG BYTE.  IF FIELD CAN BE CHANGED,                 
*      FLAG BYTE IS SET TO X'80'.                                               
*                                                                               
FIELDTBL EQU   *                                                                
*        DC    AL4(BUYDAYSH-TWAD),X'00'                                         
*        DC    AL4(BUYTIMSH-TWAD),X'00'                                         
*        DC    AL4(BUYLENH-TWAD),X'00'                                          
*        DC    AL4(BUYDTESH-TWAD),X'00'                                         
*        DC    AL4(BUYDTE2H-TWAD),X'00'                                         
*        DC    AL4(BUYTYPH-TWAD),X'80'                                          
*        DC    AL4(BUYDPTH-TWAD),X'80'                                          
*        DC    AL4(BUYCLSH-TWAD),X'80'                                          
*        DC    AL4(BUYSECH-TWAD),X'80'                                          
*        DC    AL4(BUYPLNH-TWAD),X'80'                                          
*        DC    AL4(BUYNPWH-TWAD),X'00'                                          
*        DC    AL4(BUYRATEH-TWAD),X'00'                                         
*        DC    AL4(BUYCBC1H-TWAD),X'80'                                         
*        DC    AL4(BUYCBC2H-TWAD),X'80'                                         
*        DC    AL4(BUYCBC3H-TWAD),X'80'                                         
*        DC    AL4(BUYCBC4H-TWAD),X'80'                                         
*        DC    X'FF'               DELIMITER                                    
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071RECNT3E   05/01/02'                                      
         END                                                                    
