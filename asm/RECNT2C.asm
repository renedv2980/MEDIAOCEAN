*          DATA SET RECNT2C    AT LEVEL 187 AS OF 10/06/11                      
*PHASE T8022CA                                                                  
         TITLE 'T8022C - REPPAK MAKEGOOD OFFER TOTALS DISPLAY'                  
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT29 (T8022C) --- MAKEGOOD OFFER TOTALS DISPLAY       *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 11NOV98 JRD REMOVE UNUSED OUTDAY INCLUDE                        *             
*             MORE INFORMATIVE TAG LINE FOR SCREEN                *             
* 14DEC98 RHV FINISH THIS THING                                   *             
* 04JAN99 RHV HANDLE NON-DELETED CANCELLED BUYS                   *             
* 12SEP11 SMY EXPAND SCREEN OUTPUTS                               *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8022C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T8022C                                           
         LR    R7,RC                                                            
         USING MYWORKD,R7          LOCAL WORK AREA                              
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
MKG      USING RMKGREC,IOAREA                                                   
*                                                                               
* MAINLINE CODE                                                                 
*                                                                               
         BRAS  RE,MAKEGOOD         GET THE MAKEGOOD TOTALS                      
*                                                                               
         BRAS  RE,CONTRACT         NOW GET THE CONTRACT TOTALS                  
*                                                                               
         BRAS  RE,DISPLAY          MOVE IT TO THE SCREEN                        
*                                                                               
         LA    R2,CONCACTH         SET CONACT MODIFIED SO CAN GET BACK          
         NI    4(R2),X'FF'-X'20'   NOT PREV VALIDATED                           
         OI    1(R2),X'01'         MOD                                          
         OI    4(R2),X'80'         INPUT THIS TIME                              
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    DMCB(24),DMCB       DISPLAY MESSAGE                              
         MVC   DMCB+2(2),=AL2(143)                                              
         GOTO1 VDISMSG,DMCB,,                                                   
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* THIS ROUTINE PROCESSES THE SELECTED MAKEGOOD RECORD                           
*                                                                               
MAKEGOOD NTR1                                                                   
         XC    ACCUM(ACCUMLEN),TMKGSPOT CLEAR THE ACCUMULATORS                  
         XC    BUYLIST,BUYLIST     CLEAR THE LIST OF BUYS READ                  
         XC    CMISSPOT,CMISSPOT   CLEAR CURRENT MISSED SPOTS                   
         XC    CMISDOL,CMISDOL     CLEAR CURRENT MISSED DOLLARS                 
         XC    COFFSPOT,COFFSPOT   CLEAR CURRENT OFFER SPOTS                    
         XC    COFFDOL,COFFDOL     CLEAR CURRENT OFFER DOLLARS                  
         XC    CHNGSPOT,CHNGSPOT   CLEAR CHANGE OFFER SPOTS                     
         XC    CHNGDOL,CHNGDOL     CLEAR CHANGE OFFER DOLLARS                   
         MVI   MGTFLAGS,0          INIT                                         
*                                                                               
* READ THE SELECTED RECORD                                                      
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAMKGD2  GET THE D/A                                  
         DROP  RF                                                               
*                                                                               
MKG0050  EQU   *                                                                
*                                                                               
         GOTO1 VGETREC,DMCB,MKG.RMKGREC READ THE RECORD                         
         CLI   MKG.RMKGKPLN,0      GROUP RECORD?                                
         JE    MKG0100             YES - SO CONTINUE                            
*                                                                               
         XC    MGTHDRT,MGTHDRT                                                  
*                             123456789.123456789.123456789.123456789.          
         MVC   MGTHDRT(32),=C'*** MAKEGOOD GROUP XX TOTALS ***'                 
         MVC   MGTHDRT+19(2),MKG.RMKGKGRP                                       
*        EDIT  MKG.RMKGKLIN,(2,MGTHDRT+27)                                      
         OI    MGTHDRTH+6,X'80'    TRANSMIT                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(21),MKG.RMKGKEY                                              
         GOTO1 VHIGH                                                            
         GOTO1 VGETREC,DMCB,MKG.RMKGREC READ THE RECORD                         
*                                                                               
         TM    MKG.RMKGSCST,X'80'  GROUP APPLIED??                              
         BZ    MKG0070                                                          
         OI    MGTFLAGS,X'01'      FLAG GROUP APPLIED                           
*                                                                               
MKG0070  EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAMKGD2  GET THE D/A                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,MKG.RMKGREC READ THE RECORD                         
*                                                                               
MKG0080  EQU   *                                                                
         BRAS  RE,DETAIL           ELSE - GO PROCESS DETAIL RECORD              
         GOTO1 VSEQ                GET NEXT KEY                                 
         CLC   KEY(26),DETKEY      ANOTHER DETAIL FOR THIS LINE?                
         JNE   MKG0200                                                          
         GOTO1 VGETREC,DMCB,MKG.RMKGREC READ THE RECORD                         
         J     MKG0080                                                          
*                                                                               
MKG0100  EQU   *                                                                
         XC    MGTHDRT,MGTHDRT                                                  
*                             123456789.123456789.123456789.123456789.          
         MVC   MGTHDRT(32),=C'*** MAKEGOOD GROUP XX TOTALS ***'                 
         MVC   MGTHDRT+19(2),MKG.RMKGKGRP                                       
         OI    MGTHDRTH+6,X'80'    TRANSMIT                                     
*                                                                               
         TM    MKG.RMKGSCST,X'80'  GROUP APPLIED??                              
         BZ    MKG0110                                                          
         OI    MGTFLAGS,X'01'      FLAG GROUP APPLIED                           
*                                                                               
MKG0110  EQU   *                                                                
         BRAS  RE,HEADER           GO PROCESS HEADER/GROUP RECORD               
*                                                                               
MKG0200  EQU   *                                                                
*                                                                               
         MVC   OBUYSPOT,TBUYSPOT   ORIGINAL BUY SPOTS                           
         MVC   OBUYDOL,TBUYDOL     ORIGINAL BUY DOLLARS                         
*                                                                               
         ZICM  R8,OBUYSPOT,(3)     ORIGINAL BUY SPOTS                           
*                                                                               
         TM    MGTFLAGS,X'01'      IF APPLIED, ORIGINAL = NEW                   
         BO    MKG0210                                                          
         ZICM  R5,TMISSPOT,(3)                                                  
         SR    R8,R5               - MISSED SPOTS                               
         ZICM  R5,TMKGSPOT,(3)                                                  
         AR    R8,R5               + REPLACEMENT SPOTS                          
*                                                                               
MKG0210  EQU   *                                                                
         STCM  R8,3,NBUYSPOT       = NEW BUY SPOTS                              
*                                                                               
         ZICM  R8,OBUYDOL,(15)     ORIGINAL BUY DOLLARS                         
         TM    MGTFLAGS,X'01'      IF APPLIED, ORIGINAL = NEW                   
         BO    MKG0220                                                          
         ZICM  R5,TMISDOL,(15)                                                  
         SR    R8,R5               - MISSED DOLLARS                             
         ZICM  R5,TMKGDOL,(15)                                                  
         AR    R8,R5               + REPLACEMENT DOLLARS                        
*                                                                               
MKG0220  EQU   *                                                                
         STCM  R8,15,NBUYDOL       = NEW BUY DOLLARS                            
*                                                                               
         ZICM  R8,TMISSPOT,(3)                                                  
         MHI   R8,-1               = CHANGE TO MISSED LINES SPOTS               
         STCM  R8,3,CMISSPOT                                                    
         ZICM  R8,TMISDOL,(15)                                                  
         MHI   R8,-1               = CHANGE TO MISSED LINES DOLLARS             
         STCM  R8,15,CMISDOL                                                    
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE READS ALL THE RECORDS FOR THE CONTRACT TO GET                    
* ITS TOTALS                                                                    
*                                                                               
CONTRACT NTR1                                                                   
         XC    ACCUM(ACCUMLEN),TMKGSPOT CLEAR THE ACCUMULATORS                  
         XC    BUYLIST,BUYLIST     CLEAR THE LIST OF BUYS READ                  
*                                                                               
         XC    OCONSPOT,OCONSPOT                                                
         XC    OCONDOL,OCONDOL                                                  
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   FULL,RCONKCON                                                    
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,FULL),(3,RBUYKCON)                   
         GOTO1 VHIGH               READ THE KEY                                 
         B     CON0110                                                          
*                                                                               
CON0100  EQU   *                                                                
         GOTO1 VSEQ                                                             
*                                                                               
CON0110  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   CON0200             NO - SO ALL DONE                             
         DROP  R6                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         CLI   RBUYCHGI,C'C'       DON'T COUNT CANCELLED BUYS                   
         BE    CON0100                                                          
*                                                                               
         MVC   HALF,RBUYTSPT                                                    
         MVC   FULL,RBUYTCOS                                                    
         ZICM  R1,OCONSPOT,2                                                    
         AH    R1,HALF             UPDATE SPOT TOTAL                            
         STCM  R1,3,OCONSPOT                                                    
         ICM   R1,15,OCONDOL                                                    
         A     R1,FULL             UPDATE DOLLAR TOTAL                          
         STCM  R1,15,OCONDOL                                                    
*                                                                               
         B     CON0100             NEXT BUY                                     
*                                                                               
CON0200  EQU   *                   FIGURE NEW CONTRACT TOTALS                   
         ZICM  R1,OCONSPOT,2                                                    
         TM    MGTFLAGS,X'01'      IF APPLIED, OLD = NEW                        
         BO    CON0210                                                          
         MVC   HALF,NBUYSPOT                                                    
         AH    R1,HALF             + NEW BUY                                    
         MVC   HALF,OBUYSPOT                                                    
         SH    R1,HALF             - OLD BUY                                    
*                                                                               
CON0210  EQU   *                                                                
         STCM  R1,3,NCONSPOT                                                    
*                                                                               
         ICM   R1,15,OCONDOL                                                    
         TM    MGTFLAGS,X'01'      IF APPLIED, OLD = NEW                        
         BO    CON0220                                                          
         MVC   FULL,NBUYDOL                                                     
         A     R1,FULL             + NEW BUY                                    
         MVC   FULL,OBUYDOL                                                     
         S     R1,FULL             - OLD BUY                                    
*                                                                               
CON0220  EQU   *                                                                
         STCM  R1,15,NCONDOL                                                    
         B     XIT                                                              
*                                                                               
* THIS ROUTINE MOVES THE TOTALS TO THE SCREEN                                   
*                                                                               
DISPLAY  NTR1                                                                   
         BAS   RE,DISCHEAD         DISPLAY CONTRACT HEADER INFO                 
*                                                                               
         BAS   RE,DISCALC          "FINAL" SPOTS/DOLLARS CALCULATIONS           
*                                                                               
* OUTPUT THE SPOTS AND DOLLARS                                                  
*                                                                               
         LA    R5,TABLE1                                                        
         USING TABLE1D,R5                                                       
*                                                                               
DISP0100 EQU   *                                                                
*                                                                               
         CLC   INPUT,=F'0'         END OF TABLE?                                
         JE    DISP0300            YES - SO CONTINUE                            
*                                                                               
         L     R2,OUTPUT           R2 = RELOCATABLE A(OUTPUT HEADER)            
         AR    R2,RA               RELOCATE IT                                  
         L     R1,INPUT            R1 = RELOCATABLE A(INPUT)                    
         AR    R1,R7               RELOCATE IT                                  
*                                                                               
         CLI   TYPE,C'S'           EDIT SPOTS?                                  
         JNE   DISP0200            NO - SO CONTINUE                             
*                                                                               
         LH    R1,0(R1)            R1 = SPOTS                                   
         EDIT  (R1),(4,8(R2)),ZERO=NOBLANK,FLOAT=-                              
         J     DISP0250            AND CONTINUE                                 
*                                                                               
DISP0200 EQU   *                                                                
*                                                                               
         L     R1,0(R1)            R1 = DOLLARS                                 
         EDIT  (R1),(10,8(R2)),2,ZERO=NOBLANK,FLOAT=-                           
*                                                                               
DISP0250 EQU   *                                                                
*                                                                               
         OI    6(R2),X'80'         SET THE TRANSMIT BIT                         
         LA    R5,LTABLE1D(R5)     INC TO NEXT TABLE ENTRY                      
         J     DISP0100            AND GO PROCESS IT                            
         DROP  R5                                                               
*                                                                               
DISP0300 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* THIS ROUTINE DISPLAYS VARIOUS CONTRACT HEADER INFO                            
*                                                                               
DISCHEAD NTR1                                                                   
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,MGTAGY,0,DUB                   
*                                                                               
         MVC   MGTADV(4),RCONKADV                                               
*                                                                               
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,MGTSTA                         
*                                                                               
         CLC   RCONPRD,MYSPACES                                                 
         BE    DISKH10                                                          
         MVC   MGTPRD(2),=C'C='                                                 
         MVC   MGTPRD+2(3),RCONPRD                                              
         MVI   MGTPRD+5,L'CONPRD                                                
         MVC   MGTPRD+6(14),WPRDEXP                                             
         B     DISKH20                                                          
*                                                                               
DISKH10  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO X'05'                                     
         MVC   MGTPRD,2(R6)        PRODUCT EXPANSION                            
         MVI   MGTPRDH+5,L'CONPRD                                               
*                                                                               
DISKH20  DS    0H                                                               
         MVC   MGTSAL,RCONSAL                                                   
         MVC   MGTSALN,WSALEXP                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,MGTODTS)                             
         GOTO1 (RF),(R1),(3,RCONDATE+3),(5,MGTODTS+9)                           
         MVI   MGTODTS+8,C'-'                                                   
*                                                                               
DISKHX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*          THIS ROUTINE CALCULATES SPOTS/DOLLARS                                
*        FOR "MISSED LINE(S) IF OFFER IS APPLIED" AND                           
*        FOR "CHANGE IS #/$ IF OFFER IS APPLIED"                                
*                                                                               
DISCALC  NTR1                                                                   
*                                                                               
         MVC   HALF,OBUYSPOT                                                    
         MVC   FULL,OBUYDOL                                                     
         ZICM  R1,NBUYSPOT,(3)     SUBTRACT "MISSED LINES TOTALS"               
         SH    R1,HALF             FROM "TOTAL NEW LINES"                       
         STCM  R1,3,CHNGSPOT       TO GET "CHANGE IS . . ."                     
         ZICM  R1,NBUYDOL,(15)                                                  
         S     R1,FULL                                                          
         STCM  R1,15,CHNGDOL                                                    
*                                                                               
         MVC   HALF,COFFSPOT                                                    
         MVC   FULL,COFFDOL                                                     
         ZICM  R1,NBUYSPOT,(3)     SUBTRACT "OFFERRED TOTALS"                   
         SH    R1,HALF             FROM "TOTAL NEW LINES"                       
         STCM  R1,3,NBUYSPOT       TO GET "MISSED LINE(S) . . ."                
         ZICM  R1,NBUYDOL,(15)                                                  
         S     R1,FULL                                                          
         STCM  R1,15,NBUYDOL                                                    
*                                                                               
DISCALX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
* THIS ROUTINE PROCESSES THE HEADER/GROUP MAKEGOOD RECORD                       
*                                                                               
HEADER   NTR1                                                                   
         MVC   HEADKEY,MKG.RMKGKEY SAVE THE KEY                                 
         MVC   KEY(27),HEADKEY     MOVE TO KEY                                  
         GOTO1 VHIGH               RE-READ THE KEY                              
*                                                                               
HEAD0100 EQU   *                                                                
*                                                                               
         GOTO1 VSEQ                READ THE NEXT KEY                            
         CLC   KEY(21),HEADKEY     SAME BASE KEY (I.E. DETAIL RECORD)?          
         JNE   HEAD0200            NO - SO ALL DONE                             
*                                                                               
         GOTO1 VGETREC,DMCB,MKG.RMKGREC ELSE - READ THE RECORD                  
         BRAS  RE,DETAIL           GO PROCESS THE DETAIL RECORD                 
*                                                                               
         J     HEAD0100            AND GET NEXT KEY                             
*                                                                               
HEAD0200 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE PROCESSES THE DETAIL MAKEGOOD RECORD                             
*                                                                               
DETAIL   NTR1                                                                   
         MVC   DETKEY,MKG.RMKGKEY  SAVE THE KEY                                 
*                                                                               
         ZIC   R8,MKG.RMKGKRTY     CHECK FOR ALL VS. CHOICE                     
         SRL   R8,4                SHIFT OUT THE LINE NUMBER                    
         OR    R8,R8               CHOICE INDICATOR SET?                        
         JZ    DET0100             NO - SO CONTINUE                             
*                                                                               
* READ THE X'20' ELEMENT TO SEE IF THIS MAKEGOOD HAS BEEN 'CHOSEN'              
*                                                                               
         LA    R6,MKG.RMKGREC      A(RECORD)                                    
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSTEL,R6                                                      
*                                                                               
         CLI   RMKGSTCH,0          CHOSEN?                                      
         JE    DET0500             NO - SO JUST LEAVE                           
*                                                                               
DET0100  EQU   *                                                                
*                                                                               
* ADD THE 'NEW' SPOTS AND DOLARS TO THE TOTALS                                  
*                                                                               
         ZICM  R8,TMKGSPOT,(3)     NEW SPOTS                                    
         AH    R8,MKG.RMKGTSPT                                                  
         STCM  R8,3,TMKGSPOT                                                    
         ZICM  R8,TMKGDOL,(15)     NEW DOLLARS                                  
         ZICM  R5,MKG.RMKGTCOS,(15)                                             
         AR    R8,R5                                                            
         STCM  R8,15,TMKGDOL                                                    
*                                                                               
* ADD THE 'NEW' SPOTS AND DOLARS TO THE CURRENT OFFER                           
*                                                                               
         ZICM  R8,COFFSPOT,(3)     CURRENT SPOTS                                
         AH    R8,MKG.RMKGTSPT                                                  
         STCM  R8,3,COFFSPOT                                                    
         ZICM  R8,COFFDOL,(15)     CURRENT DOLLARS                              
         ZICM  R5,MKG.RMKGTCOS,(15)                                             
         AR    R8,R5                                                            
         STCM  R8,15,COFFDOL                                                    
*                                                                               
* READ THE BUY(S) FOR THE 'CURRENT' SPOTS AND DOLLARS                           
*                                                                               
* READ THE X'05' ELEMENTS TO GET THE MISSED BUY(S) LINE NUMBER(S)               
*                                                                               
         LA    R6,MKG.RMKGREC      A(RECORD)                                    
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DET0500             IF NOT FOUND, THIS IS A BONUS                
         USING RMKGMGEL,R6                                                      
*                                                                               
         CLI   RMKGMGSP,0          ANY SPOTS MISSED?                            
         JE    DET0400             NO - SO SKIP IT                              
*                                                                               
DET0200  EQU   *                                                                
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R8,KEY                                                           
         USING RBUYKEY,R8                                                       
*                                                                               
* BUILD THE BASE KEY                                                            
*                                                                               
         MVI   RBUYKTYP,X'0B'      KEY TYPE FOR BUY                             
         MVC   RBUYKREP,MKG.RMKGKREP REP CODE                                   
         MVC   RBUYKCON,MKG.RMKGKCON CONTRACT #                                 
         MVC   RBUYKPLN,MKG.RMKGKPLN PLAN                                       
*                                                                               
         GOTO1 VHIGH               READ THE KEY                                 
         J     DET0350             JUMP OVER SEQ                                
*                                                                               
DET0300  EQU   *                                                                
*                                                                               
         GOTO1 VSEQ                READ THE NEXT KEY                            
*                                                                               
DET0350  EQU   *                                                                
*                                                                               
         CLC   KEY(25),KEYSAVE     SAME THRU PLAN?                              
         JE    *+6                 YES - SO CONTINUE                            
*                                                                               
         DC    H'0'                ELSE - WADDID I DO WRONG?                    
*                                                                               
         CLC   RBUYKLIN,RMKGMGLI   CORRECT LINE NUMBER?                         
         JNE   DET0300             NO - SO GET NEXT KEY                         
*                                                                               
         DROP  R8                                                               
         GOTO1 VGETREC,DMCB,RBUYREC ELSE - READ THE RECORD                      
*                                                                               
* GET THE 'MISSED' SPOTS AND DOLLARS                                            
*                                                                               
         ZICM  R8,TMISSPOT,(3)     MISSED SPOTS                                 
*                                                                               
* CHECK IF MISSED DATE MORE THAN ONE WEEK                                       
*                                                                               
         BRAS  RE,CHKWKS                                                        
         AR    R8,R5                                                            
*                                                                               
         STCM  R8,3,TMISSPOT                                                    
         STH   R5,HALF             STORE MISSED SPOTS                           
         ZICM  R8,RBUYCOS,(15)     ORIGINAL COST PER SPOT                       
         MH    R8,HALF             * NUMBER MISSED SPOTS                        
         ZICM  R5,TMISDOL,(15)                                                  
         AR    R8,R5                                                            
         STCM  R8,15,TMISDOL       = MISSED DOLLARS                             
*                                                                               
         LA    R2,BUYLIST          GET A(LIST OF ALREADY READ BUYS)             
         ZIC   R0,RMKGMGLI         THIS BUY'S LINE NUMBER                       
         AR    R2,R0               INDEX INTO LIST                              
         BCTR  R2,0                MAKE ZERO RELATIVE                           
         CLI   0(R2),0             BUY ALREADY READ?                            
         JNE   DET0400             YES - SO SKIP IT                             
*                                                                               
         MVI   0(R2),1             ELSE - SET 'BUY ALREADY READ'                
*                                                                               
* GET THE 'ORIGINAL' SPOTS AND DOLARS                                           
*                                                                               
         ZICM  R8,TBUYSPOT,(3)     ORIGINAL SPOTS                               
         AH    R8,RBUYTSPT                                                      
         STCM  R8,3,TBUYSPOT                                                    
         ZICM  R8,TBUYDOL,(15)     ORIGINAL DOLLARS                             
         ZICM  R5,RBUYTCOS,(15)                                                 
         AR    R8,R5                                                            
         STCM  R8,15,TBUYDOL                                                    
*                                                                               
DET0400  EQU   *                                                                
*                                                                               
         BRAS  RE,NEXTEL           ANY MORE ELEMENTS?                           
         JE    DET0200             YES - SO LOOP BACK                           
*                                                                               
DET0500  EQU   *                                                                
*                                                                               
         MVC   KEY(27),DETKEY      RESTORE THE DETAIL KEY                       
         GOTO1 VHIGH               AND RE-READ IT                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CHECK IF MISSED MORE THAN ONE WEEK. IF SO, CALCULATE TOTAL SPOTS              
* MISSED                                                                        
*                                                                               
CHKWKS   NTR1                                                                   
         ZIC   R5,RMKGMGSP                                                      
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    CWKX                                                             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),(5,WORK)                                
         MVI   WORK+8,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RMKGMGD2),(5,WORK+9)                              
*                                                                               
         L     RE,ACOMFACS         CALL COMFACS FOR ADDRESS OF PERVAL           
         USING COMFACSD,RE                                                      
         L     RF,CPERVAL                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,(17,WORK),PERVBUF                                      
         CLI   DMCB+4,0                                                         
         BNE   CWKX                ERROR, JUST EXIT FOR NOW                     
*                                                                               
WKD      USING PERVALD,PERVBUF                                                  
CWK10    DS    0H                                                               
         STH   R5,HALF                                                          
         ZICM  R5,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         MH    R5,HALF                                                          
         DROP  WKD                                                              
*                                                                               
CWKX     DS    0H                                                               
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
         DROP  R6                                                               
         LTORG                                                                  
         DS    0D                  ALIGNMENT                                    
         SPACE 3                                                                
TABLE1   EQU   *                                                                
         DC    A(CHNGSPOT-MYWORKD),A(MGTCLTSH-TWAD),C'S'                        
         DC    A(COFFSPOT-MYWORKD),A(MGTNLTSH-TWAD),C'S'                        
         DC    A(CHNGDOL-MYWORKD),A(MGTCLTDH-TWAD),C'D'                         
         DC    A(COFFDOL-MYWORKD),A(MGTNLTDH-TWAD),C'D'                         
*                                                                               
         DC    A(CMISSPOT-MYWORKD),A(MGTMLOSH-TWAD),C'S'                        
         DC    A(OBUYSPOT-MYWORKD),A(MGTMLTSH-TWAD),C'S'                        
         DC    A(CMISDOL-MYWORKD),A(MGTMLODH-TWAD),C'D'                         
         DC    A(OBUYDOL-MYWORKD),A(MGTMLTDH-TWAD),C'D'                         
*                                                                               
         DC    A(NCONSPOT-MYWORKD),A(MGTNCTSH-TWAD),C'S'                        
         DC    A(OCONSPOT-MYWORKD),A(MGTOCTSH-TWAD),C'S'                        
         DC    A(NCONDOL-MYWORKD),A(MGTNCTDH-TWAD),C'D'                         
         DC    A(OCONDOL-MYWORKD),A(MGTOCTDH-TWAD),C'D'                         
*                                                                               
         DC    F'0'                E.O.T.                                       
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTDCD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
*                                                                               
* LOCAL WORK AREA                                                               
*                                                                               
MYWORKD  DSECT                                                                  
HEADKEY  DS    XL27                HEADER RECORD'S KEY                          
DETKEY   DS    XL27                DETAIL RECORD'S KEY                          
*                                                                               
BUYLIST  DS    XL255               LIST OF BUYS READ                            
*                                                                               
ACCUM    EQU   *                   TEMPORARY ACCUMULATORS                       
TMKGSPOT DS    XL2                 TOTAL SPOTS FOR MAKEGOOD                     
TMKGDOL  DS    XL4                 TOTAL DOLLARS FOR MAKEGOOD                   
TBUYSPOT DS    XL2                 TOTAL SPOTS FOR OLD BUY                      
TBUYDOL  DS    XL4                 TOTAL DOLLARS FOR OLD BUY                    
TMISSPOT DS    XL2                 TOTAL SPOTS FOR MISSED BUY                   
TMISDOL  DS    XL4                 TOTAL DOLLARS FOR MISSED BUY                 
ACCUMLEN EQU   *-TMKGSPOT          L(THESE ACCUMULATORS)                        
*                                                                               
OBUYSPOT DS    XL2                 TOTAL SPOTS FOR 'OLD' BUY                    
OBUYDOL  DS    XL4                 TOTAL DOLLARS FOR 'OLD' BUY                  
NBUYSPOT DS    XL2                 TOTAL SPOTS FOR 'NEW' BUY                    
NBUYDOL  DS    XL4                 TOTAL DOLLARS FOR 'NEW' BUY                  
OCONSPOT DS    XL2                 TOTAL SPOTS FOR 'OLD' CONTRACT               
OCONDOL  DS    XL4                 TOTAL DOLLARS FOR 'OLD' CONTRACT             
NCONSPOT DS    XL2                 TOTAL SPOTS FOR 'NEW' CONTRACT               
NCONDOL  DS    XL4                 TOTAL DOLLARS FOR 'NEW' CONTRACT             
*                                                                               
MGTFLAGS DS    X                   FLAGS: X'01' = GROUP APPLIED                 
*                                  CURRENT ACCUMULATORS                         
CMISSPOT DS    XL2                 CURRENT MISSED SPOTS FOR MAKEGOOD            
CMISDOL  DS    XL4                 CURRENT MISSED DOLLARS FOR MAKEGOOD          
COFFSPOT DS    XL2                 CURRENT OFFERED SPOTS FOR MAKEGOOD           
COFFDOL  DS    XL4                 CURRENT OFFERED DOLLARS FOR MAKEGOOD         
CHNGSPOT DS    XL2                 SPOTS CHANGE IF OFFER APPLIED                
CHNGDOL  DS    XL4                 DOLLARS CHANGE IF OFFER APPLIED              
*                                                                               
PERVBUF  DS    XL255               BUFFER FOR PERVAL                            
*                                                                               
MYWORKX  EQU   *                                                                
*                                                                               
TABLE1D  DSECT                                                                  
INPUT    DS    A                   A(INPUT FIELD)                               
OUTPUT   DS    A                   A(OUTPUT FIELD)                              
TYPE     DS    CL1                 S = SPOTS EDIT, D = $ EDIT                   
         DS    0F                                                               
LTABLE1D EQU   *-TABLE1D                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'187RECNT2C   10/06/11'                                      
         END                                                                    
