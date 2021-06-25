*          DATA SET SPINF18    AT LEVEL 036 AS OF 07/23/13                      
*PHASE T21A18A                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SPBVAL                                                                 
*INCLUDE SPFMTINO                                                               
         TITLE 'T21A18 - SPOTPAK INFO BILL RECORD DISPLAY'                      
T21A18   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BILLWRKL,T21A18,CLEAR=YES                                        
         LR    R3,RC                                                            
         USING BILLWRKD,R3                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R7,2048(RB)         R7 IS 2ND BASE REG                           
         LA    R7,2048(R7)                                                      
         USING T21A18+4096,R7                                                   
*                                  GET B1X PROFILE                              
MAINPR   DS    0H                                                               
         XC    B1XPROF,B1XPROF                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR EXT PROF             
         MVC   WORK+4(2),SVAGYA                                                 
         MVC   WORK+6(1),SVEBCMED                                               
         MVC   WORK+7(3),SVEBCCLT                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFF   OFFICE                                       
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,B1XPROF,VDATAMGR                                  
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
*                                                                               
         XC    PROFB9,PROFB9                                                    
         MVC   WORK(4),=C'S0B9'    GET B9 PROFILE TO CHK RETAIL                 
         GOTO1 (RF),(R1),,PROFB9                                                
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
*                                                                               
         XC    B1PROF,B1PROF                                                    
         MVC   WORK(4),=C'S0B1'    GET B1 PROFILE FOR SPFMTINO                  
         GOTO1 (RF),(R1),,B1PROF                                                
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
*                                                                               
         ZIC   RF,B1XPROF+14       START DATE FOR INVNO YM FEATURE              
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  B1XYR,DUB                                                        
         ZIC   RF,B1XPROF+15                                                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  B1XMM,DUB                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
         USING FLDHDRD,R2                                                       
         LA    R5,REC              SET RECORD ADDRESS                           
         ST    R5,AREC                                                          
*                                                                               
         MVI   NOUSER,C'N'                                                      
*                                                                               
         CLI   PROFB9,0       IS THIS A RETAIL CLIENT?                          
         BE    *+8            IF SO DEFAULT TO Y                                
         MVI   NOUSER,C'Y'    SO I'LL DISPLAY ACCOUNT                           
*                                                                               
*                                                                               
         GOTO1 USER1,DUB,(64,SINIFLT),(6,=C'NOUSER')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    *+8                                                              
         MVI   NOUSER,C'Y'         DON'T DISPLAY USER (FOR SOON BILLS)          
*                                                                               
         GOTO1 USER1,DUB,(64,SINIFLT),(4,=C'USER')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    *+12                                                             
         MVI   NOUSER,C'N'         DISPLAY USER                                 
*                                                                               
*   NOTE - IF THEY HAPPEN TO ENTER BOTH, THE "USER" ONE WILL APPLY              
*                                                                               
         MVI   LMGFLAG,C'N'                                                     
***                                                                             
***      NO-OP DISPLAY OF 'BILLED TO' FOR DDS TERMINALS                         
***                                                                             
***      CLI   T21AFFD+1,C'*'      DDS TERMINAL?                                
***      BNE   *+12                NO-CHECK AGENCY                              
***      MVI   LMGFLAG,C'Y'   YES,DISPLAY "BILL TO" COLUMN FOR EVERYONE         
***      B     CHKEST                                                           
*                                                                               
         CLC   AGYALPHA,=C'GZ'     FOR NON-DDS TERMINALS                        
         BNE   CHKEST              DISPLAY "BILL TO" FOR GZ ONLY                
*                                                                               
         GOTO1 USER1,DUB,(64,SINIFLT),(8,=C'GMBILLTY')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKEST                                                           
         MVI   LMGFLAG,C'Y'                                                     
*                                                                               
CHKEST   DS    0H                                                               
         XC    EST1(2),EST1                                                     
         GOTO1 USER1,DUB,(64,SINIFLT),(4,=C'EST=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKMOS                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         CLC   0(2,R4),=C'NO'                                                   
         BNE   CE1                                                              
         MVC   EST1(2),=X'01FF'                                                 
         B     CHKMOS                                                           
*                                                                               
CE1      BAS   R9,GETNUM                                                        
         L     R6,4(R1)                                                         
         LA    RE,4                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(11),=C'EST=NNN-NNN'                                         
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         L     R4,4(R1)            CONVERT ESTIMATE 1                           
         LA    R4,4(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)        * EXECUTED *                                  
         CVB   RF,DUB                                                           
         STC   RF,EST1                                                          
         STC   RF,EST2                                                          
         LA    R4,1(R5,R4)                                                      
         CLI   0(R4),C'-'          ESTIMATE SERIES                              
         BNE   CHKMOS               NO - CHECK DATA FORMAT                      
         LA    R4,1(R4)                                                         
         LR    RF,R4                                                            
         BAS   R9,GETNUM                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)        * EXECUTED *                                  
         CVB   RF,DUB                                                           
         STC   RF,EST2                                                          
         CLI   EST1,0                                                           
         BE    FLTERR                                                           
         CLC   EST1,EST2                                                        
         BH    FLTERR                                                           
         EJECT                                                                  
CHKMOS   XC    MOS,MOS                                                          
         GOTO1 USER1,DUB,(64,SINIFLT),(4,=C'MOS=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKBMO                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         LR    R6,R4                                                            
         XC    WORK,WORK                                                        
         GOTO1 VDATVAL,DMCB,(R4),WORK2  VALIDATE FOR MDY                        
         LA    RE,8                                                             
         CLI   DMCB+3,0                 WHICH IS NOT VALID                      
         BNE   FLTERR                                                           
         GOTO1 (RF),(R1),(2,(R4))       VALIDATE FOR MY                         
         MVC   WORK(10),=C'MOS=MMM/YY'                                          
         LA    RE,8                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         GOTO1 VDATCON,DMCB,WORK2,(3,DUB)                                       
         MVC   MOS(2),DUB                                                       
         EJECT                                                                  
CHKBMO   XC    FBILMO,FBILMO                                                    
         GOTO1 USER1,DUB,(64,SINIFLT),(7,=C'BILLMO=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKBNO                                                           
         XC    WORK,WORK                                                        
         LA    RE,7                                                             
         MVC   WORK(13),=C'BILLMO=MMM/YY'                                       
         L     R4,4(R1)                                                         
         LA    R4,7(R4)                                                         
         LR    R6,R4                                                            
         GOTO1 VDATVAL,DMCB,(R4),WORK2+8  VALIDATE FOR MDY                      
         LA    RE,7                                                             
         CLI   DMCB+3,0                   WHICH IS NOT VALID                    
         BNE   FLTERR                                                           
         GOTO1 (RF),(R1),(2,(R4))  VALIDATE FOR M/Y                             
         LA    RE,7                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         MVC   BILLMO,WORK2+8      SAVE FULL MOS FOR 2ND CHECK                  
         GOTO1 VDATCON,DMCB,WORK2+8,(3,DUB)                                     
         SR    RE,RE               GET LAST DIGIT OF YEAR                       
         IC    RE,DUB                                                           
         SRDL  RE,32                                                            
         D     RE,=F'10'                                                        
         SLL   RE,4                REMAINDER=YEAR                               
         SR    RF,RF                                                            
         IC    RF,DUB+1            GET MONTH                                    
         OR    RE,RF                                                            
         STC   RE,FBILMO                                                        
*                                                                               
* CHECK FOR BILL NUMBER FILTER                                                  
CHKBNO   DS    0H                                                               
         XC    BNNO,BNNO                                                        
         MVI   BNCNTRL,0                                                        
         GOTO1 USER1,DUB,(64,SINIFLT),(6,=C'INVNO=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKBDATE                                                         
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
*                                                                               
         MVC   FILT,0(R4)          SAVE FILTER VALUE                            
* BNCNTRL MAY BE USED LATER FOR 'GT' 'LT' FILTER OPTIONS                        
         MVI   BNCNTRL,X'70'       DEFAULT = EQ                                 
*                                                                               
*&&DO                                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(14),=C'INVNO=AANNNNNN'                                      
         LA    RE,7                                                             
         MVI   BNCNTRL,X'70'       DEFAULT = EQ                                 
         CLC   0(2,R4),=C'LT'                                                   
         BNE   *+12                                                             
         MVI   BNCNTRL,X'20'      BYPASS GREATER THAN                           
         LA    R4,2(R4)                                                         
         CLC   0(2,R4),=C'GT'                                                   
         BNE   *+12                                                             
         MVI   BNCNTRL,X'40'      BYPASS LESS THAN                              
         LA    R4,2(R4)                                                         
         CLC   0(2,R4),=C'EQ'                                                   
         BNE   *+12                                                             
         MVI   BNCNTRL,X'70'      BYPASS NOT EQUAL                              
         LA    R4,2(R4)                                                         
         LR    R6,R4               SAVE START                                   
*                                                                               
         XC    DUB,DUB                                                          
         CLI   B1XPROF+14,0        IS INV NO. MONTH YM?                         
         BE    CBN05                                                            
         LA    R4,DUB              NOTE- USERS OF THIS FEATURE                  
         MVC   DUB(6),0(R6)        CANT RECALL EARLIER BILLS BY INVNO           
         MVI   DUB,C'0'            LOSE YEAR AND CHANGE MONTH FROM              
         CLI   1(R6),C'0'          1-C TO 01-12                                 
         BNL   CBN04                                                            
         MVI   DUB,C'1'                                                         
         MVI   DUB+1,C'2'                                                       
         CLI   1(R6),C'C'                                                       
         BE    CBN04                                                            
         MVI   DUB+1,C'1'                                                       
         CLI   1(R6),C'B'                                                       
         BE    CBN04                                                            
         MVI   DUB+1,C'0'                                                       
*                                                                               
CBN04    DS    0H                                                               
         LA    R6,DUB                                                           
*                                                                               
CBN05    DS    0H                                                               
         BAS   R9,GETNUM                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R6)        * EXECUTED *                                  
         CVB   RF,DUB                                                           
         ST    RF,BNNO                                                          
*&&                                                                             
*                                                                               
* CHECK BILL DATE                                                               
CHKBDATE XC    FBILDATE,FBILDATE                                                
         MVI   FBILMO2,0                                                        
         XC    FBILDAT2,FBILDAT2                                                
         GOTO1 USER1,DUB,(64,SINIFLT),(9,=C'BILLDATE=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKBTYP                                                          
         LA    RE,9                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(17),=C'BILLDATE=MMMDD/YY'                                   
         L     R4,4(R1)                                                         
         LA    R4,9(R4)                                                         
         LR    R6,R4                                                            
         GOTO1 VDATVAL,DMCB,(R4),FBILDATE                                       
         LA    RE,9                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         SR    RE,RE                                                            
         IC    RE,DMCB+3           GET LENGTH OF INPUT                          
         AR    R4,RE               POINT TO NEXT INPUT CHARACTER                
*                                                                               
* BILLDATE FILTER DOESN'T WORK DEAD WELL ACROSS DECADES, SO DON'T               
* FILTER ON KEY, ONLY USE FULL DATE.  IF FBILMO AND FBILMO2 ARE 0,              
* IT WON'T FILTER ON THE KEY AT BNOOK.                                          
*                                                                               
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,FBILDATE,(3,DUB)                                    
         SR    RE,RE                                                            
         IC    RE,DUB              GET LAST DIGIT OF YEAR                       
         SRDL  RE,32                                                            
         D     RE,=F'10'                                                        
         SLL   RE,4                REMAINDER = YEAR                             
         SR    RF,RF                                                            
         IC    RF,DUB+1                                                         
         OR    RE,RF                                                            
         STC   RE,FBILMO                                                        
*&&                                                                             
*                                                                               
         CLI   0(R4),C'-'                                                       
         BNE   CHKBTYP                                                          
*                                                                               
         LA    R4,1(R4)                                                         
         GOTO1 VDATVAL,DMCB,(R4),FBILDAT2                                       
         LA    RE,9                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,FBILDAT2,(3,DUB)                                    
         SR    RE,RE               GET LAST DIGIT OF YEAR                       
         IC    RE,DUB                                                           
         SRDL  RE,32                                                            
         D     RE,=F'10'                                                        
         SLL   RE,4                REMAINDER=YEAR                               
         SR    RF,RF                                                            
         IC    RF,DUB+1                                                         
         OR    RE,RF                                                            
         STC   RE,FBILMO2                                                       
*&&                                                                             
         EJECT                                                                  
* CHECK BILL TYPE FILTER                                                        
CHKBTYP  XC    FBILTYP,FBILTYP                                                  
         MVI   FBILNEG,0         ZERO NEGATIVE FILTER BYTE                      
         GOTO1 USER1,DMCB,(64,SINIFLT),(9,=C'BILLTYPE=')                        
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKDIST                                                          
         LA    RE,9                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(11),=C'4-7,AOR,MAN'                                         
         LA    RF,TYPETAB                                                       
         L     R4,4(R1)                                                         
         LA    R4,9(R4)                                                         
         LR    R6,R4                                                            
**NEW 2/8/90                                                                    
         CLI   0(R4),C'-'        ALL BUT FILTER                                 
         BNE   CHKBTYP1                                                         
         OI    FBILNEG,X'01'                                                    
         LA    R4,1(R4)                                                         
*                                                                               
CHKBTYP1 CLI   0(RF),0                                                          
         BE    FLTERR                                                           
         CLC   0(1,R4),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     CHKBTYP1                                                         
         MVC   FBILTYP,1(RF)                                                    
         EJECT                                                                  
CHKDIST  DS    0H                                                               
         XC    FDIST,FDIST                                                      
         GOTO1 USER1,DMCB,(64,SINIFLT),(5,=C'DIST=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKCURR                                                          
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         SR    R6,R6                                                            
         CLI   0(R4),C'*'          BYPASS LEADING STARS                         
         BNE   *+16                                                             
         LA    R4,1(R4)                                                         
         LA    R6,1(R6)                                                         
         B     *-16                                                             
         STC   R6,FDISTDSP         LENGTH TO SKIP ON COMPARE                    
*                                                                               
         LR    R6,R4                                                            
         CLI   0(R4),C'A'          TERMINATE ON NON-ALPHA NUM                   
         BL    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         SR    R4,R6                                                            
         STC   R4,FDISTLEN         LENGTH OF INPUT                              
         MVI   FDIST,C' '                                                       
         MVC   FDIST+1(L'FDIST-1),FDIST                                         
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   FDIST(0),0(R6)                                                   
*                                                                               
         EJECT                                                                  
CHKCURR  DS    0H                                                               
         MVI   CURRNCY,0                                                        
         MVI   C2SW,C'N'                                                        
         MVC   WAGYCUR,SVCNTRY                                                  
         CLI   WAGYCUR,C'0'       AGENCY DEFAULTS TO US                         
         BNE   *+8                                                              
         MVI   WAGYCUR,C'U'                                                     
         MVC   WCLTCUR,SVCLEX+9                                                 
         CLI   WCLTCUR,0          CLIENT DEFAULTS TO AGENCY                     
         BNE   *+10                                                             
         MVC   WCLTCUR,WAGYCUR                                                  
         GOTO1 USER1,DMCB,(64,SINIFLT),(9,=C'CURRENCY=')                        
         OC    4(4,R1),4(R1)                                                    
         BZ    CKCUR9                                                           
         LA    RE,9                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(1),WAGYCUR     AGENCY CURRENCY                              
         MVI   WORK+1,C','                                                      
         MVC   WORK+2(1),WCLTCUR   CLIENT CURRENCY                              
*                                                                               
         L     R4,4(R1)                                                         
         LA    R4,9(R4)                                                         
         LR    R6,R4                                                            
         MVC   CURRNCY,0(R4)                                                    
*                                                                               
         CLC   CURRNCY,WAGYCUR    IF CURRENCY = COUNTRY                         
         BE    CKCUR9              NOTHING SPECIAL                              
         CLC   CURRNCY,WCLTCUR     = CLIENT CURRENCY?                           
         BNE   FLTERR              NO, ERROR                                    
         MVI   C2SW,C'Y'           SET TO USE 2ND CURRENCY                      
*                                                                               
CKCUR9   DS    0H                                                               
         EJECT                                                                  
CHKPW    DS    0H                  CHECK FOR WILA 'PROFIT WITHIN'               
*                                                                               
*        NOTE- THE PW GROSS,NET AND ACT ARE KEPT IN                             
*        THE CLIENT (2ND) CURRENCY FIELDS, WHILE THE                            
*        TRUE MEDIA FIGURES ARE IN THE USUAL PLACE.                             
*                                                                               
         GOTO1 USER1,DMCB,(64,SINIFLT),(3,=C'PW=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CKPW9                                                            
         LA    RE,3                                                             
         L     R4,4(R1)                                                         
         MVI   C2SW,C'N'                                                        
         CLI   3(R4),C'N'                                                       
         BE    CKPW9                                                            
         MVI   C2SW,C'Y'           SET TO USE 2ND CURRENCY                      
         CLI   3(R4),C'Y'                                                       
         BE    CKPW9                                                            
         B     FLTERR                                                           
*                                                                               
CKPW9    DS    0H                                                               
         EJECT                                                                  
CHKCAR   DS    0H            CHECK FOR CARAT COS2 (CLIENT)                      
*                                                                               
*        NOTE- JUST LIKE PW                                                     
*                                                                               
         GOTO1 USER1,DMCB,(64,SINIFLT),(5,=C'COS2=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CKCAR9                                                           
         LA    RE,5                                                             
         L     R4,4(R1)                                                         
         MVI   C2SW,C'N'                                                        
         CLI   5(R4),C'N'                                                       
         BE    CKCAR9                                                           
         MVI   C2SW,C'Y'           SET TO USE 2ND CURRENCY                      
         CLI   5(R4),C'Y'                                                       
         BE    CKCAR9                                                           
         B     FLTERR                                                           
*                                                                               
CKCAR9   DS    0H                                                               
         EJECT                                                                  
CHKPOST  DS    0H                  POSTED OPTION                                
         MVI   POSTSW,C'B'         SET TO BOTH POSTED AND UNPOSTED              
         GOTO1 USER1,DMCB,(64,SINIFLT),(6,=C'POSTED')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKPOSTX                                                         
         MVI   POSTSW,C'N'                                                      
         L     R4,4(R1)                                                         
         CLI   7(R4),C'N'                                                       
         BE    *+8                                                              
         MVI   POSTSW,C'Y'                                                      
*                                                                               
CHKPOSTX DS    0H                                                               
         SPACE 3                                                                
CHKCOM   DS    0H                  OPTION FOR SEP COMMISSION BILLS              
         MVI   SCOMSW,C'B'         SET TO BOTH UFC AND REG                      
         GOTO1 USER1,DMCB,(64,SINIFLT),(3,=C'UFC')  (UPFRONT COM)               
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKCOMX                                                          
         MVI   SCOMSW,C'N'                                                      
         L     R4,4(R1)                                                         
         CLI   4(R4),C'N'                                                       
         BE    *+8                                                              
         MVI   SCOMSW,C'Y'                                                      
*                                                                               
CHKCOMX  DS    0H                                                               
         SPACE 3                                                                
CHKNET   DS    0H                  OPTION FOR SEP COMM NET BILLS                
         MVI   SNETSW,C'B'         SET TO BOTH NET AND REG                      
         GOTO1 USER1,DMCB,(64,SINIFLT),(3,=C'NET')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKNETX                                                          
         MVI   SNETSW,C'N'                                                      
         L     R4,4(R1)                                                         
         CLI   4(R4),C'N'                                                       
         BE    *+8                                                              
         MVI   SNETSW,C'Y'                                                      
*                                                                               
CHKNETX  DS    0H                                                               
         EJECT                                                                  
CHKRUN   DS    0H                  CHECK FOR RUN TYPE FILTER                    
         MVI   FBILRUN,0                                                        
         GOTO1 USER1,DMCB,(64,SINIFLT),(3,=C'RUN')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKRUNX                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(22),=C'SOON OR OV (OVERNIGHT)'                              
         LA    RE,4               NEEDED FOR ERROR PROCESSING                   
*                                 (LENGTH OF RUN=)                              
         MVI   FBILRUN,C'S'                                                     
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         LR    R6,R4                                                            
         CLC   0(4,R4),=C'SOON'                                                 
         BE    CHKRUN5                                                          
         MVI   FBILRUN,C'O'       OVERNIGHT                                     
         CLC   0(2,R4),=C'OV'     JUST CHECK FIRST 2 CHARS                      
         BE    CHKRUN5                                                          
         MVI   FBILRUN,0           NO VALID FILTER ENTERED                      
         B     FLTERR                                                           
*                                                                               
CHKRUN5  DS    0H                                                               
*                                                                               
*                                                                               
CHKRUNX  DS    0H                                                               
         EJECT                                                                  
*                                                                               
CHKTOT   DS    0H                                                               
         MVI   TOTSW,0             DEFAULT IS NORMAL DISPLAY                    
         GOTO1 USER1,DMCB,(64,SINIFLT),(6,=C'TOTAL=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKSUB                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(9),=C'YES OR NO'                                            
         LA    RE,6                                                             
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         LR    R6,R4                                                            
         CLI   0(R4),C'N'                                                       
         BE    CHKTOTX                                                          
         CLI   0(R4),C'Y'                                                       
         BNE   FLTERR                                                           
         MVI   TOTSW,1             SHOW TOTAL LINE ONLY                         
CHKTOTX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
CHKSUB   DS    0H                  NETWORK SUB-MEDIA FILTER                     
         MVI   SUBMED,0            DEFAULT IS NORMAL DISPLAY                    
         GOTO1 USER1,DMCB,(64,SINIFLT),(7,=C'SUBMED=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    SETSCRN                                                          
         LA    RE,7                                                             
         L     R4,4(R1)                                                         
         LA    R4,7(R4)                                                         
         LR    R6,R4                                                            
         XC    WORK,WORK                                                        
         MVC   WORK(15),=C'ONLY FOR NETPAK'                                     
         CLI   SVOVSYS,X'03'     MUST BE NETPAK                                 
         BNE   FLTERR                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(11),=C'C,D,N,O,S,V'                                         
         CLI   0(R4),C'C'      CABLE                                            
         BE    CKSUB5                                                           
         CLI   0(R4),C'D'      RADIO NETWORK                                    
         BE    CKSUB5                                                           
         CLI   0(R4),C'S'      SYNDICATION                                      
         BE    CKSUB5                                                           
         CLI   0(R4),C'O'      OTHER                                            
         BE    CKSUB5                                                           
         CLI   0(R4),C'V'      VOD                                              
         BE    CKSUB5                                                           
         CLI   0(R4),C'N'      PLAIN NETWORK                                    
         BNE   FLTERR                                                           
*                                                                               
CKSUB5   MVC   SUBMED,0(R4)                                                     
CHKSUBX  DS    0H                                                               
*                                                                               
SETSCRN  LA    R4,SINHDR                                                        
         USING BILSCRND,R4                                                      
         LA    R2,SINHDRH                                                       
         CLI   SVOVSYS,3            CHECK FOR NET                               
         BNE   *+12                                                             
         BAS   RE,SETNSCRN          SET SCREEN FOR NETWORK                      
         B     STSCRN20                                                         
*                                                                               
         MVC   BSMOS(4),=C'MNTH'                                                
         MVC   BSBDATE+1(4),=C'BILL'                                            
         MVC   BSINV-1(7),=C'INVOICE'                                           
         MVC   BSBTYPE(4),=C'BILL'                                              
         MVC   BSBAMT+5(5),=C'GROSS'                                            
         MVC   BSNAMT+7(3),=C'NET'                                              
         MVC   BSAAMT+4(6),=C'ACTUAL'                                           
*                                                                               
**NO-OP  CLI   FBILRUN,C'S'        SOON BILLS ONLY                              
**NO-OP  BNE   SETS5          NO-OP TO SHOW USER (UNLESS SUPPRESSING)           
         CLI   NOUSER,C'Y'         SUPPRESSING USER?                            
         BE    SETS5                                                            
         B     SETS5X                                                           
*                                                                               
SETS5    CLI   LMGFLAG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   BSLMGREG(6),=C'BILLED'                                           
*                                                                               
SETS5X   FOUT  (R2)                                                             
*                                                                               
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   BSPRD,=C'PRD'                                                    
         MVC   BSEST,=C'EST'                                                    
         MVC   BSMOS,=C'SERV'                                                   
         MVC   BSBDATE+1(4),=C'DATE'                                            
         MVC   BSINV,=C'NUMBER '                                                
         MVC   BSBTYPE(4),=C'TYPE'                                              
         MVC   BSBAMT+4(6),=C'AMOUNT'                                           
         MVC   BSNAMT+4(6),=C'AMOUNT'                                           
         MVC   BSAAMT+4(6),=C'AMOUNT'                                           
*                                                                               
*                                                                               
**NO-OP  CLI   FBILRUN,C'S'        SOON BILLS ONLY                              
**NO-OP  BNE   SETS8          NO-OP TO SHOW USER (UNLESS SUPPRESSING)           
         CLI   NOUSER,C'Y'         SUPPRESSING USER?                            
         BE    SETS8                                                            
         MVC   BSLMGREG(7),=C'USER ID'                                          
         B     SETS8X                                                           
*                                                                               
SETS8    CLI   LMGFLAG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   BSLMGREG(3),=C'TO:'                                              
*                                                                               
SETS8X   FOUT  (R2)                                                             
*                                                                               
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   BSPRD,DASH                                                       
         MVC   BSEST,DASH                                                       
         MVC   BSMOS,DASH                                                       
         MVC   BSBDATE,DASH                                                     
         MVC   BSINV,DASH                                                       
         MVC   BSBTYPE(4),DASH                                                  
         MVC   BSBAMT+4(6),DASH                                                 
         MVC   BSNAMT+4(6),DASH                                                 
         MVC   BSAAMT+4(6),DASH                                                 
*                                                                               
*                                                                               
**NO-OP  CLI   FBILRUN,C'S'        SOON BILLS ONLY                              
**NO-OP  BNE   SETS9          NO-OP TO SHOW USER (UNLESS SUPPRESSING)           
         CLI   NOUSER,C'Y'         SUPPRESSING USER?                            
         BE    SETS9                                                            
         MVC   BSLMGREG(7),DASH                                                 
         B     SETS9X                                                           
*                                                                               
SETS9    CLI   LMGFLAG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   BSLMGREG(8),DASH                                                 
*                                                                               
SETS9X   FOUT  (R2)                                                             
*                                                                               
STSCRN20 LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         LA    R4,LINLEN(R4)                                                    
         LA    R9,12           SO TOTAL LINE CAN DISPLAY IF                     
*                              SCREEN GETS FULL                                 
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING BILLRECD,R5                                                      
         MVC   BKEYAM,SVAGYMD                                                   
         MVC   BKEYCLT,SVCLT                                                    
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    *+10                                                             
         MVC   BKEYPRD,SVEBCPRD                                                 
         OC    PREVKEY,PREVKEY                                                  
         BNZ   *+8                                                              
         BAS   RE,BTOTCLR                                                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
*                                                                               
GBHIGH   GOTO1 HIGH                                                             
         B     HAVREC                                                           
*                                                                               
GBSEQ    GOTO1 SEQ                                                              
HAVREC   LA    R5,KEY                                                           
         USING BILLRECD,R5                                                      
         CLC   KEY(4),KEYSAVE      CHECK A/M/C                                  
         BNE   BEND                                                             
         CLI   BKEYYSRV,0                                                       
         BE    GBSEQ                                                            
         CLC   SVEBCPRD,=C'ALL'    CHECK PRODUCT                                
         BE    PRDOK                                                            
****     CLC   SVEBCPRD,=C'POL'                                                 
****     BE    PRDOK                                                            
         CLC   BKEYPRD,SVEBCPRD                                                 
         BNE   BEND                                                             
PRDOK    CLI   EST1,0              CHECK ESTIMATE                               
         BE    ESTOK                                                            
         CLC   BKEYEST,EST1                                                     
         BNL   CEEND                                                            
         MVC   BKEYEST,EST1                                                     
         XC    BKEYYSRV(5),BKEYYSRV                                             
         B     GBHIGH                                                           
CEEND    CLC   BKEYEST,EST2                                                     
         BNH   ESTOK                                                            
         MVC   BKEYEST(6),HIGHK                                                 
         B     GBHIGH                                                           
ESTOK    CLI   MOS,0               CHECK MONTH OF SERVICE                       
         BE    MOSOK                                                            
         CLC   MOS,BKEYYSRV                                                     
         BNE   GBSEQ                                                            
*                                                                               
MOSOK    DS    0H                                                               
*                                                                               
* FILTERING BY BILL NUMBER TAKES PLACE AFTER GETREC                             
*&&DO                                                                           
         CLI   BNCNTRL,0           CHECK BILL NUMBER                            
         BE    BNOOK                                                            
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),BKEYMBIL                                               
         NI    HALF+1,X'0F'                                                     
*                                                                               
         CLI   B1XPROF+4,80        INVOICE NO. BASE YEAR? (1980+)               
         BL    MOSOK4                                                           
         ZIC   RF,BKEYMBIL                                                      
         SRL   RF,4                SHIFT OUT MONTH                              
         LA    R0,9                YEARS 0 THRU 8                               
         LA    RE,90               ARE 1990'S                                   
         CR    RF,R0                                                            
         BNH   *+8                                                              
         LA    RE,80               9 THRU 9 ARE 1980'S                          
         AR    RF,RE                                                            
         ZIC   RE,B1XPROF+4        LESS BASE YEAR                               
         SR    RF,RE                                                            
         MHI   RF,12               DIFFERENCE TIMES 12                          
         AH    RF,HALF             PLUS INVOICE MONTH                           
         STH   RF,HALF                                                          
*                                                                               
MOSOK4   DS    0H                                                               
         LHI   R8,10000                                                         
         MH    R8,HALF                                                          
         MVC   HALF,BKEYINV                                                     
         AH    R8,HALF                                                          
         ST    R8,FULL                                                          
         CLC   FULL,BNNO                                                        
         IC    R8,BNCNTRL                                                       
         STC   R8,*+5              SET COMPARE CONDITION                        
         BC    0,GBSEQ                                                          
*&&                                                                             
*                                                                               
BNOOK    CLI   FBILMO,0                                                         
         BE    BMOOK                                                            
         CLI   FBILMO2,0                                                        
         BE    BMOEQ                                                            
         CLC   BKEYMBIL,FBILMO                                                  
         BL    GBSEQ                                                            
         CLC   BKEYMBIL,FBILMO2                                                 
         BH    GBSEQ                                                            
         B     BMOOK                                                            
BMOEQ    CLC   BKEYMBIL,FBILMO                                                  
         BNE   GBSEQ                                                            
BMOOK    DS    0H                                                               
         MVC   FBDSK,KEY+14                                                     
         L     R5,AREC                                                          
         GOTO1 GETREC              GET A RECORD                                 
*                                                                               
         CLI   BNCNTRL,0           CHECK BILL NUMBER                            
         BE    BNOOK2                                                           
         BRAS  RE,FORMINVN                                                      
         CLC   FILT,INVN                                                        
         BNE   GBSEQ                                                            
BNOOK2   DS    0H                                                               
*                                                                               
         OC    BILLMO,BILLMO       ANY BILLMO FILTER?                           
         BZ    *+14                 NO                                          
         CLC   BILLMO,BDATE         YES - MAKE SURE FULL YEAR MATCHES           
         BNE   GBSEQ                                                            
*                                                                               
         OC    FBILTYP,FBILTYP     CHECK BILL TYPE                              
         BZ    BTYPOK                                                           
         CLI   FBILTYP,X'FF'       SPECIAL RETAIL                               
         BE    BTY4                                                             
         CLI   FBILTYP,X'FE'       SPECIAL AOR                                  
         BE    BTY3                                                             
         CLI   FBILTYP,X'FD'       EXCHANGE                                     
         BE    BTY5                                                             
         CLI   FBILTYP,X'FC'       MANUAL                                       
         BE    BTY6                                                             
         CLI   FBILTYP,X'FB'       GRP MIDAS TRADE                              
         BE    BTY7                                                             
*                                                                               
**NEW 2/8/90                                                                    
         TM    FBILNEG,X'01'                                                    
         BNO   BTY2                                                             
         CLC   BTYPE,FBILTYP        'ALL BUT' FILTER                            
         BE    GBSEQ                                                            
         B     BTYPOK                                                           
**NEW 2/8/90                                                                    
BTY2     CLC   BTYPE,FBILTYP                                                    
         BNE   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY3     DS    0H                  AOR                                          
         CLI   FBILTYP+1,X'00'     AOR=YES                                      
         BNE   BTY3D                                                            
**NEW 2/8/90                                                                    
         TM    FBILNEG,X'01'                                                    
         BNO   BTY3B                                                            
         TM    BILSTAT,X'20'       'ALL BUT' AOR                                
         BO    GBSEQ                                                            
         B     BTYPOK                                                           
**NEW 2/8/90                                                                    
BTY3B    TM    BILSTAT,X'20'       MUST BE AOR BILL                             
         BZ    GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY3D    DS    0H                                                               
**NEW 2/8/90                                                                    
*                                 LEAVE OLD LOGIC FOR SKIPPING AOR              
*                                 WHICH WAS 'REG' IN BILLTYPE                   
         TM    FBILNEG,X'01'       ALLOW FOR -REG - SAME AS AOR                 
         BO    BTY3B                                                            
**NEW 2/8/90                                                                    
         TM    BILSTAT,X'20'       ELSE MUST NOT BE AOR                         
         BNZ   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY4     DS    0H                  RETAIL                                       
**NEW 2/8/90                                                                    
         TM    FBILNEG,X'01'        CHK FOR 'ALL BUT'                           
         BNO   BTY4D                                                            
         CLC   FBILTYP+1(1),BRETAIL                                             
         BE    GBSEQ                SKIP RETAIL                                 
         B     BTYPOK                                                           
**NEW 2/8/90                                                                    
*                                                                               
BTY4D    CLC   FBILTYP+1(1),BRETAIL                                             
         BNE   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY5     DS    0H                  EXCHANGE                                     
         TM    FBILNEG,X'01'        CHK FOR 'ALL BUT'                           
         BNO   BTY5D                                                            
         TM    BILSTAT3,BSTTRCNQ    (X'02')                                     
         BO    GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY5D    TM    BILSTAT3,BSTTRCNQ     (X'02')                                    
         BNO   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY6     DS    0H                   MANUAL                                      
         TM    FBILNEG,X'01'        CHK FOR 'ALL BUT'                           
         BNO   BTY6D                                                            
         TM    BILSTAT,X'40'                                                    
         BO    GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY6D    TM    BILSTAT,X'40'                                                    
         BNO   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY7     DS    0H                  GRP M MIDAS TRADE                            
         TM    FBILNEG,X'01'        CHK FOR 'ALL BUT'                           
         BNO   BTY7D                                                            
         TM    BILSTAT3,X'01'       BSTMBARQ                                    
         BO    GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTY7D    TM    BILSTAT3,X'01'       BSTMBARQ                                    
         BNO   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BTYPOK   DS    0H                                                               
*                                                                               
CKRUN    CLI   FBILRUN,0          RUN TYPE ENTERED?                             
         BE    RUNTOK                                                           
         CLI   FBILRUN,C'S'       SOON BILLS ONLY?                              
         BNE   CKRUN5                                                           
         TM    BILSTAT3,BSTSOONQ                                                
         BO    RUNTOK                                                           
         B     GBSEQ                                                            
*                                                                               
CKRUN5   CLI   FBILRUN,C'O'       OVERNIGHT ONLY?                               
         BNE   RUNTOK                                                           
         TM    BILSTAT3,BSTSOONQ  SKIP SOON BILLS                               
         BO    GBSEQ                                                            
*                                                                               
RUNTOK   DS    0H                                                               
*                                                                               
         CLI   FBILDATE,0          CHECK BILL DATE                              
         BE    BDDOK                                                            
         CLI   FBILDAT2,0                                                       
         BNE   BDRANGE                                                          
         CLC   FBILDATE,BDATE                                                   
         BNE   GBSEQ                                                            
         B     BDDOK                                                            
BDRANGE  CLC   BDATE,FBILDATE                                                   
         BL    GBSEQ                                                            
         CLC   BDATE,FBILDAT2                                                   
         BH    GBSEQ                                                            
*                                                                               
BDDOK    DS    0H                                                               
         CLI   FDIST,0             RETAIL DISTRIBUTOR                           
         BE    BDISTOK                                                          
         CLI   BRETAIL,0           RETAIL BILL?                                 
         BE    GBSEQ               NO: SKIP THIS BILL                           
         ZIC   RE,FDISTDSP                                                      
         LA    RE,BRETACCT(RE)     START OF COMPARE                             
         ZIC   RF,FDISTLEN         LENGTH TO CHECK                              
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FDIST(0),0(RE)                                                   
         BNE   GBSEQ                                                            
*                                                                               
BDISTOK  DS    0H                                                               
         CLI   POSTSW,C'N'         POSTED FILTER                                
         BNE   BPST2                                                            
         OC    BILPOST,BILPOST                                                  
         BZ    BPSTOK                                                           
         B     GBSEQ                                                            
BPST2    DS    0H                                                               
         CLI   POSTSW,C'Y'                                                      
         BNE   BPSTOK                                                           
         OC    BILPOST,BILPOST                                                  
         BNZ   BPSTOK                                                           
         B     GBSEQ                                                            
*                                                                               
BPSTOK   DS    0H                                                               
         CLI   SCOMSW,C'N'        SEP COMMISSION FILTER                         
         BNE   BSCOM2                                                           
         TM    BILSTAT,BSTSCOMQ                                                 
         BZ    BSCOMOK                                                          
         B     GBSEQ                                                            
BSCOM2   DS    0H                                                               
         CLI   SCOMSW,C'Y'                                                      
         BNE   BSCOMOK                                                          
         TM    BILSTAT,BSTSCOMQ                                                 
         BZ    GBSEQ                                                            
         TM    BILSTAT,BSTTAORQ    SKIP TRUE AOR BILLS EVEN                     
         BZ    BSCOMOK             SEPARATE COMMISSION                          
         B     GBSEQ                                                            
BSCOMOK  DS    0H                                                               
         CLI   SNETSW,C'N'        SEP COMMISSION/NET FILTER                     
         BNE   BSNET2                                                           
         TM    BILSTAT,BSTSNETQ                                                 
         BZ    BSNETOK                                                          
         B     GBSEQ                                                            
BSNET2   DS    0H                                                               
         CLI   SNETSW,C'Y'                                                      
         BNE   BSNETOK                                                          
         TM    BILSTAT,BSTSNETQ                                                 
         BZ    GBSEQ                                                            
BSNETOK  DS    0H                                                               
*                                                                               
         CLI   SUBMED,0         NET SUB-MEDIA FILTER ENTERED?                   
         BE    BSUBMOK                                                          
         CLI   SUBMED,C'N'                                                      
         BNE   BSUBM5                                                           
         CLI   BLMED,X'40'                                                      
         BH    BSUBM5                                                           
         B     BSUBMOK              IF NOT PRESENT - PROCESS                    
*                                                                               
BSUBM5   CLC   BLMED,SUBMED         MATCH TYPES                                 
         BNE   GBSEQ                                                            
         B     BSUBMOK                                                          
*                                                                               
BSUBMOK  DS    0H                                                               
         EJECT                                                                  
* END OF FILTERS SO DISPLAY RECORD                                              
BDOK     MVC   BSPRD,BKEYPRD                                                    
         SR    RF,RF                                                            
         IC    RF,BKEYEST                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BSEST,DUB+6(2)                                                   
*                                                                               
**Y2K**  MVC   BSMOS(4),BMONSERV                                                
         MVC   WORK(4),BMONSERV                                                 
         MVC   WORK+4(2),=C'01'    FAKE DAY FOR DATCON                          
         GOTO1 VDATCON,DMCB,WORK,(X'20',DUB)                                    
         MVC   BSMOS(4),DUB                                                     
*                                                                               
**Y2K**  MVC   BSBDATE,BDATE                                                    
         GOTO1 VDATCON,DMCB,BDATE,(X'20',BSBDATE)                               
*                                                                               
         SPACE 2                                                                
********************************************************************            
* USE CALL TO SPFMTINO TO DISPLAY INVOICE NUMBER INSTEAD OF DOING  *            
* IT HERE.                                                         *            
* EJOR 05JAN00                                                     *            
********************************************************************            
         SPACE 2                                                                
         GOTO1 =V(SPFMTINO),DMCB,BDATE,(6,BINVNO),(SVEBCMED,B1PROF),   X        
               B1XPROF,RR=Y                                                     
         L     R1,DMCB+4                                                        
*                                                                               
* FORMAT INV NIM AS MNNNNN, NOT MN-NNNN                                         
         MVC   BSINV(2),0(R1)                                                   
         MVC   BSINV+2(4),3(R1)                                                 
         SPACE 2                                                                
*&&DO                                                                           
         MVC   BSINV(6),BINVNO                                                  
*                                                                               
*             NOTE- FOR BILLS AFTER ROUGHLY MAY 1994, THE CORRECT               
*             MONTH WILL ALREADY BE IN BINVNO(2), BUT YOU CANT ALWAYS           
*             TELL AND IT DOESNS'T HURT TO RE-SET IT IN ANY CASE                
*                                                                               
         CLI   B1XPROF+14,0        INVNO WITH YM ACTIVE?                        
         BE    BDOK2               NO                                           
         CLC   BDATE(4),B1XYR      YES, IS BILL DATE OK?                        
         BNL   BDOK3               YES, BINVNO PROPERLY SET                     
*                                                                               
BDOK2    DS    0H                                                               
         CLI   B1XPROF+4,80        INVOICE NO. BASE YEAR (1980+)                
         BL    BDOK3               NO                                           
*                                                                               
**Y2K**  PACK  DUB,BDATE(2)        YEAR OF BILL                                 
**Y2K**  CVB   R0,DUB                                                           
         GOTO1 VDATCON,DMCB,BDATE,(3,DUB)                                       
         ZIC   R0,DUB                                                           
*                                                                               
         ZIC   RF,B1XPROF+4                                                     
         SR    R0,RF               LESS BASE YEAR                               
         BNP   BDOK3                                                            
         MHI   R0,12               DIFFERENCE TIMES 12                          
**Y2K**  PACK  DUB,BDATE+2(2)                                                   
**Y2K**  CVB   RF,DUB                                                           
         IC    RF,DUB+1                                                         
*                                                                               
         AR    R0,RF               PLUS THIS BILL MONTH                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BSINV(2),DUB                                                     
*&&                                                                             
*                                                                               
BDOK3    DS    0H                                                               
         SR    RF,RF               OLD STYLE BILLS                              
         IC    RF,BTYPE+1                                                       
         BCTR  RF,0                                                             
         MHI   RF,5                                                             
         LA    RF,TYPETAB2(RF)                                                  
         MVC   BSBTYPE,1(RF)                                                    
         CLI   BTYPE+1,4                                                        
         BNH   BDOK3F                                                           
         MVC   BSBTYPE,=C'    '    NEW STYLE BILLS                              
         MVC   BSBTYPE(2),BTYPE                                                 
*                                                                               
         TM    BILSTAT3,BSTTRCNQ SEE IF EXCHANGE BILL                           
         BNO   *+8                                                              
         MVI   BSBTYPE+2,C'X'                                                   
*                                                                               
         TM    BILSTAT3,X'01'     BSTMBARQ-GRP M TRADE MIDAS                    
         BNO   *+8                                                              
         MVI   BSBTYPE+2,C'T'                                                   
*                                                                               
         TM    BILSTAT,X'40'       TEST MANUAL BILL                             
         BZ    BDOK3E                                                           
         MVC   BSBTYPE(3),=C'MAN'                                               
         MVC   BSBTYPE+3(1),BTYPE+1                                             
BDOK3E   DS    0H                                                               
         TM    BILSTAT,X'20'       TEST AOR BILL                                
         BZ    BDOK3F                                                           
         MVC   BSBTYPE(3),=C'AOR'                                               
         MVC   BSBTYPE+3(1),BTYPE+1                                             
*                                                                               
         TM    BILSTAT3,BSTTRCNQ SEE IF EXCHANGE BILL                           
         BNO   *+8                                                              
         MVI   BSBTYPE+3,C'X'      OVERRIDES BTYPE=1 (4-7)                      
*                                                                               
         ZAP   BNETP,=P'0'         CLEAR NET BECAUSE USED TO HOLD               
*                                  ORIGINAL AGYCOM                              
BDOK3F   DS    0H                                                               
         TM    BILSTAT,BSTSCOMQ    TEST SEP COMM BILL                           
         BZ    BDOK3G                                                           
         MVC   BSBTYPE(3),=C'UFC'                                               
         MVC   BSBTYPE+3(1),BTYPE+1                                             
*                                                                               
BDOK3G   DS    0H                                                               
         TM    BILSTAT,BSTSNETQ    TEST SEP NET BILL                            
         BZ    BDOK3G2                                                          
         MVC   BSBTYPE(3),=C'NET'                                               
         MVC   BSBTYPE+3(1),BTYPE+1                                             
*                                                                               
BDOK3G2  DS    0H                                                               
         CLI   C2SW,C'Y'           DOING 2ND CURR?                              
         BE    BDOK3H                                                           
*                                                                               
         GOTO1 =V(SPBVAL),DMCB,(C'B',BILLREC),SPBVALD,0,RR=RB                   
*                                                                               
         ZAP   DGRS,SPBVGRSP       EFFECTIVE GROSS                              
*                                                                               
         TM    BILSTAT3,BSTTRCNQ SEE IF EXCHANGE BILL                           
         BNO   BDOK3G4                                                          
         L     R0,BINVSEQ+1    CALCULATED NET (WILL BE BCLDNET)                 
         CVD   R0,DUB                                                           
         ZAP   DNET,DUB                                                         
         B     BDOK3G5                                                          
*                                                                               
BDOK3G4  ZAP   DNET,SPBVNETP       AND NET                                      
BDOK3G5  ZAP   DACT,SPBVACTP       ACTUAL                                       
         L     R0,SPBVGST          PLUS GST                                     
         CVD   R0,DUB                                                           
         AP    DACT,DUB                                                         
         L     R0,SPBVPST          PLUS PST                                     
         CVD   R0,DUB                                                           
         AP    DACT,DUB                                                         
         B     BDOK4                                                            
*                                                                               
BDOK3H   DS    0H                  2ND CURRENCY                                 
         ZAP   DGRS,BGRS2P         USE GRS FOR 2ND CURR                         
         ZAP   DNET,BNET2P         USE NET FOR 2ND CURR                         
         ZAP   DACT,BACTP                                                       
*                                                                               
BDOK4    DS    0H                                                               
         CLI   SVOVSYS,3                                                        
         BNE   BOOK4A                                                           
*  NETWORK DISPLAY                                                              
         EDIT  DGRS,(13,NBSBAMT-1),2,MINUS=YES                                  
         EDIT  DNET,(13,NBSNAMT-1),2,MINUS=YES                                  
         EDIT  DACT,(13,NBSAAMT-1),2,MINUS=YES                                  
         B     BOOK4B                                                           
*                                                                               
BOOK4A   EDIT  DGRS,(12,BSBAMT-1),2,MINUS=YES                                   
         EDIT  DNET,(12,BSNAMT-1),2,MINUS=YES                                   
         EDIT  DACT,(12,BSAAMT-1),2,MINUS=YES                                   
*                                                                               
         CLI   LMGFLAG,C'Y'                                                     
         BNE   BOOK4B                                                           
*                                                                               
         TM    BILSTAT3,BSTREGQ                                                 
         BZ    *+10                                                             
         MVC   BSLMGREG(8),=C'REGIONAL'                                         
*                                                                               
         TM    BILSTAT3,BSTLMGQ                                                 
         BZ    *+10                                                             
         MVC   BSLMGREG(3),=C'LMG'                                              
*                                                                               
BOOK4B   CLI   BRETAIL,X'81'       IF CORP CONTROL SKIP FOR TOTALS              
         BNE   BDOK5                                                            
         CLI   FBILTYP+1,X'81'       UNLESS LISTING ONLY CONTROLS               
         BNE   BDOK5B                                                           
*                                                                               
BDOK5    DS    0H                  ADD TO TOTALS                                
         AP    TBAMT,DGRS                                                       
         AP    TNET,DNET                                                        
         AP    TACT,DACT                                                        
*                                                                               
BDOK5B   DS    0H                                                               
         CLI   BTYPE,C'B'                                                       
         BNE   BDOK6               OLD BILL                                     
         CLI   BRETAIL,0                                                        
         BE    BDOK9                                                            
         CLI   BRETAIL,X'40'       TREAT SPACE AS NULL                          
         BE    BDOK9                                                            
         B     BDOK8                                                            
BDOK6    DS    0H                                                               
         CLC   BRETACCT,=12C'0'                                                 
         BE    SENDB                                                            
         MVC   BSRINO(6),BRETACCT                                               
         B     SENDB                                                            
BDOK8    DS    0H                                                               
         MVI   BSBTYPE+3,C'S'                                                   
         CLI   BRETAIL,X'41'       SUMMARY                                      
         BE    BDOK8D                                                           
         MVI   BSBTYPE+3,C'C'                                                   
         CLI   BRETAIL,X'81'       CORP CONTROL                                 
         BE    BDOK8D                                                           
         MVI   BSBTYPE+3,C' '                                                   
BDOK8D   DS    0H                                                               
         MVC   BSRINO,BRETACCT                                                  
         B     BDOK9         OVERRIDE WITH USER (UNLESS SUPPRESSING)            
         GOTO1 =V(HEXOUT),DMCB,FBDSK,BSAAMT,4,0,8,RR=RB                         
BDOK9    DS    0H                                                               
*                                                                               
**NO-OP  CLI   FBILRUN,C'S'           SEE IF FILTERING ON SOON BILLS            
**NO-OP  BNE   BDOK32        NO-OP                                              
         CLI   NOUSER,C'Y'          SUPPRESSING USER ID?                        
         BE    BDOK32                                                           
         OC    BILLUID,BILLUID      SEE IF I HAVE A USERID                      
         BZ    BDOK32                                                           
*                                                                               
         CLI   SVOVSYS,3         SEE IF NETPAK                                  
         BE    *+10              NOTHING WILL BE (FOR NOW)                      
*                                                                               
         MVC   BSRINO,SPACES     REPLACE WHAT'S THERE WITH USER ID              
*                          MUST READ CONTROL FILE TO DISPLAY USERID             
         LA    RE,REC2     READ INTO REC2                                       
         LA    RF,400*5                                                         
         XCEF                                                                   
*                                                                               
         LA    R6,REC2                                                          
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,BILLUID    ID NUMBER                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R6),(R6)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
BDOK31A  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
*                                                                               
         DROP  R6                                                               
*                                                                               
         CLI   0(RE),X'02'         USERID ALPHA ELEMENT (X'02')                 
         BE    BDOK31C                                                          
         ZIC   R1,1(RE)            NEXT ELEMENT                                 
         AR    RE,R1                                                            
         B     BDOK31A                                                          
*                                                                               
BDOK31C  CLI   SVOVSYS,3           SEE IF NETPAK                                
         BNE   BDOK31D                                                          
         MVC   NBSUSER(8),2(RE)    PUT ID IN NBSUSER INSTEAD                    
         B     BDOK32                                                           
*                                                                               
BDOK31D  MVC   BSRINO(10),2(RE)                                                 
*                                                                               
BDOK32   DS    0H                                                               
SENDB    DS    0H                                                               
         CLI   TOTSW,0                                                          
         BE    SENDB5                                                           
         XC    0(80,R4),0(R4)      SHOWING TOTAL LINE ONLY                      
         B     GBSEQ               CONTINUE READING BILLS                       
*                                                                               
SENDB5   FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
*                                                                               
*        OC    EST1(BFLTRLN),EST1  NOOP THIS CLEARING OF                        
*        BNZ   *+8                 TOTALS, HOW COULD IT POSSIBLY                
*        BAS   RE,BTOTCLR          BE RIGHT?                                    
*                                                                               
         BCT   R9,GBSEQ                                                         
         GOTO1 SEQ                                                              
         CLC   KEY(4),KEYSAVE                                                   
         BNE   BEND                                                             
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
*                                                                               
BEND     DS    0H                                                               
         CLI   TOTSW,1             IF SHOWING ONLY TOTAL                        
         BE    BEND2               ALWAYS SHOW SOMETHING                        
         CP    TBAMT,=P'0'                                                      
         BNE   BEND2                                                            
         CP    TNET,=P'0'                                                       
         BNE   BEND2                                                            
         CP    TACT,=P'0'                                                       
         BE    MODEXIT                                                          
BEND2    DS    0H                                                               
         MVC   BSMOS(6),=C'*TOTAL'                                              
         CLI   SVOVSYS,3                                                        
         BNE   BEND2A                                                           
         EDIT  (P6,TBAMT),(13,NBSBAMT-1),2,MINUS=YES                            
         EDIT  (P6,TNET),(13,NBSNAMT-1),2,MINUS=YES                             
         EDIT  (P6,TACT),(13,NBSAAMT-1),2,MINUS=YES                             
         B     BEND2B                                                           
*                                                                               
BEND2A   EDIT  (P6,TBAMT),(12,BSBAMT-1),2,MINUS=YES                             
         EDIT  (P6,TNET),(12,BSNAMT-1),2,MINUS=YES                              
         EDIT  (P6,TACT),(12,BSAAMT-1),2,MINUS=YES                              
BEND2B   FOUT  (R2)                                                             
         BAS   RE,BTOTCLR                                                       
         XC    PREVKEY,PREVKEY                                                  
         B     MODEXIT                                                          
         EJECT                                                                  
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)                                                  
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    RF,23(RF)                                                        
         MVC   0(22,RF),WORK                                                    
         LA    R2,SINIFLTH                                                      
         MVI   ERRCD,X'FF'                                                      
         MVI   ERRAREA,X'FF'                                                    
         B     MODEXIT2                                                         
*                                                                               
GETNUM   LA    R5,0                                                             
GN1      CLI   0(R4),C'-'                                                       
         BER   R9                                                               
         CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),X'00'                                                      
         BER   R9                                                               
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'0'                                                       
         BL    GNERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GNERR                                                            
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GN1                                                              
GNERR    SR    R5,R5                                                            
         BR    R9                                                               
         SPACE 2                                                                
*                                                                               
*  SET HEADERS FOR NETWORK                                                      
*                                                                               
SETNSCRN LA    R4,SINHDR                                                        
         USING BILSCRND,R4                                                      
*                                                                               
         ST    RE,SAVERE                                                        
         MVC   BSMOS(4),=C'MNTH'                                                
         MVC   BSBDATE+1(4),=C'BILL'                                            
         MVC   BSINV-1(7),=C'INVOICE'                                           
         MVC   BSBTYPE(4),=C'BILL'                                              
         MVC   NBSBAMT+6(5),=C'GROSS'                                           
         MVC   NBSNAMT+8(3),=C'NET'                                             
         MVC   NBSAAMT+5(6),=C'ACTUAL'                                          
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   BSPRD,=C'PRD'                                                    
         MVC   BSEST,=C'EST'                                                    
         MVC   BSMOS,=C'SERV'                                                   
         MVC   BSBDATE+1(4),=C'DATE'                                            
         MVC   BSINV,=C'NUMBER '                                                
         MVC   BSBTYPE(4),=C'TYPE'                                              
         MVC   NBSBAMT+5(6),=C'AMOUNT'                                          
         MVC   NBSNAMT+5(6),=C'AMOUNT'                                          
         MVC   NBSAAMT+5(6),=C'AMOUNT'                                          
*                                                                               
**NO-OP  CLI   FBILRUN,C'S'        SOON BILLS ONLY                              
**NO-OP  BNE   SETNS8X       NO-OP TO SHOW USER (UNLESS SUPPRESSING)            
         CLI   NOUSER,C'Y'         SUPPRESSING USER?                            
         BE    SETNS8X                                                          
         MVC   NBSUSER(7),=C'USER ID'                                           
         B     SETNS8X                                                          
*                                                                               
SETNS8X  FOUT  (R2)                                                             
*                                                                               
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   BSPRD,DASH                                                       
         MVC   BSEST,DASH                                                       
         MVC   BSMOS,DASH                                                       
         MVC   BSBDATE,DASH                                                     
         MVC   BSINV,DASH                                                       
         MVC   BSBTYPE(4),DASH                                                  
         MVC   NBSBAMT+5(6),DASH                                                
         MVC   NBSNAMT+5(6),DASH                                                
         MVC   NBSAAMT+5(6),DASH                                                
*                                                                               
**NO-OP  CLI   FBILRUN,C'S'        SOON BILLS ONLY                              
**NO-OP  BNE   SETNS9X        NO-OP TO SHOW USER (UNLESS SUPPRESSING)           
         CLI   NOUSER,C'Y'         SUPPRESSING USER?                            
         BE    SETNS9X                                                          
         MVC   NBSUSER(7),DASH                                                  
         B     SETNS9X                                                          
*                                                                               
SETNS9X  FOUT  (R2)                                                             
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
BTOTCLR  DS    0H                                                               
         ZAP   TBAMT,=P'0'                                                      
         ZAP   TNET,=P'0'                                                       
         ZAP   TACT,=P'0'                                                       
         BR    RE                                                               
         EJECT                                                                  
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT2 OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*                                  BILL TYPE TABLE                              
TYPETAB  DC    C'4',C'B4'                                                       
         DC    C'5',C'B5'                                                       
         DC    C'6',C'B6'                                                       
         DC    C'7',C'B7'                                                       
*        DC    C'M',X'0001'        OLD STYLE BILL TYPES NO-OPED                 
*        DC    C'O',X'0003'                                                     
*        DC    C'A',X'0002'        A NOW AOR                                    
*        DC    C'D',X'0004'                                                     
         DC    C'S',X'FF41'        RETAIL SUMMARY                               
         DC    C'C',X'FF81'        RETAIL CONTROL                               
         DC    C'A',X'FE00'        AOR                                          
         DC    C'R',X'FE01'        REG - NON-AOR                                
         DC    C'T',X'FB02'        GRP M MIDAS                                  
         DC    C'X',X'FD01'        EXCHANGE                                     
         DC    C'M',X'FC40'        MANUAL                                       
         DC    X'0000'                                                          
DASH     DC    40C'-'                                                           
HIGHK    DC    10X'FF'                                                          
TYPETAB2 DC    X'01',C'MAN '       OLD STYLE BILL TYPES                         
         DC    X'02',C'AUTH'                                                    
         DC    X'03',C'ORG '                                                    
         DC    X'04',C'DET '                                                    
         DC    X'00'                                                            
*                                                                               
LINLEN   EQU   88                                                               
*                                                                               
FORMINVN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 =V(SPFMTINO),DMCB,BDATE,(6,BINVNO),(SVEBCMED,B1PROF),   X        
               B1XPROF,RR=Y                                                     
         L     R1,DMCB+4                                                        
*                                                                               
         MVC   INVN(2),0(R1)                                                    
         MVC   INVN+2(4),3(R1)                                                  
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
BILLWRKD DSECT                                                                  
EST1     DS    C                   ESTIMATE FILTER 1                            
EST2     DS    C                   ESTIMATE FILTER 2                            
MOS      DS    CL2                 MONTH OF SERVICE FILTER                      
FBILMO   DS    CL1                 BILLING MONTH FILTER                         
BILLMO   DS    CL4                 ACTUAL BILLMO FILT MOS                       
FBILNO   DS    0CL4                BILL NUMBER FILTER                           
BNCNTRL  DS    CL1                  COMPARE CONTROL                             
BNNO     DS    F                   BILL NO MONTH                                
SAVERE   DS    F                                                                
FBILMO2  DS    CL1               BILL MONTH END                                 
FBILNEG  DS    CL1               X'01' IF 'ALL BUT' BILLTYPE FILTER             
FBILTYP  DS    CL2               BILL TYPE FILTER                               
FBILDATE DS    CL6               BILL DATE FILTER                               
FBILDAT2 DS    CL6                                                              
FBILRUN  DS    CL1               RUN FILTER S=SOON O=OVERNIGHT                  
FDIST    DS    CL12                                                             
FDISTLEN DS    X                                                                
FDISTDSP DS    X                                                                
FBDSK    DS    CL4                                                              
CURRNCY  DS    CL1                                                              
C2SW     DS    CL1                                                              
TOTSW    DS    XL1               X'01' - SHOW ONLY TOTAL                        
SUBMED   DS    C                   NET SUB-MEDIA FILTER                         
POSTSW   DS    C                                                                
SCOMSW   DS    C                                                                
SNETSW   DS    C                                                                
BFLTRLN  EQU   FDISTLEN-EST1                                                    
*                                                                               
DGRS     DS    PL6                 GROSS TO BE DISPLAYED                        
DNET     DS    PL6                 NET                                          
DACT     DS    PL6                 ACTUAL                                       
*                                                                               
WAGYCUR  DS    CL1                                                              
WCLTCUR  DS    CL1                                                              
*                                                                               
B1PROF   DS    XL16                                                             
B1XPROF  DS    XL16                                                             
PROFB9   DS    XL16                                                             
B1XYR    DS    CL2                                                              
B1XMM    DS    CL2                                                              
*                                                                               
FILT     DS    CL6                                                              
INVN     DS    CL6                                                              
*                                                                               
LMGFLAG  DS    C                                                                
NOUSER   DS    C       Y = NO USER ID FOR SOON BILL'S ONLY                      
*                                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
BILLWRKL EQU   *-BILLWRKD                                                       
*                                                                               
         EJECT                                                                  
BILSCRND DSECT                                                                  
BSPRD    DS    CL3                                                              
         DS    CL1                                                              
BSEST    DS    CL3                                                              
         DS    CL1                                                              
BSMOS    DS    CL4                                                              
         DS    CL1                                                              
BSBDATE  DS    CL6                                                              
         DS    CL1                                                              
BSINV    DS    CL6                                                              
         DS    CL1                                                              
BSBTYPE  DS    CL4                                                              
         DS    CL1                                                              
BSBAMT   DS    CL11                                                             
         DS    CL1                                                              
BSNAMT   DS    CL11                                                             
         DS    CL1                                                              
BSAAMT   DS    CL11                                                             
         DS    CL1                                                              
BSRINO   DS    CL12                                                             
         ORG   BSRINO                                                           
BSLMGREG DS    CL12                                                             
         ORG   BSBAMT                                                           
*  NETWORK LAYOUT                                                               
NBSBAMT  DS    CL12                                                             
         DS    CL1                                                              
NBSNAMT  DS    CL12                                                             
         DS    CL1                                                              
NBSAAMT  DS    CL12                                                             
         DS    CL1                                                              
NBSUSER  DS    CL8                                                              
*                                                                               
         EJECT                                                                  
*SPINFWORK                                                                      
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
*                                                                               
         ORG   T21AFFD+16                                                       
         DS    0F                                                               
BTOTAL   DS    0CL18                                                            
TBAMT    DS    PL6                                                              
TNET     DS    PL6                                                              
TACT     DS    PL6                                                              
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SPINF18   07/23/13'                                      
         END                                                                    
