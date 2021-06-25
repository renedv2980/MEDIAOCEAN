*          DATA SET ACINQ0B    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T6060BA,*,NOAUTO                                                         
*INCLUDE ACJOBCOL                                                               
         TITLE 'ACCOUNT ENQUIRY MK2 - JOB ESTIMATES - T6060B'                   
T6060B   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'ES' IN ACCOUNT ENQUIRY PROGRAM                  
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T6060B)                                               
         DC    A(FILTABLE-T6060B)                                               
         DC    A(KNTRYPNT-T6060B)                                               
         DC    A(FNTRYPNT-T6060B)                                               
         DC    A(DNTRYPNT-T6060B)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODUNIT-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODLEDG-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'BILLED'                                                     
         DC    CL2'BI'                                                          
         DC    X'00'                                                            
         DC    CL8'ESTIMATE'                                                    
         DC    X'01'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(ACBLCR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'BILLED'                                                     
         DC    CL2'BI'                                                          
         DC    X'00'                                                            
         DC    CL8'OVEREST'                                                     
         DC    X'02'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(ACBLCR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'BILLED'                                                     
         DC    CL2'BI'                                                          
         DC    X'00'                                                            
         DC    CL8'UNDEREST'                                                    
         DC    X'04'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(ACBLCR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'CHARGES'                                                    
         DC    CL2'CH'                                                          
         DC    X'00'                                                            
         DC    CL8'ESTIMATE'                                                    
         DC    X'01'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(ACBLDR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'CHARGES'                                                    
         DC    CL2'CH'                                                          
         DC    X'00'                                                            
         DC    CL8'OVEREST'                                                     
         DC    X'02'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(ACBLDR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'CHARGES'                                                    
         DC    CL2'CH'                                                          
         DC    X'00'                                                            
         DC    CL8'UNDEREST'                                                    
         DC    X'04'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(ACBLDR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
**       DC    CL10'CHARGES'                                                    
**       DC    CL2'CH'                                                          
**       DC    X'00'                                                            
**       DC    CL8'%EST'                                                        
**       DC    X'01'                                                            
**       DC    AL2(ABAL-GWS)                                                    
**       DC    AL1(ACBLDR-ACBALD)                                               
**       DC    AL1(1)                                                           
**       DC    AL2(EDITOPT1-GWS)                                                
**       DC    X'FF00'                                                          
*                                                                               
**       DC    CL10'CHARGES'                                                    
**       DC    CL2'CH'                                                          
**       DC    X'00'                                                            
**       DC    CL8'%OVEREST'                                                    
**       DC    X'02'                                                            
**       DC    AL2(ABAL-GWS)                                                    
**       DC    AL1(ACBLDR-ACBALD)                                               
**       DC    AL1(1)                                                           
**       DC    AL2(EDITOPT1-GWS)                                                
**       DC    X'FF00'                                                          
*                                                                               
**       DC    CL10'CHARGES'                                                    
**       DC    CL2'CH'                                                          
**       DC    X'00'                                                            
**       DC    CL8'%UNDER'                                                      
**       DC    X'04'                                                            
**       DC    AL2(ABAL-GWS)                                                    
**       DC    AL1(ACBLDR-ACBALD)                                               
**       DC    AL1(1)                                                           
**       DC    AL2(EDITOPT1-GWS)                                                
**       DC    X'FF00'                                                          
*                                                                               
         DC    CL10'ESTIMATE'                                                   
         DC    CL2'ES'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'08'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
MEDIA    DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'             TO FIX DISPLACEMENT INTO ELEMENT             
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'SHOW'                                                       
         DC    CL2'SH'                                                          
         DC    X'00'                                                            
         DC    CL8'BALANCE'                                                     
         DC    X'40'                                                            
         DC    AL2(0)                                                           
         DC    CL6' '                                                           
*                                                                               
         DC    CL10'SHOW'                                                       
         DC    CL2'SH'                                                          
         DC    X'00'                                                            
         DC    CL8'UNBILLED'                                                    
         DC    X'80'                                                            
         DC    AL2(0)                                                           
         DC    CL6' '                                                           
*                                                                               
         DC    CL10'UNBILLED'                                                   
         DC    CL2'UN'                                                          
         DC    X'00'                                                            
         DC    CL8'ZERO'                                                        
         DC    X'10'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'UNBILLED'                                                   
         DC    CL2'UN'                                                          
         DC    X'02'                                                            
         DC    CL8'NONZERO'                                                     
         DC    X'10'                                                            
         DC    AL2(ABAL-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'OVERRIDE'                                                   
         DC    CL2'OV'                                                          
         DC    X'80'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(3)                                                           
         DC    AL2(EDITOVER-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'CLOSED'                                                     
         DC    CL2'CL'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'40'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTSTAT-ACSTATD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LOCKED'                                                     
         DC    CL2'LO'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'20'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTSTAT-ACSTATD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*                                  HANDLE SET-UP OF MEDIA FILTER ENTRY          
*                                  AND APPLICATION OF BILLED,CHARGES,           
*                                  ESTIMATE AND UNBILLED FILTERS                
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQB**,RR=RF                                                 
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060B,RB                                                        
         USING T606TWA,RA                                                       
         L     R7,ALOCAL                                                        
         USING LOCALD,R7                                                        
         ST    RF,MYRELO                                                        
         SPACE 1                                                                
F01      CLC   MEDIA,0(R4)         MEDIA FILTER TABLE ENTRY REQUIRES            
         BNE   F05                 REFERENCE TO HIERARCHY FOR KEY POS'N         
         USING FILTERSD,R4                                                      
         ZIC   R1,SAVEHIER+2       PRODUCT LENGTH                               
         LA    R1,3(R1)            PLUS C/U/L                                   
         STC   R1,FILDISP                                                       
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 1                                                                
F05      OC    ACOLIST,ACOLIST                                                  
         BNZ   *+8                                                              
         BAS   RE,INITIAL                                                       
*                                                                               
         ICM   R5,15,ABAL          R5 = A(BALANCE ELEMENT)                      
         USING ACBALD,R5                                                        
         USING FTBD,R6             R6 = A(FILTER TABLE ENTRY)                   
*                                                                               
F10      CLI   FTBVAL,X'08'        BILLED=                                      
         BH    F20                                                              
*                                                                               
         BAS   RE,RDOPT                                                         
         BAS   RE,LOOKUP                                                        
*                                                                               
         CLI   FTBVAL,X'08'                                                     
         BE    F15                                                              
         MVI   WORK,X'01'          =ESTIMATE                                    
         TM    OPTIONS,X'01'       IS IT A PCNT CASE                            
         BNO   *+8                                                              
         BAS   RE,PERCENTS                                                      
         SP    CURREST,0(L'ACBLCR,R2)                                           
         BZ    FXIT                                                             
         MVI   WORK,X'02'          =OVEREST                                     
         BM    FXIT                                                             
         MVI   WORK,X'04'          =UNDEREST                                    
         B     FXIT                                                             
         SPACE 1                                                                
F15      MVI   WORK,X'08'          ESTIMATE=YES/NO                              
         CP    CURREST,=P'0'                                                    
         BNE   FXIT                                                             
         MVI   WORK,0                                                           
         B     FXIT                                                             
         SPACE 1                                                                
F20      MVI   WORK,X'10'          UNBILLED=ZERO                                
         ZAP   WORK+10(6),ACBLDR                                                
         SP    WORK+10(6),ACBLCR                                                
         BZ    FXIT                                                             
         MVI   WORK,0              =NONZERO                                     
         SPACE 1                                                                
FXIT     LA    R2,WORK             RETURN TEST-UNDER-MASK VALUE IN 0(2)         
         XIT1  REGS=(R2)                                                        
         DROP  R5,R6                                                            
         EJECT                                                                  
*              HANDLE PERCENT ESTIMATES                                         
         SPACE 1                                                                
PERCENTS BR    RE                                                               
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQB**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060B,RB                                                        
         USING T606TWA,RA                                                       
         L     R7,ALOCAL                                                        
         USING LOCALD,R7                                                        
         ST    R2,MYRELO                                                        
         SPACE 1                                                                
         L     R9,AIO                                                           
         CLI   0(R9),RUNLAST       END                                          
         BE    TXIT                                                             
*                                                                               
         OC    ACOLIST,ACOLIST                                                  
         BNZ   *+8                                                              
         BAS   RE,INITIAL                                                       
*                                                                               
         CLI   LEVEL+1,3           MUST BE A JOB                                
         BNE   TNEXT                                                            
         CLI   LINE+1,19           SCREEN FULL                                  
         BE    TFULL                                                            
         CLI   VIRGIN,C'H'                                                      
         BE    T2                                                               
         SPACE 1                                                                
         MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,3                                                         
         BNL   T2                                                               
         MVC   INFDAT2,HEADING1    FIRST TIME SET UP HEADINGS                   
         MVC   INFDAT3,HEADING2                                                 
         OI    INFDAT2H+6,X'80'                                                 
         OI    INFDAT3H+6,X'80'                                                 
*&&UK                                                                           
         TM    SCMPSTA2,X'20'                                                   
         BNO   *+16                                                             
         MVC   INFDAT2+33(8),=C'EXTERNAL'                                       
         MVC   INFDAT2+42(8),=C'INTERNAL'                                       
*&&                                                                             
         MVI   LINE+1,3                                                         
         TM    OPTIONS,SHOWUNB                                                  
         BNO   T1                                                               
         MVC   INFDAT2+69(8),=C'UNBILLED'                                       
         MVC   INFDAT3+69(8),=C'ESTIMATE'                                       
         B     T2                                                               
T1       TM    OPTIONS,SHOWBAL                                                  
         BNO   T2                                                               
         MVC   INFDAT2+69(8),=C'  ACTUAL'                                       
         MVC   INFDAT3+69(8),=C'-BILLING'                                       
         B     T2                                                               
HEADING1 DC    CL39'JOB ACCOUNT  ACCOUNT NAME        ORIGIN'                    
         DC    CL39'AL  CURRENT   ACTUAL   ACTUAL ESTIMATE'                     
HEADING2 DC    CL39'-----------  ------------        ESTIMA'                    
         DC    CL39'TE ESTIMATE  CHARGES  BILLING -CHARGES'                     
         SPACE 1                                                                
T2       LH    R6,LINE             SET UP DISPLAY FOR A RECORD                  
         LR    R9,R6               R9 = LINE NUMBER                             
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         ICM   R5,15,ANAM                                                       
         BZ    T4                                                               
         USING ACNAMED,R5          NAME - ON UP TO 3 LINES                      
         ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'2'                                                         
         GOTO1 VCHOPPER,DMCB,((RF),ACNMNAME),(20,SCANBLCK),3                    
         ICM   RF,15,DMCB+8                                                     
         BZ    T4                                                               
         AR    R9,RF                                                            
         CH    R9,=H'20'                                                        
         BNL   TFULL                                                            
         BCTR  R9,0                                                             
         LR    R8,R6                                                            
         USING LINED,R8                                                         
         LA    R3,SCANBLCK                                                      
T3       OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+13(20),0(R3)                                            
         LA    R3,20(R3)                                                        
         LA    R8,86(R8)                                                        
         BCT   RF,T3                                                            
         DROP  R8                                                               
T4       L     R5,AKEY                                                          
         USING ACKEYD,R5                                                        
         MVC   LINEDATA(12),ACKEYACC+3                                          
         SPACE 1                                                                
T5       BAS   RE,RDOPT                                                         
         BAS   RE,LOOKUP                                                        
*                                                                               
         LA    R2,ORIGEST                                                       
         LA    R3,LINEDATA+33                                                   
         BAS   RE,EDIT                                                          
         LA    R2,CURREST                                                       
         LA    R3,LINEDATA+42                                                   
         BAS   RE,EDIT                                                          
         ICM   R5,15,ABAL                                                       
         BZ    T6                                                               
         USING ACBALD,R5                                                        
         ZAP   THISBUCK,ACBLDR                                                  
         LA    R2,THISBUCK                                                      
         LA    R3,LINEDATA+51                                                   
         BAS   RE,EDIT                                                          
         ZAP   THISBUCK,ACBLCR                                                  
         LA    R2,THISBUCK                                                      
         LA    R3,LINEDATA+60                                                   
         BAS   RE,EDIT                                                          
         TM    OPTIONS,SHOWBAL                                                  
         BNO   *+14                                                             
         ZAP   CURREST,ACBLDR                                                   
         B     T5A                                                              
         TM    OPTIONS,SHOWUNB                                                  
         BO    T5A                                                              
         SP    CURREST,ACBLDR      ESTIMATE MINUS CHARGES                       
         B     *+10                OR                                           
T5A      SP    CURREST,ACBLCR      UNBILLED ESTIMATE = CURRENT ESTIMATE         
         LA    R2,CURREST                              MINUS BILLING            
         LA    R3,LINEDATA+69                                                   
         LA    RE,T6                                                            
EDIT     EDIT  (P6,0(R2)),(12,WORK+20),2,MINUS=YES                              
         CLC   WORK+20(8),SPACES                                                
         BER   RE                                                               
         MVC   WORK+28(1),WORK+31                                               
         MVC   0(9,R3),WORK+20                                                  
         BR    RE                                                               
         SPACE 1                                                                
T6       OI    LINEHDR+6,X'80'     SET CC TO POSITIVE FOR NEXT REC PLSE         
         LA    R9,1(R9)                                                         
         STH   R9,LINE                                                          
         SPACE 1                                                                
TNEXT    LTR   RB,RB                                                            
         B     TXIT                                                             
         SPACE 1                                                                
TFULL    LNR   RB,RB               SET CC TO NEGATIVE FOR SCREEN FULL           
         MVI   LINE+1,3            AND SAVE HEADS                               
TXIT     XIT1                                                                   
         EJECT                                                                  
INITIAL  NTR1  ,                                                                
*                                                                               
         LA    R2,ROUTTAB                                                       
         LA    R3,ROUTTABL                                                      
         LA    R4,ROUTINES                                                      
*                                                                               
INIT2    L     RF,0(R2)            RELOCATE ADCONS                              
         A     RF,MYRELO                                                        
         ST    RF,0(R4)                                                         
         LA    R2,L'ROUTTAB(R2)                                                 
         LA    R4,L'ROUTINES(R4)                                                
         BCT   R3,INIT2                                                         
*                                                                               
         LA    RF,COLIST                                                        
         ST    RF,ACOLIST                                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'50',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)          GET A(TABLES)                                
*                                                                               
         LM    R0,R1,0(RF)         GET DISP TO/LENGTH OF COLUMN TABLE           
         AR    R0,RF                                                            
         STM   R0,R1,ACOLTAB                                                    
*                                                                               
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
*                                                                               
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
         CLI   4(R1),0                                                          
         BNE   INIT4                                                            
         DC    H'0'                                                             
*                                                                               
INIT4    MVC   KEYB(L'ACCKEY),SPACES READ AND BUFFER COMPANY RECORD             
         MVC   KEYB(1),MYCO                                                     
         GOTO1 AREADB                                                           
         L     R4,AIOB             R4=A(COMPANY RECORD)                         
         USING ACKEYD,R4                                                        
         LH    RF,ACLENGTH                                                      
         L     RE,ACOMP            RE=DESTINATION                               
         LR    R0,R4               R0=SOURCE                                    
         LR    R1,RF                                                            
         MVCL  RE,R0               SAVE THE COMPANY RECORD                      
*                                                                               
INITX    B     TXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
RDOPT    NTR1                                                                   
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOABUFF,AOPTBUFF                                                 
         MVC   GOLBUFF,=AL4(L'OPTBUFF)                                          
         MVC   GOACOMP,ACOMP                                                    
         L     R4,AIO              RE-READ RECORD AT END                        
         ST    R4,GOAKEY                                                        
*                                                                               
         ST    R4,GOAJOB                                                        
*                                                                               
         MVC   GOSELCUL,0(R4)                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         MVC   GOSELCLI,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,SAVEHIER                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELPRO,SPACES                                                  
         SR    R3,R3                                                            
         IC    R3,SAVEHIER                                                      
         IC    R1,SAVEHIER+2                                                    
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELJOB,SPACES                                                  
         IC    R3,SAVEHIER+2                                                    
         IC    R1,SAVEHIER+4                                                    
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(R4)                                                
*                                                                               
         MVI   GOWHICH,0                                                        
         MVI   GOANYWC,C'N'        DO NOT NEED WORKCODE LEVEL                   
*                                                                               
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
*                                                                               
RDOPTX   B     TXIT                                                             
         EJECT                                                                  
LOOKUP   NTR1                                                                   
*                                                                               
         ZAP   ORIGEST,=P'0'                                                    
         ZAP   CURREST,=P'0'                                                    
         LA    R5,JOBLOCKA                                                      
         USING JBLOCKD,R5                                                       
         L     R1,AIO                                                           
         ST    R1,JBAJOB                                                        
         ST    R1,JBAKEY                                                        
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         LA    R1,GOBLOCK                                                       
         ST    R1,JBAGOBLK                                                      
         MVC   JBAIO,AIOC                                                       
*                                                                               
         MVC   JBGETOPT,VGETOPT                                                 
*                                                                               
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
*                                                                               
         GOTO1 VJOBBER,DMCB,JOBLOCKA                                            
         CLI   JBERROR,X'00'                                                    
         BE    LOOKUPX                                                          
         DC    H'0'                                                             
*                                                                               
LOOKUPX  L     R3,JBACOLTB                                                      
         USING JBCOLD,R3                                                        
         ZAP   ORIGEST,JBCOLVAL                                                 
         ZAP   CURREST,JBCOLVAL+6(6)                                            
         B     TXIT                                                             
         DROP  R3,R5                                                            
         SPACE 3                                                                
LOOKFLDH DC    AL1(L'LOOKFLD+8),4X'00',AL1(L'LOOKFLD),2X'00'                    
LOOKFLD  DC    C'OE,CE'                                                         
         EJECT                                                                  
SHOWBAL  EQU   X'40'               OPTIONS BIT SETTINGS                         
SHOWUNB  EQU   X'80'                                                            
*                                                                               
ROUTTAB  DS    0A             TABLE OF ADCONS TO BE RELOCATED                   
         DC    V(ACJOBCOL)                                                      
         DC    A(COMP)                                                          
         DC    A(OPTBUFF)                                                       
ROUTTABL EQU   (*-ROUTTAB)/L'ROUTTAB                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
COMP     DC    2000X'00'                                                        
*                                                                               
OPTBUFF  DS    CL3000                                                           
         ORG   OPTBUFF                                                          
         DC    (L'OPTBUFF)X'00'                                                 
         EJECT                                                                  
LOCALD   DSECT                                                                  
MYRELO   DS    A                                                                
*                                                                               
CURREST  DS    PL6                                                              
ORIGEST  DS    PL6                                                              
THISBUCK DS    PL6                                                              
*                                                                               
ROUTINES DS    0A                                                               
VJOBCOL  DS    V                                                                
ACOMP    DS    A                                                                
AOPTBUFF DS    A                                                                
         DS    5A                  SPARE                                        
*                                                                               
ACOLIST  DS    A                   A(COLUMN LIST)                               
ACOLTAB  DS    A                   A(COLUMN OUTPUT TABLE)                       
LCOLTAB  DS    F                   L'COLUMN OUTPUT TABLE                        
AOPVTAB  DS    A                   A(OPERAND VALUE TABLE)                       
LOPVTAB  DS    F                   L'OPERAND VALUE TABLE                        
COLIST   DS    XL200                                                            
*                                                                               
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
*                                                                               
JOBLOCKA DS    (JBLOCKL)X                                                       
         SPACE 2                                                                
         DS    CL(LOCALLEN-(*-LOCALD))  SPARE                                   
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
*ACINQDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACINQ0B   05/01/02'                                      
         END                                                                    
