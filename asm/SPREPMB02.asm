*          DATA SET SPREPMB02  AT LEVEL 054 AS OF 05/01/02                      
*PHASE SPMB02B,+0                                                               
         TITLE 'SPMB02 - STATUS OF BUDGET REPORT'                               
SPMB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPMB**,RR=R2                                                 
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ST    R2,RELO                                                          
         LA    R8,SPACEND                                                       
         USING MYD,R8                                                           
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         MVI   MEDEXTDM,1                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         L     R6,=A(BUFFALOC)                                                  
         A     R6,RELO                                                          
         ST    R6,BUFFBUFF                                                      
         LA    R6,MYBUFFIO                                                      
         ST    R6,BUFFIO                                                        
         USING DRECD,R6                                                         
         CLI   MODE,RUNFRST                                                     
         BNE   MB2                                                              
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'120'                                                 
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
*              REQUEST AND ESTIMATE FIRST                                       
         SPACE 2                                                                
MB2      CLI   MODE,REQFRST                                                     
         BNE   MB4                                                              
         MVC   WORK,QBOOK1                                                      
         CLC   WORK(6),SPACES                                                   
         BNE   *+10                                                             
         MVC   WORK(6),=C'991231'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,PURCHEND)                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,1                                                       
         MVI   FCRDBUYS,C'Y'                                                    
         MVI   FCRDGOAL,C'Y'                                                    
         MVI   REQSW,C'Y'                                                       
         XC    REQPCT,REQPCT                                                    
         CLI   QUESTOR+3,C'.'                                                   
         BNE   XIT                                                              
         CLI   QUESTOR+6,C'%'      PERCENT SIGN HERE                            
         BNE   XIT                                                              
         MVC   WORK,QUESTOR                                                     
         OC    WORK(6),=6X'F0'                                                  
         PACK  DUB,WORK+1(2)                                                    
         CVB   R1,DUB                                                           
         PACK  DUB,WORK+4(2)                                                    
         CVB   R0,DUB                                                           
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         CLI   QUESTOR,C'-'                                                     
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         ST    R1,REQPCT                                                        
         B     XIT                                                              
         SPACE 2                                                                
MB4      CLI   MODE,ESTFRST                                                     
         BNE   MB10                                                             
         CLI   REQSW,C'Y'                                                       
         BNE   XIT                                                              
         MVI   REQSW,C'N'                                                       
         MVC   PAGE,=H'1'                                                       
         MVC   MEDDFORM,SPOTPROF+2                                              
         GOTO1 MEDDATE,DMCB,(RA)                                                
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R2)                                        
         XC    MYBUFFIO,MYBUFFIO                                                
         BAS   RE,CLRCUMS                                                       
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE GOALS                                                     
         SPACE 3                                                                
MB10     CLI   MODE,PROCGOAL                                                    
         BNE   MB20                                                             
         L     R5,ADGOAL                                                        
         USING GOALREC,R5                                                       
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         LA    R2,MEDMON01                                                      
         LA    R3,DGOAL                                                         
         LA    R4,12                                                            
         SPACE 2                                                                
MB12     OC    0(4,R2),0(R2)                                                    
         BZ    MB14                                                             
         L     R1,4(R2)            A(CHUNK)                                     
         L     R1,0(R1)            GOAL DOLLARS                                 
         CVD   R1,DUB                                                           
         AP    0(8,R3),DUB        POST THESE                                    
*                                                                               
         LA    R2,12(R2)                                                        
         LA    R3,8(R3)                                                         
         BCT   R4,MB12                                                          
         SPACE 2                                                                
MB14     LA    R2,GDELEM                                                        
         SPACE 2                                                                
MB16     ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'40'         LOOK FOR BUDGET ELEMENTS                     
         BNE   MB16                                                             
         USING GLBUDEL,R2                                                       
         MVC   DUB,GLBUDDOL        AND POST                                     
*                                                                               
         XC    NUM1,NUM1                                                        
         L     R1,DUB                                                           
         CVD   R1,NUM1+8                                                        
         SRP   NUM1,64-2,5        DIVIDE BY 100                                 
         AP    DBUDGET,QUOTIENT                                                 
         B     MB16                                                             
         EJECT                                                                  
*              HANDLE BUYS                                                      
         SPACE 3                                                                
MB20     CLI   MODE,PROCBUY                                                     
         BNE   MB40                                                             
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         LA    R2,PSLIST                                                        
         SPACE 2                                                                
MB22     OC    0(2,R2),0(R2)       LOOP ON BRAND/SL                             
         BZ    XIT                                                              
         CLI   BPRD,X'FF'                                                       
         BE    MB24                                                             
         CLC   BPRD,0(R2)                                                       
         BNE   MB28                                                             
         SPACE 2                                                                
MB24     MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
         LA    R5,MEDMON01                                                      
         LA    R3,DPURCH                                                        
         LA    R4,12                                                            
         SPACE 2                                                                
MB26     OC    0(4,R5),0(R5)       LOOK UP PURCHASED FOR EACH MONTH             
         BZ    MB28                                                             
         CLC   0(2,R5),PURCHEND    CHECK PURCHASE END OPTION                    
         BH    MB28                                                             
         L     R1,4(R5)                                                         
         USING MEDDATA,R1                                                       
         L     R0,MEDBYD                                                        
         CVD   R0,DUB                                                           
         AP    0(8,R3),DUB                                                      
*                                                                               
         LA    R5,12(R5)                                                        
         LA    R3,8(R3)                                                         
         BCT   R4,MB26                                                          
         SPACE 2                                                                
MB28     LA    R2,2(R2)                                                         
         B     MB22                                                             
         EJECT                                                                  
*              MARKET LAST ROUTINES                                             
         SPACE 3                                                                
MB40     CLI   MODE,MKTLAST                                                     
         BNE   MB50                                                             
         XC    DKEY,DKEY                                                        
         MVI   DTYPE,1                                                          
         MVC   DBREAK(12),=CL12'MARKET'                                         
         MVC   DBREAK+7(4),MARKET                                               
         MVC   DNAME,MKTNM                                                      
         LA    R2,DGOAL            ESTABLISH WHETHER MONTHS ARE                 
         LA    R3,DPURCH           PLANNED OR PURCHASED                         
         LA    R4,12                                                            
         SPACE 2                                                                
*                                                                               
MB42     CP    0(8,R3),=P'0'                                                    
         BE    MB43                                                             
         ZAP   0(8,R2),=P'0'                                                    
MB43     LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,MB42                                                          
         BAS   RE,FORMAT           PRINT MARKET TOTALS                          
         SPACE 1                                                                
         MVI   DTYPE,2             PUT OUT MARKET GROUP RECORDS                 
         MVC   DBREAK(36),MGR3BK                                                
         BAS   RE,BPUT                                                          
         MVI   DTYPE,3                                                          
         MVC   DBREAK(36),MGR2BK                                                
         BAS   RE,BPUT                                                          
         MVI   DTYPE,4                                                          
         MVC   DBREAK(36),MGR1BK                                                
         BAS   RE,BPUT                                                          
         SPACE 1                                                                
         MVI   DTYPE,5             PRODUCT RECORD                               
         MVC   DBREAK(36),SPACES                                                
         MVC   DBREAK(7),=C'PRODUCT'                                            
         BAS   RE,BPUT                                                          
         SPACE 1                                                                
         MVI   DTYPE,6             CLIENT RECAP                                 
         MVC   DNAME(20),PRDNM                                                  
         XC    DPRG,DPRG                                                        
         CLC   PGR1BK,SPACES                                                    
         BE    *+10                                                             
         MVC   DPRG,PGR1                                                        
         CLC   PGR2BK,SPACES                                                    
         BE    *+10                                                             
         MVC   DPRG,PGR2                                                        
         MVC   DPROD,PRODUCT                      PRODUCT                       
         BAS   RE,BPUT                                                          
         MVC   DPROD,=X'FFFF01'                                                 
         MVC   DBREAK(36),PGR2BK                  PGROUP 2                      
         CLC   PGR2BK,SPACES                                                    
         BE    *+8                                                              
         BAS   RE,BPUT                                                          
         MVC   DPRG,PGR1                                                        
         LA    R2,DPRG                                                          
         LA    R3,5                                                             
         SPACE 2                                                                
MB44     CLI   0(R2),C' '          ENSURE GROUP 1 COMES BELOW GROUP 2           
         BH    *+8                                                              
         MVI   0(R2),X'FF'                                                      
         LA    R2,1(R2)                                                         
         BCT   R3,MB44                                                          
         MVI   DPROD+2,2                                                        
         MVC   DBREAK(36),PGR1BK                  PGROUP 1                      
         CLC   PGR1BK,SPACES                                                    
         BE    *+8                                                              
         BAS   RE,BPUT                                                          
         MVC   DPRG,=5X'FF'                       CLIENT                        
         MVC   DBREAK(36),SPACES                                                
         MVC   DBREAK(6),=C'CLIENT'                                             
         BAS   RE,BPUT                                                          
         XC    MYBUFFIO,MYBUFFIO                                                
         BAS   RE,CLRCUMS                                                       
         B     XIT                                                              
         SPACE 2                                                                
BPUT     NTR1                                                                   
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R2),MYBUFFIO                               
         B     XIT                                                              
         EJECT                                                                  
*              CLEAR ACCUMS                                                     
CLRCUMS  NTR1                                                                   
         ZAP   DBUDGET,=P'0'                                                    
         LA    R0,12                                                            
         LA    R1,DGOAL                                                         
         LA    R2,DPURCH                                                        
CLR10    ZAP   0(8,R1),=P'0'                                                    
         ZAP   0(8,R2),=P'0'                                                    
         LA    R1,8(R1)                                                         
         LA    R2,8(R2)                                                         
         BCT   R0,CLR10                                                         
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE CONTROL BREAKS                                            
         SPACE 3                                                                
MB50     LA    R2,2                                                             
         CLI   MODE,MGR3LAST                                                    
         BE    MB52                                                             
         LA    R2,3                                                             
         CLI   MODE,MGR2LAST                                                    
         BE    MB52                                                             
         LA    R2,4                                                             
         CLI   MODE,MGR1LAST                                                    
         BE    MB52                                                             
         LA    R2,5                                                             
         CLI   MODE,PRDLAST                                                     
         BE    MB52                                                             
         LA    R2,6                                                             
         CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         SPACE 2                                                                
MB52     L     R3,BUFFBUFF                                                      
         CLI   REQSW,C'Y'                                                       
         BE    XIT                                                              
         XC    MYKEY,MYKEY                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',((R2),(R3)),MYBUFFIO,0                     
         B     MB56                                                             
         SPACE 2                                                                
MB54     GOTO1 BUFFALO,DMCB,=C'SEQ',((R2),(R3)),MYBUFFIO,0                      
         SPACE 2                                                                
MB56     TM    DMCB+8,X'80'                                                     
         BO    MB58                                                             
         BAS   RE,FORMAT                                                        
         MVC   MYKEY,MYBUFFIO                                                   
         B     MB54                                                             
         SPACE 2                                                                
MB58     GOTO1 BUFFALO,DMCB,=C'CLEAR',((R2),(R3)),(X'80',1)                     
         XC    MYBUFFIO,MYBUFFIO                                                
         BAS   RE,CLRCUMS                                                       
         CLI   MODE,PRDLAST                                                     
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT AND PRINT                                      
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         CLI   DTYPE,6                                                          
         BNE   FORMAT2                                                          
         MVI   RCSUBPRG,2                                                       
         SPACE 2                                                                
FORMAT2  CLI   DTYPE,1             MARKET                                       
         BNE   FORMAT4                                                          
         MVC   P(4),DBREAK+7                                                    
         MVC   P+5(24),DNAME                                                    
         B     FORMAT8                                                          
         SPACE 2                                                                
FORMAT4  CLI   DTYPE,6                                                          
         BNE   FORMAT6                                                          
         CLC   DBREAK(7),=C'PRODUCT'                                            
         BNE   FORMAT6                                                          
         MVC   P(20),DNAME                                                      
         B     FORMAT8                                                          
         SPACE 2                                                                
FORMAT6  MVC   P+5(10),=C'TOTALS FOR'                                           
         MVC   P+16(12),DBREAK                                                  
         MVC   P2+5(24),DNAME                                                   
         SPACE 2                                                                
FORMAT8  MVI   P+30,C'G'                                                        
         MVI   P3+30,C'P'                                                       
         EJECT                                                                  
*              EDIT THE NUMBERS - 1                                             
         SPACE 3                                                                
         LA    R2,DGOAL            GOALS                                        
         LA    R3,P+32                                                          
         BAS   RE,EDIT            CALCULATE                                     
         ZAP   DOUBLE,DUB         SAVE GOAL AMOUNT                              
*                                                                               
         LA    R3,P+115                                                         
         BAS   RE,EDIT10          PRINT                                         
*                                                                               
         LA    R2,DPURCH          PURCHASED                                     
         LA    R3,P3+32                                                         
         BAS   RE,EDIT            CALCULATE                                     
*                                                                               
         LA    R3,P3+115                                                        
         BAS   RE,EDIT10          PRINT                                         
         AP    DUB,DOUBLE         ADD GOALS TO PURCHASED                        
         LA    R3,P5+115          TOTAL                                         
         BAS   RE,EDIT10                                                        
         LA    R3,P3+123          AND AGAIN                                     
         BAS   RE,EDIT10                                                        
         ZAP   DOUBLE,DUB         SAVE AGAIN                                    
*                                                                               
         XC    NUM1,NUM1                                                        
         ICM   R1,15,REQPCT                                                     
         CVD   R1,NUM1+8                                                        
         MP    NUM1,DBUDGET                                                     
         DP    NUM1,=PL8'10000'   DIVIDE BY 10,000                              
         AP    QUOTIENT,DBUDGET                                                 
         ZAP   DUB,QUOTIENT                                                     
*                                                                               
         LA    R3,P+123                                                         
         BAS   RE,EDIT10           PRINT                                        
*                                                                               
         SP    DUB,DOUBLE                                                       
         XI    DUB+7,B'1'          REVERSE THE SIGN                             
*                                                                               
         LA    R3,P5+123                                                        
         BAS   RE,EDIT10                                                        
         SPACE 2                                                                
         CLC   P2+30(102),SPACES                                                
         BNE   STACK2                                                           
         MVC   P2+30(102),P3+30                                                 
         MVC   P3+30(102),P4+30                                                 
         MVC   P4+30(102),P5+30                                                 
         MVC   P5+30(102),P6+30                                                 
         MVC   P6+30(102),SPACES                                                
         SPACE 2                                                                
STACK2   CLC   P3+30(102),SPACES                                                
         BNE   STACK4                                                           
         MVC   P3+30(102),P4+30                                                 
         MVC   P4+30(102),P5+30                                                 
         MVC   P5+30(102),P6+30                                                 
         MVC   P6+30(102),SPACES                                                
         SPACE 2                                                                
STACK4   CLC   P4+30(102),SPACES                                                
         BNE   STACK6                                                           
         MVC   P4+30(102),P5+30                                                 
         MVC   P5+30(102),P6+30                                                 
         MVC   P6+30(102),SPACES                                                
         SPACE 2                                                                
STACK6   CLC   P5+30(102),SPACES                                                
         BNE   STACK8                                                           
         MVC   P5+30(102),P6+30                                                 
         MVC   P6+30(102),SPACES                                                
         SPACE 2                                                                
STACK8   MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              EDIT THE NUMBERS - 2                                             
         SPACE 3                                                                
EDIT     NTR1                                                                   
         LA    R4,12               FIT 12 INTO 7 CHARACTER SLOTS                
         ZAP   TOT,=P'0'                                                        
         ZAP   TEMP,=P'0'                                                       
         ZAP   TOTDOL,=P'0'       CUM DOLLARS SEP                               
         ZAP   TOTPEN,=P'0'       CUM PENNIES SEP                               
         SPACE 2                                                                
EDIT2    XC    NUM1,NUM1                                                        
         ICM   R1,15,REQPCT                                                     
         CVD   R1,NUM1+8                                                        
         MP    NUM1,0(8,R2)                                                     
         DP    NUM1,=PL8'10000'   DIVIDE BY 10,000                              
         AP    QUOTIENT,0(8,R2)                                                 
* ADD DOLLARS TO DOLLARS AND PENNIES TO PENNIES BEFORE ROUNDING                 
         ZAP   TEMP,QUOTIENT                                                    
         DP    TEMP,=PL8'100'                                                   
         AP    TOTDOL(8),TEMP(8)    ADD DOLLARS                                 
         AP    TOTPEN(8),TEMP+8(8)  ADD PENNIES                                 
*                                                                               
         SRP   QUOTIENT,64-2,5    ROUND FOR PRINTING                            
*                                                                               
         CP    QUOTIENT,=P'0'                                                   
         BE    EDIT8                                                            
*                                                                               
         EDIT  (P8,QUOTIENT),(9,DMCB)                                           
         CLC   DMCB(3),SPACES                                                   
         BNE   EDIT4                                                            
         MVC   0(7,R3),DMCB+2                                                   
         B     EDIT8                                                            
         SPACE 2                                                                
EDIT4    SH    R3,=H'2'                                                         
         CLC   0(9,R3),SPACES                                                   
         BNE   EDIT6                                                            
         MVC   0(9,R3),DMCB                                                     
         LA    R3,2(R3)                                                         
         B     EDIT8                                                            
         SPACE 2                                                                
EDIT6    MVC   132(9,R3),DMCB      PUT BELOW IF IT WONT FIT                     
         LA    R3,2(R3)                                                         
         SPACE 2                                                                
EDIT8    LA    R2,8(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R4,EDIT2                                                         
         ZAP   TOT,TOTDOL                                                       
         MP    TOT,=PL8'100'                                                    
         AP    TOT(16),TOTPEN                                                   
         SRP   TOT,64-2,5         ROUND TO DOLLARS                              
         ZAP   DUB(8),TOT+8(8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
EDIT10   NTR1                           EDIT DUB INTO 12 CHARACTER SLOT         
         EDIT  (P8,DUB),(9,DMCB),FLOAT=-                                        
         CLC   DMCB(2),SPACES                                                   
         BNE   EDIT12                                                           
         MVC   2(7,R3),DMCB+2                                                   
         B     XIT                                                              
         SPACE 2                                                                
EDIT12   CLC   0(9,R3),SPACES                                                   
         BE    *+8                                                              
         LA    R3,132(R3)                                                       
         MVC   0(9,R3),DMCB                                                     
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
         DS    0H                                                               
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RE,4(RD)                                                         
         CLC   0(4,RE),=C'REPT'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     *-14                                                             
         LM    RE,RC,12(RE)                                                     
         DROP  RF                                                               
         LA    R2,MEDMON01                                                      
         LA    R3,H10+34                                                        
         LA    R4,12                                                            
         SPACE 2                                                                
HOOK2    OC    0(4,R2),0(R2)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(2,0(R2)),(4,0(R3))                                  
         GOTO1 DATCON,DMCB,(2,2(R2)),(4,132(R3))                                
         LA    R2,12(R2)                                                        
         LA    R3,7(R3)                                                         
         BCT   R4,HOOK2                                                         
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER DATA RECORDS                                      
         SPACE 3                                                                
DRECD    DSECT                                                                  
DREC     DS    0CL248                                                           
DKEY     DS    0CL12     KEY                                                    
DTYPE    DS    CL1                 1=MARKET                                     
*                                  2=MARKET GROUP 3                             
*                                  3=MARKET GROUP 2                             
*                                  4=MARKET GROUP 1                             
*                                  5=PRODUCT                                    
*                                  6=CLIENT                                     
DPRG     DS    CL5                          PRODUCT GROUP (X'FF'=CLT)           
DPROD    DS    CL3                          PRODUCT CODE  (X'FF'=PGR)           
         DS    CL3                 SPARE                                        
         SPACE 2                                                                
DBREAK   DS    CL12      DATA      BREAK NAME                                   
DNAME    DS    CL24                NAME                                         
         SPACE 2                                                                
DGOAL    DS    12PL8     ACCUMS    GOAL      BY MONTH                           
DPURCH   DS    12PL8               PURCHASED BY MONTH                           
DBUDGET  DS    PL8                 BUDGET    YEARLY                             
         SPACE 3                                                                
*              PROGRAM STORAGE                                                  
         SPACE 1                                                                
MYD      DSECT                                                                  
REQSW    DS    CL1                                                              
PSLIST   DS    CL100                                                            
         DS    0F                                                               
MYBUFFIO DS    CL248                                                            
MYKEY    DS    CL12                                                             
REQPCT   DS    F                                                                
PURCHEND DS    CL2                                                              
*                                                                               
         DS    0D                                                               
QUOTIENT DS    PL8                                                              
REMAIND  DS    PL8                                                              
         ORG QUOTIENT                                                           
NUM1     DS    PL16                                                             
NUM2     DS    PL8                                                              
*                                                                               
TOTDOL   DS    PL8                                                              
TOTPEN   DS    PL8                                                              
*                                                                               
TOT      DS    PL16                                                             
TEMP     DS    PL16                                                             
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
         SPACE 3                                                                
         BUFF  LINES=400,ROWS=1,COLUMNS=25,FLAVOR=PACKED,              X        
               COMMENT=36,KEYLIST=(12,A)                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPREPMB02 05/01/02'                                      
         END                                                                    
