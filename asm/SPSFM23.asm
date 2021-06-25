*          DATA SET SPSFM23    AT LEVEL 037 AS OF 11/19/18                      
*PHASE T21723A                                                                  
         TITLE 'T21723  -SPB- SPLIT BILLING RECORDS'                            
T21723   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21723                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   USEIONUM,1          BECAUSE DISPKEY LOGIC SCREWS IT UP           
         MVC   AIO,AIO1            (DITTO)                                      
         OI    GENSTAT4,CONFDEL    FORCE CONFIRMATION OF DELETES                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       LIST RECORDS                                 
         BE    PR                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING SPBKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVI   SPBKTYP,SPBKTYPQ                                                 
         MVI   SPBKSUB,SPBKSUBQ                                                 
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    VK1                                                              
         CLI   ACTNUM,ACTREP       TEST ACTION=REPORT                           
         BNE   VK10                                                             
VK1      LA    R2,SPLMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   SPBKAGMD,BAGYMD                                                  
         LA    R2,SPLCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VK2                                                              
         OI    LISTIND,LCLT                                                     
         GOTO1 VALICLT                                                          
         MVC   SPBKCLT,BCLT                                                     
*                                                                               
VK2      LA    R2,SPLPRDH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VK4                                                              
         OI    LISTIND,LPRD                                                     
         GOTO1 VALIPRD                                                          
         CLC   QPRD,=C'POL'        DISALLOW PRD=POL                             
         BE    EPRD                                                             
         MVC   SPBKPRD,QPRD                                                     
*                                                                               
VK4      LA    R2,SPLESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BE    VK6                                                              
         OI    LISTIND,LEST                                                     
         GOTO1 VALIEST                                                          
         MVC   SPBKEST,BEST                                                     
*                                                                               
VK6      LA    R3,SPBKCLT-SPBKEY-1                                              
         CLI   LISTIND,0           CHECK FOR MISSING FIELDS                     
         BE    VK8                                                              
         LA    R2,SPLCLTH                                                       
         TM    LISTIND,LCLT                                                     
         BZ    EMIS                                                             
         LA    R3,L'SPBKCLT(R3)                                                 
         TM    LISTIND,LPRD+LEST                                                
         BZ    VK8                                                              
         LA    R2,SPLPRDH                                                       
         TM    LISTIND,LPRD                                                     
         BZ    EMIS                                                             
         LA    R3,L'SPBKPRD(R3)                                                 
         TM    LISTIND,LEST                                                     
         BZ    VK8                                                              
         LA    R3,L'SPBKEST(R3)                                                 
*                                                                               
VK8      STC   R3,LKEYCOMP         SAVE LENGTH FOR KEY COMPARE                  
         B     VK20                                                             
*                                                                               
*                                  ACTION NOT LIST                              
VK10     LA    R2,SPBMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   SPBKAGMD,BAGYMD                                                  
         MVC   SPBMNM,MEDNM                                                     
         OI    SPBMNMH+6,X'80'                                                  
         LA    R2,SPBCLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
         MVC   SPBKCLT,BCLT                                                     
         MVC   SPBCNM,CLTNM                                                     
         OI    SPBCNMH+6,X'80'                                                  
         LA    R2,SPBPRDH          PRODUCT                                      
         GOTO1 VALIPRD                                                          
         CLC   QPRD,=C'POL'        DISALLOW PRD=POL                             
         BE    EPRD                                                             
         MVC   SPBKPRD,QPRD                                                     
*                                                                               
         BRAS  RE,CKCHILD                                                       
         BNE   ECHILD                                                           
*                                                                               
         MVC   SPBPNM,PRDNM                                                     
         OI    SPBPNMH+6,X'80'                                                  
         LA    R2,SPBESTH          ESTIMATE                                     
         GOTO1 VALIEST                                                          
         MVC   SPBKEST,BEST                                                     
         MVC   SPBENM,ESTNM                                                     
         OI    SPBENMH+6,X'80'                                                  
*                                                                               
VK20     MVC   KEY(13),SVKEY       SET KEY                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING SPBRECD,R6                                                       
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR ALL SUBSIDIARY RECORDS          
*                                                                               
         MVC   SPBMED,QMED         MEDIA                                        
         OI    SPBMEDH+6,X'80'                                                  
         MVI   SPBMEDH+5,1         FUDGE INPUT LENGTH                           
         LA    R2,SPBMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   SPBMNM,MEDNM                                                     
         OI    SPBMNMH+6,X'80'                                                  
*                                                                               
         GOTO1 CLUNPK,DMCB,SPBKCLT,SPBCLT    CLIENT                             
         OI    SPBCLTH+6,X'80'                                                  
         MVI   SPBCLTH+5,3         FUDGE INPUT LENGTH                           
         LA    R2,SPBCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   SPBCNM,CLTNM                                                     
         OI    SPBCNMH+6,X'80'                                                  
*                                                                               
         MVC   SPBPRD,SPBKPRD      PRODUCT                                      
         OI    SPBPRDH+6,X'80'                                                  
         MVI   SPBPRDH+5,3         FUDGE INPUT LENGTH                           
         LA    R2,SPBPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   SPBPNM,PRDNM                                                     
         OI    SPBPNMH+6,X'80'                                                  
*                                                                               
         ZIC   RE,SPBKEST          ESTIMATE                                     
         EDIT  (RE),(3,SPBEST),ALIGN=LEFT                                       
         OI    SPBESTH+6,X'80'                                                  
         STC   R0,SPBESTH+5        FUDGE INPUT LENGTH                           
         OI    SPBESTH+4,X'08'     FUDGE NUMERIC                                
         LA    R2,SPBESTH          ESTIMATE                                     
         GOTO1 VALIEST                                                          
         MVC   SPBENM,ESTNM                                                     
         OI    SPBENMH+6,X'80'                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,PPCELCDQ     REMOVE ALL ELEMENTS                          
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R0,NPRODS           R0=MAX N'PRODUCTS                            
         LA    R1,PRDLIST                                                       
         XC    0(3,R1),0(R1)       CLEAR PRODUCT LIST (NO DUPS YET)             
         LA    R1,3(R1)                                                         
         BCT   R0,*-10                                                          
         XC    NUMPRDS,NUMPRDS                                                  
*                                                                               
         XC    ELEM,ELEM           BUILD NEW ELEMENTS                           
         LA    R6,ELEM                                                          
         USING PPCEL,R6                                                         
         MVI   PPCELCD,PPCELCDQ    BUILD PRODUCT PERCENTAGE ELEMENTS            
         MVI   PPCELLN,PPCELLNQ                                                 
         LA    R0,NPRODS           R0=MAX N'PRODUCTS                            
         LA    R3,1                R3=SEQUENCE NUMBER                           
         SR    R5,R5               R5=PERCENT ACCUMULATOR                       
         LA    R2,SPBPRD1H                                                      
         LR    R4,R2                                                            
         USING SPBPRD1H,R4                                                      
         CLI   5(R2),0             TEST FIRST PRODUCT MISSING                   
         BE    EMIS                                                             
         B     VR3                                                              
*                                                                               
VR2      CLI   5(R4),0             TEST END OF PRODUCTS                         
         BE    VR4                                                              
*                                                                               
VR3      STC   R3,PPCSEQ           SEQ NUM                                      
         LA    R2,SPBPRD1H                                                      
         CLI   5(R2),3             VALIDATE PRODUCT                             
         BH    EINV                                                             
         CLC   =C'AAA',8(R2)       DO NOT ALLOW AAA AS A SPLIT PRODUCT          
         BE    EINV                                                             
         CLC   =C'POL',8(R2)       DO NOT ALLOW POL AS A SPLIT PRODUCT          
         BE    EINV                                                             
         MVC   MYKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+4(0),8(R2)                                                   
         OC    KEY+4(3),=X'404040'                                              
         MVC   QPRD,KEY+4                                                       
         GOTO1 READ                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   SPBPNM1,PNAME-PRDHDRD(R1)                                        
         OI    SPBPNM1H+6,X'80'                                                 
         MVC   PPCPRD,QPRD                                                      
         L     RF,AIO1             SPLIT BILLING RECORD                         
         CLC   QPRD,SPBKPRD-SPBKEY(RF)  IS THIS THE PRODUCT IN THE KEY?         
         BE    EPRDKEY             YES -- THAT'S A NO-NO                        
*                                                                               
*                                                                               
         GOTO1 BINSRCH,DMCB,QPRD,PRDLIST,NUMPRDS,3,(0,3),NPRODS                 
         TM    DMCB,X'01'          IS THE PRODUCT IN THE LIST ALREADY?          
         BZ    EDUPPRD             YES -- ERROR                                 
*                                                                               
         GOTO1 BINSRCH,DMCB,(X'01',QPRD),PRDLIST,NUMPRDS,3,(0,3),NPRODS         
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL -- CAN'T HAPPEN                
         MVC   NUMPRDS,DMCB+8                                                   
*                                                                               
         BRAS  RE,CKPARNT                                                       
         BNE   EPARNT                                                           
*                                  MAKE SURE EST IS OPEN FOR THIS PROD          
         MVC   KEY+7(1),BEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EESTPRD             EST/PRD ERROR                                
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY                                                        
*                                                                               
         LA    R2,SPBPCT1H         VALIDATE PERCENT                             
         SR    R7,R7                                                            
         ICM   R7,1,5(R2)                                                       
         BZ    EMIS                                                             
         GOTO1 CASHVAL,DMCB,SPBPCT1,(R7)                                        
         CLI   0(R1),0                                                          
         BNE   EINV                                                             
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BNP   EINV                                                             
         AR    R5,RE                                                            
         CH    R5,=H'10000'        TEST TOTAL PCT NOT OVER 100                  
         BH    EPCT                                                             
         STCM  RE,3,PPCPCT         PERCENT                                      
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         LA    R3,1(R3)                                                         
         LA    R4,SPBPRD2H-SPBPRD1H(R4)  NEXT PRODUCT                           
         BCT   R0,VR2                                                           
*                                                                               
VR4      CH    R5,=H'10000'        TEST TOTAL PERCENT = 100                     
         BNE   EPCT                                                             
*                                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,SPBPRD1H         CLEAR SCREEN                                 
         USING SPBPRD1H,R2                                                      
         BAS   RE,CLRSCRN                                                       
         L     R6,AIO                                                           
         USING SPBRECD,R6                                                       
         LA    R6,SPBFSTEL         FIND PERCENT ELEMENTS                        
         LA    R7,NPRODS                                                        
*                                                                               
DR2      CLI   0(R6),0                                                          
         BE    DRX                                                              
         CLI   0(R6),PPCELCDQ                                                   
         BNE   DR4                                                              
         USING PPCEL,R6                                                         
         MVC   SPBPRD1,PPCPRD      PRODUCT                                      
         OI    SPBPRD1H+6,X'80'                                                 
         SR    RE,RE               PERCENT                                      
         ICM   RE,3,PPCPCT                                                      
         EDIT  (RE),(6,SPBPCT1),2                                               
         OI    SPBPCT1H+6,X'80'                                                 
         XC    KEY,KEY             GET PRODUCT RECORD                           
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),PPCPRD                                                  
         GOTO1 READ                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   SPBPNM1,PNAME-PRDHDRD(R1)    PRODUCT NAME                        
         OI    SPBPNM1H+6,X'80'                                                 
         LA    R2,SPBPRD2H-SPBPRD1H(,R2)                                        
         BCT   R7,DR4                                                           
         B     DR6                                                              
*                                                                               
DR4      ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR2                                                              
*                                                                               
DR6      MVC   AIO,AIO1                                                         
*                                                                               
DRX      B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R2,SPLSELH                                                       
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
         MVI   NLISTS,16                                                        
         ZIC   R3,LKEYCOMP         R3=LENGTH FOR KEY COMPARE                    
         LA    R6,KEY                                                           
         OC    KEY,KEY                                                          
         BNZ   LR2                                                              
*                                                                               
         XC    KEY,KEY                                                          
         USING SPBRECD,R6                                                       
         MVC   KEY(13),SVKEY                                                    
*                                                                               
LR2      GOTO1 HIGH                                                             
         B     LR6                                                              
*                                                                               
LR4      GOTO1 SEQ                                                              
*                                                                               
LR6      MVC   SVKEY,KEY                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   LRX                                                              
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   BLOCK(L'SPACES),SPACES                                           
         LA    R4,BLOCK            BUILD LIST LINE IN BLOCK                     
         USING LINED,R4                                                         
         GOTO1 CLUNPK,DMCB,SPBKCLT,LLCLT                                        
         MVC   LLPRD,SPBKPRD                                                    
         EDIT  SPBKEST,(3,LLEST),FILL=0                                         
         LA    R5,LLPCTS                                                        
         LA    R6,SPBFSTEL                                                      
         LA    R7,NPRODS                                                        
*                                                                               
LR8      CLI   0(R6),0                                                          
         BE    LR12                                                             
         CLI   0(R6),PPCELCDQ                                                   
         BNE   LR10                                                             
         USING PPCEL,R6                                                         
         MVC   0(3,R5),PPCPRD                                                   
         LA    R5,2(R5)                                                         
         CLI   PPCPRD+2,C' '                                                    
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'='                                                       
         LA    R5,1(R5)                                                         
         EDIT  PPCPCT,(6,(R5)),2,ALIGN=LEFT                                     
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         BCT   R7,LR10                                                          
         B     LR12                                                             
*                                                                               
LR10     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LR8                                                              
*                                                                               
LR12     BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
*                                                                               
         MVC   LISTAR,BLOCK                                                     
         LA    R4,LISTAR                                                        
         CLI   LLEOL,C' '          DID WE COMPLETELY FILL UP THE LINE?          
         BNH   *+8                                                              
         MVI   LLEOL,C'>'          INDICATE THAT WE'RE OUT OF ROOM              
         MVC   AIO,AIO1                                                         
         GOTO1 LISTMON                                                          
         B     LR4                                                              
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       DS    0H                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         ZIC   R3,LKEYCOMP         R3=LENGTH FOR KEY COMPARE                    
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING SPBRECD,R6                                                       
         MVC   KEY(13),SVKEY                                                    
*                                                                               
PR2      GOTO1 HIGH                                                             
         B     PR6                                                              
*                                                                               
PR4      GOTO1 SEQ                                                              
*                                                                               
PR6      EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   PRX                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LA    R4,P                                                             
         USING PRLINED,R4                                                       
         CLC   SPBKCLT,SVCLT                                                    
         BE    PR7                                                              
         MVI   PRLCLT,C'*'                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
PR7      MVC   SVCLT,SPBKCLT                                                    
         GOTO1 CLUNPK,DMCB,SPBKCLT,PRLCLT                                       
         MVC   PRLPRD,SPBKPRD                                                   
         EDIT  SPBKEST,(3,PRLEST),FILL=0                                        
         LA    R5,PRLPCTS                                                       
         LA    R6,SPBFSTEL                                                      
         LA    R7,NPRODS                                                        
*                                                                               
PR8      CLI   0(R6),0                                                          
         BE    PR12                                                             
         CLI   0(R6),PPCELCDQ                                                   
         BNE   PR10                                                             
         USING PPCEL,R6                                                         
         MVC   0(3,R5),PPCPRD                                                   
         LA    R5,2(R5)                                                         
         CLI   PPCPRD+2,C' '                                                    
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'='                                                       
         LA    R5,1(R5)                                                         
         EDIT  PPCPCT,(6,(R5)),2,ALIGN=LEFT                                     
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         BCT   R7,PR10                                                          
         B     PR12                                                             
*                                                                               
PR10     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PR8                                                              
*                                                                               
PR12     BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR4                                                              
*                                                                               
PRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
HDHK     NTR1                                                                   
         MVC   H1+9(1),QMED                                                     
         B     EXIT                                                             
*                                                                               
HEADING  SSPEC H1,3,C'MEDIA'                                                    
         SSPEC H1,32,C'SPLIT BILLING RECORD REPORT'                             
         SSPEC H1,68,AGYNAME                                                    
         SSPEC H2,32,C'----------------------------'                            
         SSPEC H2,68,AGYADD                                                     
         SSPEC H3,3,REPORT                                                      
         SSPEC H4,3,RUN                                                         
         SSPEC H4,68,REQUESTOR                                                  
         SSPEC H4,89,PAGE                                                       
         SSPEC H6,3,C'CLT'                                                      
         SSPEC H7,3,C'---'                                                      
         SSPEC H6,8,C'PRD'                                                      
         SSPEC H7,8,C'---'                                                      
         SSPEC H6,13,C'EST'                                                     
         SSPEC H7,13,C'---'                                                     
         SSPEC H6,18,C'PERCENTAGES'                                             
         SSPEC H7,18,C'-------------------------------------------'             
         DC    H'0'                                                             
*****************************                                                   
* ROUTINE TO CLEAR THE SCREEN                                                   
* FROM FIELD AT R2                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS4                                                              
         EX    RE,CSOC                                                          
         BZ    CS4                                                              
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS4      LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS2                                                              
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* ERROR EXITS                                                                   
*                                                                               
EPARNT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'PRD ALREADY DEFINED AS PARENT'                 
         B     MSGERR                                                           
*                                                                               
ECHILD   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'PRD ALREADY DEFINED AS A CHILD'                
         B     MSGERR                                                           
*                                                                               
EPCT     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'PERCENTS MUST ADD UP TO 100PCT'                   
         B     MSGERR                                                           
*                                                                               
EESTPRD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'ESTIMATE NOT OPEN FOR PRODUCT'                    
         B     MSGERR                                                           
*                                                                               
EPRDKEY  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'PRODUCT CANNOT BE SAME AS KEY'                    
         B     MSGERR                                                           
*                                                                               
EDUPPRD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(17),=C'DUPLICATE PRODUCT'                                
         B     MSGERR                                                           
*                                                                               
EMIS     MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
EPRD     MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
NPRODS   EQU   20                  MAX PRODUCTS PER SCREEN                      
*                                                                               
SVCLT    DS    CL2                                                              
LISTIND  DS    XL1                                                              
LCLT     EQU   X'80'                                                            
LPRD     EQU   X'40'                                                            
LEST     EQU   X'20'                                                            
*                                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*                                                                               
CKCHILD  NTR1  BASE=*,LABEL=*                                                   
         MVC   DATADISP,=H'24'     SPOTFILE                                     
         MVC   FULL,AIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(SPBKPRD-SPBKEY),SVKEY                                        
*                                                                               
         GOTO1 HIGH                                                             
         B     CKCH20                                                           
*                                                                               
CKCH10   GOTO1 SEQ                                                              
CKCH20   CLC   KEY(SPBKPRD-SPBKEY),KEYSAVE                                      
         BNE   CKCHQX                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PPCELCDQ                                                  
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CKCH50   BRAS  RE,NEXTEL                                                        
         BNE   CKCH10                                                           
*                                                                               
         CLI   0(R6),PPCELCDQ                                                   
         BNE   CKCH10                                                           
         CLC   SPBKPRD-SPBKEY+SVKEY(L'SPBKPRD),PPCPRD-PPCEL(R6)                 
         BE    CKCHNQX                                                          
         B     CKCH50                                                           
*                                                                               
CKCHQX   MVC   AIO,FULL                                                         
         J     EQXIT                                                            
CKCHNQX  MVC   AIO,FULL                                                         
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
* PRD IS IN QPROD                                                               
CKPARNT  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(SPBKPRD-SPBKEY),SVKEY                                        
*                                                                               
         GOTO1 HIGH                                                             
         B     CKPA20                                                           
*                                                                               
CKPA10   GOTO1 SEQ                                                              
CKPA20   CLC   KEY(SPBKPRD-SPBKEY),KEYSAVE                                      
         BNE   CKPAQX                                                           
*                                                                               
         CLC   SPBKPRD-SPBKEY+KEY(L'SPBKPRD),QPRD                               
         BE    CKPANQX                                                          
         B     CKPA10                                                           
*                                                                               
CKPAQX   MVC   KEY,WORK                                                         
         J     EQXIT                                                            
CKPANQX  MVC   KEY,WORK                                                         
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD6D                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSPBL                                                      
         EJECT                                                                  
* SPSFMWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
LKEYCOMP DS    XL1                                                              
MYKEY    DS    XL(L'KEY)                                                        
PRDLIST  DS    (NPRODS)CL3         LIST OF PRODUCTS (TO CHECK FOR DUPS)         
NUMPRDS  DS    A                   (SHOULD BE TYPE F, BUT GOTO1. . .)           
         EJECT                                                                  
LINED    DSECT                     LIST LINE DSECT                              
LLCLT    DS    CL3                                                              
         DS    CL2                                                              
LLPRD    DS    CL3                                                              
         DS    CL2                                                              
LLEST    DS    CL3                                                              
         DS    CL2                                                              
LLPCTS   DS    CL58                                                             
LLEOL    DS    C                                                                
         EJECT                                                                  
PRLINED  DSECT                     PRINT LINE DSECT                             
         DS    CL2                                                              
PRLCLT   DS    CL3                                                              
         DS    CL2                                                              
PRLPRD   DS    CL3                                                              
         DS    CL2                                                              
PRLEST   DS    CL3                                                              
         DS    CL2                                                              
PRLPCTS  DS    CL100                                                            
         EJECT                                                                  
         PRINT OFF                                                              
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPSFM23   11/19/18'                                      
         END                                                                    
