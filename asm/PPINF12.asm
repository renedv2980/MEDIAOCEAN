*          DATA SET PPINF12    AT LEVEL 065 AS OF 05/01/02                      
*PHASE T41A12A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41A12  PRINTPAK INFO PRODUCT HEADERS'                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 06/18/01 NEW DATA=LEGALW FILTER FOR LEGAL WARNINGS                       
*                                                                               
* KWAN 05/99    CORRECT FILTER ERROR DISPLAY                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41A12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41A12                                                         
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
*                                                                               
         LA    RE,REC2             CLEAR SAVE AREA                              
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVI   FLTSW,0             INITIALIZE FILTER FIELD SWITCH               
*                                                                               
         XC    DIVFLT,DIVFLT                                                    
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(4,=C'DIV=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CKADJC                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BAS   R9,GETNUM                                                        
         L     R6,4(R1)                                                         
         LA    RE,4                                                             
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         C     R5,=F'3'                                                         
         BH    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,4(0,R6)         * EXECUTED *                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  DIVFLT,DUB                                                       
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
*                                                                               
CKADJC   XC    ADJCFLT,ADJCFLT                                                  
         MVI   ADJCSW,0                                                         
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(5,=C'ADJC=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CKDT                GO AROUND IF NO ADJC= FILTER                 
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         BAS   R9,GETALPH       ALPHA NUMERIC                                   
         L     R6,4(R1)                                                         
         LA    RE,5                                                             
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         C     R5,=F'3'                                                         
         BH    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ADJCFLT(0),5(R6)                                                 
         MVI   ADJCSW,1                                                         
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKDT     DS    0H                  CHK FOR DATA= KEYWORDS                       
         MVI   USESW,0                                                          
         MVI   BFSW,0                                                           
         MVI   LWSW,0              LEGAL WARNING SWITCH                         
*                                                                               
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(5,=C'DATA=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CKUSER1                                                          
         L     R4,4(R1)                                                         
         L     R6,4(R1)                                                         
         LA    R4,5(R4)                                                         
         LA    RE,5                                                             
         CLC   0(5,R4),=C'BILLF'                                                
         BNE   CKDT10                                                           
         MVI   BFSW,1                                                           
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
         B     CKUSER1                                                          
*                                                                               
CKDT10   CLC   0(5,R4),=C'USER1'                                                
         BNE   CKDT20                                                           
         MVI   USESW,1                                                          
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
         B     CKUSER1                                                          
*                                                                               
CKDT20   CLC   0(5,R4),=C'USER2'                                                
         BNE   CKDT30                                                           
         MVI   USESW,2                                                          
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
         B     CKUSER1                                                          
*                                                                               
CKDT30   CLC   0(4,R4),=C'ADJC'                                                 
         BNE   CKDT40                                                           
         MVI   ADJCSW,1                                                         
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
         B     CKUSER1                                                          
*                                                                               
CKDT40   CLC   0(6,R4),=C'LEGALW'  LEGAL WARNINGS?                              
         BNE   FLTERR                                                           
         MVI   LWSW,1              SET FILTER SWITCH ON                         
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
*                                                                               
CKUSER1  DS    0H                  CHK FOR USER DESCRIPTION                     
         XC    USERDATA,USERDATA                                                
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'USER1=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CKUSER2                                                          
         MVI   USESW,1                                                          
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R6,USERDAT1                                                      
         BAS   RE,CKUDATA                                                       
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
*                                                                               
CKUSER2  DS    0H                  CHK FOR USER DESCRIPTION                     
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'USER2=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CKXXX                                                            
         MVI   USESW,2                                                          
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R6,USERDAT2                                                      
         BAS   RE,CKUDATA                                                       
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
*                                                                               
CKXXX    DS    0H                  END OF KEYWORDS CHECKINGS                    
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PREAD    DS    0H                                                               
         CLI   SINIFLTH+5,0        ANY BAD INPUTS?                              
         BE    PR00                                                             
         CLI   FLTSW,1             VALID KEYWORD IS ENTERED?                    
         BE    PR00                                                             
         LA    R2,SINIFLTH         POINT TO FILTER FIELD                        
         LA    R3,2                FIELD INVALID ERR MSG                        
         B     ERROR                                                            
*                                                                               
PR00     LA    R2,SINHDRH          BUILD HEADLINES                              
         CLI   USESW,0                                                          
         BE    PR5                                                              
         BAS   RE,GETCLTU          GET USER DESC FROM CLT                       
         MVC   FLDDATA+1(4),=CL17'PRD/'                                         
         MVC   FLDDATA+5(20),CUSER                                              
         MVC   FLDDATA+40(24),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(24),DASH                                               
         MVC   FLDDATA+40(24),DASH                                              
*                                                                               
* SCAN BACKWARDS FOR FIRT NON-SPACE TO FIX UNDERLINING OF CURSOR                
*                                                                               
         LA    RE,CUSER+19                                                      
         LA    RF,FLDDATA+24                                                    
         LA    R6,19               FOR BCT                                      
PR2      CLI   0(RE),C' '                                                       
         BH    PR4                                                              
         BCTR  RE,0                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R6,PR2                                                           
*                                                                               
* SCAN BACKWARDS FOR FIRT NON-SPACE TO FIX UNDERLINING OF CUSER                 
*                                                                               
PR4      LA    RE,CUSER+19                                                      
         LA    RF,FLDDATA+63                                                    
         LA    R6,19               FOR BCT                                      
PR4C     CLI   0(RE),C' '                                                       
         BH    PR4X                                                             
         BCTR  RE,0                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R6,PR4C                                                          
*                                                                               
PR4X     LA    R7,28                                                            
         B     PR20                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR5      CLI   LWSW,0              DISPLAYING LEGAL WARNINGS?                   
         BE    PR6                                                              
         MVC   FLDDATA+01(18),=C'PRD/LEGAL WARNINGS'                            
         MVC   FLDDATA+27(18),=C'PRD/LEGAL WARNINGS'                            
         MVC   FLDDATA+53(18),=C'PRD/LEGAL WARNINGS'                            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+01(19),DASH                                              
         MVC   FLDDATA+27(19),DASH                                              
         MVC   FLDDATA+53(19),DASH                                              
         LA    R7,42               3 COLUMNS WITH 14 ENTRIES EACH               
         B     PR20                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR6      CLI   ADJCSW,0                                                         
         BE    PR10                                                             
         CLI   BFSW,0                                                           
         BNE   PR10                                                             
         MVC   FLDDATA+1(19),=C'PRD/ADJACENCY CODES'                            
         MVC   FLDDATA+27(19),FLDDATA+1                                         
         MVC   FLDDATA+53(19),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(19),DASH                                               
         MVC   FLDDATA+27(19),DASH                                              
         MVC   FLDDATA+53(19),DASH                                              
         LA    R7,42               3 COLUMNS WITH 14 ENTRIES EACH               
         B     PR20                                                             
*                                                                               
PR10     MVC   FLDDATA+1(17),=C'PRODUCT CODE/NAME'                              
         CLI   BFSW,0                                                           
         BE    *+10                                                             
         MVC   FLDDATA+1(17),=CL17'PRD CODE/FORMULA'                            
         MVC   FLDDATA+27(17),FLDDATA+1                                         
         MVC   FLDDATA+53(17),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVC   FLDDATA+27(17),DASH                                              
         MVC   FLDDATA+53(17),DASH                                              
         LA    R7,42               3 COLUMNS WITH 14 ENTRIES EACH               
         B     PR20                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR20     FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)       SKIP CURRENT SCREEN LINE                     
         LA    R2,LINLEN(R2)       SKIP A BLANK SCREEN LINE                     
         LA    R6,REC2             TABLE FOR STORING FILTERED DATA              
*                                                                               
         LA    R5,KEY              BUILD FIRST KEY                              
         USING PRDHDRD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   PPRDKAGY,SVAGY                                                   
         MVC   PPRDKMED,SVEBCMED                                                
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,SVCLT                                                   
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   PPRDKPRD,SVPRD                                                   
         OC    PREVKEY,PREVKEY     FIRST TIME                                   
         BZ    *+10                                                             
         MVC   KEY,PREVKEY         NO - RESTORE PREV KEY                        
         XC    PREVKEY,PREVKEY                                                  
*                                                                               
RHI      BAS   RE,HIGH                                                          
         B     HAVREC                                                           
*                                                                               
RSEQ     BAS   RE,SEQ                                                           
*                                                                               
HAVREC   LA    R5,KEY                                                           
         CLC   KEY(7),KEYSAVE      CLIENT IS DIFFERENT?                         
         BNE   REND                                                             
         BAS   RE,GETREC                                                        
*                                                                               
         L     R5,AREC             BUILD TABLE FOR ALPHA LIST                   
         CLI   DIVFLT,0            SEE IF FILTERING ON DIV                      
         BE    HAVR1               NO                                           
         CLC   PPRDDIV,DIVFLT                                                   
         BNE   RSEQ                                                             
*                                                                               
HAVR1    CLI   ADJCSW,0                                                         
         BE    HAVR2                                                            
         CLI   ADJCFLT,0           NO FILTER KEEP GOING                         
         BE    HAVR2                                                            
         OC    PPRDEXCL,PPRDEXCL   FILTER AND NO CODE GET NEXT RECORD           
         BZ    RSEQ                                                             
         BAS   RE,ADJMTCH                                                       
         CLI   0(R8),1                                                          
         BE    RSEQ                                                             
*                                                                               
HAVR2    DS    0H                                                               
         CLC   USERDAT1,=CL20' '   FILTERING ON USER                            
         BNH   HAVR3                                                            
         LA    R1,USERDAT1                                                      
         BAS   RE,FNDUSER                                                       
         BNE   RSEQ                                                             
*                                                                               
HAVR3    DS    0H                                                               
         CLC   USERDAT2,=CL20' '   FILTERING ON USER                            
         BNH   HAVR4                                                            
         LA    R1,USERDAT2                                                      
         BAS   RE,FNDUSER                                                       
         BNE   RSEQ                                                             
*                                                                               
HAVR4    DS    0H                                                               
         CLI   LWSW,0              FILTERING ON LEGAL WARNINGS?                 
         BE    HAVR5                                                            
         LR    R3,R5               READY TO LOOK UP LEGAL WARNING ELEM          
         MVI   BYTE2,X'40'                                                      
         BRAS  RE,IGETEL           R3 = A(RECORD) & BYTE2 = ELEM CODE           
         BNE   RSEQ                                                             
         USING PPRDLWEL,R3                                                      
         MVC   00(03,R6),PPRDKPRD                                               
         OC    PPRDROTA,PPRDROTA                                                
         BNZ   *+6                                                              
         DC    H'0'                ELEM EXIST, SO WARNINGS MUST EXIST!          
         MVC   03(04,R6),PPRDROTA                                               
         B     HAVR10                                                           
         DROP  R3                                                               
*                                                                               
HAVR5    MVC   00(03,R6),PPRDKPRD                                               
         MVC   03(20,R6),PPRDNAME                                               
         CLI   BFSW,0              SEE IF DOING BILLING PROFILE                 
         BE    HAVR6               NO                                           
         BAS   RE,GETFORM                                                       
         MVC   3(20,R6),WORK2                                                   
         B     HAVR10                                                           
*                                                                               
HAVR6    CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    HAVR9               NO                                           
         BAS   RE,GETUSER                                                       
         MVC   3(32,R6),WORK2                                                   
         LA    R6,35(R6)                                                        
         B     HAVR11                                                           
*                                                                               
HAVR9    CLI   ADJCSW,0                                                         
         BE    HAVR10                                                           
         XC    3(20,R6),3(R6)                                                   
         OC    PPRDEXCL,PPRDEXCL                                                
         BNZ   *+14                                                             
         MVC   3(4,R6),=C'NONE'                                                 
         B     *+10                                                             
         MVC   3(3,R6),PPRDEXCL                                                 
*                                                                               
HAVR10   LA    R6,23(R6)                                                        
HAVR11   BCT   R7,RSEQ                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVC   PREVKEY,KEY         SAVE KEY FOR NEXT READ                       
*                                                                               
         MVI   PPRDKPRD+3,X'FF'                                                 
*                                                                               
* END OF PRODUCT HEADERS SO FORMAT SCREEN                                       
*                                                                               
REND     CLI   REC2,0              ANY DATA                                     
         BE    FRMTEND                                                          
*                                                                               
FORMAT   XC    DMWORK(20),DMWORK                                                
         LA    R6,REC2             POINTS TO TABLE OF STORED DATA               
         LA    R7,0                                                             
FORMAT1  CLI   0(R6),0             COUNT NUMBER OF ENTRIES                      
         BE    FORMAT2                                                          
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORM1               NO                                           
         LA    R6,35(R6)                                                        
         B     *+8                                                              
FORM1    LA    R6,23(R6)                                                        
         LA    R7,1(R7)            INCREMENT ENTRIES COUNTER                    
         B     FORMAT1                                                          
*                                                                               
FORMAT2  CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORM2               NO                                           
         GOTO1 FRMTALPH,DMCB,(35,REC2),(R7),14,(2,DMWORK)                       
         B     FORMAT3                                                          
*                                                                               
FORM2    GOTO1 FRMTALPH,DMCB,(23,REC2),(R7),14,(3,DMWORK)                       
*                                                                               
FORMAT3  LA    R6,DMWORK                                                        
         LA    RF,FLDDATA+1                                                     
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
*                                                                               
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         MVC   0(3,RF),0(R7)       MOVE PRD CODE TO SCREEN LINE                 
         MVI   3(RF),C'/'                                                       
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORMAT20            NO                                           
         MVC   4(32,RF),3(R7)                                                   
         B     *+10                                                             
*                                                                               
FORMAT20 MVC   4(20,RF),3(R7)                                                   
         CLI   LWSW,0              NEED TO DISPLAY LEGAL WARNINGS?              
         BE    *+8                                                              
         BRAS  RE,FRMTLW           FORMAT LEGAL WARNING DISPLAYS                
*                                                                               
         SR    RE,RE               DECREMENT COUNT                              
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORMAT30            NO                                           
         LA    R5,35(R5)                                                        
         B     *+8                                                              
FORMAT30 LA    R5,23(R5)                                                        
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT COLUMN                                  
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORMAT40            NO                                           
         LA    RF,40(RF)                                                        
         B     *+8                                                              
FORMAT40 LA    RF,26(RF)                                                        
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3                                                          
*                                                                               
FRMTEND  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT  OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*        GET USER=XXXXX INTO USERFIELD (R6)                                     
*        R4 - START OF USER FIELD                                               
*                                                                               
CKUDATA  NTR1                                                                   
         LR    R2,R4                                                            
         LA    R5,SINIFLT+64       END OF FILTER FIELD                          
         SR    R1,R1               COUNTER                                      
*                                                                               
CKU10    CR    R4,R5                                                            
         BNL   CKU30                                                            
         CLI   0(R4),C','                                                       
         BE    CKU30                                                            
         CLI   0(R4),C' '          SPACES ARE ALLOWED                           
         BNL   CKU20                                                            
         CLI   0(R4),C'-'          AND SO ARE DASHES                            
         BE    CKU20                                                            
         CLI   0(R4),C'*'          '*' IS A WILDCARD                            
         BNE   *+8                                                              
*                                                                               
CKU20    LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    R4,1(R4)                                                         
         B     CKU10                                                            
*                                                                               
CKU30    LTR   R1,R1                                                            
         BZ    CKUX                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R2)                                                    
*                                                                               
CKUX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*MATCHES ADJACENCY CODE IN FILTER TO THAT IN RECORD                             
*                                                                               
ADJMTCH  NTR1                                                                   
         LA    R8,ADJCFLT                                                       
         LA    R7,PPRDEXCL                                                      
         LA    R6,ADJCFLT+2                                                     
MTCHLP   CLC   0(1,R7),0(R8)   CHECK RECORD=FILTER                              
         BE    ADJX                                                             
         CLI   1(R7),X'00'                                                      
         BE    MTCHLP2                                                          
         CLC   1(1,R7),0(R8)                                                    
         BE    ADJX                                                             
         CLI   2(R7),X'00'                                                      
         BE    MTCHLP2                                                          
         CLC   2(1,R7),0(R8)                                                    
         BE    ADJX                                                             
MTCHLP2  LA    R8,1(R8)                                                         
         CR    R8,R6                                                            
         BNH   MTCHLP                                                           
         MVI   0(R8),1                                                          
ADJX     XIT1 REGS=(R8)                                                         
*                                                                               
*        FILTERING ON USERDAT FIELD                                             
*        R1 - USERDATX FIELD                                                    
*                                                                               
FNDUSER  NTR1                                                                   
         USING PRDHDRD,R5                                                       
         LR    R2,R1                                                            
         LA    R3,PPRDELEM                                                      
         USING PPRDUDEF,R3                                                      
*                                                                               
FD10     ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0             END OF RECORD                                
         BE    FDNO                                                             
         CLI   0(R3),X'08'         UDEF ELEMENT                                 
         BNE   FD10                                                             
         LA    R1,20               MAX L'USERDATX FIELD                         
         LA    R4,PUSER1                                                        
         CLI   USESW,1             SET R4 TO CORRECT USERDATA FIELD             
         BE    *+8                 NO                                           
         LA    R4,PUSER2                                                        
*                                                                               
FD20     CLI   0(R2),0             END OF DATA                                  
         BE    FDYES                                                            
         CLI   0(R2),C'*'          WILDCARD                                     
         BE    FD30                                                             
         CLC   0(1,R2),0(R4)                                                    
         BNE   FDNO                                                             
*                                                                               
FD30     LA    R2,1(R2)            BUMP TO NEXT CHARACTER                       
         LA    R4,1(R4)                                                         
         BCT   R1,FD20                                                          
*                                                                               
FDYES    SR    RC,RC                                                            
FDNO     LTR   RC,RC                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
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
GETFORM  NTR1                                                                   
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(19),WORK2                                                
         MVC   WORK2(4),=C'NONE'                                                
         LA    R4,PPRDBILP                                                      
         USING BILPROFD,R4                                                      
         LA    R7,BILBASA                                                       
         CLI   BILCMSW,C'C'        SEE IF COMMISSION ONLY                       
         BNE   *+8                                                              
         OI    0(R7),X'10'                                                      
         LA    R9,WORK2                                                         
         BAS   RE,FMTBAS                                                        
         LA    R9,WORK2+10                                                      
         OC    BILADJ,BILADJ                                                    
         BNZ   DISP2                                                            
         CLI   BILBASA,0                                                        
         BE    DISP4                                                            
         MVC   WORK2+6(2),=C'+0'                                                
         B     DISP4                                                            
*                                                                               
DISP2    LA    R9,WORK2+7                                                       
         EDIT  (B3,BILADJ),(8,0(R9)),4,ALIGN=LEFT,DROP=2                        
         MVI   WORK2+6,C'+'                                                     
         TM    BILADJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   WORK2+6,C'-'                                                     
         LA    R9,WORK2+14                                                      
         CLI   0(R9),C' '                                                       
         BH    DISP3                                                            
         BCT   R9,*-8                                                           
*                                                                               
DISP3    LA    R9,2(R9)                                                         
DISP4    LA    R7,BILBASB                                                       
         BAS   RE,FMTBAS                                                        
XIT      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        READ CLT RECORD & GET USER DESCRIPTION                                 
*                                                                               
GETCLTU  NTR1                                                                   
         MVC   CUSER,SPACES                                                     
         MVC   CUSER(13),=C'* UNDEFINED *'                                      
         LA    R5,KEY              BUILD FIRST KEY                              
         USING CLTHDRD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   PCLTKAGY,SVAGY                                                   
         MVC   PCLTKMED,SVEBCMED                                                
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVCLT                                                   
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE      MUST BE THERE                                
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETREC                                                        
         L     R5,AREC                                                          
         LA    R3,PCLTELEM                                                      
         USING PCLTUDEF,R3                                                      
*                                                                               
GC10     ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0             END OF RECORD                                
         BE    GCX                                                              
         CLI   0(R3),X'20'         UDEF ELEMENT                                 
         BNE   GC10                                                             
         LA    R4,PCLTPU1                                                       
         CLI   USESW,1             SET R4 TO CORRECT USERDATA FIELD             
         BE    *+8                 NO                                           
         LA    R4,PCLTPU2                                                       
         OC    0(20,R4),0(R4)      SEE IF DEFINED                               
         BZ    GCX                                                              
         MVC   CUSER,0(R4)                                                      
*                                                                               
GCX      B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
         SPACE 2                                                                
GETUSER  NTR1                                                                   
         USING PRDHDRD,R5                                                       
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(30),WORK2                                                
         LA    R3,PPRDELEM                                                      
         USING PPRDUDEF,R3                                                      
*                                                                               
GU10     ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0             END OF RECORD                                
         BE    GUX                                                              
         CLI   0(R3),X'08'                                                      
         BNE   GU10                                                             
         CLI   USESW,1             IF USER 1 DATA                               
         BNE   GU20                NO                                           
         MVC   WORK2(32),PUSER1                                                 
         B     GUX                                                              
*                                                                               
GU20     MVC   WORK2(16),PUSER2                                                 
*                                                                               
GUX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
GETALPH  SR    R5,R5                                                            
GETAL1   CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),0                                                          
         BER   R9                  END OF INPUT                                 
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'-'          ACCEPT -                                     
         BE    GETAL4                                                           
         CLI   0(R4),C'A'                                                       
         BL    GETAERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    GETAERR                                                          
GETAL4   LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GETAL1                                                           
*                                                                               
*                                                                               
GETAERR  SR    R5,R5                                                            
         BR    R9                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* SEND FILTER ERROR MESSAGE                                                     
*                                                                               
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)   * EXECUTED *                                   
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    R2,SINIFLTH                                                      
         FOUT  (R2)                                                             
         MVI   ERRAREA,X'FF'                                                    
         B     MODEXIT                                                          
*                                                                               
FMTBAS   NTR1                                                                   
         LA    R5,BASLST                                                        
         LA    R6,BASLSTN                                                       
FB2      CLC   0(1,R7),0(R5)                                                    
         BE    FB3                                                              
         LA    R5,6(R5)                                                         
         BCT   R6,FB2                                                           
         B     FBX                                                              
*                                                                               
FB3      MVC   0(5,R9),1(R5)                                                    
*                                                                               
FBX      XIT   XIT1                                                             
*                                                                               
BASLST   DS    0C                                                               
         DC    X'01',C'G    '                                                   
         DC    X'02',C'N    '                                                   
         DC    X'05',C'G-CD '                                                   
         DC    X'06',C'N-CD '                                                   
         DC    X'08',C'AC   '                                                   
         DC    X'11',C'CG   '                                                   
         DC    X'12',C'CN   '                                                   
         DC    X'15',C'CG-CD'                                                   
         DC    X'16',C'CN-CD'                                                   
         DC    X'18',C'CAC  '                                                   
BASLSTN  EQU   (*-BASLST)/6        NUMBER IN LIST                               
         SPACE 2                                                                
DASH     DC    40C'-'                                                           
SPACES   DC    40C' '                                                           
WORK2    DS    CL40                                                             
DIVFLT   DS    CL3                 DIVISION FILTER                              
ADJCFLT  DS    CL3                                                              
ADJCSW   DS    CL1                                                              
BFSW     DS    CL1                                                              
LWSW     DS    CL1                 LEGAL WARNING SWITCH                         
USESW    DS    CL1                                                              
USERDATA DS    0CL40                                                            
USERDAT1 DS    CL20                                                             
USERDAT2 DS    CL20                                                             
CUSER    DS    CL20                                                             
*                                                                               
FLTSW    DS    XL1                 FILTER FIELD SWITCH                          
*                                                                               
LINLEN   EQU   88                                                               
NOFNDERR EQU   53                                                               
         EJECT                                                                  
*                                                                               
*        THESE ARE THE NEEDED PARTS OF PPGENEROL                                
*        ++INCLUDE PPGENEROL REMOVED                                            
*                                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3               LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
IGETEL   NTR1  BASE=*,LABEL=*      R3 MUST BE POINTING TO RECORD                
*                                                                               
         LA    R3,33(R3)           POINT TO FIRST ELEMENT                       
         CLC   BYTE2,0(R3)         ELEMENT CODE MATCHED?                        
         BE    IGETELX             YES                                          
*                                                                               
         SR    R0,R0                                                            
IGETEL20 IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   BYTE2,0(R3)         ELEMENT CODE MATCHED?                        
         BE    IGETELX             YES                                          
         CLI   0(R3),0             END OF RECORD?                               
         BE    IGETELER            YES, ELEMENT NOT FOUND                       
         B     IGETEL20            TRY NEXT ELEMENT IN RECORD                   
*                                                                               
IGETELX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
IGETELER LTR   RB,RB               NOT EQUAL (ERROR)                            
         XIT1  REGS=(R3)           POSITION OF ELEMENT IN RECORD                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FRMTLW   NTR1  BASE=*,LABEL=*      LEGAL WARNING DISPLAY                        
*                                                                               
         XC    04(20,RF),04(RF)    CLR AFTER PRD CODE ON SCREEN LINE            
*                                                                               
         MVC   04(01,RF),03(R7)    FIRST LEGAL WARNING                          
         MVC   05(02,RF),=C'1,'    FIRST QUARTER AND A COMMA                    
*                                                                               
         MVC   07(01,RF),04(R7)    SECOND LEGAL WARNING                         
         MVC   08(02,RF),=C'2,'    SECOND QUARTER AND A COMMA                   
*                                                                               
         MVC   10(01,RF),05(R7)    THIRD LEGAL WARNING                          
         MVC   11(02,RF),=C'3,'    THIRD QUARTER AND A COMMA                    
*                                                                               
         MVC   13(01,RF),06(R7)    FOURTH LEGAL WARNING                         
         MVI   14(RF),C'4'         FIRST QUARTER AND NO COMMA                   
*                                                                               
FRMTLWX  CR    RB,RB               EQUAL (ALWAYS EQUAL FOR NOW)                 
         B     *+6                                                              
FRMTLWER LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
         SPACE 2                                                                
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSINFOWRK                                                     
