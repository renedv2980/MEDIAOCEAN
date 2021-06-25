*          DATA SET PPREPF402  AT LEVEL 044 AS OF 01/05/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE PPF402A                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'PPF402 - PRINTPAK ESTIMATE BUCKET CREATION'                     
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*     BPLA 1/29/14   FIX PLANNED COST5 BUCKET LOGIC                             
*                                                                               
*     BOBY 3/21/07   ADD PLANNED COST BUCKET                                    
*                                                                               
*     BPLA 6/21/00   AT ESTL DO BUCKET EVEN IF NO ACTIVITY                      
*                                                                               
*     BPLA 5/13/94   EXPAND ACCUMULATORS TO PL8 FROM PL6                        
*                                                                               
*     BPLA 1/11/91   WHEN DOING BUCKET COMPARE (QOPT1=C)                        
*                    X'21' AND X'31' ELEMS ARE REMOVED FROM                     
*                    'OLD' BUCKET REC BEFORE LENGTH COMPARE                     
*                                                                               
PPF402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPF402                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPF4WK,R8                                                        
*                                                                               
*                                                                               
*        QOPT1   P=PRINT BUCKETS                                                
*                C=COMPARE BUCKETS TO FILE                                      
*        QOPT2   Y=PRINT OUTPUT BUCKET RECORD TRACE                             
*        QOPT3   S=SUPPRESS STEWARDSHIP ESTIMATES FROM REPORT                   
*                *NOTE*: ONLY HONORED WHEN WRITE=NO. THIS IS MEANT TO           
*                        AFFECT DRAFT REPORTING ONLY.                           
*                                                                               
         SPACE 2                                                                
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         CLI   MODE,LBUYEST                                                     
         BE    ESTL                                                             
         CLI   MODE,LBUYCLI                                                     
         BE    CLTL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,FBUYCLI                                                     
         BE    CLTF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,FBUYEST                                                     
         BE    ESTF                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    REQF                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*        RUN FIRST                                                              
         SPACE 2                                                                
RUNF     DS    0H                                                               
         LHI   R0,#ACCUMS                                                       
         LA    R1,WTOTS                                                         
         BAS   RE,CLRWTOT                                                       
*                                                                               
         LA    R0,60               CLEAR EST, CLIENT AND RUN TOTALS             
         LA    R1,ETOTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         STM   R8,RC,SVREGS                                                     
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FF'-X'02'                                             
*                                                                               
         LAY   R1,BKTREC                                                        
         ST    R1,ABKTREC          CHANGE I/OAREAS                              
         LAY   R1,BKTREC2                                                       
         ST    R1,ABKTREC2                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        REQ FIRST                                                              
*                                                                               
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTEST,C'Y'      SET TO READ TEST BUYS                         
         B     EXIT                                                             
         SPACE 3                                                                
*        CLIENT FIRST                                                           
         SPACE 2                                                                
CLTF     DS    0H                                                               
         CLI   QOPT1,C'C'       FOR COMPARE                                     
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'     NO FILE MARKING                                 
*                                                                               
         CLI   QEST,C' '                                                        
         BH    *+10                                                             
         MVC   QEST,=C'ALL'                                                     
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   CF3                                                              
         CLC   QEST,=C'ALL'                                                     
         BE    CF3                                                              
*                                                                               
         MVC   P(26),=C'EST MUST BE ALL IF PRD ALL'                             
         MVC   PSECOND(80),QPROG                                                
         BAS   RE,RPRT                                                          
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
CF3      DS    0H                                                               
*                                  MARK ALL BUCKET POINTERS DELETED             
         XC    KEY,KEY             BUILD STARTING KEY FOR BUCKET RECS           
         MVC   KEY(7),PCLTKAGY                                                  
         MVI   KEY+3,X'09'                                                      
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CF8                                                              
         MVC   KEY+7(3),QPRODUCT                                                
         CLC   QEST,=C'ALL'                                                     
         BE    CF8                                                              
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,KEY+10                                                        
*                                                                               
CF8      DS    0H                                                               
         GOTO1 HIGH                READ FIRST BUCKET RECORD                     
         B     CF9B                                                             
*                                                                               
CF9      DS    0H                                                               
         GOTO1 SEQ                 READ NEXT BUCKET RECORD                      
CF9B     DS    0H                                                               
         CLC   KEY(7),KEYSAVE      DONE ON CHANGE IN CLIENT                     
         BNE   CF12                DONE                                         
         CLI   KEYSAVE+7,0                                                      
         BE    *+14                                                             
         CLC   KEY(10),KEYSAVE     THRU PRODUCT                                 
         BNE   CF12                                                             
         OC    KEYSAVE+10(2),KEYSAVE+10                                         
         BZ    *+14                                                             
         CLC   KEY(12),KEYSAVE     THRU EST                                     
         BNE   CF12                                                             
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   CF9                                                              
         OI    KEY+25,X'80'        DELETE                                       
         GOTO1 WRT                                                              
         B     CF9                                                              
CF12     DS    0H                                                               
         CLI   QOPT1,C'P'                                                       
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 3                                                                
*        EST FIRST                                                              
         SPACE                                                                  
ESTF     DS    0H                                                               
         MVI   ESTACT,C'N'                                                      
         L     R6,ABKTREC          BUILD BKT REC                                
         USING PBKREC,R6                                                        
         XC    0(250,R6),0(R6)                                                  
         MVC   PBKKAGY,RCSVAGY                                                  
         MVC   PBKKMED,RCSVMED                                                  
         MVI   PBKKRCD,X'09'                                                    
         MVC   PBKKCLT,RCSVCLI                                                  
         MVC   PBKKPRD,RCSVPRD                                                  
         MVC   PBKKEST,PESTKEST                                                 
         MVC   PBKLEN,=H'33'                                                    
*                                                                               
***TESTBUY***                                                                   
         MVI   TESTSW,C'N'                                                      
         TM    PESTTEST,X'80'       SEE IF THIS IS A TEST ESTIMATE              
         BZ    *+8                                                              
         MVI   TESTSW,C'Y'                                                      
*                                                                               
         MVI   STEWSW,C'N'                                                      
         TM    PESTTEST,X'40'      STEWARDSHIP ESTIMATE?                        
         BZ    *+8                                                              
         MVI   STEWSW,C'Y'         YES                                          
***TESTBUY***                                                                   
         XIT1                                                                   
         SPACE 3                                                                
*        LAST FOR ESTIMATE                                                      
         SPACE 2                                                                
ESTL     DS    0H                                                               
******   CLI   ESTACT,C'Y'     FIX BUCKET EVEN IF NO ACTIVITY                   
******   BNE   ESTL4                                                            
         BAS   RE,BUCKPUT                                                       
         LA    R2,RPRTNTR                                                       
         CLI   QOPT1,C'P'                                                       
         BE    *+6                                                              
         SR    R2,R2               SUPPRESS PRINTING                            
         GOTOR BUCKPRT,DMCB,(C'P',ABKTREC),P,WTOTS,DATCON,(R2)                  
*                                                                               
         LA    R1,WTOTS                                                         
         LA    R2,ETOTS                                                         
         BAS   RE,ROLWTOT         ROLL WTOTS TO ETOTS                           
*                                                                               
         LA    R1,ETOTS                                                         
         BAS   RE,ROLTOTS         NOW I CAN ROLL TO CTOTS                       
*                                                                               
ESTL4    DS    0H                                                               
         CLI   QOPT1,C'C'          DO BUCKET COMPARE                            
         BNE   *+8                                                              
         BAS   RE,BUCKCOMP                                                      
         B     EXIT                                                             
         SPACE 3                                                                
*        CLIENT LAST                                                            
         SPACE 2                                                                
CLTL     DS    0H                                                               
         MVC   P(14),=C'**CLIENT CCC**'                                         
         MVC   P+9(3),PCLTKCLT                                                  
         LA    R1,CTOTS                                                         
         MVI   PCSW,0              KILL PLANNED COSTS SWITCH                    
         BAS   RE,PRTOTS                                                        
         BAS   RE,RPRT                                                          
*                                  PRINT PC TOTALS                              
         LA    R1,CTOTS+96         POINT TO BILL MONTH PC'S                     
*                                  PRINT PC TOTALS                              
         CLC   0(18,R1),=3PL8'0'   SKIP IF NO TOTALS                            
         BE    CLTLX                                                            
*                                                                               
         MVI   PCSW,C'P'           SET PLANNED COSTS SWITCH                     
         BAS   RE,PRPCTOTS                                                      
         BAS   RE,RPRT                                                          
CLTLX    DS    0H                                                               
         LA    R1,CTOTS                                                         
         BAS   RE,ROLTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*        RUN LAST                                                               
         SPACE 2                                                                
RUNL     DS    0H                                                               
         MVC   P(14),=C'**RUN TOTALS**'                                         
         LA    R1,RTOTS                                                         
         MVI   PCSW,0              KILL PLANNED COSTS SWITCH                    
         BAS   RE,PRTOTS                                                        
         BAS   RE,RPRT                                                          
*                                  PRINT PC TOTALS                              
         LA    R1,RTOTS+96         POINT TO BILL MONTH PC'S                     
*                                  PRINT PC TOTALS                              
         CLC   0(18,R1),=3PL8'0'   SKIP IF NO TOTALS                            
         BE    RUNLX                                                            
*                                                                               
         MVI   PCSW,C'P'           SET PLANNED COSTS SWITCH                     
         BAS   RE,PRPCTOTS                                                      
RUNLX    DS    0H                                                               
         LA    R1,RTOTS                                                         
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
*        PROCESS BUY                                                            
         SPACE 2                                                                
PRBUY    DS    0H                                                               
*                                                                               
***TESTBUY***                                                                   
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BNE   PB2                                                              
         CLI   TESTSW,C'Y'         SEE IF PROCESSING TEST BUYS                  
         BNE   EXIT                NO-SKIP THIS BUY                             
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES ?                                  
         BE    PB2                 YES: IGNORE QOPT3 SETTING                    
         CLI   QOPT3,C'S'          OPTION SET TO SUPPRESS PRINTING...           
         BNE   PB2                 ...OF STEWARDSHIP ESTIMATES?                 
         CLI   STEWSW,C'Y'         YES: IS THIS A STEWARDSHIP ESTIMATE?         
         BNE   EXIT                YES: SKIP THIS BUY                           
***TESTBUY***                                                                   
PB2      DS    0H                                                               
*                                                                               
*        GET PLANNED COST DATA                                                  
*                                                                               
         GOTO1 GETINS,DMCB,PBUYREC,(C'P',PCVALS),RCSVPRD,0,0                    
*                                                                               
         CLI   DMCB+4,C'X'         IF THERE ARE NO PLANNED COSTS                
         BNE   *+10                                                             
         XC    PCVALS,PCVALS           CLEAR RESULTS                            
*                                                                               
         TM    PBUYCNTL,X'80'                                                   
         BZ    *+16                                                             
         XC    GROSS(20),GROSS                                                  
         XC    PCVALS(20),PCVALS                                                
*                                                                               
         OC    GROSS(12),GROSS                                                  
         BNZ   PB4                                                              
         OC    PGROSS(12),PGROSS                                                
         BNZ   PB4                                                              
         OC    PCVALS(12),PCVALS                                                
         BZ    EXIT                                                             
*                                                                               
PB4      DS    0H                                                               
         MVI   ESTACT,C'Y'                                                      
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING BKELEMD,R3                                                       
         MVI   BKELEM+1,46       X'2E'                                          
*                                                                               
         MVI   BKELEM,X'22'        REG - BILL MONTH                             
         MVC   BKYM,PBDBDATE                                                    
         MVC   BKDATE,PBDBDATE                                                  
         MVI   BKDATE+2,1                                                       
         BAS   RE,FINDEL                                                        
*                                                                               
         MVI   BKELEM,X'23'        SPECIAL - INS MONTH                          
         MVC   BKYM,PBUYKDAT                                                    
         MVC   BKDATE,PBUYKDAT                                                  
         MVI   BKDATE+2,1                                                       
         MVI   BKTYPE,C'I'                                                      
         BAS   RE,FINDEL                                                        
*                                                                               
         OC    PCVALS(12),PCVALS   SKIP IF NO PLANNED COSTS                     
         BE    PBPCX                                                            
*                                                                               
*  NOTE- PLANNED COST BUCKETS ARE SMALLER                                       
*                                                                               
         MVI   BKELEM,X'42'        'REGULAR' BILL MONTH                         
         MVI   BKELEM+1,28         X'1C'                                        
         MVC   BKYM,PBDBDATE                                                    
         MVC   BKDATE,PBDBDATE                                                  
         MVI   BKDATE+2,1          DAY=1                                        
         MVI   BKTYPE,0            BILL MONTH                                   
         BAS   RE,FINDEL                                                        
*                                                                               
         MVI   BKELEM,X'43'        SPECIAL - INS MONTH                          
         MVI   BKELEM+1,28         X'1C'                                        
         MVC   BKYM,PBUYKDAT                                                    
         MVC   BKDATE,PBUYKDAT                                                  
         MVI   BKDATE+2,1                                                       
         MVI   BKTYPE,C'I'                                                      
         BAS   RE,FINDEL                                                        
*                                                                               
PBPCX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*                        UPDATE ELEM IF PRESENT, ELSE ADD                       
FINDEL   NTR1                                                                   
         L     R2,ABKTREC                                                       
         LA    R2,33(R2)                                                        
FE2      DS    0H                                                               
         CLC   BKELEM(8),0(R2)                                                  
         BE    FE4                                                              
         BL    FE8                                                              
         CLI   0(R2),0                                                          
         BE    FE8                                                              
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     FE2                                                              
*                                                                               
*                                  HAVE ELEM                                    
FE4      DS    0H                                                               
         LR    R3,R2                                                            
***TESTBUY***                                                                   
         CLI   TESTSW,C'Y'         SEE IF DOING A TEST EST                      
         BNE   *+8                                                              
         MVI   BKIND,X'01'                                                      
***TESTBUY***                                                                   
         CLI   BKELEM,X'42'        CHECK FOR PC COSTS ELEMENT                   
         BE    FEPC                                                             
         CLI   BKELEM,X'43'        CHECK FOR PC COSTS ELEMENT                   
         BE    FEPC                                                             
*                                                                               
         L     R1,GROSS                                                         
         CVD   R1,DUB                                                           
         AP    BKOGRS,DUB                                                       
*                                                                               
         S     R1,AGYCOM                                                        
         CVD   R1,DUB                                                           
         AP    BKONET,DUB                                                       
*                                                                               
         L     R1,CSHDSC                                                        
         CVD   R1,DUB                                                           
         AP    BKOCD,DUB                                                        
*                                                                               
         L     R1,PGROSS                                                        
         CVD   R1,DUB                                                           
         AP    BKPGRS,DUB                                                       
*                                                                               
         S     R1,PAGYCOM                                                       
         CVD   R1,DUB                                                           
         AP    BKPNET,DUB                                                       
*                                                                               
         L     R1,PCSHDSC                                                       
         CVD   R1,DUB                                                           
         AP    BKPCD,DUB                                                        
*                                                                               
         B     FEX                                                              
*                                                                               
FEPC     DS    0H                                                               
*                                                                               
         L     R1,GROSS-GROSS+PCVALS                                            
         CVD   R1,DUB                                                           
         AP    BKOGRS,DUB                                                       
*                                                                               
         S     R1,AGYCOM-GROSS+PCVALS                                           
         CVD   R1,DUB                                                           
         AP    BKONET,DUB                                                       
*                                                                               
         L     R1,CSHDSC-GROSS+PCVALS                                           
         CVD   R1,DUB                                                           
         AP    BKOCD,DUB                                                        
*                                                                               
         B     FEX                                                              
*                                                                               
FE8      DS    0H                                                               
*                                                                               
         MVC   BKOGRS(36),=6PL6'0'                                              
*                                                                               
         L     RF,ABKTREC                                                       
*                                                                               
         GOTO1 RECUP,DMCB,(X'FE',ABKTREC),BKELEM,(C'R',(R2)),USERSYS            
         CLI   8(R1),C'R'          TEST RECORD FULL                             
         BE    FE4                 NO - OK- ADD TO ELEM                         
*                                                                               
         MVC   P(26),=C'**BUCKET RECORD OVERFLOW**'                             
         BAS   RE,RPRT                                                          
*                                                                               
FEX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*        ADD BUCKET REC TO FILE                                                 
         SPACE 2                                                                
BUCKPUT  NTR1                                                                   
         CLI   QOPT2,C'Y'                                                       
         BNE   BP2                                                              
         GOTO1 =V(PRTREC),DMCB,ABKTREC,(33,25),PRINT,HEXOUT,C'DOME',   +        
               =C'UPDATED BUCKET RECORD'                                        
*                                                                               
BP2      DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   BPX                                                              
         L     R6,ABKTREC                                                       
*                                                                               
         USING PBKREC,R6           ESTABLISH BUCKET RECORD                      
         CLI   PBKKRCD,X'09'       SKIP IF NOT A BUCKET RECORD                  
         BNE   BPX                                                              
*                                                                               
         MVC   KEY(25),PBKREC                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   BP4                                                              
*                                                                               
         NI    KEY+25,X'FF'-X'80'  UNDELETE                                     
         GOTO1 WRT                                                              
         MVC   AREC,ABKTREC2       READ INTO COMMENT REC                        
         GOTO1 GETPRT                                                           
         MVC   AREC,ABKTREC                                                     
         GOTO1 PUTPRT                                                           
         B     BPX                                                              
*                                  NOT ALREADY ON FILE                          
BP4      DS    0H                                                               
         MVC   AREC,ABKTREC                                                     
         GOTO1 ADDPRT                                                           
*                                                                               
BPX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
BUCKCOMP NTR1                                                                   
         L     R6,ABKTREC                                                       
         MVC   KEY(25),PBKREC                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   BC20                                                             
         MVC   AREC,ABKTREC2       READ INTO SECOND RECORD AREA                 
         GOTO1 GETPRT                                                           
*                                                                               
         L     R5,AREC             FILE RECORD                                  
*                                  REMOVE TODAY ELEMENT FROM                    
*                                  OLD BUCKET RECORD                            
         LA    R2,33(R5)                                                        
         MVI   ELCODE,X'21'                                                     
         CLI   0(R2),X'21'                                                      
         BE    BC3                                                              
         BAS   RE,NEXTEL                                                        
         BNE   BC5                                                              
BC3      GOTO1 RECUP,DMCB,(X'FE',0(R5)),(R2),0,USERSYS                          
*                                                                               
BC5      LA    R2,33(R5)                                                        
         CLI   0(R2),X'31'                                                      
         BE    BC5C                                                             
         MVI   ELCODE,X'31'        REMOVE TODAY ELEMENT WITH GST                
         BAS   RE,NEXTEL                                                        
         BNE   BC6                                                              
BC5C     GOTO1 RECUP,DMCB,(X'FE',0(R5)),(R2),0,USERSYS                          
*                                                                               
BC6      LA    R2,33(R5)                                                        
         CLI   0(R2),X'41'                                                      
         BE    BC6C                                                             
         MVI   ELCODE,X'41'        REMOVE TODAY ELEMENT WITH GST                
         BAS   RE,NEXTEL                                                        
         BNE   BC7                                                              
BC6C     GOTO1 RECUP,DMCB,(X'FE',0(R5)),(R2),0,USERSYS                          
*                                                                               
BC7      DS    0H                                                               
         MVC   FULL(2),25(R5)      LENGTH                                       
         LH    RF,FULL                                                          
         SH    RF,=H'33'                                                        
         STH   RF,FULL                                                          
         LA    R5,33(R5)                                                        
         L     R6,ABKTREC          NEW                                          
         MVC   FULL+2(2),25(R6)    LENGTH                                       
         LH    RF,FULL+2                                                        
         SH    RF,=H'33'                                                        
         STH   RF,FULL+2                                                        
         LA    R6,33(R6)                                                        
*                                                                               
BC10     DS    0H                                                               
         CLC   FULL(2),FULL+2      COMPARE LENGTHS                              
         BNE   BC19                                                             
*                                                                               
         LH    R4,FULL                                                          
BC16     DS    0H                                                               
         CH    R4,=H'256'                                                       
         BNH   BC18                                                             
*                                                                               
         CLC   0(256,R5),0(R6)                                                  
         BNE   BC19                                                             
         LA    R5,256(R5)                                                       
         LA    R6,256(R6)                                                       
         SH    R4,=H'256'                                                       
         B     BC16                                                             
*                                                                               
BC18     DS    0H                                                               
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R6)                                                    
         BE    BCX                                                              
BC19     DS    0H                                                               
         MVC   P(20),=C'**BUCKETS DISAGREE**'                                   
         MVC   P+25(7),=C'BUCKETS'                                              
         BAS   RE,RPRT                                                          
         GOTOR BUCKPRT,DMCB,(C'P',ABKTREC2),P,X,DATCON,RPRTNTR                  
         MVC   P(20),=C'**BUCKETS DISAGREE**'                                   
         MVC   P+25(4),=C'FILE'                                                 
         BAS   RE,RPRT                                                          
         GOTOR BUCKPRT,DMCB,(C'P',ABKTREC),P,X,DATCON,RPRTNTR                   
         B     BCX                                                              
*                                                                               
BC20     DS    0H                                                               
         MVC   P(15),=C'**NOT ON FILE**'                                        
         BAS   RE,RPRT                                                          
BC22     DS    0H                                                               
         GOTOR BUCKPRT,DMCB,(C'P',ABKTREC),P,X,DATCON,RPRTNTR                   
*                                                                               
BCX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
USERSYS  DC    AL2(33,25,4096)                                                  
*                                                                               
NEXTEL   ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF RECORD                                
         BE    NEXTEL2                                                          
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
PRTOTS   NTR1                                                                   
         LR    R4,R1                                                            
         LA    R2,P+18                                                          
         LA    R3,6                                                             
*                                                                               
PRTOTS2  DS    0H         STAGGER TOTALS P AND PSECOND                          
         CH    R3,=H'5'                                                         
         BE    PRTOTS4                                                          
         CH    R3,=H'3'                                                         
         BE    PRTOTS4                                                          
         CH    R3,=H'1'                                                         
         BE    PRTOTS4                                                          
*                                                                               
         EDIT  (P8,0(R4)),(17,0(R2)),2,FLOAT=-,COMMAS=YES                       
*                                                                               
PRTOTS3  LA    R2,15(R2)    THIS LOOKS WRONG BUT IS RIGHT                       
         LA    R4,8(R4)                                                         
         BCT   R3,PRTOTS2                                                       
         XIT1                                                                   
*                                                                               
PRTOTS4  DS    0H                                                               
         EDIT  (P8,0(R4)),(17,132(R2)),2,FLOAT=-,COMMAS=YES                     
         B     PRTOTS3                                                          
*                                                                               
         SPACE 3                                                                
PRPCTOTS NTR1                                                                   
         LR    R4,R1                                                            
         LA    R2,P+18                                                          
         LA    R3,3                                                             
*                                                                               
PRPCTOT2 DS    0H         STAGGER TOTALS P AND PSECOND                          
         CHI   R3,2                                                             
         BE    PRPCTOT4                                                         
*                                                                               
         EDIT  (P8,0(R4)),(17,0(R2)),2,FLOAT=-,COMMAS=YES                       
         MVI   17(R2),C'P'                                                      
*                                                                               
PRPCTOT3 LA    R2,15(R2)    THIS LOOKS WRONG BUT IS RIGHT                       
         LA    R4,8(R4)                                                         
         BCT   R3,PRPCTOT2                                                      
         XIT1                                                                   
*                                                                               
PRPCTOT4 DS    0H                                                               
         EDIT  (P8,0(R4)),(17,132(R2)),2,FLOAT=-,COMMAS=YES                     
         MVI   17+132(R2),C'P'                                                  
         B     PRPCTOT3                                                         
*                                                                               
         SPACE 3                                                                
CLRTOTS  DS    0H                                                               
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,CLRTOTS                                                       
         BR    RE                                                               
         SPACE 3                                                                
CLRWTOT  DS    0H                 USE FOR WTOTS                                 
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,CLRWTOT                                                       
         BR    RE                                                               
         SPACE 3                                                                
ROLTOTS  DS    0H                                                               
         LHI   R0,#ACCUMS                                                       
RT2      DS    0H                                                               
         AP    ACCUML(8,R1),0(8,R1)                                             
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,RT2                                                           
         BR    RE                                                               
         SPACE 3                                                                
ROLWTOT  DS    0H        USED TO ROLL WTOTS (PL6) TO ETOTS (PL8)                
         LHI   R0,#ACCUMS          R1 TO WTOTS - R2 TO ETOTS                    
ROLWT2   DS    0H                                                               
         AP    0(8,R2),0(6,R1)                                                  
         ZAP   0(6,R1),=P'0'                                                    
         LA    R2,8(R2)                                                         
         LA    R1,6(R1)                                                         
         BCT   R0,ROLWT2                                                        
         BR    RE                                                               
         SPACE 3                                                                
RPRT     DS    0H                                                               
         LA    RF,RPRTNTR                                                       
RPRTNTR  NTR1                                                                   
         LM    R8,RC,SVREGS-RPRTNTR(RF)     RESTORE REGS                        
         J     RPRT2                                                            
SVREGS   DS    5F                                                               
*                                                                               
RPRT2    DS    0H                                                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                  R5 POINTS TO REC                             
         LH    R2,0(R5)            LENGTH                                       
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'BUCKPRT - PRINT EST BUCKET DETAILS'                             
*        THIS IS A COPY OF DDBUCKPRT TO HANDLE PLANNED COSTS                    
*                                                                               
*        PARAM1     A(RECORD)         HOB - S=SPOT,P=PRINT                      
*        PARAM2     A(PRINT LINE)                                               
*        PARAM3     A(TOTALS)         12PL6 - 6 REG, 6 SPEC                     
*        PARAM4     V(DATCON)                                                   
*        PARAM5     V(PRINT ROUTINE)  IF ZERO NO PRINTING                       
*                                                                               
BUCKPRT  NMOD1 BPWKX-BPWK,BUCKPRT                                               
*                                                                               
         ST    R1,SAVR1                                                         
         MVC   PARMS,0(R1)                                                      
         L     R2,VREC                                                          
         L     R1,ATOTS            CLEAR TOTALS                                 
         LHI   R0,#ACCUMS                                                       
         ZAP   0(6,R1),=PL6'0'                                                  
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         MVI   PCSW,0              TURN OFF PLANNED COSTS SWITCH                
*                                                                               
         L     R3,APL                                                           
         USING PLD,R3                                                           
         OC    PRNT,PRNT                                                        
         BZ    BP9                 SKIP PRINT                                   
         MVI   PL,C' '                                                          
         MVC   PL+1(L'PL-1),PL                                                  
         CLI   SYS,C'P'                                                         
         BNE   BP5                                                              
*                                                                               
*                                  PRINTPAK KEY                                 
         MVC   0(7,R3),0(R2)       AGM/CLT                                      
         MVI   3(R3),C'/'                                                       
         MVI   7(R3),C'/'                                                       
         MVC   8(3,R3),7(R2)                                                    
         MVI   11(R3),C'/'                                                      
         EDIT  (B2,10(R2)),(3,12(R3)),FILL=0                                    
         B     BP8                                                              
*                                                                               
BP5      DS    0H                  SPOTPAK KEY                                  
*                                                                               
BP8      DS    0H                                                               
         GOTO1 PRNT                                                             
         GOTO1 PRNT                SKIP A LINE                                  
*                                                                               
BP9      DS    0H                                                               
         LA    R4,33(R2)                                                        
         CLI   SYS,C'P'                                                         
         BE    *+8                                                              
         LA    R4,24(R2)                                                        
*                                                                               
         USING BKELEM,R4                                                        
BP10     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    BP40                EOR                                          
         CLI   0(R4),X'21'         TODAY                                        
         BE    BP12                                                             
         CLI   0(R4),X'22'         REG                                          
         BE    BP14                                                             
         CLI   0(R4),X'23'         SPEC                                         
         BE    BP16                                                             
*                                                                               
         CLI   0(R4),X'41'         PC TODAY                                     
         BE    BPPC12                                                           
*                                                                               
         CLI   0(R4),X'42'         PC REG                                       
         BE    BPPC14                                                           
*                                                                               
         CLI   0(R4),X'43'         PC SPEC                                      
         BE    BPPC16                                                           
*                                                                               
BP11     DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BP10                                                             
*                                                                               
BP12     DS    0H                  TODAY                                        
         OC    PRNT,PRNT                                                        
         BZ    BP11                SKIP PRINT                                   
         MVI   PCSW,0              TURN OFF PLANNED COSTS SWITCH                
         MVC   PLEL,=C'TD'                                                      
         GOTO1 VDATCON,DMCB,(3,BKDATE),(5,PLMOS)                                
         B     BP30                                                             
*                                                                               
BP14     DS    0H                  REG                                          
         BAS   RE,RUNTOTS                                                       
         OC    PRNT,PRNT                                                        
         BZ    BP11                SKIP PRINT                                   
         MVI   PCSW,0              TURN OFF PLANNED COSTS SWITCH                
         MVC   PLEL,=C'RG'                                                      
         B     BP18                                                             
*                                                                               
BP16     DS    0H                  SPEC                                         
         BAS   RE,RUNTOTS                                                       
         OC    PRNT,PRNT                                                        
         BZ    BP11                SKIP PRINT                                   
         MVI   PCSW,0              TURN OFF PLANNED COSTS SWITCH                
         MVC   PLEL,=C'SP'                                                      
         B     BP18                                                             
*                                                                               
BPPC12   DS    0H                  TODAY                                        
         OC    PRNT,PRNT                                                        
         BZ    BP11                SKIP PRINT                                   
         MVC   PLEL,=C'TD'                                                      
         MVI   PCSW,C'P'           TURN ON PLANNED COSTS SWITCH                 
         GOTO1 VDATCON,DMCB,(3,BKDATE),(5,PLMOS)                                
         B     BP30                                                             
*                                                                               
BPPC14   DS    0H                  REG                                          
         BAS   RE,RUNTOTS                                                       
         OC    PRNT,PRNT                                                        
         BZ    BP11                SKIP PRINT                                   
         MVI   PCSW,C'P'           TURN ON PLANNED COSTS SWITCH                 
         MVC   PLEL,=C'RG'                                                      
         B     BP18                                                             
*                                                                               
BPPC16   DS    0H                  SPEC                                         
         BAS   RE,RUNTOTS                                                       
         OC    PRNT,PRNT                                                        
         BZ    BP11                SKIP PRINT                                   
         MVI   PCSW,C'P'           TURN ON PLANNED COSTS SWITCH                 
         MVC   PLEL,=C'SP'                                                      
*                                                                               
BP18     DS    0H                                                               
         GOTO1 VDATCON,DMCB,(3,BKYM),(6,PLMOS)                                  
         GOTO1 (RF),(R1),(3,BKDATE),(5,WORK)                                    
         MVC   PLSTRT,WORK                                                      
         MVC   PLTYPE,BKTYPE                                                    
         CLI   PLTYPE,C'A'                                                      
         BNL   *+8                                                              
         OI    PLTYPE,X'F0'        MAKE VALUE READABLE                          
*                                                                               
BP30     DS    0H                                                               
         LA    R5,BKOGRS                                                        
         BAS   RE,PTOTS                                                         
         GOTO1 PRNT                                                             
         B     BP11                                                             
         SPACE 3                                                                
BP40     DS    0H                  TOTALS                                       
         OC    PRNT,PRNT                                                        
         BZ    BP60                                                             
*                                                                               
         L     R5,ATOTS                                                         
*                                                                               
         CLC   0(36,R5),36(R5)     IF RG + SP = PRINT ONLY 1 TOTAL              
         BE    BP42                                                             
         CLC   0(18,R5),=3PL6'0'   SKIP IF NO COSTS                             
*                                                                               
         MVC   PLEL,=C'RG'                                                      
         MVC   PLMOS,=C'TOTAL '                                                 
         MVI   PCSW,0              TURN OFF PLANNED COSTS SWITCH                
         L     R5,ATOTS                                                         
         BAS   RE,PTOTS                                                         
         GOTO1 PRNT                                                             
*                                                                               
         MVC   PLEL,=C'SP'                                                      
BP42     DS    0H                                                               
         MVC   PLMOS,=C'TOTAL '                                                 
         MVI   PCSW,0              TURN OFF PLANNED COSTS SWITCH                
         BAS   RE,PTOTS                                                         
         GOTO1 PRNT                                                             
*                                                                               
*        PRINT PLANNED COSTS TOTALS                                             
*                                                                               
         L     R5,ATOTS                                                         
         LA    R5,72(R5)           POINT TO REGULAR PC'S                        
*                                                                               
         CLC   0(18,R5),18(R5)     IF RG + SP = PRINT ONLY 1 TOTAL              
         BNE   BP43A                                                            
*                                                                               
         CLC   0(18,R5),=3PL6'0'   BUT SKIP IF NO COSTS                         
         BE    BP44                                                             
         B     BP43                                                             
*                                                                               
BP43A    DS    0H                                                               
*                                                                               
         MVC   PLEL,=C'RG'                                                      
         MVC   PLMOS,=C'TOTAL '                                                 
         MVI   PCSW,C'P'           TURN ON PLANNED COSTS SWITCH                 
         BAS   RE,PTOTS                                                         
         GOTO1 PRNT                                                             
*                                                                               
         MVC   PLEL,=C'SP'                                                      
*                                                                               
BP43     DS    0H                                                               
*                                                                               
         MVC   PLMOS,=C'TOTAL '                                                 
         MVI   PCSW,C'P'           TURN ON PLANNED COSTS SWITCH                 
         BAS   RE,PTOTS                                                         
         GOTO1 PRNT                                                             
*                                                                               
BP44     DS    0H                                                               
*                                                                               
         GOTO1 PRNT                                                             
*                                                                               
         GOTO1 PRNT                                                             
BP60     DS    0H                                                               
         L     R1,SAVR1                                                         
         MVC   0(24,R1),PARMS      RESTORE PARAM LIST                           
         XIT1                                                                   
*                                                                               
PTOTS    DS    0H                                                               
*                                                                               
         LA    R7,6                                                             
*                                                                               
         CLI   PCSW,C'P'           IF PLANNED COSTS                             
         BE    *+8                                                              
         CLI   BKELEM,X'41'        IF PLANNED COSTS                             
         BE    *+8                                                              
         CLI   BKELEM,X'42'        IF PLANNED COSTS                             
         BE    *+8                                                              
         CLI   BKELEM,X'43'        IF PLANNED COSTS                             
         BNE   *+8                                                              
         LA    R7,3                   PRINT ONLY 3 BUCKETS                      
*                                                                               
         LA    R6,PLAMTS                                                        
*                                                                               
PT04     DS    0H                                                               
*                                                                               
         EDIT  (P6,0(R5)),(14,1(R6)),2,FLOAT=-                                  
*                                                                               
         CLI   PCSW,C'P'           IF PLANNED COSTS                             
         BE    *+8                                                              
         CLI   BKELEM,X'41'        IF PLANNED COSTS                             
         BE    *+8                                                              
         CLI   BKELEM,X'42'        IF PLANNED COSTS                             
         BE    *+8                                                              
         CLI   BKELEM,X'43'        IF PLANNED COSTS                             
         BNE   *+8                                                              
         MVI   15(R6),C'P'            FLAG                                      
         LA    R5,6(R5)                                                         
         LA    R6,15(R6)                                                        
         BCT   R7,PT04                                                          
         BR    RE                                                               
         SPACE 3                                                                
RUNTOTS  DS    0H                                                               
         L     R1,ATOTS                                                         
         LA    R0,6                                                             
*                                                                               
         CLI   BKELEM,X'23'                                                     
         BNE   *+8                                                              
         LA    R1,36(R1)                                                        
*                                                                               
         CLI   BKELEM,X'42'                                                     
         BNE   *+12                                                             
         LA    R0,3                                                             
         LA    R1,72(R1)                                                        
*                                                                               
         CLI   BKELEM,X'43'                                                     
         BNE   *+12                                                             
         LA    R0,3                                                             
         LA    R1,90(R1)                                                        
*                                                                               
         LA    R5,BKOGRS                                                        
RT04     DS    0H                                                               
         AP    0(6,R1),0(6,R5)                                                  
         LA    R1,6(R1)                                                         
         LA    R5,6(R5)                                                         
         BCT   R0,RT04                                                          
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
BPWK     DS    0D                                                               
PARMS    DS    0CL24                                                            
VREC     DS    A                                                                
         ORG   VREC                                                             
SYS      DS    XL1                                                              
         DS    XL3                                                              
APL      DS    A                                                                
ATOTS    DS    A                                                                
VDATCON  DS    V                                                                
PRNT     DS    V                                                                
*                                                                               
         DS    F                                                                
SAVR1    DS    F                                                                
BPWKX    EQU   *                                                                
*                                                                               
         SPACE 3                                                                
PLD      DSECT                                                                  
PL       DS    0CL110                                                           
PLEL     DS    CL2                                                              
         DS    CL1                                                              
PLMOS    DS    CL6                                                              
         DS    CL1                                                              
PLSTRT   DS    CL5                                                              
         DS    CL1                                                              
PLTYPE   DS    CL1                                                              
         DS    CL3                                                              
PLAMTS   DS    6CL15                                                            
         SPACE 3                                                                
*                                                                               
PPF402   CSECT                                                                  
*                                                                               
BKTREC   DS    4096X                                                            
BKTREC2  DS    4096X                                                            
*                                                                               
         SPACE 3                                                                
PPF4WK   DSECT                                                                  
*                                                                               
ABKTREC  DS    A                                                                
ABKTREC2 DS    A                                                                
ESTACT   DS    C                                                                
***TESTBUY***                                                                   
TESTSW   DS    CL1                                                              
***TESTBUY***                                                                   
STEWSW   DS    CL1                 C'Y' = STEWARDSHIP ESTIMATE                  
PCSW     DS    CL1                 C'P' - PLANNED COSTS                         
ELCODE   DS    XL1                                                              
X        DS    XL100                                                            
*                                                                               
#ACCUMS  EQU   18                                                               
*                                                                               
WTOTS    DS    (#ACCUMS)PL6        RETURNED BY BUCKPRT                          
*                                  6 BILL MONTH/6 INS MONTH                     
*                                  3 PC BILL MONTH/3 PC INS MONTH               
ETOTS    DS    (#ACCUMS)PL8                                                     
CTOTS    DS    (#ACCUMS)PL8                                                     
RTOTS    DS    (#ACCUMS)PL8                                                     
*                                                                               
ACCUML   EQU   (#ACCUMS)*8                                                      
*                                                                               
         DS    0F                  ALIGNMENT                                    
PCVALS   DS    XL96        RETURN AREA FOR PLANNED COST                         
*                             CALL TO GETINS                                    
*                             USE GROSS ETC AS DSECT                            
         SPACE 2                                                                
PBKRECD  DSECT                                                                  
       ++INCLUDE PBKREC                                                         
*                                                                               
BKELEMD  DSECT                                                                  
       ++INCLUDE DDBKELEM                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044PPREPF402 01/05/16'                                      
         END                                                                    
