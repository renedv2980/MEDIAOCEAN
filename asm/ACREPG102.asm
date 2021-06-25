*          DATA SET ACREPG102  AT LEVEL 037 AS OF 05/01/02                      
*PHASE ACG102A                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'GENERAL LEDGER BUCKET REPORT'                                   
ACG102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACG1**,RR=R5                                                 
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
*                                                                               
         USING ACG102D,RC                                                       
*                                                                               
         L     R1,=V(PERVERT)                                                   
         AR    R1,R5                                                            
         ST    R1,PERVERT                                                       
         EJECT ,                                                                
GLA      CLI   MODE,REQFRST                                                     
         BNE   GLB                                                              
         SPACE 1                                                                
*              BUILD PDATES FROM QSTART AND QEND                                
         SPACE 1                                                                
GLADEF   DS    0H                                                               
         MVC   WORK(4),QEND        CONVERT QEND TO PACKED                       
         MVC   WORK+4,=CL2'01'                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,DUB)                                     
*                                                                               
         LA    R1,PDATES                                                        
         EX    R0,PMOVE                                                         
         LA    R1,PDATES+2                                                      
         EX    R0,PMOVE                                                         
         LA    R1,PDATES+10                                                     
         EX    R0,PMOVE                                                         
         SPACE 1                                                                
*                                  FIND THIS MONTH LAST YEAR                    
         GOTO1 ADDAY,DMCB,(C'Y',WORK),(0,WORK+6),F'-1'                          
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,DUB)                                   
*                                                                               
         LA    R1,PDATES+4                                                      
         EX    R0,PMOVE                                                         
         LA    R1,PDATES+6                                                      
         EX    R0,PMOVE                                                         
         LA    R1,PDATES+14                                                     
         EX    R0,PMOVE                                                         
         SPACE 1                                                                
         MVC   WORK(4),QSTART      CONVERT QSTART TO PACKED                     
         MVC   WORK+4,=CL2'01'                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,DUB)                                     
*                                                                               
         LA    R1,PDATES+8                                                      
         EX    R0,PMOVE                                                         
*                                  FIND THIS MONTH LAST YEAR                    
         GOTO1 ADDAY,DMCB,(C'Y',WORK),(0,WORK+6),F'-1'                          
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,DUB)                                   
*                                                                               
         LA    R1,PDATES+12                                                     
         EX    R0,PMOVE                                                         
         SPACE 1                                                                
         MVC   PERIOD,SPACES                                                    
         MVC   WORK(4),QSTART      CONVERT QSTART TO MMM/YY                     
         MVC   WORK+4,=CL2'01'                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(9,PERIOD)                                  
*                                                                               
         MVC   PERIOD+7(2),=C'TO'                                               
*                                                                               
         MVC   WORK+6(4),QEND        CONVERT QEND TO MMM/YY                     
         MVC   WORK+10,=CL2'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(9,PERIOD+10)                             
*                                                                               
         SPACE 1                                                                
         MVI   DROPSW,C'N'                                                      
*                                  CONVERT QSTART TO PACKED                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,PSTART)                                  
*                                                                               
*                                  CALCULATE THE NUMBER OF MONTHS               
*                                  FROM QSTART TO QEND                          
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         CLC   DMCB+14,=H'54'      MAXIMUM BUCKETS IS 54                        
         BNH   *+8                 IF REQUEST FOR MORE THAN 54 MONTHS           
         MVI   DROPSW,C'Y'         ASSUME WE DROPPED SOME                       
         B     GLEXIT                                                           
         SPACE 1                                                                
PMOVE    MVC   0(2,R1),DUB                                                      
         EJECT ,                                                                
GLB      CLI   MODE,LEDGFRST                                                    
         BNE   GLC                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         MVI   REQTOTSW,C'N'                                                    
         SPACE 1                                                                
         XC    LEVACNT(4*L'LEVACNT),LEVACNT                                     
         SPACE 1                                                                
         LA    R1,ACCUMTAB         CLEAR ACCUMTAB                               
         LA    R2,5                4 LEVELS & A REQUEST TOTAL                   
         SPACE 1                                                                
GLB02    MVC   0(48,R1),SPACES                                                  
         LA    R3,48(,R1)                                                       
         LA    R4,7                7 COLUMNS OF FIGURES                         
         SPACE 1                                                                
         ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(,R3)                                                        
         BCT   R4,*-10                                                          
         LA    R1,L'ACCUMTAB(,R1)                                               
         BCT   R2,GLB02                                                         
         MVC   ACCUMTAB(14),=C'REQUEST TOTALS'                                  
         SPACE 1                                                                
         L     R4,ADLDGHIR                                                      
*                                                                               
         USING ACHEIRD,R4                                                       
*                                                                               
         LA    R1,ACHRLEVA                                                      
         SR    R2,R2                                                            
         LA    R3,4                                                             
         SPACE 1                                                                
GLB04    CLI   0(R1),0             COUNT THE LEVELS                             
         BE    GLB06                                                            
         LA    R2,1(,R2)                                                        
         LA    R1,16(,R1)                                                       
         BCT   R3,GLB04                                                         
         SPACE 1                                                                
         DROP  R4                  KEEP IT CLEAN                                
         SPACE 1                                                                
GLB06    SR    R3,R3               R2 NOW = NUMBER OF LEVELS                    
         TM    QOPT1,X'F0'         REQUESTED LEVEL OF DETAIL                    
         BNO   GLB08                                                            
         MVC   BYTE,QOPT1                                                       
         NI    BYTE,X'0F'          CVB                                          
         IC    R3,BYTE                                                          
         CR    R3,R2                                                            
         BL    GLB10                                                            
         SPACE 1                                                                
GLB08    LR    R3,R2               DEFAULT IS TO SHOW ALL LEVELS                
         SPACE 1                                                                
GLB10    SLL   R3,28                                                            
         SLDL  R2,4                IS THIS EXTREMELY CLEVER                     
         STC   R2,ELCODE                                                        
         LA    R4,LEVTAB                                                        
*                                                                               
         USING LEVTABD,R4                                                       
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         CLI   LEVTOPT,C'Z'        IF Z THE IGNORE POSSIBLE OPTIONS             
         BE    GLB12                                                            
         SPACE 1                                                                
GLB12    ST    R4,ALEVEL                                                        
         SPACE 1                                                                
         MVI   ELCODE,C'Z'         DEFAULT IS Z                                 
         LA    R4,FACTAB                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         ST    R4,AFACT                                                         
         B     GLEXIT                                                           
         SPACE 2                                                                
         GETEL R4,0,ELCODE                                                      
         SPACE 2                                                                
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
GLC      CLI   MODE,PROCACC                                                     
         BNE   GLC1                                                             
         MVC   OLDBUCK,=2X'FF'                                                  
         ZAP   TOTBUCK,=P'0'                                                    
         B     GLEXIT                                                           
         SPACE 1                                                                
GLC1     CLI   MODE,PROCLEVA                                                    
         BNE   GLD                                                              
         SPACE 1                                                                
         MVI   BYTE,C'A'                                                        
         BAS   RE,FINDLEV                                                       
         SPACE 1                                                                
         BAS   RE,BUCKZAP                                                       
         SPACE 1                                                                
         L     R6,ADHEIRA          GET KEY ADDRESS                              
         L     R7,ADLVANAM         AND NAME ADDRESS                             
         BAS   RE,UPLEV            SET UP NEW NAME & NO                         
         SPACE 1                                                                
         B     GLEXIT                                                           
         SPACE 2                                                                
GLD      CLI   MODE,PROCLEVB                                                    
         BNE   GLE                                                              
         SPACE 1                                                                
         MVI   BYTE,C'B'                                                        
         BAS   RE,FINDLEV                                                       
         SPACE 1                                                                
         BAS   RE,BUCKZAP                                                       
         SPACE 1                                                                
         L     R6,ADHEIRB          GET KEY ADDRESS                              
         L     R7,ADLVBNAM         AND NAME ADDRESS                             
         BAS   RE,UPLEV                                                         
         SPACE 1                                                                
         B     GLEXIT                                                           
         SPACE 2                                                                
GLE      CLI   MODE,PROCLEVC                                                    
         BNE   GLF                                                              
         SPACE 1                                                                
         MVI   BYTE,C'C'                                                        
         BAS   RE,FINDLEV                                                       
         SPACE 1                                                                
         BAS   RE,BUCKZAP                                                       
         SPACE 1                                                                
         L     R6,ADHEIRC          GET KEY ADDRESS                              
         L     R7,ADLVCNAM         AND NAME ADDRESS                             
         BAS   RE,UPLEV                                                         
         SPACE 1                                                                
         B     GLEXIT                                                           
         SPACE 2                                                                
GLF      CLI   MODE,PROCLEVD                                                    
         BNE   GLG                                                              
         SPACE 1                                                                
         MVI   BYTE,C'D'                                                        
         BAS   RE,FINDLEV                                                       
         SPACE 1                                                                
         BAS   RE,BUCKZAP                                                       
         SPACE 1                                                                
         L     R6,ADACC            GET KEY ADDESS                               
         L     R7,ADACCNAM         AND NAME ADDRESS                             
         BAS   RE,UPLEV                                                         
         SPACE 1                                                                
         B     GLEXIT                                                           
         EJECT ,                                                                
GLG      CLI   MODE,PROCHIST       DO WE HAVE A HISTORY RECORD ?                
         BNE   GLH                 NO, SEE IF AT ACCLAST                        
         L     R6,ADTRANS          YES, GET ADDRESS OF RECORD                   
*                                                                               
         USING TRHISTD,R6                                                       
*                                                                               
         CLI   TRHSEL,X'45'                                                     
         BNE   GLEXIT                                                           
         CLC   TRHSYEAR(2),OLDBUCK   KEEP OLDEST BUCK YYMM                      
         BH    *+10                                                             
         MVC   OLDBUCK,TRHSYEAR                                                 
         ZAP   DUB,TRHSDR                                                       
         SP    DUB,TRHSCR                                                       
         AP    TOTBUCK,DUB         ADD UP ALL BUCKETS                           
         BAS   RE,GLG01                                                         
         B     GLEXIT                                                           
         SPACE 1                                                                
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING FACTD,R4                                                         
         SPACE 1                                                                
GLG01    NTR1                                                                   
         L     R4,AFACT                                                         
         LA    R1,PWORK                                                         
         ZIC   R2,FACCOLS                                                       
         LA    R3,FACINT                                                        
         SPACE 1                                                                
         LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                HORRIBLE ERROR                               
         SPACE 1                                                                
GLG02    BAS   RE,INTRPRT          USE FACINT TO UPDATE BUCKET                  
         LA    R3,L'FACINT(,R3)                                                 
         LA    R1,L'PWORK(,R1)                                                  
         BCT   R2,GLG02                                                         
         SPACE 1                                                                
         LA    R3,ACCUMTAB                                                      
         LA    R5,5                                                             
         SPACE 1                                                                
GLG04    CLC   0(48,R3),SPACES     CHECK FOR ACCOUNT NAME & NO                  
         BE    GLG08                                                            
         ZIC   R2,FACCOLS                                                       
         LA    R1,PWORK                                                         
         LA    R7,48(,R3)                                                       
         SPACE 1                                                                
GLG06    AP    0(8,R7),0(8,R1)                                                  
         LA    R1,L'PWORK(,R1)                                                  
         LA    R7,L'PWORK(,R7)                                                  
         BCT   R2,GLG06                                                         
         SPACE 1                                                                
GLG08    LA    R3,L'ACCUMTAB(,R3)                                               
         BCT   R5,GLG04                                                         
         SPACE 1                                                                
         B     GLEXIT                                                           
         SPACE 1                                                                
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING TRHISTD,R6                                                       
         SPACE 1                                                                
INTRPRT  NTR1                                                                   
         ZAP   0(L'PWORK,R1),=P'0'                                              
         CLC   0(2,R3),=C'PD'      PD FOR PDATES                                
         BE    INT10                                                            
         SPACE 1                                                                
         B     EXIT                                                             
         SPACE 1                                                                
INT10    LA    R2,PDATES                                                        
         IC    R3,2(,R3)                                                        
         SLL   R3,28                                                            
         SRL   R3,28                                                            
         SPACE 1                                                                
         LA    R2,L'PDATES(,R2)                                                 
         BCT   R3,*-4                                                           
         LA    R3,L'PDATES                                                      
         SR    R2,R3                                                            
         SPACE 1                                                                
         CLC   TRHSYEAR(2),0(R2)                                                
         BL    EXIT                                                             
         CLC   TRHSYEAR(2),2(R2)                                                
         BH    EXIT                                                             
         SPACE 1                                                                
         AP    0(L'PWORK,R1),DUB                                                
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
GLH      CLI   MODE,ACCLAST                                                     
         BNE   GLI                                                              
         SPACE 1                                                                
         CLI   DROPSW,C'Y'                                                      
         BNE   GLH4                BRANCH IF NO OLD BUCKEST LOST                
         CLI   OLDBUCK,X'FF'                                                    
         BE    GLH4                NO OLD BUCKETS                               
         L     R2,ADACCBAL                                                      
         LTR   R2,R2                                                            
         BZ    GLH4                NO BALANCE ELEMENT                           
*                                                                               
         USING ACBALD,R2                                                        
*                                                                               
         ZAP   DUB,ACBLFRWD        BALANCE                                      
         AP    DUB,ACBLDR          PLUS DEBITS                                  
         SP    DUB,ACBLCR          LESS CREDITS                                 
         SP    DUB,TOTBUCK         LESS TOTAL BUCKETS                           
         CP    DUB,=P'0'                                                        
         BE    GLH4                GIVES DIFFERENCE(BUCKETS LOST)               
         LA    R6,OLDBUCK-2                                                     
         BAS   RE,GLG01            ADD IN OLD BUCKETS                           
         SPACE 1                                                                
         DROP  R2                  KEEP IT CLEAN                                
         SPACE 1                                                                
         USING LEVTABD,R4                                                       
         SPACE 1                                                                
GLH4     L     R4,ALEVEL                                                        
         ZIC   R1,LEVTLDG                                                       
         SRL   R1,4                                                             
         SPACE 1                                                                
         LA    R2,LEVTPROF                                                      
         LA    R2,L'LEVTPROF(,R2)                                               
         BCT   R1,*-4                                                           
         SPACE 1                                                                
         LA    R1,L'LEVTPROF                                                    
         SR    R2,R1                                                            
         ST    R2,ALEV             FOUND THE PROF FOR THIS LEVEL                
         SPACE 1                                                                
         BAS   RE,PRINTLEV                                                      
         B     GLEXIT                                                           
         SPACE 2                                                                
GLI      CLI   MODE,LEVCLAST                                                    
         BNE   GLJ                                                              
         SPACE 1                                                                
         MVI   BYTE,C'C'                                                        
         BAS   RE,FINDLEV                                                       
         BAS   RE,PRINTLEV                                                      
         XC    LEVDCNT,LEVDCNT                                                  
         B     GLEXIT                                                           
         SPACE 2                                                                
GLJ      CLI   MODE,LEVBLAST                                                    
         BNE   GLK                                                              
         SPACE 1                                                                
         MVI   BYTE,C'B'                                                        
         BAS   RE,FINDLEV                                                       
         BAS   RE,PRINTLEV                                                      
         XC    LEVCCNT,LEVCCNT                                                  
         B     GLEXIT                                                           
         SPACE 2                                                                
GLK      CLI   MODE,LEVALAST                                                    
         BNE   GLL                                                              
         SPACE 1                                                                
         MVI   BYTE,C'A'                                                        
         BAS   RE,FINDLEV                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRINTLEV                                                      
         XC    LEVBCNT,LEVBCNT                                                  
         B     GLEXIT                                                           
         SPACE 1                                                                
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
GLL      CLI   MODE,REQLAST                                                     
         BNE   GLEXIT                                                           
         SPACE 1                                                                
         MVI   REQTOTSW,C'Y'                                                    
         BAS   RE,PRINTLEV                                                      
         B     GLEXIT                                                           
         SPACE 2                                                                
GLEXIT   XMOD1 1                                                                
         SPACE 2                                                                
EXIT     XIT1                                                                   
         EJECT ,                                                                
         SPACE 1                                                                
         USING LEVTABD,R6                                                       
         SPACE 1                                                                
FINDLEV  NTR1                                                                   
         L     R6,ALEVEL                                                        
         ZIC   R1,LEVTLDG                                                       
         SRL   R1,4                GIVES NO OF LEVELS IN R1                     
         LA    R4,LEVTPROF                                                      
         SPACE 1                                                                
FIND10   CLC   BYTE,0(R4)                                                       
         BE    FINDEX                                                           
         LA    R4,L'LEVTPROF(,R4)                                               
         BCT   R1,FIND10                                                        
         DC    H'0'                HORRIBLE ERROR                               
         SPACE 1                                                                
FINDEX   ST    R4,ALEV                                                          
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING FACTD,R6                                                         
         SPACE 1                                                                
PRINTLEV NTR1                                                                   
         L     R6,AFACT                                                         
         MVC   RCSUBPRG,FACSPROG                                                
         SPACE 1                                                                
         CLI   REQTOTSW,C'Y'                                                    
         BNE   PRINT10                                                          
         LA    R2,ACCUMTAB                                                      
         MVI   SPACING,2                                                        
         MVC   HEAD3+79(L'PERIOD),PERIOD                                        
         GOTO1 ACREPORT                                                         
         LA    R5,P                                                             
         MVC   1(14,R5),=C'REQUEST TOTALS'                                      
         B     PRINT30                                                          
         SPACE 1                                                                
         USING LEVD,R4                                                          
         SPACE 1                                                                
PRINT10  L     R4,ALEV                                                          
         CLI   LEVWANT,C'Y'                                                     
         BNE   EXIT                                                             
         SPACE 1                                                                
         ZIC   R1,LEVLET                                                        
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         LA    R2,ACCUMTAB                                                      
         LA    R2,L'ACCUMTAB(,R2)                                               
         BCT   R1,*-4                                                           
         SPACE 1                                                                
         CLC   0(48,R2),SPACES                                                  
         BE    EXIT                                                             
         SPACE 1                                                                
         BAS   RE,DOIPRINT                                                      
         CLI   PRINTSW,C'Y'                                                     
         BNE   EXIT                                                             
         SPACE 1                                                                
         LA    R5,P                                                             
         CLI   LEVULIN,C'Y'                                                     
         BNE   *+8                                                              
         LA    R5,PSECOND                                                       
         MVC   1(12,R5),0(R2)                                                   
         SPACE 1                                                                
         CLI   LEVULIN,C'Y'                                                     
         BNE   PRINT20                                                          
         SPACE 1                                                                
         BAS   RE,UNDRLINE                                                      
         SPACE 1                                                                
PRINT20  GOTO1 CHOPPER,DMCB,(36,12(R2)),(16,14(R5)),(C'P',3)                    
         SPACE 1                                                                
PRINT30  BAS   RE,FIXTOTS                                                       
         BAS   RE,EDITOR                                                        
         MVC   HEAD3+79(L'PERIOD),PERIOD                                        
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING LEVD,R4                                                          
         SPACE 1                                                                
DOIPRINT NTR1                                                                   
         MVI   PRINTSW,C'N'                                                     
         CLI   LEVLOW,C'Y'                                                      
         BNE   DOI10                                                            
         SPACE 1                                                                
         CLC   48(56,R2),=7PL8'0'                                               
         BNE   DOIYES                                                           
         B     EXIT                                                             
         SPACE 1                                                                
DOI10    ZIC   R1,LEVLET                                                        
         SLL   R1,28                                                            
         SRL   R1,26                                                            
         EX    R0,LACNT(R1)                                                     
         SPACE 1                                                                
         OC    0(2,R3),0(R3)                                                    
         BNZ   DOIYES                                                           
         B     EXIT                                                             
         SPACE 1                                                                
DOIYES   MVI   PRINTSW,C'Y'                                                     
         ZIC   R1,LEVLET                                                        
         BCTR  R1,0                                                             
         SLL   R1,28                                                            
         SRL   R1,26                                                            
         EX    R0,LACNT(R1)                                                     
         LH    R1,0(,R3)                                                        
         AH    R1,=H'1'                                                         
         STH   R1,0(,R3)                                                        
         B     EXIT                                                             
         SPACE 2                                                                
LACNT    LA    R3,LEVACNT                                                       
         LA    R3,LEVBCNT                                                       
         LA    R3,LEVCCNT                                                       
         LA    R3,LEVDCNT                                                       
         SPACE 1                                                                
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
UNDRLINE NTR1                                                                   
         LA    R3,L'P+1(,R5)       PT TO START OF UNDERLINING                   
         LA    R2,11                                                            
UNDR10   LA    R4,1(R2,R5)                                                      
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,UNDR10                                                        
         SPACE 1                                                                
         EX    R2,UNDRMOVE                                                      
         B     EXIT                                                             
         SPACE 1                                                                
UNDRMOVE MVC   0(0,R3),=12C'*'                                                  
         EJECT ,                                                                
         SPACE 1                                                                
         USING FACTD,R4                                                         
         SPACE 1                                                                
EDITOR   NTR1                                                                   
         L     R4,AFACT                                                         
         LA    R5,30(,R5)          R5 PTS TO PRINTLINE                          
         LA    R2,48(,R2)          R2 PTS TO LINE IN ACCUMTAB                   
         ZIC   R3,FACSPACE                                                      
         ZIC   R1,FACCOLS                                                       
         SPACE 1                                                                
ED10     ZAP   DUB,0(8,R2)                                                      
         AP    DUB,=P'50'                                                       
         CP    DUB,=P'0'                                                        
         BH    *+10                                                             
         SP    DUB,=P'100'                                                      
         DP    DUB,=P'100'                                                      
         ZAP   0(8,R2),DUB(6)                                                   
         SPACE 1                                                                
         EDIT  (P8,(R2)),(11,(R5)),0,MINUS=YES                                  
         SPACE 1                                                                
         LA    R5,0(R3,R5)                                                      
         LA    R2,8(,R2)                                                        
         BCT   R1,ED10                                                          
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING FACTD,R4                                                         
         SPACE 1                                                                
FIXTOTS  NTR1                                                                   
         L     R4,AFACT                                                         
         LA    R2,48(,R2)          R2 NOW PTS TO RELEVANT ACCUMULATORS          
         LR    R7,R2                                                            
         ZIC   R1,FACCOLS                                                       
         LA    R3,FACINT                                                        
         SPACE 1                                                                
FIXTOT10 CLI   1(R3),C'-'          LOOK FOR ARITHMETIC OPERATOR                 
         BNE   FIXTOT20                                                         
         SPACE 1                                                                
         ZIC   R5,0(,R3)           CONVERT TO BINARY & MULT BY 8                
         ZIC   R6,2(,R3)                                                        
         BCTR  R5,0                                                             
         BCTR  R6,0                                                             
         SLL   R5,28                                                            
         SRL   R5,25                                                            
         SLL   R6,28                                                            
         SRL   R6,25                                                            
         SPACE 1                                                                
         LA    R5,0(R5,R2)                                                      
         LA    R6,0(R6,R2)                                                      
         ZAP   DUB,0(8,R5)                                                      
         SP    DUB,0(8,R6)                                                      
         ZAP   0(8,R7),DUB                                                      
         SPACE 1                                                                
         SPACE 1                                                                
FIXTOT20 LA    R7,8(,R7)                                                        
         LA    R3,L'FACINT(,R3)                                                 
         BCT   R1,FIXTOT10                                                      
         SPACE 1                                                                
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING LEVD,R4                                                          
         SPACE 1                                                                
BUCKZAP  NTR1                                                                   
         L     R4,ALEV                                                          
         ZIC   R1,LEVLET                                                        
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         SPACE 1                                                                
         LA    R2,ACCUMTAB                                                      
         LA    R2,L'ACCUMTAB(,R2)                                               
         BCT   R1,*-4                                                           
         SPACE 1                                                                
         LA    R2,48(,R2)                                                       
         LA    R1,7                                                             
BUCK10   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(,R2)                                                        
         BCT   R1,BUCK10                                                        
         SPACE 1                                                                
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING LEVD,R4                                                          
         SPACE 1                                                                
UPLEV    NTR1                                                                   
         L     R4,ALEV                                                          
         CLI   LEVWANT,C'Y'                                                     
         BNE   EXIT                                                             
         SPACE 1                                                                
         ZIC   R1,LEVLET                                                        
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         LA    R2,ACCUMTAB                                                      
         LA    R2,L'ACCUMTAB(,R2)                                               
         BCT   R1,*-4                                                           
         SPACE 1                                                                
         MVC   0(48,R2),SPACES                                                  
         MVC   0(12,R2),3(R6)      SAVE THE KEY                                 
         LR    R4,R7               NOW GET THE NAME                             
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,UPLEVMV                                                       
         B     EXIT                                                             
*                                                                               
UPLEVMV  MVC   12(0,R2),ACNMNAME                                                
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              LEVEL TABLE AND DSECTS                                           
         SPACE 2                                                                
LEVTAB   DC    X'FF02'             SO I CAN START WITH NEXTEL                   
Z1       DC    X'11',AL1(Z2-Z1),C'Z',C'AYNY'                                    
Z2       DC    X'21',AL1(Z3-Z2),C'Z',C'AYNYBNNN'                                
Z3       DC    X'22',AL1(Z4-Z3),C'Z',C'AYYNBYNY'                                
Z4       DC    X'31',AL1(Z5-Z4),C'Z',C'AYNYBNNNCNNN'                            
Z5       DC    X'32',AL1(Z6-Z5),C'Z',C'AYYNBYNYCNNN'                            
Z6       DC    X'33',AL1(Z7-Z6),C'Z',C'AYYNBYYNCYNY'                            
Z7       DC    X'41',AL1(Z8-Z7),C'Z',C'AYNYBNNNCNNNDNNN'                        
Z8       DC    X'42',AL1(Z9-Z8),C'Z',C'AYYNBYNYCNNNDNNN'                        
Z9       DC    X'43',AL1(ZA-Z9),C'Z',C'AYYNBYYNCYNYDNNN'                        
ZA       DC    X'44',AL1(ZB-ZA),C'Z',C'AYYNBYYNCYYNDYNY'                        
ZB       DC    X'0000'                                                          
         SPACE 2                                                                
LEVTABD  DSECT                                                                  
LEVTLDG  DS    CL1                 E.G. 31 MEANS 3 LEVLS,SHOW ONLY 1            
LEVTLEN  DS    CL1                                                              
LEVTOPT  DS    CL1                                                              
LEVTPROF DS    0CL4                STRING OF LEVD TYPE ELEMENTS                 
         SPACE 1                                                                
LEVD     DSECT                                                                  
LEVLET   DS    CL1                 A FOR LEVEL A ETC                            
LEVWANT  DS    CL1                 Y=WANTED                                     
LEVULIN  DS    CL1                 Y=UNDERLINE THIS LEVEL IN REPORT             
LEVLOW   DS    CL1                 Y=THIS IS LOWEST LEVEL                       
         EJECT ,                                                                
ACG102   CSECT                                                                  
*              PRINT HANDLING & ACCUMULATOR UPDATE TABLE                        
FACTAB   DC    X'FF02'                                                          
FZ1      DC    C'Z',AL1(FZ2-FZ1),X'0',X'0F',X'05',C'PD1',C'PD2',C'PD3',*        
               C'PD4',C'3-4'                                                    
FZ2      DC    X'0000'                                                          
         SPACE 2                                                                
FACTD    DSECT                                                                  
FACOPT   DS    CL1                                                              
FACLEN   DS    CL1                                                              
FACSPROG DS    CL1                 SUBPRG VALUE FOR THIS OPTION                 
FACSPACE DS    CL1                 SPACING OF FIGURES ON THE REPORT             
FACCOLS  DS    CL1                 NO OF COLS OF FIGURES                        
FACINT   DS    0CL3                MEANING OF NTH COL E.G. PD2                  
*                                  SPECIFIES PERIOD COVERED BY 2ND              
*                                  PDATES ELEMENT,3-4 GIVES COL3-COL4           
         EJECT ,                                                                
ACG102   CSECT                                                                  
         LTORG                                                                  
         EJECT ,                                                                
ACG102D  DSECT                                                                  
         SPACE 1                                                                
ACCUMTAB DS    0CL104                                                           
         DS    CL12,CL36,7PL8      ACCOUNT NO NAME FIGURES                      
         DS    CL12,CL36,7PL8                                                   
         DS    CL12,CL36,7PL8                                                   
         DS    CL12,CL36,7PL8                                                   
         DS    CL12,CL36,7PL8                                                   
         DS    CL12,CL36,7PL8                                                   
         SPACE 1                                                                
PDATES   DS    7PL4                                                             
PERIOD   DS    CL16                                                             
AFACT    DS    A                                                                
ALEVEL   DS    A                                                                
ALEV     DS    A                                                                
LEVACNT  DS    H                                                                
LEVBCNT  DS    H                                                                
LEVCCNT  DS    H                                                                
LEVDCNT  DS    H                                                                
PWORK    DS    7PL8                                                             
REQTOTSW DS    CL1                                                              
ELCODE   DS    CL1                                                              
PRINTSW  DS    CL1                                                              
OLDBUCK  DS    PL2                OLDEST BUCKET FOUND YYMM PACKED               
TOTBUCK  DS    PL8                SUM OF ALL ACCOUNT BUCKETS                    
PSTART   DS    PL3                YYMMDD PACKED                                 
DROPSW   DS    CL1                                                              
PERVERT  DS    A                                                                
         EJECT ,                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACREPG102 05/01/02'                                      
         END                                                                    
