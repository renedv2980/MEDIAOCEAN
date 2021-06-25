*          DATA SET ACREPLC02  AT LEVEL 009 AS OF 01/17/13                      
***********************************************************************         
*             QOPT1:'D'= TO DUMP RECORDS                              *         
*             QOPT2:'L'= COPY LEDGER RECORDS                          *         
*             QOPT3:' '= COPY ALL LEVELS                              *         
*                   'A'= COPY LEVEL A                                 *         
*                   'B'= COPY LEVEL B                                 *         
*                   'C'= COPY LEVEL C                                 *         
*                   'D'= COPY LEVEL D                                 *         
*             QOPT4:' '= DO NOT COPY DB AND CA ELEMENTS               *         
*                   'Y'= COPY DB AND CA ELEMENTS                      *         
***********************************************************************         
*PHASE ACLC02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE HEXIN                                                                  
         TITLE 'LEDGER COPY PROGRAM'                                            
ACLC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACLC**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACLCD,RC                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
         SPACE 1                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   LDGF00                                                           
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)                                      
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         XC    RLN,RLN             INIT RECORD LENGTH FIELD                     
         MVC   ELDISP,=Y(ACCRFST-ACCRECD)                                       
         LA    R5,FILEOUT                                                       
         OPEN  ((R5),OUTPUT)                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
         SPACE 1                                                                
LDGF00   CLI   MODE,LEDGFRST                                                    
         BNE   PRCA00                                                           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   LDGCNT,=P'0'                                                     
         ZAP   DMPCNT,=P'0'                                                     
         ZAP   DMPTOT,=P'0'                                                     
*                                  GET NEW CMP/UNT/LDG FROM QSELECT             
         GOTO1 HEXIN,DMCB,QSELECT,NEWC,2                                        
         MVC   NEWU(2),QSELECT+2                                                
         L     R4,ADLEDGER                                                      
         CLI   QOPT2,C'L'          COPY LEDGER RECORD                           
         BNE   XIT                                                              
         BAS   RE,COPY                                                          
         BAS   RE,PRNT                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL A                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRCA00   CLI   MODE,PROCLEVA                                                    
         BNE   PRCB00                                                           
         CLI   QOPT3,C' '          COPY LEVEL IN BLANK                          
         BE    *+8                                                              
         CLI   QOPT3,C'A'          COPY LEVEL A ONLY?                           
         BNE   XIT                                                              
         L     R4,ADHEIRA                                                       
         BAS   RE,COPY             COPY                                         
         BAS   RE,PRNT             AND PRINT                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL B                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRCB00   CLI   MODE,PROCLEVB                                                    
         BNE   PRCC00                                                           
         CLI   QOPT3,C' '          COPY LEVEL IN BLANK                          
         BE    *+8                                                              
         CLI   QOPT3,C'B'          COPY LEVEL B ONLY?                           
         BNE   XIT                                                              
         L     R4,ADHEIRB                                                       
         BAS   RE,COPY             COPY                                         
         BAS   RE,PRNT             AND PRINT                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL C                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRCC00   CLI   MODE,PROCLEVC                                                    
         BNE   PRCD00                                                           
         CLI   QOPT3,C' '          COPY LEVEL IN BLANK                          
         BE    *+8                                                              
         CLI   QOPT3,C'C'          COPY LEVEL C ONLY?                           
         BNE   XIT                                                              
         L     R4,ADHEIRC                                                       
         BAS   RE,COPY             COPY                                         
         BAS   RE,PRNT             AND PRINT                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL D                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRCD00   CLI   MODE,PROCLEVD                                                    
         BNE   LDGL00                                                           
         CLI   QOPT3,C' '          COPY LEVEL IN BLANK                          
         BE    *+8                                                              
         CLI   QOPT3,C'D'          COPY LEVEL D ONLY?                           
         BNE   XIT                                                              
         L     R4,ADACC                                                         
         BAS   RE,COPY             COPY                                         
         BAS   RE,PRNT             AND PRINT                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR LEDGER                                                               
***********************************************************************         
         SPACE 1                                                                
LDGL00   CLI   MODE,LEDGLAST                                                    
         BNE   RUNL00                                                           
         MVC   P+1(16),=C'TOTAL FOR LEDGER'                                     
         EDIT  LDGCNT,(7,P+20)                                                  
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUNLAST                                                             *         
***********************************************************************         
         SPACE 1                                                                
RUNL00   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         LA    R5,FILEOUT                                                       
         CLOSE ((R5))                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COPY RECORD                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R4                                                       
COPY     NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,DMREAD,DIR,(R4),IO                                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         MVC   DA,ACCKDA                                                        
         GOTO1 DATAMGR,DMCB,GETREC,MST,DA,IO,WORK                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         SR    R5,R5                                                            
         ICM   R5,3,ACCRLEN                                                     
         LA    R2,IO                                                            
         LR    R3,R5                                                            
         MVCL  R2,R4               RECORD TO IO AREA                            
         LA    R4,IO                                                            
         MVC   ACCKEY(3),NEWCUL    NEW C/U/L                                    
         BAS   RE,ELM              FIX ELEMENTS                                 
         SR    R5,R5                                                            
         ICM   R5,3,ACCRLEN                                                     
         AH    R5,=H'4'                                                         
         STH   R5,RLN              SET RECORD LENGTH                            
         LA    R1,FILEOUT                                                       
         LA    R2,RLN                                                           
         PUT   (R1),(R2)                                                        
         AP    LDGCNT,=P'1'                                                     
         CLI   QOPT1,C'D'          DUMP                                         
         BNE   *+8                                                              
         BAS   RE,DMPUT            DITTO THE OUTPUT                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REMOVE / FIX ELEMENTS                                               *         
***********************************************************************         
         SPACE 1                                                                
ELM      NTR1  ,                                                                
         LA    R4,ACCRFST                                                       
*                                                                               
ELM00    DS    0H                                                               
*                                                                               
         USING GLPELD,R4                                                        
ELM15    CLI   0(R4),X'15'         POSTING RULES                                
         BNE   ELM30                                                            
         CLC   GLPACC1(2),QUNIT                                                 
         BNE   *+10                                                             
         MVC   GLPACC1(2),NEWU   REPLACE SUPERLEDGER CODES(IF USED)             
         CLI   GLPLN,40                                                         
         BL    ELMX                                                             
         CLC   GLPACC2(2),QUNIT                                                 
         BNE   *+10                                                             
         MVC   GLPACC2(2),NEWU                                                  
         B     ELMX                                                             
*                                                                               
         USING RSTELD,R4                                                        
ELM30    CLI   0(R4),X'30'         STATUS ELEMENT                               
         BNE   ELM31                                                            
         MVC   RSTBDATE,TODAY                                                   
         MVC   RSTTDATE,TODAY                                                   
         B     ELMX                                                             
*                                                                               
         USING ASTELD,R4                                                        
ELM31    CLI   0(R4),X'31'         ACCOUNT STATUS ELEMENT                       
         BNE   ELM32                                                            
         MVI   ASTEL,X'FF'         DELETE ACCOUNT STATUS ELEMENT                
         B     ELMX                                                             
*                                                                               
         USING ABLELD,R4                                                        
ELM32    CLI   0(R4),X'32'         BALANCE ELEMENT                              
         BNE   ELM33                                                            
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         XC    ABLTXS,ABLTXS                                                    
         B     ELMX                                                             
*                                                                               
         USING APOELD,R4                                                        
ELM33    CLI   0(R4),X'33'         PEEL ELEMENT                                 
         BNE   ELM62                                                            
         XC    APOPLDT(6),APOPLDT PEEL ELEMENT                                  
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         B     ELMX                                                             
*                                                                               
         USING DSCELD,R4                                                        
ELM62    CLI   0(R4),X'62'         SCHEME                                       
         BNE   ELMCA                                                            
         MVI   DSCEL,X'FF'         DELETE SCHEME ELEMENTS                       
         B     ELMX                                                             
*                                                                               
         USING  APRELD,R4                                                       
ELMCA    CLI    0(R4),X'CA'        ACCOUNT EQUIVALENCY                          
         BNE    ELMDB                                                           
         MVI    APREL,X'FF'        DEFAULT = DELETE ACCOUNT EQUIV.              
         CLI    QOPT4,C'Y'         COPY ELEMENTS?                               
         BNE    ELMX               NO                                           
         CLI    QOPT2,C'L'         COPY LEDGER?                                 
         BNE    ELMX               NO,MUST BE ON SO EXIT                        
         MVI    APREL,APRELQ       FIX ELEMENT                                  
         B      ELMX                                                            
*                                                                               
         USING  FFTELD,R4                                                       
ELMDB    CLI    0(R4),X'DB'                                                     
         BNE    ELMFA                                                           
         CLI    FFTTYPE,FFTTEPTR   ACCOUNT POINTER TYPE                         
         BNE    ELMFA                                                           
         MVI    FFTEL,X'FF'        DEFAULT = DELETE ELEMENT                     
         CLI    QOPT4,C'Y'         COPY ELEMENTS?                               
         BNE    ELMX               NO                                           
         CLI    QOPT2,C'L'         COPY LEDGER?                                 
         BNE    ELMX               NO,MUST BE ON SO EXIT                        
         MVI    FFTEL,FFTELQ       FIX ELEMENT                                  
         B      ELMX                                                            
*                                                                               
         USING PTRELD,R4                                                        
ELMFA    CLI   0(R4),PTRELQ        POINTER ELEMENTS, X'FA'                      
         BNE   ELMX                                                             
         MVI   PTREL,X'FF'         DELETE                                       
*                                                                               
ELMX     SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0             END OF RECORD ?                              
         BNE   ELM00               NO, LOOP                                     
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'ACCMST'),(X'FF',IO),0                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCKEY,R4                                                        
PRNT     NTR1  ,                                                                
         LA    R4,IO                                                            
         MVC   P+1(14),ACCKEY+1    CODE                                         
         MVI   ELCODE,X'20'        AND NAME                                     
         BAS   RE,GETEL                                                         
         BNE   PRNTX                                                            
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+17(0),NAMEREC                                                  
*                                                                               
PRNTX    GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DUMP OUTPUT RECORDS                                      *         
***********************************************************************         
*                                                                               
DMPUT    NTR1  ,                                                                
         CP    DMPTOT,MAXDUMP                                                   
         BH    XIT                                                              
         AP    DMPTOT,=P'1'                                                     
         AP    DMPCNT,=P'1'                                                     
         ZAP   DUB,DMPCNT                                                       
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,RLN                                                         
         GOTO1 PRNTBL,DMCB,0,RLN,C'DUMP',(R8),=C'2D'                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MISCELLANEOUS ROUTINES                                              *         
***********************************************************************         
*                                                                               
         SPACE 1                                                                
         GETEL R4,ELDISP,ELCODE                                                 
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
DIR      DC    CL6'ACCDIR'                                                      
MST      DC    CL6'ACCMST'                                                      
GETREC   DC    CL6'GETREC'                                                      
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
LDGCNT   DC    PL6'0'                                                           
*                                                                               
DMPCNT   DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
DMPTOT   DC    PL4'0'                                                           
MAXDUMP  DC    PL4'50'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            X        
               BLKSIZE=32760,LRECL=4004,BUFNO=2                                 
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PROGRAM WORK AREA                                         *         
***********************************************************************         
*                                                                               
ACLCD    DSECT                                                                  
TODAY    DS    XL3                                                              
TODAY2   DS    CL2                                                              
DA       DS    XL4                                                              
ELCODE   DS    CL1                                                              
ELDISP   DS    H                                                                
*                                                                               
NEWCUL   DS    0CL3                NEW COMPANY/UNIT/LEDGER                      
NEWC     DS    XL1                                                              
NEWU     DS    CL1                                                              
NEWL     DS    CL1                                                              
*                                                                               
RLN      DS    F                   RECORD LENGTH                                
IO       DS    2000C                                                            
         EJECT                                                                  
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPLC02 01/17/13'                                      
         END                                                                    
