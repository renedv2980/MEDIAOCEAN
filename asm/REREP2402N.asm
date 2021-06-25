*          DATA SET REREP2402N AT LEVEL 054 AS OF 04/09/99                      
*PHASE RE2402N,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'MODULE TO FIND DUPLICATE CONTRACTS'                             
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 11NOV97 (SKU) DATE OF CONCEPTION                                *             
*                                                                 *             
* TEMP VERSION WILL BE DELETED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    TOTAL,TOTAL                                                      
         OPEN  (FILEIN,(INPUT))                                                 
*                                                                               
PCGET    DS    0H                                                               
         LA    R4,INCONREC-4                                                    
         GET   FILEIN,(R4)                                                      
*                                                                               
         XC    KEY,KEY             READ ALL CONTRACT RECORDS                    
         MVC   KEY(27),INCONREC                                                 
*&&DO                                                                           
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),INCONREC+2                                             
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),INCONREC+23                                               
         MVI   DUB+4,X'0F'                                                      
         SRP   DUB(5),64-1,0                                                    
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB(5)                                                
         MVO   WORK(5),WORK+10(5)       CONTRACT NUM IN 9'S COMP                
*                                                                               
         MVC   KEY+23(4),WORK                                                   
*&&                                                                             
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'80'                                                     
         BO    PCX                                                              
*                                                                               
PC20     DS    0H                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PC25                                                             
         MVC   P(8),=C'MISSING:'                                                
         MVC   P+10(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
         B     PCGET                                                            
*                                                                               
PC25     DS    0H                                                               
         GOTO1 (RF),(R1),(X'88',GETREC),REPFILE,KEY+28,CONAREA,DMWORK           
*                                                                               
         LA    R0,CONAREA          SET LENGTH AND ADDRESS OF RECORDS            
         ZICM  R1,CONAREA+27,2                                                  
         LA    RE,INCONREC                                                      
         ZICM  RF,INCONREC+27,2                                                 
         CLCL  R0,RE                                                            
         BNE   PC30                                                             
         MVC   P(12),=C'EXACT MATCH:'                                           
         MVC   P+16(2),CONAREA+2                                                
         GOTO1 =V(HEXOUT),DMCB,CONAREA+23,P+20,4                                
         AP    EXACTC,=P'1'                                                     
*        GOTO1 REPORT                                                           
         B     PCGET                                                            
*                                                                               
PC30     DS    0H                                                               
         LA    R6,CONAREA                                                       
         USING RCONREC,R6                                                       
*                                                                               
         CLC   RCONMODD,=X'630401'                                              
         BH    PC85                                                             
*                                                                               
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   RCONSRD3,=X'C67D'   MAR29/99                                     
         BL    PC40                                                             
         CLC   RCONSRD3,=X'C681'   APR01/99 APRIL FOOL!!                        
         BNH   PCGET                                                            
PC40     CLC   RCONSRD2,=X'C67D'   MAR29/99                                     
         BL    PC50                                                             
         CLC   RCONSRD2,=X'C681'   APR01/99 APRIL FOOL!!                        
         BNH   PCGET                                                            
PC50     CLC   RCONSRD1,=X'C67D'   MAR29/99                                     
         BL    PC60                                                             
         CLC   RCONSRD1,=X'C681'   APR01/99 APRIL FOOL!!                        
         BNH   PCGET                                                            
PC60     CLC   RCONS2D3,=X'C67D'   MAR29/99                                     
         BL    PC70                                                             
         CLC   RCONS2D3,=X'C681'   APR01/99 APRIL FOOL!!                        
         BNH   PCGET                                                            
PC70     CLC   RCONS2D2,=X'C67D'   MAR29/99                                     
         BL    PC80                                                             
         CLC   RCONS2D2,=X'C681'   APR01/99 APRIL FOOL!!                        
         BNH   PCGET                                                            
PC80     CLC   RCONS2D1,=X'C67D'   MAR29/99                                     
         BL    PC83                                                             
         CLC   RCONS2D1,=X'C681'   APR01/99 APRIL FOOL!!                        
         BNH   PCGET                                                            
*                                                                               
* WRITE NEW CONTRACT BACK                                                       
*                                                                               
PC83     DS    0H                                                               
*        GOTO1 DATAMGR,DMCB,(X'08',PUTREC),REPFILE,KEY+28,INCONREC,    X        
               DMWORK                                                           
*        TM    DMCB+8,X'FD'                                                     
*        BZ    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         ZICM  R0,INCONREC+27,2                                                 
         GOTO1 =V(PRNTBL),DMCB,0,INCONREC,C'DUMP',(R0),FORMAT                   
         GOTO1 =V(PRINTER)                                                      
         B     PC90                                                             
FORMAT   DC    C'1D'                                                            
*                                                                               
PC85     DS    0H                                                               
         LA    R6,CONAREA                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCTL10                                                           
         MVC   P+30(4),=C'DARE'                                                 
PCTL10   DS    0H                                                               
         MVC   P(9),=C'TOO LATE!'                                               
         MVC   P+16(2),CONAREA+2                                                
         GOTO1 =V(HEXOUT),DMCB,CONAREA+23,P+20,4                                
*                                                                               
         LA    R6,CONAREA                                                       
         SR    R3,R3                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
PCTL15   BNE   PCTL20                                                           
         USING RCONBKEL,R6                                                      
         ZICM  R2,RCONBKAM,4                                                    
         AR    R3,R2                                                            
         BAS   RE,NEXTEL                                                        
         B     PCTL15                                                           
         DROP  R6                                                               
*                                                                               
PCTL20   DS    0H                                                               
         EDIT  (R3),(12,P+40),2,COMMAS=YES                                      
         GOTO1 REPORT                                                           
         AP    NOGOOD,=P'1'                                                     
*                                                                               
*&&DO                                                                           
         LA    R6,INCONREC                                                      
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCTL25                                                           
         ZIC   R0,1(R6)                                                         
         GOTO1 =V(PRNTBL),DMCB,0,(R6),C'DUMP',(R0),FORMAT                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PCTL25   DS    0H                                                               
         LA    R6,CONAREA                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCTL30                                                           
         ZIC   R0,1(R6)                                                         
         GOTO1 =V(PRNTBL),DMCB,0,(R6),C'DUMP',(R0),FORMAT                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PCTL30   DS    0H                                                               
         LA    R6,CONAREA                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCTL40                                                           
         ZIC   R0,1(R6)                                                         
         GOTO1 =V(PRNTBL),DMCB,0,(R6),C'DUMP',(R0),FORMAT                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PCTL40   DS    0H                                                               
*&&                                                                             
         B     PCGET                                                            
*                                                                               
PC90     DS    0H                                                               
*&&DO                                                                           
         MVC   P(9),=C'REPLACED:'                                               
         MVC   P+16(2),INCONREC+2                                               
         GOTO1 =V(HEXOUT),DMCB,INCONREC+23,P+20,4                               
         GOTO1 REPORT                                                           
*&&                                                                             
         AP    REPLACED,=P'1'                                                   
*                                                                               
         B     PCGET                                                            
*                                                                               
PCX      DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         MVC   P(8),=C'EXACT  :'                                                
         EDIT  (P5,EXACTC),(7,P+10)                                             
         GOTO1 REPORT                                                           
         MVC   P(8),=C'NO GOOD:'                                                
         EDIT  (P5,NOGOOD),(7,P+10)                                             
         GOTO1 REPORT                                                           
         MVC   P(9),=C'REPLACED:'                                               
         EDIT  (P5,REPLACED),(7,P+10)                                           
         GOTO1 REPORT                                                           
         CLOSE (FILEIN,)                                                        
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,          DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04200,                                            X        
               MACRF=GM,                                               X        
               EODAD=EXIT                                                       
*                                                                               
*                                                                               
REPLACED DC    PL5'0'                                                           
NOGOOD   DC    PL5'0'                                                           
EXACTC   DC    PL5'0'                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
REPLIST  DC    C'BL'                                                            
         DC    X'0000'                                                          
*EPLIST  DC    C'CMD4GPIFI1I2I8I9MGRMS1TO'                                      
*                                                                               
         LTORG                                                                  
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
SYSCODE  DS    CL2                                                              
CONKEY   DS    CL(L'KEY)                                                        
BUYKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
TOTAL    DS    XL4                                                              
MYWORK   DS    CL256                                                            
         DS    D                                                                
INCONREC DS    CL4000                                                           
*                                                                               
CONAREA  DS    CL4000                                                           
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
*      ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054REREP2402N04/09/99'                                      
         END                                                                    
