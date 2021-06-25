*          DATA SET REREP2402S AT LEVEL 108 AS OF 05/01/02                      
*PHASE RE2402A,*                                                                
         TITLE 'MODULE TO MARK BUY COMBOS'                                      
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 31MAR93 (SKU) DATE OF CONCEPTION                                *             
*                                                                 *             
*                                                                 *             
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
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,REPLIST                                                       
*                                                                               
PC10     DS    0H                                                               
         LA    R2,BUFF                                                          
         ZAP   COUNT,=P'0'                                                      
         ZAP   CCOUNT,=P'0'                                                     
         ZAP   BCOUNT,=P'0'                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING RCONKEY,R4                                                       
         XC    KEY,KEY             READ ALL CONTRACT RECORDS                    
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,0(R3)                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLC   KEYSAVE(23),KEY                                                  
         BNE   PBUY                                                             
*                                                                               
         GOTO1 (RF),(R1),(X'08',GETREC),REPFILE,KEY+28,IOAREA,DMWORK            
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'17'        CHECK IF COMBO                               
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
*                                                                               
         MVC   0(2,R2),RCONPREP    REP                                          
         MVC   2(4,R2),RCONPCON    K # (9'S COMPLEMENT)                         
         AP    CCOUNT,=P'1'                                                     
         LA    R2,6(R2)                                                         
         B     PCSEQ                                                            
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* MARK BUYS                                                                     
*********************************************************************           
PBUY     DS    0H                                                               
         LA    R2,BUFF                                                          
*                                                                               
PB10     DS    0H                                                               
         CP    COUNT(5),CCOUNT(5)  CHECK IF ALL CONTRACTS PROCESSED             
         BNL   PBXIT                                                            
*                                                                               
         MVC   P(3),=C'REP'                                                     
         MVC   P+4(2),0(R2)                                                     
         MVI   P+8,C'#'                                                         
*                                                                               
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),2(4,R2)                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5)                                             
         EDIT  (P5,WORK+5),(7,P+10),ALIGN=LEFT                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(11),=C'BUYLINE(S):'                                            
         LA    R7,P+13                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING RBUYKEY,R4                                                       
         XC    KEY,KEY             READ ALL CONTRACT RECORDS                    
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,0(R2)                                                   
         PACK  RBUYKCON(1),5(1,R2)                                              
         PACK  RBUYKCON+1(1),4(1,R2)                                            
         PACK  RBUYKCON+2(1),3(1,R2)                                            
         PACK  RBUYKCON+3(1),2(1,R2)                                            
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PBHI     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PB20                                                             
         DC    H'0'                                                             
*                                                                               
PBSEQ    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PB20     DS    0H                                                               
         CLC   KEYSAVE(22),KEY                                                  
         BNE   PB30                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         USING RBUYREC,R6                                                       
         OI    RBUYRTS,X'40'       FLAG COMBO                                   
*                                                                               
         CLI   QOPTION1,C'Y'                                                    
         BNE   PB23                                                             
         GOTO1 DATAMGR,DMCB,PUTREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PB23     DS    0H                                                               
         AP    BCOUNT,=P'1'                                                     
         EDIT  RBUYKLIN,(3,(R7)),ALIGN=LEFT                                     
         DROP  R6                                                               
*                                                                               
         LA    R7,4(R7)            IF E-O-L PRINT IT AND ADVANCE                
         LA    RF,P+126                                                         
         CR    R7,RF                                                            
         BNH   PB25                                                             
         GOTO1 REPORT                                                           
         LA    R7,P+13                                                          
*                                                                               
PB25     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PBSEQ                                                            
         DC    H'0'                                                             
*                                                                               
PB30     DS    0H                                                               
         GOTO1 REPORT                                                           
         LA    R2,6(R2)                                                         
         AP    COUNT,=P'1'                                                      
         B     PB10                                                             
*                                                                               
PBXIT    DS    0H                                                               
         MVC   P+3(15),=C'TOTAL CONTRACTS'                                      
         EDIT  (P5,CCOUNT),(7,P+17)                                             
         GOTO1 REPORT                                                           
         MVC   P+3(18),=C'TOTAL BUYS CHANGED'                                   
         EDIT  (P5,BCOUNT),(7,P+20)                                             
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'       EJECT FOR NEXT REP                           
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'FF'         ALL DONE?                                    
         BNE   PC10                                                             
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
REPLIST  DS    0CL2                                                             
         DC    C'TO',C'I1',C'HN',C'DI',C'GP',C'MG',C'I8',C'I9'                  
         DC    C'I2'                                                            
         DC    X'FF'                                                            
*                                                                               
COUNT    DC    PL5'0'                                                           
CCOUNT   DC    PL5'0'                                                           
BCOUNT   DC    PL5'0'                                                           
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
*                                                                               
IOAREA   DS    CL1000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
BUFF     DS    600000C                                                          
         SPACE 2                                                                
*  INCLUDE REGENALL                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108REREP2402S05/01/02'                                      
         END                                                                    
