*          DATA SET REREPPV02  AT LEVEL 197 AS OF 09/18/96                      
*PHASE REPV02A,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'MODULE TO FIX PETRY WKYC BUG'                                   
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREPPV02 (REPV02) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 03JAN96 (SKU) DATE OF CONCEPTION                                *             
*                                                                 *             
*******************************************************************             
*                                                                               
REPV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REPV02,R9,RR=R5                                              
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
PC05     DS    0H                                                               
         LA    R4,KEY                                                           
         USING RCONKEY,R4                                                       
         XC    KEY,KEY             READ CONTRACT RECORDS                        
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,=C'PV'                                                  
         MVC   RCONKGRP,=C'PH'                                                  
         MVC   RCONKSTA,=C'WKYC '                                               
         DROP  R4                                                               
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
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLC   KEYSAVE(RCONKOFF-RCONREC),KEY                                    
         BNE   PCX                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
*                                                                               
         MVI   MATCH,C'N'                                                       
         MVC   P(10),=C'FOUND ONE!'                                             
         AP    COUNT,=P'1'                                                      
         GOTO1 REPORT                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         XC    ELEM2,ELEM2                                                      
         USING RCONSPEL,R6                                                      
         ZIC   R2,RCONSPLN                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONSPEL                                                 
         DROP  R6                                                               
*                                                                               
         LA    R4,ELEM+9                                                        
*                                                                               
PC25     DS    0H                                                               
         LA    R3,9(R4)                                                         
*                                                                               
PC30     DS    0H                                                               
         CLI   0(R3),X'00'                                                      
         BE    PC40                                                             
         CLI   0(R3),C'*'                                                       
         BE    PC33                                                             
         CLC   0(5,R4),0(R3)                                                    
         BE    PC35                                                             
*                                                                               
PC33     DS    0H                                                               
         LA    R3,9(R3)                                                         
         B     PC30                                                             
*                                                                               
PC35     DS    0H                                                               
         ZICM  R7,5(R4),4                                                       
         ZICM  R8,5(R3),4                                                       
         AR    R7,R8                                                            
         STCM  R7,15,5(R4)                                                      
         MVI   0(R3),C'*'                                                       
         MVI   MATCH,C'Y'                                                       
         B     PC30                                                             
*                                                                               
PC40     DS    0H                                                               
         LA    R4,9(R4)                                                         
         CLI   0(R4),C'*'                                                       
         BE    PC40                                                             
         CLI   0(R4),X'00'                                                      
         BNE   PC25                                                             
*                                                                               
* NOW COLLAPSE THE ELEMENT                                                      
*                                                                               
         CLI   MATCH,C'N'          NO DUPES FOUND SKIP UPDATE                   
         BE    PCSEQ                                                            
*                                                                               
         MVC   ELEM2(18),ELEM                                                   
         LA    R3,ELEM2+18                                                      
         LA    R4,ELEM+18                                                       
PC50     CLI   0(R4),X'00'                                                      
         BE    PC70                                                             
         CLI   0(R4),C'*'                                                       
         BNE   PC60                                                             
         LA    R4,9(R4)                                                         
         B     PC50                                                             
*                                                                               
PC60     DS    0H                                                               
         MVC   0(9,R3),0(R4)                                                    
         LA    R3,9(R3)                                                         
         LA    R4,9(R4)                                                         
         B     PC50                                                             
*                                                                               
* NOW CALCULATE NUMBER OF MINI-ELEMENTS                                         
*                                                                               
PC70     DS    0H                                                               
         LA    R4,ELEM2+9                                                       
         SR    R3,R3                                                            
PC80     CLI   0(R4),X'00'                                                      
         BE    PC90                                                             
         LA    R4,9(R4)                                                         
         LA    R3,1(R3)                                                         
         B     PC80                                                             
*                                                                               
PC90     DS    0H                                                               
         STC   R3,ELEM2+8          SET NUMBER OF MINI-ELEMENTS                  
*                                                                               
         MH    R3,=H'9'            NUMBER OF MINI-ELEMENT * LEN OF EACH         
         LA    R3,9(R3)            PLUS NINE FOR OVERHEAD                       
         STC   R3,ELEM2+1          EQUAL LENGTH OF ELEMENT                      
*                                                                               
         GOTO1 =V(RECUP),DMCB,(2,IOAREA),(R6),0                                 
         GOTO1 =V(RECUP),DMCB,(2,IOAREA),ELEM2,(R6)                             
*                                                                               
*        GOTO1 DATAMGR,DMCB,PUTREC,REPFILE,KEY+28,IOAREA,DMWORK                 
*        TM    DMCB+8,X'FD'                                                     
*        BZ    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         AP    DCOUNT,=P'1'                                                     
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+4,4,=C'TOG'                           
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(8),=C'ELEMENT:'                                                
         GOTO1 REPORT                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R6)                                                       
         GOTO1 REPORT                                                           
*                                                                               
         CLC   =X'02365354',IOAREA+23                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     PCSEQ                                                            
                                                                                
PCX      DS    0H                                                               
         MVC   P(6),=C'TOTAL:'                                                  
         EDIT  (P5,COUNT),(7,P+8)                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(6),=C'DUPES:'                                                  
         EDIT  (P5,DCOUNT),(7,P+8)                                              
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
COUNT    DC    PL5'0'                                                           
DCOUNT   DC    PL5'0'                                                           
MATCH    DC    C'N'                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(REPV02,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
SYSCODE  DS    CL2                                                              
CONKEY   DS    CL(L'KEY)                                                        
BUYKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
*                                                                               
ELEM     DS    XL256                                                            
ELEM2    DS    XL256                                                            
IOAREA   DS    CL2000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
*      ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'197REREPPV02 09/18/96'                                      
         END                                                                    
