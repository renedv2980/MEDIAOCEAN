*          DATA SET SPREPLD02  AT LEVEL 039 AS OF 08/29/00                      
*PHASE SPLD02A                                                                  
         TITLE 'MENU DEMO LISTING PGM'                                          
         PRINT NOGEN                                                            
SPLD02   CSECT                                                                  
         NMOD1 0,SPLD02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUN1                                                             
         CLI   MODE,PROCREC        MODE FROM SPRQ03                             
         BE    PROCESS             DUMMY CONTROLLER                             
DONE     B     EXIT                                                             
EXIT     XIT1                                                                   
RUN1     DS    0H                                                               
         B     EXIT                                                             
PROCESS  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    FLTMSG,FLTMSG                                                    
         CLC   QPROG+33(4),=4C' '                                               
         BE    INIT10                                                           
         CLC   QPROG+33(4),=CL4'ALL '                                           
         BE    INIT10                                                           
         MVC   FLTMSG(14),=C'MENUS FILTERED'                                    
         LA    R5,QPROG+33                                                      
         LA    R6,4                FOR BCT                                      
         LA    R7,FLTMSG+15                                                     
INIT4    MVC   0(1,R7),0(R5)                                                    
         TM    0(R5),X'40'                                                      
         BNZ   INIT8                                                            
         MVI   0(R7),C'-'                                                       
         MVC   1(1,R7),0(R5)                                                    
         OI    1(R7),X'40'                                                      
         MVI   2(R7),C','                                                       
         LA    R7,3(R7)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,INIT4                                                         
         B     INIT9                                                            
*                                                                               
INIT8    MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,INIT4                                                         
*                                                                               
INIT9    BCTR  R7,0                BLANK OUT LAST COMMA                         
         MVI   0(R7),C' '                                                       
*                                                                               
INIT10   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      0D26/A-M                                     
         BNE   NORECMSG                                                         
         B     FLTR1                                                            
SEQU     DS    0H                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(3),KEYSAVE      0D26/A-M                                     
         BNE   DONE                                                             
*                                                                               
FLTR1    CLC   QPROG+33(4),=4C' '                                               
         BE    GETIT               NO FILTER                                    
         CLC   QPROG+33(4),=CL4'ALL '                                           
         BE    GETIT               OR ALL MENUS                                 
         LA    R5,QPROG+33         CHK FOR MENU FILTER                          
         LA    R6,4                FOR BCT                                      
         LA    R7,KEY+3                                                         
FLTR2    CLI   0(R5),C'*'                                                       
         BE    FLTR10                                                           
         CLI   0(R5),C' '                                                       
         BE    FLTR10                                                           
         TM    0(R5),X'40'         TEST NEGATIVE FILTER                         
         BZ    FLTR6                                                            
         CLC   0(1,R7),0(R5)       CODES MUST MATCH                             
         BE    FLTR10                                                           
         B     SEQU                                                             
*                                                                               
FLTR6    MVC   DUB(1),0(R5)                                                     
         OI    DUB,X'40'                                                        
         CLC   DUB(1),0(R7)                                                     
         BE    SEQU                                                             
*                                                                               
FLTR10   LA    R5,1(R5)                                                         
         LA    R7,1(R7)                                                         
         BCT   R6,FLTR2                                                         
*                                                                               
GETIT    DS    0H                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         L     R8,AREC                                                          
         USING DMNRECD,R8                                                       
RECLOOP  DS    0H                                                               
         LA    R3,24(R8)           DMNEL01 FIELD                                
         CLI   0(R3),1             IS IT AN 01 ELEMENT                          
         BNE   ERR1                                                             
         CLC   DMNACDAT,TODAYB     IS IT TODAY'S DATE                           
         BNE   NOTTODAY                                                         
*                                                                               
*              * TO DENOTE ADDED OR CHANGED TODAY                               
*              NO-OPED                                                          
*        MVI   P1+1,C'*'                                                        
*                                                                               
NOTTODAY DS    0H                                                               
         ZIC   R4,1(R3)            GET LENGTH OF ELEMENT                        
         AR    R3,R4               POINT TO 05 ELEMENT                          
         MVC   P1+2(4),DMNKCODE    MOVE IN CODE TO MENU FIELD                   
         LA    R5,P1+14            ADDR OF PRNT BUFFER                          
         SR    R1,R1                                                            
         USING DMNEL05,R3                                                       
O5LOOP   DS    0H                                                               
         CLI   0(R3),0             IS IT END OF RECORD                          
         BE    PRNT                                                             
         CLI   0(R3),5             IS IT AN 05 ELEMENT                          
         BNE   ERR1                                                             
         MVC   0(7,R5),DMNRTG     MOVE IN DEMO RATING GROUP                     
         LA    R5,11(R5)           POINT TO NEXT DEMO FIELD                     
         LA    R1,1(R1)            BUMP COUNTER                                 
         C     R1,=F'10'                                                        
         BNE   *+8                                                              
         LA    R5,P2+14            CAN ONLY DO 10 PER LINE                      
         ZIC   R4,1(R3)            LENGTH OF 05 ELEMENT                         
         AR    R3,R4               POINT TO NEXT 05 ELEMENT                     
         B     O5LOOP                                                           
PRNT     DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 PRINTIT                                                          
         B     SEQU                                                             
NORECMSG DS    0H                                                               
         MVC   P1(26),=C'NO DEMO MENU RECORDS FOUND'                            
         GOTO1 PRINTIT                                                          
         B     DONE                                                             
ERR1     DS    0H                                                               
         MVC   P1(11),=C'BAD MENU  -'                                           
         MVC   P1+13(4),DMNKCODE                                                
         GOTO1 PRINTIT                                                          
         B     SEQU                                                             
*                                                                               
         DS    F'0'                                                             
PRINTIT  ST    RE,PRINTIT-4                                                     
         MVC   HEAD4+2(L'FLTMSG),FLTMSG                                         
         GOTO1 REPORT                                                           
         L     RE,PRINTIT-4                                                     
         BR    RE                                                               
*                                                                               
FLTMSG   DS    CL30                                                             
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENDMN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039SPREPLD02 08/29/00'                                      
         END                                                                    
