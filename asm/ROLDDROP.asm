*          DATA SET ROLDDROP   AT LEVEL 005 AS OF 06/04/97                      
*          DATA SET ROLDMOVE   AT LEVEL 006 AS OF 06/03/97                      
*PHASE ROLDDROP                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'ROLDMOVE - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
*  CHANGES ALL GP RECS TO IF, AND ADDS 100,000 TO CONTRACT NUMBERS              
*                                                                               
* LEV  01 MAY29/97 BG                                                           
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
*******************************************************************             
*                                                                 *             
*  DROP ALL BUT REP 4                                             *             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE                                                                  
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE                                                                  
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE                                                                  
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE                                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         CLI   0(R5),X'2F'         STATION RECORD                               
         BE    DMXR10                                                           
         B     DMXKEEP                                                          
         EJECT                                                                  
*******************************************************************             
* PROCESS FOR PAPER WORK RECORDS                                                
*******************************************************************             
DMXR10   DS    0H                  KRG SUBREPS                                  
         USING RPWCRECD,R5                                                      
         MVI   ELCODE,0                                                         
         CLC   38(2,R5),=X'011C'                                                
         BE    *+14                                                             
         MVI   ELCODE,1                                                         
         AP    BADEL1,=P'1'                                                     
         SPACE                                                                  
         CLC   66(2,R5),=X'0266'                                                
         BE    *+14                                                             
         MVI   ELCODE,2                                                         
         AP    BADEL2,=P'1'                                                     
         SPACE                                                                  
         CLC   168(2,R5),=X'0366'                                               
         BE    *+14                                                             
         MVI   ELCODE,3                                                         
         AP    BADEL3,=P'1'                                                     
         SPACE                                                                  
         CLC   270(2,R5),=X'0416'                                               
         BE    *+14                                                             
         MVI   ELCODE,4                                                         
         AP    BADEL4,=P'1'                                                     
         SPACE                                                                  
         CLC   292(2,R5),=X'EF0E'                                               
         BE    *+14                                                             
         MVI   ELCODE,X'EF'                                                     
         AP    BADELE,=P'1'                                                     
         SPACE                                                                  
         CLI   ELCODE,0            ANY ERROR                                    
         BE    DMXR30                                                           
         AP    BADELEM,=P'1'                                                    
         SPACE                                                                  
         GOTO1 =V(HEXOUT),DMCB,ELCODE,BADELEN+4,1,0                             
         SR    R3,R3                                                            
         ICM   R3,3,RPWCLEN                                                     
         LA    R4,BADELEN                                                       
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R5),C'DUMP',(R3),=C'0D'               
         SPACE                                                                  
         MVC   38(2,R5),=X'011C'                                                
         MVC   66(2,R5),=X'0266'                                                
         MVC   168(2,R5),=X'0366'                                               
         MVC   270(2,R5),=X'0416'                                               
         MVC   292(2,R5),=X'EF0E'                                               
         B     DMXR30                                                           
BADELEN  DC    CL20'BAD XX ELCODE LENGTH'                                       
         SPACE                                                                  
         DS    0H                                                               
DMXR30   DS   0H                                                                
         CLC   =C'GP',RPWCKREP                                                  
         BNE   DMXR60                                                           
         SPACE                                                                  
         CP    CHANGE,=P'100'                                                   
         BH    DMXR40                                                           
         SR    R3,R3                                                            
         ICM   R3,3,RPWCLEN                                                     
         LA    R4,=CL20'PWC REC TO BE MOVED'                                    
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R5),C'DUMP',(R3),=C'0D'               
*                                                                               
DMXR40   AP    CHANGE,=P'1'                                                     
*                                                                               
         MVC   DUB(4),RPWCKCDE                                                  
         MVI   DUB+4,X'0C'                                                      
         AP    DUB(5),=P'1000000'                                               
         MVC   RPWCKCDE,DUB                                                     
         MVC   RPWCKREP,=C'IF'                                                  
         SPACE                                                                  
         CP    CHANGE,=P'100'                                                   
         BH    DMXR60                                                           
         SR    R3,R3                                                            
         ICM   R3,3,RPWCLEN                                                     
         LA    R4,=CL20'CONVERTED PWC REC'                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R5),C'DUMP',(R3),=C'0D'               
         SPACE                                                                  
DMXR60   LA    R0,TABCT                                                         
         LA    R1,TAB                                                           
DMXR64   CLC   RPWCKREP,0(R1)                                                   
         BE    DMXPURGE                                                         
         LA    R1,2(,R1)                                                        
         BCT   R0,DMXR64                                                        
         B     DMXKEEP                                                          
         DROP  R5                                                               
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE                                                                  
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+12)                                              
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(11),=C'BAD ELEM 01'                                          
         EDIT  (P5,BADEL1),(7,P+15)                                             
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(11),=C'BAD ELEM 02'                                          
         EDIT  (P5,BADEL2),(7,P+15)                                             
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(11),=C'BAD ELEM 03'                                          
         EDIT  (P5,BADEL3),(7,P+15)                                             
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(11),=C'BAD ELEM 04'                                          
         EDIT  (P5,BADEL4),(7,P+15)                                             
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(11),=C'BAD ELEM EF'                                          
         EDIT  (P5,BADELE),(7,P+15)                                             
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+3(12),=C'TOT BAD ELEM'                                         
         EDIT  (P5,BADELEM),(7,P+15)                                            
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
*                                                                               
         B     DMXIT                                                            
         SPACE                                                                  
CHANGE   DC    PL5'0'                                                           
PURGE    DC    PL5'0'                                                           
BADEL1   DC    PL5'0'                                                           
BADEL2   DC    PL5'0'                                                           
BADEL3   DC    PL5'0'                                                           
BADEL4   DC    PL5'0'                                                           
BADELE   DC    PL5'0'                                                           
BADELEM  DC    PL5'0'                                                           
*                                                                               
         LTORG                                                                  
TAB      DC    CL2'AQ'                                                          
         DC    CL2'B1'                                                          
         DC    CL2'CN'                                                          
         DC    CL2'D4'                                                          
         DC    CL2'GN'                                                          
         DC    CL2'IF'                                                          
         DC    CL2'IR'                                                          
         DC    CL2'I2'                                                          
         DC    CL2'I8'                                                          
         DC    CL2'I9'                                                          
         DC    CL2'KX'                                                          
         DC    CL2'MG'                                                          
         DC    CL2'RM'                                                          
         DC    CL2'S1'                                                          
         DC    CL2'UN'                                                          
         DC    CL2'UV'                                                          
         DC    CL2'W2'                                                          
         DC    CL2'W3'                                                          
         DC    CL2'XJ'                                                          
         DC    CL2'XZ'                                                          
         DC    CL2'X7'                                                          
         DC    CL2'YD'                                                          
         DC    CL2'ZO'                                                          
TABCT    EQU   (*-TAB)/2                                                        
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
RECUP    DS    V                                                                
         SPACE                                                                  
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
STARTYM  DS    XL2                                                              
ENDYM    DS    XL2                                                              
THISYM   DS    XL2                                                              
YMD      DS    XL3                                                              
WORK     DS    CL64                                                             
WORK2    DS    CL64                                                             
ELEM     DS    CL256                                                            
ELEM2    DS    CL256                                                            
WORKX    EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDPERVALD                                                      
RPWCRECD DSECT                                                                  
       ++INCLUDE REGENPWC                                                       
       ++INCLUDE REGENSDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ROLDDROP  06/04/97'                                      
         END                                                                    
