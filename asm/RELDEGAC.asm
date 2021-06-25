*          DATA SET RELDEGAC   AT LEVEL 019 AS OF 05/01/02                      
*PHASE RELDEGAC,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'RELDKRG - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*  KRG EXTERN                                                     *             
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
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
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
*                                                                               
* PROCESS CONTRACT RECORDS                                                      
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RCONREC,R5                                                       
         CLI   RCONKTYP,X'0C'                                                   
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   =C'BF',RCONKREP                                                  
         BE    DMX010                                                           
         CLC   =C'K4',RCONKREP                                                  
         BE    DMX010                                                           
         CLC   =C'KU',RCONKREP                                                  
         BE    DMX010                                                           
         CLC   =C'KF',RCONKREP                                                  
         BE    DMX010                                                           
         CLC   =C'EA',RCONKREP                                                  
         BE    DMX010                                                           
         CLC   =C'CR',RCONKREP                                                  
         BE    DMX010                                                           
         CLC   =C'K6',RCONKREP                                                  
         BE    DMX010                                                           
         CLC   =C'S3',RCONKREP                                                  
         BNE   DMXKEEP                                                          
*                                                                               
DMX010   DS    0H                                                               
         CLC   =C'EGAC',RCONKAGY                                                
         BNE   DMX020                                                           
         CLC   =C'EGAC',RCONKADV                                                
         BE    DMXPURGE                                                         
         DROP  R5                                                               
*                                                                               
DMX020   DS    0H                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
*                                                                               
         AP    CHANGE,=P'1'                                                     
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=CL8'REPFILE'),(X'04',AREC),0,0             
*                                                                               
         AP    CONCTR,=P'1'                                                     
*                                                                               
         CLC   CONCTR,=PL5'100'                                                 
         BH    DMXKEEP                                                          
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RCONREC,R5                                                       
         MVC   P(2),RCONKREP                                                    
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+4,4,=C'TOG'                           
         GOTO1 VPRINTER                                                         
*                                                                               
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
         SPACE 1                                                                
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
CONCTR   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
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
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
ELEM     DS    CL256                                                            
ELEM2    DS    CL256                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019RELDEGAC  05/01/02'                                      
         END                                                                    
