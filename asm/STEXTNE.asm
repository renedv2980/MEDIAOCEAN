*          DATA SET STEXTNE    AT LEVEL 011 AS OF 11/06/00                      
*PHASE STEXTNE,*                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTREC                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SEARCH FOR ALL BUY REC TRASHED BY SPOT SEED                                   
* 11/06/00                                                                      
* SEARCH FOR DNCH R/JPC/RC/1 LINE 1 KIGNF  BUY ACTIVITY RECORDS                 
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
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         CLC   =X'72A5E2FF01BD5BF2E101010100',0(R3)                             
         BNE   DMXPURGE                                                         
         SPACE                                                                  
         SPACE                                                                  
         AP    TOTFND,=P'1'                                                     
         SPACE                                                                  
PRT      DS    0H                                                               
         GOTO1 =V(PRTREC),DMCB,(R3),(24,13),V(PRINT),V(HEXOUT)                  
         B     DMXKEEP                                                          
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(28),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,33(,R3)                                                       
         BCT   R2,DMXEOF10                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOT RECS READ'                                       
TOTFND   DC    PL5'0',CL28'TOT MISS RECS'                                       
TOTCTRS  EQU   (*-TOTRD)/33                                                     
          SPACE                                                                 
WORK     DS    CL64                                                             
         SPACE                                                                  
RTITLE   DC    CL100'FIND DNCH BAD BUY ACT UPDATE'                              
         LTORG                                                                  
          SPACE                                                                 
         SPACE 2                                                                
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
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
STARECD   DSECT                                                                 
       ++INCLUDE SPGENSTA                                                       
ADDRECD   DSECT                                                                 
       ++INCLUDE SPGENADD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011STEXTNE   11/06/00'                                      
         END                                                                    
