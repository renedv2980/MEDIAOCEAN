*          DATA SET PZLDEXT    AT LEVEL 011 AS OF 08/10/00                      
*PHASE PZLDEXTA                                                                 
         TITLE 'PZLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
PZLDEXT  CSECT                                                                  
         NMOD1 20,PZLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
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
         L     RF,CNTR             KEEP COUNT OF PURGES                         
         LA    RF,1(RF)                                                         
         ST    RF,CNTR                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
*                                                                               
         XC    CNTR,CNTR           CLEAR COUNTER                                
         B     DMXIT                                                            
         SPACE 2                                                                
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
*                                                                               
         CLC   0(2,R3),=X'04F3'    SJR UNIT RECS                                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
         SPACE                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         MVC   P(12),=C'PURGED RECS='                                           
         L     R4,CNTR                                                          
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+14(6),DUB                                                      
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
CNTR     DS    F                                                                
         EJECT                                                                  
DMXRECD  DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
         CLI   0(R3),X'00'         POSIT 1=X'00'                                
         BNE   DMX05               POSIT 2=X'11',X'12',X'13'                    
         CLI   1(R3),X'11'         KEEP RECORD                                  
         BE    DMXKEEP                                                          
         CLI   1(R3),X'12'                                                      
         BE    DMXKEEP                                                          
         CLI   1(R3),X'13'                                                      
         BE    DMXKEEP                                                          
         B     DMX10                                                            
         SPACE                                                                  
DMX05    CLI   0(R3),X'11'           POSIT 1=X'11',X'12',X'13'                  
         BE    DMXKEEP               KEEP RECORD                                
         CLI   0(R3),X'12'                                                      
         BE    DMXKEEP                                                          
         CLI   0(R3),X'13'                                                      
         BE    DMXKEEP                                                          
         BH    DMXPGEOF              POSIT 1=GRTR THAN X'13' EOF                
         CLI   0(R3),X'02'                                                      
         BE    DMX10                                                            
         CLI   0(R3),X'0B'                                                      
         BE    DMX10                                                            
         CLI   0(R3),X'0E'                                                      
         BE    DMX10                                                            
         B     DMXKEEP                                                          
         SPACE                                                                  
DMX10    B     DMXPURGE                                                         
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PZLDEXT   08/10/00'                                      
         END                                                                    
