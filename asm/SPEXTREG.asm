*          DATA SET SPEXTREG   AT LEVEL 025 AS OF 01/05/89                      
*PHASE SPEXTREG,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE MSUNPK                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* OCT 4, 1988                                                                   
* DELETE JWT NSID RECORDS                                                       
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
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   L     R3,AREC                                                          
         CLC   0(2,R3),=X'0E19'                                                 
         BNE   DMXKEEP                                                          
*                                                                               
         AP    TOTRD,=P'1'                                                      
*                                                                               
         CLI   2(R3),X'C1'         '0E19C1'                                     
         BNE   DMX10                                                            
         MVC   20(2,R3),=C'SJ'     ALPHA CODE                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         LA    R4,2(R3)                                                         
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
DMX10    B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         LA    R3,TOTRD                                                         
         UNPK  DUB,0(4,R3)                                                      
         OI    DUB+7,X'F0'                                                      
         MVC   P(8),DUB                                                         
         MVC   P+12(32),4(R3)                                                   
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 2                                                                
TOTRD    DC    PL4'0',CL28'TOTAL RECS READ '                                    
WORK     DS    CL64                                                             
*                                                                               
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
         PRINT OFF                                                              
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPEXTREG  01/05/89'                                      
         END                                                                    
