*          DATA SET SPLDEXTAN5 AT LEVEL 087 AS OF 07/09/98                      
*PHASE SPEXTANN,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - PEEL AN AGY/CLT OFF THE SPOT FILE'                    
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
         CLI   0(R3),X'00'         AGY/CLT/PRD/EST/BILL                         
         BE    AGY                                                              
         CLI   0(R3),X'02'         GOAL RECORD                                  
         BE    GOAL                                                             
         CLI   0(R3),X'03'         SYND RECORD                                  
         BE    SYND                                                             
         CLI   0(R3),X'05'         ADVH RECORD                                  
         BE    ADVH                                                             
         CLI   0(R3),X'06'         AGYH RECORD                                  
         BE    AGYH                                                             
         CLI   0(R3),X'08'         DPTH RECORD                                  
         BE    DPTH                                                             
         CLI   0(R3),X'09'         EQUH RECORD                                  
         BE    EQUH                                                             
         CLI   0(R3),X'10'         BUY RECORD                                   
         BH    BUY                                                              
         B     DMXPURGE                                                         
*                                                                               
AGY      DS    0H                                                               
         MVC   BYTE,1(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'A0'          BN = X'A0'                                   
         BNE   DMXPURGE                                                         
         OC    2(2,R3),2(R3)       CLT                                          
         BZ    DMXKEEP                                                          
         CLC   2(2,R3),=X'96FF'    FX = X'96FF'                                 
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
GOAL     DS    0H                                                               
         MVC   BYTE,1(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'A0'          BN = X'A0'                                   
         BNE   DMXPURGE                                                         
         OC    2(2,R3),2(R3)       CLT                                          
         BZ    DMXKEEP                                                          
         CLC   2(2,R3),=X'96FF'    FX = X'96FF'                                 
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
SYND     DS    0H                  AGY ?????                                    
         OC    8(2,R3),8(R3)       CLT                                          
         BZ    DMXKEEP                                                          
         CLC   2(2,R3),=X'96FF'    FX = X'96FF'                                 
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
ADVH     DS    0H                                                               
         CLC   1(3,R3),=C'FX '     FX                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
AGYH     DS    0H                                                               
         CLC   1(2,R3),=C'BN'      AGY=BN                                       
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
DPTH     DS    0H                                                               
         CLC   1(2,R3),=C'BN'      AGY=BN                                       
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
EQUH     DS    0H                                                               
         CLC   1(2,R3),=C'BN'      AGY=BN                                       
         BNE   DMXPURGE                                                         
         OC    4(2,R3),4(R3)       CLT                                          
         BZ    DMXKEEP                                                          
         CLC   4(2,R3),=X'96FF'    FX = X'96FF'                                 
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
BUY      DS    0H                                                               
         MVC   BYTE,0(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'A0'          BN = X'A0'                                   
         BNE   DMXPURGE                                                         
         OC    1(2,R3),1(R3)       CLT                                          
         BZ    DMXKEEP                                                          
         CLC   1(2,R3),=X'96FF'    FX = X'96FF'                                 
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXR30   DS    0H                                                               
         AP    NUMRECS,=P'1'                                                    
         B     DMXKEEP                                                          
*                                                                               
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'GOAL RECORD'                                            
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF RECS ='                                       
         EDIT  (P6,NUMRECS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0024'                                                          
*                                                                               
*                                                                               
NUMRECS  DC    PL6'0'                                                           
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
BYTE     DS    XL1                                                              
NUMBUYS  DS    PL6                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENBUY                                                                       
SPGENBUYD      DSECT                                                            
       ++INCLUDE SPGENBUY                                                       
*SPGENEST                                                                       
SPGENESTD      DSECT                                                            
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087SPLDEXTAN507/09/98'                                      
         END                                                                    
