*          DATA SET SPLDEXTANB AT LEVEL 100 AS OF 01/28/00                      
*PHASE SPEXTANB,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - CANCADIAN NETWORK BUYS WITH NETWORK AS LOCAL'         
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
         USING SPGENBUYD,R3                                                     
         L     R3,AREC             POINT TO RECORD                              
         CLI   0(R3),X'B3'         YRTO NETWORK BUY                             
         BNE   DMXKEEP                                                          
         CLC   =X'0000',4(R3)       MKT=0                                       
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R6,BDELEM                                                        
DMX10    CLI   0(R6),0                                                          
         BE    DMXKEEP                                                          
         CLI   0(R6),X'68'                                                      
         BNE   DMX15                                                            
         CLC   =X'0000',2(R6)      MKT =0                                       
         BE    DMX20                                                            
DMX15    SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DMX10                                                            
*                                                                               
DMX20    BAS   RE,KEYPRNT                                                       
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
KEYPRNT  NTR1                                                                   
         GOTO1 =V(HEXOUT),DMCB,(R3),P+2,13                                      
         GOTO1 =V(HEXOUT),DMCB,1(R3),P+30,2                                     
         GOTO1 =V(HEXOUT),DMCB,6(R3),P+35,4                                     
         EDIT  (B1,9(R3)),(3,P+45),FILL=0                                       
         EDIT  (B1,10(R3)),(3,P+50),FILL=0                                      
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
*                                                                               
BUYPRNT  NTR1                                                                   
BP10     LA    R4,=CL20'BUY RECORD'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
BPX      XIT1                                                                   
*                                                                               
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF BUYS ='                                       
         EDIT  (P10,NUMBUYS),(15,P+25)                                          
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
NEXTEST  DC    F'0'                                                             
LASTEST  DC    F'0'                                                             
DATADISP DC    H'0024'                                                          
NUMBUYS  DC    PL10'0'                                                          
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
**                                                                              
SVCLIST  DS    CL880                                                            
ESTTAB   DS    3000CL6                                                          
**                                                                              
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
APBYTE   DS    XL1                                                              
ESTOWSDY DS    CL1                                                              
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORK2    DS    CL64                                                             
NUMRECS  DS    PL6                                                              
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
*SPGENCLT                                                                       
SPGENCLTD      DSECT                                                            
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100SPLDEXTANB01/28/00'                                      
         END                                                                    
