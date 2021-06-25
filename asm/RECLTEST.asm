*          DATA SET RECLTEST   AT LEVEL 002 AS OF 08/31/00                      
*          DATA SET RECLTEST   AT LEVEL 001 AS OF 06/30/83                      
*PHASE RECLTESA RECLTEST                                                        
*INCLUDE PRNTBL                                                                 
         TITLE 'RECLTEST - LOAD/DUMP EXTERN TO TEST CLOSEOUT'                   
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
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
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
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
*                                                                               
         B     DMXIT                                                            
         SPACE 2                                                                
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING RINVD,R3                                                         
         CLI   RINVKTYP,X'12'      TEST FOR INVENTORY RECD                      
         BNE   DMXKEEP                                                          
         CLC   RINVKREP,=C'BL'     TEST FOR BLAIR                               
         BNE   DMXKEEP                                                          
         TM    29(R3),X'01'        TEST FOR CLOSED RECORD                       
         BNO   DMXKEEP                                                          
         CLI   RINVKSRC,0          TEST FOR INV HEADER                          
         BE    DMXKEEP                                                          
         CLI   RINVKSRC,X'FF'      TEST FOR TEXT RECD                           
         BE    DMXKEEP                                                          
         CLI   RINVKSRC,C'M'       TEST FOR MARKET FACT REC                     
         BE    DMXKEEP                                                          
         CLI   RINVKSRC,C'S'       TEST FOR STATION FACT RECD                   
         BE    DMXKEEP                                                          
         CLI   RINVKBK,X'51'       TEST FOR 1981 YEAR ON THE BOOK RECD          
         BE    DMXKEEP                                                          
         L     R1,PRINTED                                                       
         LA    R1,1(R1)                                                         
         CH    R1,=H'100'          PRINT FIRST 100 RECS THAT QUALIFY            
         BH    DMXKEEP                                                          
         ST    R1,PRINTED                                                       
         MVC   HALF,27(R3)         RECORD LENGTH                                
         LH    R4,HALF                                                          
         LA    R2,L'MSG                                                         
         MVC   MSG,SPACES                                                       
         MVC   MSG(6),=C'CLOSED'                                                
         GOTO1 =V(PRNTBL),DMCB,((R2),MSG),(R3),C'DUMP',(R4),=C'2D',    X        
               RR=YES                                                           
         B     DMXKEEP                                                          
         SPACE 2                                                                
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PRINTED  DC    F'0'                                                             
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
MSG      DS    CL8                                                              
WORK     DS    CL48                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* INVENTORY RECORD DSECT                                                        
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002RECLTEST  08/31/00'                                      
         END                                                                    
