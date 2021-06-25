*          DATA SET GELDEXT    AT LEVEL 001 AS OF 03/04/14                      
*PHASE GELDEXTA                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'GELDEXT -GENDIR/FIL LOAD/DUMP MODEL EXTERNAL ROUTINE'           
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
*                               X'FE'= CHANGED RECORD (RECOVERY)                
*                               X'FD'= NEW RECORD (RECOVERY)                    
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
         NMOD1 (WORKX-WORKD),GELDEXT                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
                                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
                                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
                                                                                
DMXADD   L     R1,APARM            ADD RECORD EXIT                              
         MVI   0(R1),X'FD'                                                      
         AP    RECADD,=P'1'                                                     
         B     DMXIT                                                            
                                                                                
DMXCHG   L     R1,APARM            CHANGE RECORD EXIT                           
         MVI   0(R1),X'FE'                                                      
         AP    RECCHG,=P'1'                                                     
         B     DMXIT                                                            
                                                                                
DMXPRG   L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    RECDEL,=P'1'                                                     
         B     DMXIT                                                            
                                                                                
DMXPRGEO L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         AP    RECDEL,=P'1'                                                     
         B     DMXIT                                                            
                                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  B     DMXIT                                                            
                                                                                
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   AP    RECINP,=P'1'                                                     
         L     R2,AREC                                                          
*NOP*    TM    34(R2),X'80'        TEST IF DELETED                              
*NOP*    BO    DMXPRG                                                           
         CP    RECINP,=P'100'                                                   
         BE    DMXADD                                                           
         CP    RECINP,=P'200'                                                   
         BE    DMXCHG                                                           
         CP    RECINP,=P'300'                                                   
         BE    DMXPRG                                                           
         B     DMXKEEP                                                          
                                                                                
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   LA    R5,RECCNT           PRINT TABLE OF RECORD COUNTERS               
         L     R1,APARM                                                         
         MVI   0(R1),0                                                          
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 VPRINTER                                                         
DMXEOF1  MVC   P(16),4(R5)                                                      
         UNPK  P+17(9),0(4,R5)                                                  
         OI    P+25,C'0'                                                        
         GOTO1 VPRINTER                                                         
         LA    R5,L'RECCNT(R5)                                                  
         CLI   4(R5),C' '                                                       
         BNE   DMXEOF1                                                          
         B     DMXIT                                                            
         LTORG                                                                  
                                                                                
RECCNT   DS    0CL20                                                            
RECINP   DC    PL4'0',CL16'GELDEXT INPUT'                                       
RECADD   DC    PL4'0',CL16'GELDEXT ADDED'                                       
RECCHG   DC    PL4'0',CL16'GELDEXT CHANGED'                                     
RECDEL   DC    PL4'0',CL16'GELDEXT DELETED'                                     
RECCNTX  DC    PL4'0',CL16' '                                                   
                                                                                
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
*                                                                               
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
                                                                                
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GELDEXT   03/04/14'                                      
         END                                                                    
