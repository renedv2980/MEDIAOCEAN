*          DATA SET STLDEXTANN AT LEVEL 003 AS OF 02/25/99                      
*PHASE STEXTANN                                                                 
         TITLE 'STLDEXT - TRAFFIC- CHANGE ALPHA AGENCY CODE - PU-->QA'          
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
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
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
*          DATA SET SPLDEXTCBL AT LEVEL 139 AS OF 03/18/98                      
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         CLC   =C'PU',20(R3)       TEST AGENCY = PU                             
         BNE   DMXKEEP                                                          
         MVC   20(2,R3),=C'QA'     THEN CHANGE TO QA                            
         AP    RECSCHGD,=P'1'                                                   
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         EDIT  RECSCHGD,                                                        
         B     DMXIT                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
RECSCHGD DC    PL8'0'                                                           
BYTE     DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
TABADDR  DS    A                                                                
TABCOUNT DS    F            MARKT/STAT                                          
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMED  DS    CL1                                                              
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
WORKD    DSECT                                                                  
*UB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
ELCODE   DS    X                                                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         EJECT                                                                  
DARERECD DSECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
NBUYRECD DSECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
TRFPATD  DSECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003STLDEXTANN02/25/99'                                      
         END                                                                    
