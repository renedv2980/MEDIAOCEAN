*          DATA SET SPLDEXTBSN AT LEVEL 005 AS OF 12/24/96                      
*PHASE SPEXTBSN                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTBSN - EXTRACT BSNY NON-TV BUYS'                          
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
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
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
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         CLC   =X'0051',0(R6)     GET RID OF BSNY/TV HEADERS                    
         BE    DMXPURGE                                                         
         CLI   0(R6),X'51'         GET RID OF BSNY/TV BUYS                      
         BE    DMXPURGE                                                         
         L     R1,COUNT            NUMBER OF BUY RECORDS READ                   
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
DMREC10  CLC   COUNT,=F'100'                                                    
         BH    DMXKEEP                                                          
         GOTO1 VHEXOUT,DMCB,(R6),P,13,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(12),=C'RECORDS KEPT'                                           
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
         DS    0F                                                               
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
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPLDEXTBSN12/24/96'                                      
         END                                                                    
