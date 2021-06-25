*          DATA SET SPLDEXTEJ  AT LEVEL 001 AS OF 01/27/97                      
*PHASE SPEXTEJ                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE MSUNPK                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTEJ - FIND OLD BN AGY REC'                                
*                                                                               
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
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(CLUNPK)                                                    
         ST    RF,VCLUNPK                                                       
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         L     RF,=V(MSUNPK)                                                    
         ST    RF,VMSUNPK                                                       
         L     RF,=V(PRINT)                                                     
         ST    RF,VPRINT                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
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
         USING AGYRECD,R6                                                       
         CLI   AGYKTYPE,X'06'      AGENCY RECORD?                               
         BNE   DMXKEEP             NO                                           
         CLC   =C'BN',AGYKAGY      BN AGENCY?                                   
         BNE   DMXKEEP             NO                                           
*                                                                               
DMXRX    GOTO1 =V(PRNTBL),DMCB,=C'AGYREC',0(R6),C'DUMP',500,=C'1D',    X        
               (C'P',VPRINT)                                                    
         B     DMXKEEP                                                          
         EJECT                                                                  
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*        GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
VCLUNPK  DS    A                                                                
VMSUNPK  DS    A                                                                
VPRINT   DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPLDEXTEJ 01/27/97'                                      
         END                                                                    
