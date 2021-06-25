*          DATA SET STLDJWTFR  AT LEVEL 016 AS OF 03/09/98                      
*PHASE STJWFR                                                                   
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'STLDJWTFR - COPY JW STATION INFO TO FDMJW (FR)'                 
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
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFR',AREC,C'DUMP',18,=C'1D'                  
         CLI   0(R6),C'S' STATION RECORD                                        
         BE    DMXREC30                                                         
         CLI   0(R6),C'A' ADDRESS RECORD                                        
         BE    DMXREC30                                                         
                                                                                
         CLI   0(R6),C'M' MARKET RECORD                                         
         BE    DMXREC50                                                         
         B     DMXPURGE                                                         
*                                                                               
*        CLI   0(R6),C'R'       REP RECORD                                      
*        BE    DMXREC80                                                         
*        B     DMXPURGE                                                         
*                                                                               
*  STATION RECORD LOGIC                                                         
*                                                                               
         CLI   6(R6),C'N'      IS IT BDNY                                       
         BNE   DMXPURGE                                                         
DMXREC30 CLC   7(2,R6),=C'BD'      IS IT BDNY                                   
         BNE   DMXPURGE                                                         
*                                                                               
*  BYPASS THE FOLLOWING STATIONS                                                
*                                                                               
*        CLC   2(5,R6),=CL5'TNT N'                                              
*        BE    DMXPURGE                                                         
*        CLC   2(5,R6),=CL5'NIK N'                                              
*        BE    DMXPURGE                                                         
*        CLC   2(5,R6),=CL5'NIKSN'                                              
*        BE    DMXPURGE                                                         
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'BEFR',AREC,C'DUMP',18,=C'1D'                  
         MVC   7(2,R6),=CL2'DM'    CHANGE AGENCY                                
         GOTO1 =V(PRNTBL),DMCB,=C'AFTR',AREC,C'DUMP',20,=C'1D'                  
         B     DMXKEEP                                                          
*                                                                               
*  MARKET RECORD LOGIC                                                          
*                                                                               
         CLI   1(R6),C'N'      IS IT BDNY                                       
         BNE   DMXPURGE                                                         
DMXREC50 CLC   6(2,R6),=C'BD'      IS IT JWNY                                   
         BNE   DMXPURGE                                                         
*                                                                               
*        CLC   2(4,R6),=CL4'0217'  BYPASS THE FOLLOWING MARKET                  
*        BE    DMXPURGE                                                         
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'BEFR',AREC,C'DUMP',18,=C'1D'                  
         MVC   6(2,R6),=CL2'DM'    CHANGE AGENCY                                
         GOTO1 =V(PRNTBL),DMCB,=C'AFTR',AREC,C'DUMP',20,=C'1D'                  
         B     DMXKEEP                                                          
*                                                                               
*  REP RECORD LOGIC                                                             
*                                                                               
DMXREC80 CLC   5(2,R6),=C'JW'      IS IT JWNY                                   
         BNE   DMXPURGE                                                         
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'BEFR',AREC,C'DUMP',18,=C'1D'                  
         MVC   5(2,R6),=CL2'FR'    CHANGE AGENCY                                
         GOTO1 =V(PRNTBL),DMCB,=C'AFTR',AREC,C'DUMP',20,=C'1D'                  
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
BYTE     DS    X                                                                
ACOUNT   DS    F                                                                
MCOUNT   DS    F                                                                
SCOUNT   DS    F                                                                
ELCODE   DS    X                                                                
RECLEN   DS    H                                                                
         DC    H'0'                                                             
IO       DS    XL2000                                                           
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
         PRINT OFF                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016STLDJWTFR 03/09/98'                                      
         END                                                                    
