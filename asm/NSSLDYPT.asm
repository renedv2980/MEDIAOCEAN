*          DATA SET NSSLDYPT   AT LEVEL 029 AS OF 01/13/87                      
*          DATA SET SPREPM7TP  AT LEVEL 066 AS OF 12/16/86                      
*PHASE NSSLDYPT,+0                                                              
*          DATA SET NSLDEXT    AT LEVEL 019 AS OF 12/30/86                      
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE MSUNPK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE DEMOVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'MATHER DAYPART RECORD CONVERSION'                               
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
DMLDYPT  CSECT                                                                  
         NMOD1 20,DMLDYPT,RR=R5                                                 
         USING WORKD,RC                                                         
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING DMLDYPT+4096,RA                                                  
*                                                                               
         ST    R5,RELO                                                          
         B     DMXCTL                                                           
RELO     DC    A(0)                                                             
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     R9,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,R9                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXIT               END-OF-FILE                                  
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
         LA    R1,ADCONS                                                        
         LA    RF,ADCONX                                                        
INIT1    L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         CR    R1,RF                                                            
         BL    INIT1                                                            
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R6,AREC                                                          
         USING DPTHDR,R6                                                        
*                                                                               
         CLI   DPTKTYPE,X'08'      PROCESS DAYPART RECORDS                      
         BNE   DMXKEEP                                                          
         CLI   DPTKMENU,X'00'                                                   
         BNE   DMXPURGE                                                         
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,DPTLEN                                                      
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',(R6),C'DUMP',(R4),=C'1D'              
*                                                                               
         LA    R4,DPTEL                                                         
FMT020   CLI   0(R4),X'01'                                                      
         BE    FMT040                                                           
*                                                                               
         MVI   DPTCTL,X'C0'                                                     
         SR    R9,R9                                                            
         ICM   R9,3,DPTLEN                                                      
         A     R9,=F'4'                                                         
         LR    R8,R6                                                            
         S     R8,=F'4'                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'AFTDEL',(R8),C'DUMP',(R9),=C'1D'              
         B     DMXKEEP                                                          
*                                                                               
         CLI   0(R4),X'01'                                                      
         BE    FMT040                                                           
FMT030   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FMT020                                                           
*                                                                               
FMT040   MVC   OUTAREA1(24),DPTKEY                                              
         MVC   OUTAREA1+24(80),0(R4)                                            
*                                                                               
         MVC   OUTAREA1+13(2),=XL2'00D4'                                        
         MVC   OUTAREA1+24(2),=XL2'01BC'                                        
         MVC   OUTAREA1+4(1),OUTAREA1+26                                        
         MVC   OUTAREA1+26(75),OUTAREA1+27                                      
         XC    OUTAREA1+101(120),OUTAREA1+101                                   
*                                                                               
         LA    R5,15                                                            
         LA    R9,OUTAREA1+26                                                   
*                                                                               
FMT070   CLI   0(R9),C'Z'                                                       
         BE    FMT200                                                           
         CLI   0(R9),X'00'                                                      
         BE    FMT100                                                           
         LA    R9,5(R9)                                                         
         BCT   R5,FMT070                                                        
FMT100   MVI   0(R9),C'Z'                                                       
*                                                                               
FMT200   EQU   *                                                                
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',OUTAREA,C'DUMP',220,=C'1D'             
*                                                                               
         LA    R0,OUTAREA                                                       
         L     R9,VTAPEOUT                                                      
         PUT   (R9),(R0)                                                        
*                                                                               
         B     FMT030                                                           
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         EJECT                                                                  
*******************************************************************             
ADCONS   DS    0F                  **** FIRST RELOCATABLE ADCON ****            
ACLTPTR  DC    A(CLTTAB)                                                        
ACLTTAB  DC    A(CLTTAB)                                                        
ACLTTABX DC    A(CLTTABX)                                                       
VUSDEMS  DC    V(SPDEMTAB)                                                      
VCANDEMS DC    V(CNDEMTAB)                                                      
VRECUP   DC    V(RECUP)                                                         
VMSUNPK  DC    V(MSUNPK)                                                        
VCLUNPK  DC    V(CLUNPK)                                                        
VDATCON  DC    V(DATCON)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
ASVCMT   DC    A(SVCMT)                                                         
ASVCMTX  DC    A(SVCMTX)                                                        
ASVCMT5  DC    A(SVCMT5)                                                        
ADCONX   EQU   *-1                 **** LAST RELOCATABLE ADCON ****             
*******************************************************************             
ACNTAB   DC    A(0)                                                             
DELADDR  DS    A                                                                
NEWDEMS  DS    CL256                                                            
         LTORG                                                                  
         EJECT                                                                  
         DC    C'*CLTTAB*'                                                      
CLTTAB   DS    128D                                                             
CLTTABX  EQU   *-1                                                              
         DS    0D                                                               
OUTAREA  DC    XL4'00D80000'                                                    
OUTAREA1 DS    300C                                                             
*                                                                               
         DS    0D                                                               
         DC    C'**SVCMT*'                                                      
SVCMT    DS    0D                                                               
SVCMT1   DS    A,CL80                                                           
SVCMT2   DS    A,CL80                                                           
SVCMT3   DS    A,CL80                                                           
SVCMT4   DS    A,CL80                                                           
SVCMT5   DS    A,CL80                                                           
SVCMTX   EQU   *-1                                                              
*                                                                               
SVRE     DS    F                                                                
ADDBYTES DS    F                                                                
SAVBYTES DS    F                                                                
ELCDLO   DS    C                                                                
ELCDHI   DS    C                                                                
COMDELSW DS    C                                                                
SVPDAT   DS    XL2                                                              
EKEYSAVE DS    CL7                 A-M/C/P (ESTHDR)                             
BKEYSAVE DS    CL4                 A-M/C/P (BUYREC)                             
SAVELAD  DS    F                                                                
         EJECT                                                                  
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
WORK     DS    CL24                                                             
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*SPGENCLT                                                                       
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*SPGENEST                                                                       
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
DEMOVD   DSECT                                                                  
*SPGENDOV                                                                       
       ++INCLUDE SPGENDOV                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
*SPGENBUY                                                                       
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
GLRECD   DSECT                                                                  
*SPGENGOAL                                                                      
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
DPTRECD  DSECT                                                                  
*SPGENDAYPT                                                                     
       ++INCLUDE SPGENDAYPT                                                     
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         ORG   P                                                                
*                                                                               
LAGY     DS    CL2                                                              
         DS    CL2                                                              
LMED     DS    CL1                                                              
         DS    CL1                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LSTA     DS    CL4                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LLIN     DS    CL3                                                              
         DS    CL1                                                              
         DS    CL5                                                              
LCOM     DS    CL80                                                             
LMSG     DS    CL2                                                              
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029NSSLDYPT  01/13/87'                                      
         END                                                                    
