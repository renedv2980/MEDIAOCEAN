*          DATA SET NSLDEXT    AT LEVEL 019 AS OF 12/30/86                      
*PHASE NSLDEXT,+0                                                               
*          DATA SET SPREPM7TP  AT LEVEL 066 AS OF 12/16/86                      
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE MSUNPK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE DEMOVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'NSLDEXT - FIX THE BEER'                                         
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
         NMOD1 20,DMLDEXT,RR=R5                                                 
         USING WORKD,RC                                                         
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING DMLDEXT+4096,RA                                                  
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
         MVC   TITLE(24),=CL24'SPTFILE DEMO CONVERSION'                         
*                                                                               
         LA    R1,ADCONS                                                        
         LA    RF,ADCONX                                                        
INIT1    L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         CR    R1,RF                                                            
         BL    INIT1                                                            
* CLEAR CLTTAB                                                                  
         LA    R0,4                                                             
         L     RE,ACLTTAB                                                       
         XC    0(256,RE),0(RE)                                                  
         LA    RE,256(RE)                                                       
         BCT   R0,*-10                                                          
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         LA    R3,0(R3)            CLEAR HOB                                    
         SR    RE,RE                                                            
         ICM   RE,3,13(R3)                                                      
         AR    RE,R3                                                            
         XC    0(2,RE),0(RE)       SET X'00' AT E-O-R                           
*                                                                               
         CLI   0(R3),0             TEST SPOT HEADER REC                         
         BNE   DMXPGEOF                                                         
         OC    4(9,R3),4(R3)       TEST CLIENT HEADER                           
         BNZ   XX20                                                             
* CLIENT HEADER - SAVE IT                                                       
         USING CLTHDR,R3                                                        
* MOVE CLIENT HEADER INFORMATION TO PRINT                                       
*        GOTO1 =V(PRNTBL),DMCB,=C'CLIENT',(R3),C'DUMP',13,=C'1D'                
         MVC   P(6),=C'CLIENT'                                                  
*                                                                               
         TM    CKEYAM,X'02'                                                     
         BZ    XX5                                                              
         MVC   P+10(1),=C'R'                                                    
         B     XX10                                                             
XX5      MVC   P+10(1),=C'T'                                                    
*                                                                               
XX10     GOTO1 VCLUNPK,DMCB,CKEYCLT,P+15                                        
*                                                                               
         EDIT  (2,CLEN),(4,P+50),ALIGN=LEFT                                     
*                                                                               
         MVC   P+55(20),CNAME                                                   
*                                                                               
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
*                                                                               
XX20     CLI   7(R3),0             TEST ESTIMATE = 0                            
         BE    DMXPURGE             YES - SKIP                                  
         OC    8(5,R3),8(R3)       TEST ESTIMATE = 0                            
         BNZ   DMXPURGE             YES - SKIP                                  
* ESTIMATE HEAADER - SAVE IT                                                    
         USING ESTHDR,R3                                                        
* ESTIMATE HEADER - CONVERT                                                     
         MVC   P(8),=C'ESTIMATE'                                                
*                                                                               
         TM    EKEYAM,X'02'                                                     
         BZ    XX35                                                             
         MVC   P+10(1),=C'R'                                                    
         B     XX40                                                             
XX35     MVC   P+10(1),=C'T'                                                    
*                                                                               
XX40     GOTO1 VCLUNPK,DMCB,EKEYCLT,P+15                                        
*                                                                               
         MVC   P+20(3),EKEYPRD                                                  
*                                                                               
         EDIT  (1,EKEYEST),(4,P+30),ALIGN=LEFT                                  
*                                                                               
         MVC   P+40(6),ESTART                                                   
         MVI   P+46,C'-'                                                        
         MVC   P+47(6),EEND                                                     
*                                                                               
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
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
         DS    0D                                                               
         DC    C'*CLTTAB*'                                                      
CLTTAB   DS    128D                                                             
CLTTABX  EQU   *-1                                                              
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
**PAN#1  DC    CL21'019NSLDEXT   12/30/86'                                      
         END                                                                    
