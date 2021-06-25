*          DATA SET SPLDMCNN   AT LEVEL 004 AS OF 11/19/98                      
*PHASE SPEXMCN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXTZEN - ADD 61 ELEMENT FOR ZENITH'                         
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
         GOTO1 =V(PRNTBL),DMCB,=C'DELE',AREC,C'DUMP',50,=C'1D'                  
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
         CLC   0(2,R6),=X'0053'                                                 
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   4(9,R6),=XL9'00'     CLIENT RECORD                               
         BE    CKCLT                                                            
*                                                                               
         CLC   7(6,R6),=XL6'00'     PRODUCT RECORD                              
         BNE   DMXKEEP                                                          
*                                                                               
CKCLT    LA    RE,CLITAB                                                        
*                                                                               
CKCLI20  CLI   0(RE),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   0(2,RE),2(R6)                                                    
         BE    DMXKEEP                                                          
         LA    RE,2(RE)                                                         
         B     CKCLI20                                                          
*                                                                               
                                                                                
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
CLITAB   DC    XL2'8840'            CCA                                         
         DC    XL2'8842'            CCC                                         
         DC    XL2'8847'            CCH                                         
         DC    XL2'8853'            CCT                                         
         DC    XL2'885F'            CC                                          
         DC    XL2'8940'            CKA                                         
         DC    XL2'A5A9'            JNJ                                         
         DC    X'FF'                                                            
COUNT    DS    F                   NUMBER OF RECORDS CHANGED                    
ELCODE   DS    X                                                                
PRCOUNT  DS    F                                                                
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
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPLDMCNN  11/19/98'                                      
         END                                                                    
