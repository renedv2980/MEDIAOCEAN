*          DATA SET SPLDFORD   AT LEVEL 003 AS OF 07/18/97                      
*          DATA SET SPLDEXTZE  AT LEVEL 010 AS OF 07/26/96                      
*PHASE SPEXFRD                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
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
         CLC   0(3,R6),=X'0D2013'                                               
         BNE   DMXPURGE                                                         
                                                                                
         USING NPGRECD,R6          CHECK IF HAS 61 ELEMENT                      
         CLC   NPGKEND,=XL2'C221'  JAN1/97                                      
         BL    DMXPURGE                                                         
         CLC   PRCOUNT,=F'50'                                                   
         BH    PR50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',50,=C'1D'                  
PR50     MVI   NPGKAM,X'33'      NEW AGENCY                                     
         CLC   PRCOUNT,=F'50'                                                   
         BH    DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTE',AREC,C'DUMP',50,=C'1D'                  
         L     RE,PRCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,PRCOUNT                                                       
         B     DMXKEEP                                                          
                                                                                
DMXEOF   DS    0H                                                               
         MVC   P(5),=C'COUNT'                                                   
         EDIT  (4,COUNT),(8,P+10),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
ELEMENT  DC    XL6'6106B2650100'                                                
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
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENPROGA                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPLDFORD  07/18/97'                                      
         END                                                                    
