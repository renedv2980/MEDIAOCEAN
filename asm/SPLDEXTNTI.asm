*          DATA SET SPLDEXTNTI AT LEVEL 025 AS OF 05/08/00                      
*PHASE SPEXTNTI                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTNTI - FIND INVALID CLT. FOR 0D75'                        
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
         USING SLSRECD,R6                                                       
         CLC   0(2,R6),=X'0D75'                                                 
         BNE   DMXKEEP                                                          
         DC    H'00'                                                            
         CLC   8(2,R6),=C'00'      <----- COMPARE CLIENT CODE                   
         BNE   DMXKEEP                                                          
*                                                                               
         L     R1,COUNT            NUMBER OF BUY RECORDS READ                   
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,SLSKAGMD,P,1,=C'TOG'                                
         MVC   P+4(5),SLSKSTA                                                   
         GOTO1 VHEXOUT,DMCB,SLSKCLT,P+11,2,=C'TOG'                              
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
                                                                                
DMXEOF   DS    0H                                                               
         MVC   P(15),=C'INVALID CLIENTS'                                        
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
EBCMD    DS    C                   EBCDIC MEDIA                                 
EBCBYR   DS    CL3                 EBCDIC BUYER CODE                            
BINBYR   DS    X                   BINARY BUYER CODE                            
BAGYMD   DS    X                   BINARY AGENCY MEDIA                          
DELETE   DS    C                   DELETE THE RECORD FLAG                       
CHKDTBLE DS    C                   ALREADY CHECKED THE TABLE FLAG               
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
AGYHDRD   DSECT                                                                 
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPNWSBYR                                                       
BYRD     DSECT                                                                  
BYRMD    DS    C                   EBCDIC MEDIA                                 
BYREB    DS    CL3                 EBCDIC BUYER CODE                            
BYRAGYMD DS    X                   BINARY AGENCY MEDIA                          
BYRBIN   DS    X                   BINARY BUYER CODE                            
BYRLNQ   EQU   *-BYRMD                                                          
       ++INCLUDE SPGENSLST                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPLDEXTNTI05/08/00'                                      
         END                                                                    
