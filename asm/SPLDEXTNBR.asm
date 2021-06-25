*          DATA SET SPLDEXTNBR AT LEVEL 047 AS OF 06/16/99                      
*PHASE SPEXTNBR                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'SPLDEXTNBR - SPACE MISSING IN END OF PROG, NOT SPECIAL'         
* DON'T FORGET TO CHANGE THE TWO BYTE AGENCY CODE!!!!!!!                        
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
         CLI   0(R6),X'10'         BUY REOCRD?                                  
         BNH   DMXKEEP             NO, KEEP IT                                  
*                                                                               
         USING BUYREC,R6                                                        
         CLI   BDPROGRM+L'BDPROGRM-1,0                                          
         BNE   DMXKEEP             NO PROBLEM WITH SPECIAL HERE                 
*                                                                               
         LA    RF,BDPROGRM+L'BDPROGRM-2   CHECK IF WE HAVE A '-S'               
         LA    R0,BDPROGRM                                                      
*                                                                               
DMXREC10 CLI   0(RF),C' '                                                       
         BNH   DMXREC15                                                         
         CLI   0(RF),C'S'          REAL SPECIAL?                                
         BNE   DMXREC30                                                         
         BCTR  RF,0                                                             
         CR    RF,R0                                                            
         BL    DMXREC30                                                         
         CLI   0(RF),C'-'                                                       
         BE    DMXKEEP             YES, LEAVE PROGRAM ALONE                     
         B     DMXREC30                                                         
*                                                                               
DMXREC15 BCTR  RF,0                                                             
         CR    RF,R0                                                            
         BH    DMXREC10            IF EQUAL, THEN WE CAN'T HAVE A '-S'          
*                                                                               
DMXREC30 MVI   BDPROGRM+L'BDPROGRM-1,C' '                                       
*                                                                               
DMXREC50 L     R1,COUNT            NUMBER OF BUY RECORDS WITH SPECIAL           
         LA    R1,1(R1)               THAT SHOULDN'T BE                         
         ST    R1,COUNT                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P,13,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
                                                                                
DMXEOF   DS    0H                                                               
         MVC   P(15),=C'RECORDS CHANGED'                                        
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         GETEL R6,24,ELCODE                                                     
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
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPLDEXTNBR06/16/99'                                      
         END                                                                    
