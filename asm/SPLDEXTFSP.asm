*          DATA SET SPLDEXTFSP AT LEVEL 001 AS OF 02/03/97                      
*PHASE SPEXTFSP                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTDEL - DELETE 07 (PROFILE) AND 0D60-0D64 (BWS)'           
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
         CLC   =X'07',0(06)                                                     
         BE    DMXREC10                                                         
         CLC   =X'0D41',0(R6)                                                   
         BE    DMXREC10                                                         
         CLC   =X'0D42',0(R6)                                                   
         BE    DMXREC10                                                         
         CLC   =X'0D51',0(R6)                                                   
         BE    DMXREC10                                                         
         CLC   =X'0D59',0(R6)                                                   
         BH    DMXKEEP                                                          
         CLC   =X'0D64',0(R6)                                                   
         BL    DMXKEEP                                                          
DMXREC10 DS    0H                                                               
         L     R1,COUNT            NUMBER OF RECORDS DELETED                    
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         CH    R1,=H'100'          PRINT FIRST 100 RECORDS                      
         BH    DMXPURGE                                                         
         GOTO1 VHEXOUT,DMCB,(R6),P,13,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(15),=C'RECORDS DELETED'                                        
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPLDEXTFSP02/03/97'                                      
         END                                                                    
