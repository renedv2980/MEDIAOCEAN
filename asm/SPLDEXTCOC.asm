*          DATA SET SPLDEXTCOC AT LEVEL 005 AS OF 07/25/96                      
*PHASE SPEXTCOC                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTCOC - CHANGE COKE PRODUCT FROM COC TO CO'                
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
         USING CLTHDRD,R6                                                       
         CLC   CKEYTYPE(2),=X'00B1' CHECK IF COKE CLT/PRD/EST HEADER            
         BE    *+14                                                             
         CLC   CKEYTYPE(2),=X'00B2'                                             
         BNE   DMXKEEP                                                          
                                                                                
         OC    CKEYCLT+2(9),CKEYCLT+2   CLIENT HEADER?                          
         BNZ   DMXREC40                 NO                                      
                                                                                
         LA    R5,CLIST                                                         
DMXREC10 CLI   0(R5),C'A'                                                       
         BL    DMXREC30                                                         
         CLC   0(3,R5),=C'COC'                                                  
         BE    DMXREC20                                                         
         LA    R5,4(R5)                                                         
         B     DMXREC10                                                         
                                                                                
DMXREC20 MVI   2(R5),C' '                                                       
****     MVC   P(132),CLIST                                                     
*****    GOTO1 VPRINTER                                                         
         L     R1,CLTCOUNT         NUMBER OF CLIENT RECORDS CHANGED             
         LA    R1,1(R1)                                                         
         ST    R1,CLTCOUNT                                                      
         B     DMXKEEP                                                          
                                                                                
DMXREC30 MVC   P(13),=C'COC NOT FOUND'                                          
         GOTO1 VHEXOUT,DMCB,(R6),P+20,13,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R6                                                               
                                                                                
         USING ESTHDRD,R6                                                       
DMXREC40 CLC   EKEYPRD,=C'COC'                                                  
         BNE   DMXKEEP                                                          
         MVI   EKEYPRD+2,C' '                                                   
         L     R1,PRDCOUNT                                                      
         LA    R1,1(R1)                                                         
         ST    R1,PRDCOUNT                                                      
****     GOTO1 VHEXOUT,DMCB,(R6),P,13,=C'TOG'                                   
******   GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R6                                                               
                                                                                
DMXEOF   DS    0H                                                               
         MVC   P(9),=C'CLT COUNT'                                               
         EDIT  (4,CLTCOUNT),(8,P+20),ZERO=NOBLANK                               
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'PRD AND EST COUNT'                                      
         EDIT  (4,PRDCOUNT),(8,P+20),ZERO=NOBLANK                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
BYTE     DS    X                                                                
CLTCOUNT DS    F                   NUMBER OF CLIENT RECORDS CHANGED             
PRDCOUNT DS    F                   NUMBER OF PRODUCT & ESTIMATES CHGD           
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
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPLDEXTCOC07/25/96'                                      
         END                                                                    
