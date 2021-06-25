*          DATA SET SPLDEXTBDC AT LEVEL 001 AS OF 03/13/95                      
*PHASE SPEXTBDC                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTBDC - CHANGE BDCIND AND BDCIND2'                         
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
         CLI   0(R6),X'10'                                                      
         BNH   DMXKEEP                                                          
         MVC   BYTE,0(R6)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,4                                                           
         BNH   DMREC1                                                           
         MVC   P(9),=C'NOT VALID'                                               
         GOTO1 VPRINTER                                                         
         L     R1,COUNT4           NUMBER OF BUY RECORDS W/0 01 ELEM            
         LA    R1,1(R1)                                                         
         ST    R1,COUNT4                                                        
         B     DMREC20                                                          
*                                                                               
DMREC1   L     R1,COUNT            NUMBER OF BUY RECORDS READ                   
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         USING BUYREC,R6                                                        
*                                                                               
         CLI   BDELEM,X'01'                                                     
         BE    DMREC2                                                           
         MVC   P(10),=C'NO 01 ELEM'                                             
         GOTO1 VPRINTER                                                         
         L     R1,COUNT3           NUMBER OF BUY RECORDS W/0 01 ELEM            
         LA    R1,1(R1)                                                         
         ST    R1,COUNT3                                                        
         B     DMREC20                                                          
*                                                                               
DMREC2   TM    BDCIND2,X'04'       ALREADY THE NEW WAY?                         
         BNO   DMREC5                                                           
         MVC   P(11),=C'NOT CHANGED'                                            
         GOTO1 VPRINTER                                                         
         L     R1,COUNT2           NUMBER OF BUY RECORDS NOT CHANGED            
         LA    R1,1(R1)                                                         
         ST    R1,COUNT2                                                        
         B     DMREC20                                                          
*                                                                               
DMREC5   OI    BDCIND2,X'04'       BDCIND WILL BE CHARACTERS                    
         TM    BDCIND,X'01'        NEGATIVE                                     
         BNO   *+8                                                              
         OI    BDCIND2,BDC2NEG     NEGATIVE (NEW WAY)                           
*                                                                               
         TM    BDCIND2,X'80'       COMMISSION                                   
         BNO   DMREC7                                                           
         MVI   BDCIND,BDCC                                                      
         NI    BDCIND2,X'FF'-X'80' NOT USING ANYMORE                            
         B     DMREC10                                                          
*                                                                               
DMREC7   TM    BDCIND,X'80'        FEE                                          
         BNO   *+12                                                             
         MVI   BDCIND,BDCF                                                      
         B     DMREC10                                                          
*                                                                               
         TM    BDCIND,X'40'        Q-RATE                                       
         BNO   *+12                                                             
         MVI   BDCIND,BDCQ                                                      
         B     DMREC10                                                          
*                                                                               
         TM    BDCIND,X'20'        GROSS                                        
         BNO   *+12                                                             
         MVI   BDCIND,BDCGRS                                                    
         B     DMREC10                                                          
*                                                                               
         TM    BDCIND,X'10'        N-RATE                                       
         BNO   *+12                                                             
         MVI   BDCIND,BDCN                                                      
         B     DMREC10                                                          
*                                                                               
         TM    BDCIND,X'08'        V-RATE                                       
         BNO   *+12                                                             
         MVI   BDCIND,BDCV                                                      
         B     DMREC10                                                          
*                                                                               
         TM    BDCIND,X'04'        SPECIAL - NO COMMISSION                      
         BNO   *+12                                                             
         MVI   BDCIND,BDCS                                                      
         B     DMREC10                                                          
*                                                                               
         TM    BDCIND,X'02'        X-RATE                                       
         BNO   *+12                                                             
         MVI   BDCIND,BDCX                                                      
         B     DMREC10                                                          
*                                                                               
         MVI   BDCIND,BDCNTP       OTHERWISE NTP                                
*                                                                               
DMREC10  CLC   COUNT,=F'100'                                                    
         BH    DMXKEEP                                                          
*                                                                               
DMREC20  GOTO1 VHEXOUT,DMCB,(R6),P,13,=C'TOG'                                   
         GOTO1 VHEXOUT,DMCB,BDCIND,P+30,15,=C'TOG'                              
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(12),=C'RECORDS READ'                                           
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         MVC   P(19),=C'RECORDS NOT CHANGED'                                    
         EDIT  (4,COUNT2),(8,P+25),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         MVC   P(19),=C'RECORDS W/O 01 ELEM'                                    
         EDIT  (4,COUNT3),(8,P+25),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'RECORDS NOT VALID'                                      
         EDIT  (4,COUNT4),(8,P+25),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
COUNT2   DS    F                                                                
COUNT3   DS    F                                                                
COUNT4   DS    F                                                                
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
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPLDEXTBDC03/13/95'                                      
         END                                                                    
