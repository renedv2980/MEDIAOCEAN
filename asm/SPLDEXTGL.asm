*          DATA SET SPLDEXTGL  AT LEVEL 009 AS OF 02/09/98                      
*PHASE SPEXTGL                                                                  
*INCLUDE PRNTBL                                                                 
SPEXTGL  TITLE 'SPEXTGL - CHANGE COKE RADIO GOALS TO 60 SECS'                   
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
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
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
         USING GOALRECD,R6                                                      
         CLI   GKEYTYPE,X'02'      GOAL RECORD                                  
         BNE   DMXPURGE            NO                                           
         CLC   =C'CK',GAGYALPH     CK AGENCY?                                   
         BNE   DMXPURGE            NO                                           
         CLI   GKEYPRD,X'FF'       SKIP CPP GUIDES                              
         BE    DMXPURGE                                                         
*                                                                               
         L     R5,PLIST+12         GET LDDEFN ADDRESS                           
         USING LDDEFND,R5                                                       
         CP    CHGCOUNT,=P'100'                                                 
         BH    NOPRT                                                            
         GOTO1 =V(PRNTBL),DMCB,=C'CHANGE',(R6),C'DUMP',24,=C'1D',      X        
               (C'P',LPRINT)                                                    
NOPRT    AP    CHGCOUNT,=P'1'                                                   
         B     DMXKEEP                                                          
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(8),=C'CHANGES='                                                
         OI    CHGCOUNT+3,X'0F'                                                 
         UNPK  P+8(6),CHGCOUNT                                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
CHGCOUNT DC    PL4'0'                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*        GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPLDEXTGL 02/09/98'                                      
         END                                                                    
