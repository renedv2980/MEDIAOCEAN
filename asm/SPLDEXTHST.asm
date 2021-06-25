*          DATA SET SPLDEXTHST AT LEVEL 005 AS OF 01/23/98                      
*PHASE SPEXTHST                                                                 
         TITLE 'DMLDEXTHST - REMOVE TAX IF HST IN BUYREC'                       
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
*                                                                               
         L     R2,VLDDEFN                                                       
         USING LDDEFND,R2                                                       
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
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
         L     R3,AREC                                                          
         USING BUYRECD,R3                                                       
         CLI   0(R3),X'11'         TEST BUYREC                                  
         BL    DMXKEEP                                                          
         CLI   0(R3),X'FE'         TEST BUYREC                                  
         BH    DMXKEEP                                                          
*                                                                               
         TM    15(R3),X'80'        MARKED FOR DELETION?                         
         BO    DMXKEEP                                                          
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BZ    DMXKEEP                                                          
         OC    BDNTAX,BDNTAX       TEST TAX PRESENT                             
         BZ    DMXKEEP                                                          
* LOOK FOR HST ELEMENT                                                          
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'6B'                                                     
         MVI   ELCDHI,X'6B'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
         MVC   P(2),BUYALPHA                                                    
         GOTO1 LHEXOUT,DMCB,BUYKEY,P+3,13,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         XC    BDNTAX,BDNTAX       TAX SHOULD NOT BE PRESENT                    
         B     DMXKEEP                                                          
         EJECT                                                                  
*=================================================================              
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
BYTE     DS    X                                                                
COUNT    DS    F                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
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
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL5                                                              
         DS    CL2                                                              
POVER    DS    CL4                                                              
         DS    CL3                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL4                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         DS    CL4                                                              
PERR     DS    CL25                                                             
         DS    CL1                                                              
PMYREC   DS    CL46                                                             
         PRINT OFF                                                              
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPLDEXTHST01/23/98'                                      
         END                                                                    
