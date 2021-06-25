*          DATA SET GELDEXTBC  AT LEVEL 016 AS OF 05/11/93                      
*PHASE GELDBCXT                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'GELDEXT -GENDIR/FIL LOAD/DUMP MODEL EXTERNAL ROUTINE'           
*                                                                               
* EXTERN TO PURGE OLD BROADCAST RECORDS                                         
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
         NMOD1 (WORKX-WORKD),GELDEXT                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
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
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R2,AREC                                                          
         TM    34(R2),X'80'        ALREADY MARKED FOR DELETION?                 
         BO    DMXPURGE            YES -- LET IT VANISH                         
*                                                                               
         USING BRDKEYD,R2                                                       
         CLC   =X'0009',BRDKEY     BROADCAST RECORD?                            
         BNE   DMXKEEP             NO - KEEP                                    
         CLI   BRDKTYPE,BRDKTEMQ   TEMPORARY MESSAGE?                           
         BNE   DMXKEEP             NO - KEEP                                    
         OC    BRDKMSGN,BRDKMSGN   MUST BE A MESSAGE NUMBER                     
         BZ    DMXKEEP             IT'S THE HIGH-MESSAGE RECORD                 
*                                                                               
         LA    R2,BRDFSTEL                                                      
         CLI   0(R2),BRDFLTCQ                                                   
         BE    *+6                                                              
         DC    H'0'                FIRST ELEMENT MUST BE FILTER ELEMENT         
*                                                                               
         USING BRDFLTD,R2                                                       
         CLC   BRDFENDT,=X'BAA1'   DID MESSAGE EXPIRE BEFORE MAY1/93?           
         BNL   DMXKEEP             NO                                           
*                                                                               
         AP    RECDEL,=P'1'                                                     
         B     DMXPURGE                                                         
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(37),=C'BROADCAST RECORDS DELETED BY DAVID E.'               
         EDIT  (P4,RECDEL),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         GETEL R6,42,ELCODE                                                     
ELCODE   DS    X                                                                
RECDEL   DC    PL4'0'                                                           
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
*                                                                               
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
         EJECT                                                                  
*CTGENBRD                                                                       
       ++INCLUDE CTGENBRD                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016GELDEXTBC 05/11/93'                                      
         END                                                                    
