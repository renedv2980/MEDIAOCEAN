*          DATA SET PELDEXT7   AT LEVEL 014 AS OF 09/19/02                      
*PHASE PELDEXT7,*                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'PELDEXT - DELETE PAN XREF RECORDS '                             
*************************************************************                   
*   NOTE - THIS EXTRNAL FROMS PART OF THE PANX SYSTEM       *                   
*   PLEASE DO NOT CHANGE                                    *                   
*************************************************************                   
*                                                           *                   
*   THIS EXTERNAL REMOVES ALL THE OLD PANX DATA EACH WEEK   *                   
*   TO MAKE WAY FOR THE NEXT XREF LOAD.                     *                   
*                                                           *                   
*************************************************************                   
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
         NMOD1 (WORKX-WORKD),MPLDEXT                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         L     RF,=V(HELLO)                                                     
         ST    RF,VHELLO                                                        
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
         CLC   0(1,R2),=X'02'                                                   
         BNE   DMXKEEP                                                          
         BE    DMXPURGE                                                         
*                                                                               
DMXRDIE1 MVC   P+1(18),=C'HELLO DELETE ERROR'                                   
         B     *+10                                                             
DMXRDIE2 MVC   P+1(15),=C'HELLO PUT ERROR'                                      
         GOTO1 VPRINTER                                                         
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(13),=C'MODIFIED RECS'                                       
         EDIT  (P4,MODRECS),(8,P)                                               
         AP    MODRECS,MODRECS                                                  
         BNZ   *+10                                                             
         MVC   P(8),=8C'.'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+10(25),=C'ERR 1 - NO INFO ELEMENT'                             
         EDIT  (P4,ERR1),(8,P)                                                  
         AP    ERR1,ERR1                                                        
         BNZ   *+10                                                             
         MVC   P(8),=8C'.'                                                      
         GOTO1 VPRINTER                                                         
         MVC   P+10(23),=C'ERR 2 - ZERO LEN ELEMENT'                            
         EDIT  (P4,ERR2),(8,P)                                                  
         AP    ERR2,ERR2                                                        
         BNZ   *+10                                                             
         MVC   P(8),=8C'.'                                                      
         GOTO1 VPRINTER                                                         
         MVC   P+10(21),=C'ERR 3 - INFO EXISTS    '                             
         EDIT  (P4,ERR3),(8,P)                                                  
         AP    ERR3,ERR3                                                        
         BNZ   *+10                                                             
         MVC   P(8),=8C'.'                                                      
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
FILNAME  DC    CL8'PERFIL'                                                      
MODRECS  DC    PL4'0'                                                           
ERR1     DC    PL4'0'                                                           
ERR2     DC    PL4'0'                                                           
ERR3     DC    PL4'0'                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
VHEXOUT  DS    A                                                                
VHELLO   DS    A                                                                
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PELDEXT7  09/19/02'                                      
         END                                                                    
