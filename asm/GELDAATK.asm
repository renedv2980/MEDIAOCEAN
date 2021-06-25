*          DATA SET GELDAATK   AT LEVEL 013 AS OF 03/06/96                      
*PHASE GELDAATK,*                                                               
*INCLUDE HEXOUT                                                                 
         TITLE 'GELDAATK -GENDIR/FIL LOAD/DUMP FOR CON/NFI REMOVAL'             
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
         NMOD1 (WORKX-WORKD),GELDAATK                                           
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
         USING FDRRECD,R2          FIELD RECORD DSECT                           
         TM    FDRFSTAT,X'80'      DELETED RECORD                               
         BNZ   DMXPURGE                                                         
         CLC   =C'XA',FDRKMIN      ACTION RECORD?                               
         BE    ACTIONR                                                          
         CLC   =C'XD',FDRKMIN      DOWNLOAD RECORD?                             
         BE    DLOADR                                                           
         CLC   =C'XF',FDRKMIN      FIELD RECORD?                                
         BE    FIELDR                                                           
         CLC   =C'XP',FDRKMIN      PFKEY RECORD?                                
         BE    PFKEYR                                                           
         CLC   =C'XR',FDRKMIN      RECORD RECORD?                               
         BE    RECORDR                                                          
         CLC   =C'XS',FDRKMIN      SCREEN RECORD?                               
         BE    SCREENR                                                          
         CLC   =C'XX',FDRKMIN      DEFCLM RECORD?                               
         BE    DEFCLMR                                                          
         B     DMXKEEP                                                          
*                                                                               
         USING FRARECD,R2                                                       
ACTIONR  CLI   FRAKSYS,10          KILL ALL CONTROL FILE RECORDS                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING FDLRECD,R2                                                       
DLOADR   CLI   FDLKSYS,10          KILL ALL CONTROL FILE RECORDS                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING FDRRECD,R2                                                       
FIELDR   CLI   FDRKSYS,10          KILL ALL CONTROL FILE RECORDS                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING FRPRECD,R2                                                       
PFKEYR   CLI   FRPKSYS,10          KILL ALL CONTROL FILE RECORDS                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING FRRRECD,R2                                                       
RECORDR  CLI   FRRKSYS,10          KILL ALL CONTROL FILE RECORDS                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING FSRRECD,R2                                                       
SCREENR  CLI   FSRKSYS,10          KILL ALL CONTROL FILE RECORDS                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING FCRRECD,R2                                                       
DEFCLMR  CLI   FCRKSYS,10          KILL ALL CONTROL FILE RECORDS                
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'DELETED RECS'                                        
         EDIT  (P4,RECDEL),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
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
         PRINT OFF                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         SPACE 2                                                                
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*GEGENSCR                                                                       
       ++INCLUDE GEGENSCR                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013GELDAATK  03/06/96'                                      
         END                                                                    
