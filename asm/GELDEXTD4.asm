*          DATA SET GELDEXTD4  AT LEVEL 025 AS OF 11/23/93                      
*PHASE GELDD4XT                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'GELDEXT -GENDIR/FIL LOAD/DUMP MODEL EXTERNAL ROUTINE'           
*                                                                               
* EXTERN TO CHANGE ADDS STATION RECORDS WITH REPS HN AND DI TO                  
*   TO D&R                                                                      
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
         USING STTNRECD,R2                                                      
         CLI   STTNSYS,STTNSYSQ                                                 
         BNE   DMXKEEP             NO - KEEP                                    
         CLI   STTNTYP,STTNTYPQ    ADDS STATION?                                
         BNE   DMXKEEP             NO - KEEP                                    
                                                                                
         CLC   =C'DI ',STTNDREP                                                 
         BNE   DMXREC10                                                         
         AP    DIRECCHG,=P'1'                                                   
         B     DMXRECX                                                          
                                                                                
DMXREC10 DS    0H                                                               
         CLC   =C'HN ',STTNDREP                                                 
         BNE   DMXKEEP                                                          
         AP    HNRECCHG,=P'1'                                                   
                                                                                
DMXRECX  DS    0H                  REP D&R                                      
         MVC   STTNDREP,=C'D R'                                                 
         MVI   STTNDREP+1,X'50'    AMPERSAND                                    
         B     DMXKEEP                                                          
         DROP  R2                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(32),=C'DI ADDS STATION RECORDS UPDATED.'                    
         EDIT  (P4,DIRECCHG),(8,P),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
                                                                                
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(32),=C'HN ADDS STATION RECORDS UPDATED.'                    
         EDIT  (P4,HNRECCHG),(8,P),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         GETEL R6,42,ELCODE                                                     
ELCODE   DS    X                                                                
DIRECCHG DC    PL4'0'                                                           
HNRECCHG DC    PL4'0'                                                           
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
*CTGENADSTA                                                                     
       ++INCLUDE CTGENADSTA                                                     
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025GELDEXTD4 11/23/93'                                      
         END                                                                    
