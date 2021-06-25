*          DATA SET CTGENEXT   AT LEVEL 003 AS OF 04/27/87                      
*PHASE CTGENEXT,*                                                               
*INCLUDE HEXOUT                                                                 
         TITLE 'CTGENEXT - EXTERNAL TO MODIFY ALL "EUK" TO "ENG'                
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
*                                                                               
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 (WORKX-WORKD),*EUK/NG*,RR=R2                                     
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         ST    R2,RELO                                                          
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         A     RF,RELO                                                          
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
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  B     DMXIT                                                            
         SPACE 2                                                                
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING GMSGD,R3                                                         
         CLI   GMKLANG,X'FE'                                                    
         BNE   DMXR10                                                           
         MVI   GMKLANG,X'FF'                                                    
         AP    MODRECS,=P'1'                                                    
         B     DMXKEEP                                                          
DMXR10   AP    OKRECS,=P'1'                                                     
         B     DMXKEEP                                                          
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P+1                                                   
         EDIT  (P4,OKRECS),(8,P)                                                
         AP    OKRECS,OKRECS                                                    
         BNZ   *+10                                                             
         MVC   P(8),=8C'.'                                                      
         MVC   P+9(12),=C'RECS LANG OK'                                         
         GOTO1 VPRINTER                                                         
         EDIT  (P4,MODRECS),(8,P)                                               
         AP    MODRECS,MODRECS                                                  
         BNZ   *+10                                                             
         MVC   P(8),=8C'.'                                                      
         MVC   P+9(17),=C'RECS LANG CHANGED'                                    
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
OKRECS   DC    PL4'0'                                                           
MODRECS  DC    PL4'0'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
VHEXOUT  DS    V                                                                
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
WORK     DS    20CL32              ROOM FOR 20 SCANNER BLOCKS                   
WORKX    DS    0C                                                               
         SPACE                                                                  
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*GEGENMSG                                                                       
         PRINT OFF                                                              
       ++INCLUDE GEGENMSG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTGENEXT  04/27/87'                                      
         END                                                                    
