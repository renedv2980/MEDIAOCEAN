*          DATA SET RELDPRP    AT LEVEL 059 AS OF 05/01/02                      
*PHASE RELDPRP                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRTREC                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'RELDPRP - FIX CONTRACT DAYPART ELEMENTS'                        
*                                                                               
* FILE FIX TO TRUNCATE DAYPART ELEMENTS(X'41') ELEMENTS IN CONTRACTS            
*  WHICH HAVE MORE THAN 8 DAYPARTS DUE TO A MISSING CHECK IN                    
* PROPOSER (REPRP00)                                                            
* -------------------------------------------------------------------           
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
*******************************************************************             
*                                                                 *             
* REMOVE POOL/BRAND DARE ORDERS                                   *             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
         L     RE,=V(PRINT)                                                     
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
         L     RE,=V(HEXOUT)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
*        AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
*                                                                               
         USING RCONREC,R5                                                       
         CLI   RCONKTYP,X'0C'                                                   
         BNE   DMXKEEP                                                          
*                                                                               
         AP    CONCOUNT,=P'1'                                                   
*                                                                               
         MVI   ELCODE,X'41'        GET DPT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP             NOT FOUND                                    
*                                                                               
         SR    R0,R0               CALC # OF DAYPARTS                           
         ZIC   R1,1(R5)                                                         
         AHI   R1,-RCPRDPOX                                                     
         SR    RE,RE                                                            
         LA    RE,L'RCPRDPDP                                                    
         DR    R0,RE                                                            
*                                                                               
         CHI   R1,8                8 OR LESS IS OK                              
         BNH   DMXKEEP                                                          
*                                                                               
         LR    R2,R1               SAVE NUMBER OF DAYPARTS                      
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R5)            SAVE AND DELETE ELEMENT                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R5)                                                    
         GOTO1 =V(RECUP),DMCB,(C'R',AREC),(R5)                                  
*                                                                               
         AHI   R2,-8               GET # OF DPTS TO BE DELETED                  
         MHI   R2,L'RCPRDPDP                                                    
         ZIC   RE,ELEM+1                                                        
         SR    RE,R2                                                            
*                                                                               
         XC    WORK,WORK           CREATE NEW ELEMENT                           
         LR    R2,RE                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ELEM                                                     
         STC   RE,WORK+1           SET NEW ELEMENT LENGTH                       
*                                                                               
*****    MVC   P(2),=C'B:'                                                      
*****    GOTO1 =V(HEXOUT),DMCB,ELEM,P+4,60                                      
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    MVC   P(2),=C'A:'                                                      
*****    GOTO1 =V(HEXOUT),DMCB,WORK,P+4,60                                      
*****    GOTO1 VPRINTER                                                         
*                                                                               
         L     R1,AREC                                                          
         LA    R1,RCONELEM-RCONKEY(R1)                                          
DMXR0010 CLI   0(R1),0             END OF RECORD?                               
         BE    DMXR0012            YES                                          
         CLI   0(R1),X'41'         IS THIS THE PLACE?                           
         BL    DMXR0012            YES                                          
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DMXR0010                                                         
*                                                                               
DMXR0012 DS    0H                                                               
         LR    R0,R1                                                            
         GOTO1 =V(RECUP),DMCB,(C'R',AREC),WORK,(R0)                             
*                                                                               
         AP    CHGCOUNT,=P'1'                                                   
*                                                                               
         B     DMXKEEP                                                          
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE 1                                                                
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+5(10),=CL10'CONTRACT'                                          
         MVC   P+25(10),=CL10'CHANGES'                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (P10,CONCOUNT),(12,P+5)                                          
         EDIT  (P10,CHGCOUNT),(12,P+25)                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
CONCOUNT DC    PL10'0'                                                          
CHGCOUNT DC    PL10'0'                                                          
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
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
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
PRINT    DS    A                                                                
HEXOUT   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    XL256                                                            
ELEM     DS    XL256                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059RELDPRP   05/01/02'                                      
         END                                                                    
