*          DATA SET RELDUVP    AT LEVEL 010 AS OF 06/12/08                      
*PHASE RELDUVP                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'RELDUV - LOAD/DUMP MODEL EXTERNAL ROUTINE'                      
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
*  CHECK OF CFX CONTRACTS AND BUYS WITH CODE 'M'                  *             
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
         AP    PURGE,=P'1'                                                      
*        AP    10(5,R5),=P'1'                                                   
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
         MVC   P(08),=C'STARTED!'                                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RCONREC,R5                                                       
         CLI   0(R5),X'0C'                                                      
         BE    DMXCON10                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXCON10 DS    0H                                                               
         CLC   =C'B3',RCONKREP                                                  
         BNE   DMXCON15                                                         
         CLC   =C'WAF',RCONSAL                                                  
         BE    DMXCON20                                                         
*                                                                               
DMXCON15 DS    0H                                                               
         CLC   =C'UV',RCONKREP                                                  
         BNE   DMXKEEP                                                          
*                                                                               
DMXCON20 DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RCONREC,R5                                                       
         TM    RCONMODR,X'10'      PENDING?                                     
         BO    DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         MVI   ELCODE,X'12'        EXPANPDED SAR ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   DMXCON30                                                         
                                                                                
         USING RSARXEL,R5                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS FORECAST FLAG          
         BL    DMXCON30                                                         
         TM    RSARXFLG,X'10'      FLAGGED AS FORECAST?                         
         BO    DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
DMXCON30 DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BE    DMXKEEP                                                          
*                                                                               
         L     R5,AREC                                                          
         MVI   ELCODE,4                                                         
         BAS   RE,GETEL                                                         
         BE    DMXKEEP                                                          
*                                                                               
         L     R5,AREC                                                          
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         BE    DMXKEEP                                                          
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RCONREC,R5                                                       
         MVI   RCONTYPE,C'Z'                                                    
*                                                                               
DMXCON99 DS    0H                                                               
         MVC   P(2),RCONKOFF                                                    
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+4,4                                   
         GOTO1 =V(DATCON),DMCB,(3,RCONHDRD),(5,P+14)                            
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R5                                                               
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
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'READ  '                                                
         EDIT  (P5,READ),(7,P+11)                                               
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'MKGD'                                                  
         EDIT  (P5,MKGD),(7,P+11)                                               
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+3(33),=C'DETAILS OF PURGED RECORDS FOLLOWS'                    
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
READ     DC    PL5'0'                                                           
MKGD     DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF REP CODES TO PURGE                                                   
*                                                                               
PRGLST   DS    0CL2                                                             
         DC    C'YH'                                                            
         DC    X'FF'                                                            
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
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010RELDUVP   06/12/08'                                      
         END                                                                    
