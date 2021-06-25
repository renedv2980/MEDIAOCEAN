*          DATA SET RELDEZP    AT LEVEL 050 AS OF 05/01/02                      
*PHASE RELDEZP                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'RELDCAN - SCAN FOR CONTRACTS HIT BY EZPOST'                     
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
         BNE   DMXR100                                                          
*                                                                               
         LA    R3,COUNTBLK                                                      
         CLC   =C'NB',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'UV',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'UT',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'PV',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'AM',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'CQ',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'SZ',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'B3',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLC   =C'LS',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMXR050  DS    0H                                                               
         USING COUNTBLK,R3                                                      
         AP    CONCOUNT,=P'1'                                                   
*                                                                               
         MVI   ELCODE,X'A7'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXR060                                                          
                                                                                
         USING RCONEZEL,R5                                                      
         AP    EZPCOUNT,=P'1'                                                   
         SR    RE,RE                                                            
         ICM   RE,3,RCONEZNM                                                    
         CVD   RE,DUB                                                           
         AP    DWNCOUNT,DUB                                                     
         DROP  R5,R3                                                            
*                                                                               
DMXR060  DS    0H                                                               
*                                                                               
DMXR100  DS    0H                                                               
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
         MVC   P(L'HEADERC),HEADERC                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R3,COUNTBLK                                                      
         USING COUNTBLK,R3                                                      
DMCNT010 DS    0H                                                               
         MVC   P+1(2),COUNTBLK                                                  
         EDIT  (P10,CONCOUNT),(12,P+5)                                          
         EDIT  (P10,EZPCOUNT),(12,P+22)                                         
         EDIT  (P10,DWNCOUNT),(12,P+38)                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         AHI   R3,COUNTLQ                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   DMCNT010                                                         
*                                                                               
         B     DMXIT                                                            
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
COUNTBLK DC    C'NB'                                                            
CONCOUNT DC    PL10'0'                                                          
EZPCOUNT DC    PL10'0'                                                          
DWNCOUNT DC    PL10'0'                                                          
COUNTLQ  EQU   *-COUNTBLK                                                       
         DC    C'UV'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    C'UT'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    C'PV'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    C'AM'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    C'CQ'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    C'SZ'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    C'B3'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    C'LS'                                                            
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    PL10'0'                                                          
         DC    X'FFFF'                                                          
*                0123456789-123456789-123456789-1234567890-123456789-           
HEADERC  DC    C'REP      CONTRACTS         EZ-POSTS        DOWNLOADS'          
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
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
ELEMENT  DS    XL256                                                            
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
**PAN#1  DC    CL21'050RELDEZP   05/01/02'                                      
         END                                                                    
