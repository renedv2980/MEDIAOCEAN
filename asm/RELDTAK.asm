*          DATA SET RELDTAK    AT LEVEL 010 AS OF 03/20/09                      
*          DATA SET RELDTAK    AT LEVEL 016 AS OF 06/20/08                      
*PHASE RELDTAK                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'RELDTAK - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*  FIX BUY TOTAL COST, WEEK AND SPOTS                             *             
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
         USING RBUYREC,R5                                                       
         CLC   0(2,R5),=X'0B00'                                                 
         BNE   DMXKEEP                                                          
         CLC   =C'BL',RBUYKREP                                                  
         BNE   DMXKEEP                                                          
         OC    RBUYTSPT,RBUYTSPT                                                
         BNZ   DMXKEEP                                                          
         OC    RBUYTWKS,RBUYTWKS                                                
         BNZ   DMXKEEP                                                          
         OC    RBUYTCOS,RBUYTCOS                                                
         BZ    DMXKEEP                                                          
*                                                                               
         XC    SVTOTSPT,SVTOTSPT                                                
         XC    SVTOTWKS,SVTOTWKS                                                
         MVC   SVBUYCOS,RBUYCOS                                                 
         DROP  R5                                                               
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING RBUYDTEL,R5                                                      
*                                                                               
DMXBUY20 DS    0H                                                               
         ZIC   RE,RBUYDTWK         GET NUMBER OF WEEKS                          
         L     RF,SVTOTWKS         CALCULATE TOTAL NUMBER OF WEEKS              
         AR    RF,RE                                                            
         ST    RF,SVTOTWKS         SAVE TOTAL NUMBER OF WEEKS                   
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RBUYDTNW  GET SPOTS/WEEK                               
         MH    RE,HALF             NUM WKS * SPTS/WK = TOTAL SPOTS              
         L     RF,SVTOTSPT         CALCULATE TOTAL NUMBER SPOTS                 
         AR    RF,RE                                                            
         ST    RF,SVTOTSPT         SAVE TOTAL NUMBER SPOTS                      
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DMXBUY20                                                         
         DROP  R5                                                               
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RBUYREC,R5                                                       
         SR    RE,RE                                                            
         L     RF,SVTOTSPT         LOAD TOTAL # SPOTS                           
         M     RE,SVBUYCOS         TOT SPOTS * COST =                           
                                                                                
         STCM  RF,15,RBUYTCOS      TOTAL COST OF BUY                            
         MVC   RBUYTSPT,SVTOTSPT+2 LOAD TOTAL NUMBER OF SPOTS                   
         MVC   RBUYTWKS,SVTOTWKS+3 LOAD TOTAL NUMBER OF WEEKS                   
*                                                                               
         MVC   P+10(2),RBUYKREP                                                 
*                                                                               
         PACK  MYWORK(1),RBUYKCON+3(1) REVERSE THE COMPLIMENT                   
         PACK  MYWORK+1(1),RBUYKCON+2(1)                                        
         PACK  MYWORK+2(1),RBUYKCON+1(1)                                        
         PACK  MYWORK+3(1),RBUYKCON(1)                                          
*                                                                               
         ZAP   MYWORK+20(5),=P'0'                                               
         MVO   MYWORK+20(5),MYWORK(4)                                           
         ZAP   MYWORK+10(5),=P'99999999'                                        
         SP    MYWORK+10(5),MYWORK+20(5)                                        
*                                                                               
         EDIT  (P5,MYWORK+10),(8,P+14)                                          
*                                                                               
         EDIT  RBUYKLIN,(3,P+30)                                                
*                                                                               
         EDIT  (4,RBUYCOS),(9,P+40),2,FLOAT=-                                   
         EDIT  (1,RBUYTWKS),(8,P+50)                                            
         EDIT  (2,RBUYTSPT),(8,P+60)                                            
         EDIT  (4,RBUYTCOS),(9,P+70),2,FLOAT=-                                  
         GOTO1 VPRINTER                                                         
*                                                                               
*        MVC   P(100),0(R5)                                                     
*        GOTO1 VPRINTER                                                         
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
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'DARE  '                                                
         EDIT  (P5,DARE),(7,P+11)                                               
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
DARE     DC    PL5'0'                                                           
MKGD     DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
CONNUM   DC    XL4'0'                                                           
BUYDAY   DC    XL1'0'                                                           
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
SVTOTSPT DS    F                                                                
SVTOTWKS DS    F                                                                
SVBUYCOS DS    F                                                                
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
**PAN#1  DC    CL21'010RELDTAK   03/20/09'                                      
         END                                                                    
