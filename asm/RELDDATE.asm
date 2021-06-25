*          DATA SET RELDDATE   AT LEVEL 048 AS OF 07/12/04                      
*          DATA SET RELDTEST   AT LEVEL 240 AS OF 09/20/95                      
*PHASE RELDDATE                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'RELDDEMO - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
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
*  DARE RECORD PURGE                                              *             
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
         USING RDARREC,R5                                                       
         CLC   =X'11',0(R5)                                                     
         BE    DMXREC10                                                         
         BL    DMXPGEOF                                                         
         B     DMXKEEP                                                          
         CLC   =X'4100',RDARKTYP                                                
         BE    DMXREC80                                                         
         CLC   =X'5100',RDARKTYP                                                
         BE    DMXREC80                                                         
         B     DMXKEEP                                                          
*&&DO                                                                           
         CLC   =C'CB',RDARKREP                                                  
         BNE   DMXKEEP                                                          
         CLC   =C'NY',RDARKAOF                                                  
         BE    DMXREC80                                                         
         CLC   =C'CH',RDARKAOF                                                  
         BNE   DMXKEEP                                                          
         CLC   =C'CA',RDARKAOF                                                  
         BE    DMXREC80                                                         
         CLC   =C'MI',RDARKAOF                                                  
         BE    DMXREC80                                                         
         CLC   =C'BO',RDARKAOF                                                  
         BNE   DMXKEEP                                                          
*&&                                                                             
         DROP  R5                                                               
*                                                                               
*CHECK MAKEGOOD OFFERS                                                          
*                                                                               
DMXREC10 DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RMKGREC,R5                                                       
         TM    RMKGCNTL,X'80'                                                   
         BO    DMXKEEP                                                          
         OC    RMKGKPLN,RMKGKPLN   HEADER ONLY                                  
         BNZ   DMXKEEP                                                          
         OC    RMKGDARN,RMKGDARN                                                
         BZ    DMXKEEP                                                          
*        TM    RMKGSFG1,X'08'+X'04' APPLIED/CANCELLED,SKIP                      
         TM    RMKGSFG1,X'04'                                                   
         BNZ   DMXKEEP                                                          
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXREC50                                                         
         USING RMKGXEL,R5                                                       
*        CLC   RMKGXFLT+3(3),=X'68061C'  06/28/04                               
*        BNL   DMXKEEP                                                          
         CLC   RMKGXFLT+3(3),=X'68031D'  03/29/04                               
         BNL   DMXKEEP                                                          
         CLC   RMKGXFLT+3(3),=X'670C1D'  12/29/03                               
         BL    DMXKEEP                                                          
*                                                                               
DMXREC50 DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RMKGREC,R5                                                       
         MVC   P(06),=C'OFFER:'                                                 
*                                                                               
         MVC   P+10(2),RMKGKREP                                                 
*                                                                               
         PACK  MYWORK(1),RMKGKCON+3(1) REVERSE THE COMPLIMENT                   
         PACK  MYWORK+1(1),RMKGKCON+2(1)                                        
         PACK  MYWORK+2(1),RMKGKCON+1(1)                                        
         PACK  MYWORK+3(1),RMKGKCON(1)                                          
*                                                                               
         ZAP   MYWORK+20(5),=P'0'                                               
         MVO   MYWORK+20(5),MYWORK(4)                                           
         ZAP   MYWORK+10(5),=P'99999999'                                        
         SP    MYWORK+10(5),MYWORK+20(5)                                        
*                                                                               
         EDIT  (P5,MYWORK+10),(8,P+14)                                          
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,RMKGDARN,P+24,4                                  
         GOTO1 =V(HEXOUT),DMCB,RMKGSCST,P+34,1                                  
         GOTO1 =V(HEXOUT),DMCB,RMKGSFG1,P+38,1                                  
         MVC   P+42(2),RMKGKGRP                                                 
         GOTO1 =V(DATCON),DMCB,(2,RMKGSCRD),(5,P+50)                            
         GOTO1 =V(DATCON),DMCB,(2,RMKGSLAD),(5,P+60)                            
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXREC80 DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RDARREC,R5                                                       
         CLI   RDARKRT,X'10'       HEADER RECORDS ONLY                          
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   RDARESEN,=X'D0DC'   JUN28/04                                     
         BNL   DMXKEEP                                                          
         CLC   RDARESEN,=X'D07D'   MAR29/04                                     
         BL    DMXKEEP                                                          
         AP    CHANGE,=P'1'                                                     
         MVC   P(08),=C'ACTIVE: '                                               
         CLI   0(R5),X'41'                                                      
         BE    *+10                                                             
         MVC   P(08),=C'CONFIRM:'                                               
*                                                                               
         MVC   P+10(2),RDARKREP                                                 
         GOTO1 =V(HEXOUT),DMCB,RDARREP#,P+14,4                                  
         GOTO1 =V(HEXOUT),DMCB,RDARKORD,P+24,4                                  
*                                                                               
         CLI   0(R5),X'41'                                                      
         BNE   DMXREC90                                                         
*                                                                               
         CLI   RDARBSTS,C'C'       SKIP RECALLS                                 
         BE    DMXKEEP                                                          
         CLI   RDARBSTS,C'R'       SKIP REJECTS                                 
         BE    DMXKEEP                                                          
         CLI   RDARBSTS,C'M'       SKIP AMENDS                                  
         BE    DMXKEEP                                                          
*                                                                               
         MVC   P+34(1),RDARBSTS                                                 
*                                                                               
         CLI   RDARBSTS,0                                                       
         BNE   *+8                                                              
         MVI   P+34,C'0'                                                        
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,RDARMISC,P+36,1                                  
*                                                                               
DMXREC90 DS    0H                                                               
         MVC   P+40(5),RDARKSTA                                                 
         MVC   P+50(10),RDARSNDR                                                
         MVC   P+64(16),RDARRTS                                                 
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,RDARESEN),(5,P+84)                            
*                                                                               
*        MVC   P+100(4),=C'SKIP'                                                
*        CLC   RDARESEN,=X'CF9D'   DEC29/03                                     
*        BL    *+10                                                             
*        MVC   P+100(4),=C'KEEP'                                                
*                                                                               
         CLI   0(R5),X'41'                                                      
         BE    DMXREC95                                                         
         MVC   P+110(5),=C'?????'                                               
         L     R5,AREC             POINT TO RECORD                              
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXREC95                                                         
         USING RDARCFEM,R5                                                      
         GOTO1 =V(DATCON),DMCB,(2,RDARCFDT),(5,P+110)                           
DMXREC95 DS    0H                                                               
         GOTO1 VPRINTER                                                         
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
         USING RECD,R3                                                          
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
**PAN#1  DC    CL21'048RELDDATE  07/12/04'                                      
         END                                                                    
