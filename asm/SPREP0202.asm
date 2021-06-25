*          DATA SET SPREP0202  AT LEVEL 060 AS OF 05/01/02                      
*PHASE SP0202A *,NOAUTO                                                         
*INCLUDE HEXOUT                                                                 
         SPACE 1                                                                
* QOPT1 = RECORD TYPE                                                           
*         S=STATIONS                                                            
*         R=REPS                                                                
*         M=MARKETS                                                             
*         A=STATION ADDRESSES                                                   
*                                                                               
* QOPT2 = PURGE OPTION                                                          
*         P=ONLY PURGE THOSE RECORDS WITH "*PURGE" IN NAME                      
*           (STWIX FOR STATIONS)                                                
*                                                                               
* QOPT5 = N TEST RUN (FILE NOT MARKED)                                          
*         Y = LIVE RUN                                                          
         TITLE 'STATION FILE PURGE PROGRAM - APPLICATION'                       
SP0202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP0202,R4                                                      
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         STM   R9,RB,SP02R9                                                     
         MVI   RCSUBPRG,0                                                       
         SPACE 2                                                                
* CONTROL SECTION *                                                             
         SPACE 1                                                                
         CLI   MODE,RUNFRST                                                     
         BE    SP000                                                            
         CLI   MODE,REQFRST                                                     
         BE    SP100                                                            
         CLI   MODE,PROCSTA        PROCESS STATION                              
         BE    DP200                                                            
         CLI   MODE,PROCREP        PROCESS REP                                  
         BE    DP300                                                            
         CLI   MODE,PROCMKT        PROCESS MARKET                               
         BE    DP400                                                            
         CLI   MODE,PROCADD        PROCESS STATION ADDRESS                      
         BE    DP500                                                            
         CLI   MODE,REQLAST                                                     
         BE    DP900               DO TOTALS                                    
         CLI   MODE,RUNLAST                                                     
         BE    DP999                                                            
*                                   LIST                                        
EXIT     XIT1                      EXIT                                         
         EJECT                                                                  
*        RUN FIRST                                                              
SP000    DS    0H                                                               
         LA    R0,4                                                             
         LA    RF,ADDRCTRT                                                      
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
         B     EXIT                EXIT                                         
         EJECT                                                                  
* SP100 - AT REQFRST, INITIALIZE TABLES AND COUNTERS *                          
         SPACE 1                                                                
SP100    DC    0H'0'                                                            
         MVI   ERRCD,1                                                          
         CLC   =C'ALL',QCLT                                                     
         BNE   BADREQ                                                           
         MVI   ERRCD,2                                                          
         CLC   =C'ALL',QPRD                                                     
         BNE   BADREQ                                                           
         MVI   ERRCD,3                                                          
         CLC   =C'ALL',QMKT                                                     
         BNE   BADREQ                                                           
         MVI   ERRCD,4                                                          
         CLC   =C'ALL',QSTA                                                     
         BNE   BADREQ                                                           
         SPACE 1                                                                
         RELOC RELO                                                             
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         SPACE 1                                                                
         LA    R0,5                                                             
         LA    RF,ADDRCTR                                                       
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
         B     EXIT                EXIT                                         
         EJECT                                                                  
         SPACE                                                                  
DP200    DC    0H'0'                    STATIONS                                
         MVI   RCSUBPRG,1                                                       
         CLI   QOPT5,C'Y'                                                       
         BNE   DP200C                                                           
         L     R6,ADSTAT                                                        
         ST    R6,DM4                                                           
         BAS   RE,DELREC                                                        
DP200C   AP    STACTR,=P'1'                                                     
         MVC   P(117),0(R6)                                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
*                                                                               
DP300    DS    0H                         REPS                                  
         MVI   RCSUBPRG,3                                                       
         CLI   QOPT5,C'Y'                                                       
         BNE   DP300C                                                           
         L     R6,ADREP                                                         
         ST    R6,DM4                                                           
         BAS   RE,DELREC                                                        
DP300C   AP    REPCTR,=P'1'                                                     
         MVC   P(117),0(R6)                                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
DP400    DS    0H                          MARKETS                              
         MVI   RCSUBPRG,4                                                       
         CLI   QOPT5,C'Y'                                                       
         BNE   DP400C                                                           
         L     R6,ADMARKET                                                      
         ST    R6,DM4                                                           
         BAS   RE,DELREC                                                        
DP400C   AP    MKTCTR,=P'1'                                                     
         MVC   P(117),0(R6)                                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
*                                                                               
DP500    DS    0H                          STATION ADDRESSES                    
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT5,C'Y'                                                       
         BNE   DP500C                                                           
         L     R6,ADSTATAD                                                      
         ST    R6,DM4                                                           
         BAS   RE,DELREC                                                        
DP500C   AP    ADDRCTR,=P'1'                                                    
         MVC   P(117),0(R6)                                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
DP900    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+5(13),=C'REPORT TOTALS'                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(29),=CL29'NUMBER OF STATIONS DELETED = '                    
         EDIT  (P8,STACTR),(4,P+47),ALIGN=LEFT,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(36),=CL36'NUMBER OF ADDRESS RECORDS DELETED = '             
         EDIT  (P8,ADDRCTR),(4,P+54),ALIGN=LEFT,ZERO=NOBLANK                    
         SPACE 1                                                                
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(28),=CL28'NUMBER OF MARKETS DELETED = '                     
         EDIT  (P8,MKTCTR),(4,P+46),ALIGN=LEFT,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
         MVC   P+18(28),=CL28'NUMBER OF REPS DELETED = '                        
         EDIT  (P8,REPCTR),(4,P+43),ALIGN=LEFT,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P+41(34),=CL34'TOTAL NUMBER OF RECORDS DELETED = '               
         ZAP   TOTALCTR,ADDRCTR                                                 
         AP    TOTALCTR,MKTCTR                                                  
         AP    TOTALCTR,STACTR                                                  
         AP    TOTALCTR,REPCTR                                                  
         EDIT  (P8,TOTALCTR),(4,P+75),ALIGN=LEFT,ZERO=NOBLANK                   
         GOTO1 REPORT                                                           
*                                   ROLL TO RUN TOTALS                          
         AP    ADDRCTRT,ADDRCTR                                                 
         AP    MKTCTRT,MKTCTR                                                   
         AP    STACTRT,STACTR                                                   
         AP    REPCTRT,REPCTR                                                   
         B     EXIT                                                             
*                                                                               
DP999    DS    0H                       RUN TOTALS                              
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+5(10),=C'RUN TOTALS'                                           
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(29),=CL29'NUMBER OF STATIONS DELETED = '                    
         EDIT  (P8,STACTRT),(4,P+47),ALIGN=LEFT,ZERO=NOBLANK                    
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(36),=CL36'NUMBER OF ADDRESS RECORDS DELETED = '             
         EDIT  (P8,ADDRCTRT),(4,P+54),ALIGN=LEFT,ZERO=NOBLANK                   
         SPACE 1                                                                
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(28),=CL28'NUMBER OF MARKETS DELETED = '                     
         EDIT  (P8,MKTCTRT),(4,P+46),ALIGN=LEFT,ZERO=NOBLANK                    
         GOTO1 REPORT                                                           
         MVC   P+18(28),=CL28'NUMBER OF REPS DELETED = '                        
         EDIT  (P8,REPCTRT),(4,P+43),ALIGN=LEFT,ZERO=NOBLANK                    
         GOTO1 REPORT                                                           
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P+41(34),=CL34'TOTAL NUMBER OF RECORDS DELETED = '               
         ZAP   TOTALCTR,ADDRCTRT                                                
         AP    TOTALCTR,MKTCTRT                                                 
         AP    TOTALCTR,STACTRT                                                 
         AP    TOTALCTR,REPCTRT                                                 
         EDIT  (P8,TOTALCTR),(4,P+75),ALIGN=LEFT,ZERO=NOBLANK                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
PROCSTA  EQU   40                                                               
PROCREP  EQU   50                                                               
PROCMKT  EQU   60                                                               
PROCADD  EQU   70                                                               
*                                                                               
*****************************************************************               
         EJECT                                                                  
* THIS SUBROUTINE DELETES THE RECORDS ON THE STATION FILE *                     
         SPACE 1                                                                
DELREC   NTR1                                                                   
         MVI   17(R6),C'D'         MARK RECORD AS 'TO BE DELETED'               
         B     DEL10               *** PATCH OUT TRACE ***                      
         MVC   SAVEPRT(132),P                                                   
         XC    P,P                                                              
         MVC   P+5(117),0(R6)      TRACE - PRINT RECORD                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
DEL10    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD                   
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,STATION                                       
         TM    DM3,X'FD'                                                        
         BNZ   DMGRERR                                                          
         B     EXIT                                                             
         SPACE 1                                                                
BADREQ   DS    0H                                                               
         MVC   P+2(23),=C'ERROR - XXX MUST BE ALL'                              
         ZIC   RF,ERRCD                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RE,ERRMSG                                                        
         AR    RE,RF                                                            
         MVC   P+10(3),0(RE)                                                    
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE 1                                                                
         SPACE 1                                                                
DMGRERR  DC    H'0'                                                             
         SPACE 1                                                                
         USING *,RF                                                             
SP02HDHK DC    0H'0'                                                            
         LM    R9,RB,SP02R9                                                     
         B     EXIT                                                             
         DROP  RF                                                               
SP02R9   DC    3F'0'                                                            
         EJECT                                                                  
* BINREC - BINSRCH RECORD FORMAT *                                              
         SPACE 1                                                                
BINREC   DS    0CL6                                                             
BINSTA   DS    CL3                 PACKED STATION                               
BINCLT   DS    CL2                 PACKED CLIENT                                
BINACT   DS    CL1                 ACTIVITY SWITCH                              
         SPACE 2                                                                
         DS    0D                                                               
MKTNUM   DS    D                                                                
ADDRCTRT DS    PL8                 TOTALS FOR RUN                               
MKTCTRT  DS    PL8                                                              
STACTRT  DS    PL8                                                              
REPCTRT  DS    PL8                                                              
*                                                                               
ADDRCTR  DS    PL8                TOTALS FOR REQUEST                            
MKTCTR   DS    PL8                                                              
STACTR   DS    PL8                                                              
REPCTR   DS    PL8                                                              
TOTALCTR DS    PL8                                                              
*                                                                               
TBLENGTH DS    F                                                                
PSTA     DS    0FL6                PARAMETER LIST FOR BINSRCH                   
PSTA1    DS    F                                                                
PSTA2    DS    F                                                                
PSTA3    DS    F                                                                
PSTA4    DS    F                                                                
PSTA5    DS    F                                                                
PSTA6    DS    F                                                                
RELO     DS    F                                                                
ASTATBL  DS    A                                                                
AMKTTBL  DS    A                                                                
AREPTBL  DS    A                                                                
BINMKT   DS    H                                                                
SAVEKEY  DS    CL17                                                             
SAVEPRT  DS    CL132                                                            
BUYSW    DS    C                                                                
ERRCD    DS    C                                                                
ERRMSG   DS    0H                                                               
         DC    C'CLT'                                                           
         DC    C'PRD'                                                           
         DC    C'MKT'                                                           
         DC    C'STA'                                                           
         DC    C'EST'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
STATBL   CSECT                                                                  
         DS    120000C                                                          
         SPACE 3                                                                
MKTTBL   CSECT                                                                  
         DS    30000C                                                           
         PRINT OFF                                                              
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPREP0202 05/01/02'                                      
         END                                                                    
