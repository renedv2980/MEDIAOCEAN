*          DATA SET SPREPFX02S AT LEVEL 007 AS OF 05/01/02                      
*PHASE SPFX02S                                                                  
*INCLUDE MSUNPK                                                                 
         SPACE                                                                  
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,R7,R8                                                   
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
* CONTROL SECTION *                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    SP000               GET THE NEW STATION CODE                     
         CLI   MODE,RUNLAST                                                     
         BE    PRTOUT                                                           
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
         L     RE,SSB                                                           
         OI    3(RE),X'08'         NEED TO RECOVER COPIES AND CHANGES           
         STM   R7,RB,SPSCR7                                                     
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* GET ALL CANADIAN AGENCIES                                                     
*                                                                               
SP000    DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
SP50     DS    0H                                                               
         L     R6,ADBUY                                                         
         LR    R0,R6                                                            
         SH    R0,=H'4'                                                         
         GET   FILEIN,(0)                                                       
         TM    15(R6),X'C0'                                                     
         BO    SP60                                                             
         TM    15(R6),X'80'        IF DELETED - SKIP                            
         BO    SP50                                                             
*                                                                               
SP60     DS    0H                                                               
         CLC   =X'0E01',0(R6)                                                   
         BE    *+6                                                              
         DC    H'0'                ONLY RECS ON TAPE                            
*                                                                               
         MVC   DUB(1),2(R6)        SET A/M IN DUB                               
         NI    DUB,X'0F'           TURN OFF AGY BIT                             
         MVI   MEDTYPE,C'T'                                                     
         CLI   DUB,1               TV                                           
         BE    SPSBILL                                                          
         CLI   DUB,3               NETWORK                                      
         BNE   SP50                                                             
         MVI   MEDTYPE,C'N'                                                     
*                                                                               
*                                                                               
* SPSBILL - THIS SECTION UPDATES STATION BILLING BUCKET RECORDS                 
*                                                                               
SPSBILL  MVI   TRACECDE,C'K'                                                    
         MVC   TMKST,7(R6)         UNPK OLD STATION                             
         MVI   SVNETBTS,0                                                       
         CLI   MEDTYPE,C'T'                                                     
         BE    SSB10                                                            
         MVC   SVNETBTS,11(R6)      GET NETWORK BITS                            
         NI    SVNETBTS,X'1F'      LEAVE ONLY LAST 5 BITS                       
*                                                                               
SSB10    BAS   RE,SETSTPCK                                                      
         BNE   SP50                                                             
         MVC   7(5,R6),NEWBMKST                                                 
         CLI   MEDTYPE,C'N'                                                     
         BNE   *+10                                                             
         MVC   11(1,R6),SVNETBTS                                                
         AP    TOTREC,=P'1'                                                     
         MVC   AREC,ADBUY                                                       
         GOTO1 ADD                                                              
*                                                                               
         GOTO1 HEXOUT,DMCB,ADBUY,P,32,=C'TOG'                                   
         GOTO1 REPORT                                                           
         B     SP50                DON'T CARE ABOUT OTHERS                      
         EJECT                                                                  
*                                                                               
*        CHECK IF THIS A/M NEEDS TO BE CHANGED                                  
*        CANADIAN & TV/NETWORK                                                  
*                                                                               
CHKCHG   NTR1                                                                   
         MVC   DUB+1(1),DUB        SAVE IT                                      
         NI    DUB,X'0F'           TURN OFF AGY BIT                             
         MVI   MEDTYPE,C'T'                                                     
         CLI   DUB,1               TV                                           
         BE    CC10                                                             
         CLI   DUB,3               NETWORK                                      
         BNE   CCNO                                                             
         MVI   MEDTYPE,C'N'                                                     
*                                                                               
CC10     MVC   DUB(1),DUB+1                                                     
         NI    DUB,X'F0'           TURN OFF MEDIA BIT                           
         LA    R5,AGYTAB                                                        
*                                                                               
CC20     CLI   0(R5),X'FF'                                                      
         BE    CCNO                                                             
         CLC   2(1,R5),DUB         IS IT CANADIAN                               
         BE    CCYES                                                            
         LA    R5,3(R5)                                                         
         B     CC20                                                             
*                                                                               
CCYES    LA    RE,SPBUFF                                                        
         OC    20(2,RE),20(RE)     TEST AGYALPHA THERE                          
         BNZ   *+10                                                             
         MVC   20(2,RE),0(R5)      ELSE MOVE IT IN !                            
         B     YES                                                              
CCNO     B     NO                                                               
         EJECT                                                                  
*                                                                               
*        END OF INPUT                                                           
*                                                                               
SPEND    DS    0H                                                               
         CLOSE (FILEIN,)                                                        
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        ADD A NEW RECORD TO SPTFILE                                            
*                                                                               
*&&DO                                                                           
SPPUT    AP    TOTREC,=P'1'                                                     
         PUT   FILEOUT,SPBUFF-4                                                 
*                                                                               
         CLI   SPBUFF,X'0A'        IF THIS IS TRAFFIC                           
         BNE   SPPX                                                             
         MVI   FORCEHED,C'Y'                                                    
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
*                                                                               
SPPX     B     SP50                GET NEXT RECORD                              
*&&                                                                             
         EJECT                                                                  
*                                                                               
*        PRINT OUT TOTALS                                                       
*                                                                               
PRTOUT   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,RECTAB                                                        
         LA    R5,NRECTAB                                                       
*                                                                               
PO10     OI    3(R4),X'0F'                                                      
         UNPK  P(7),0(4,R4)                                                     
         MVC   P+9(16),4(R4)                                                    
         GOTO1 REPORT                                                           
         LA    R4,L'RECTAB(R4)                                                  
         BCT   R5,PO10                                                          
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SET UP CALL TO VSTAPACK                                                       
*                                                                               
SETSTPCK NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         GOTO1 =V(MSUNPK),DMCB,TMKST,STAPQMKT,STAPQSTA                          
         CLI   STAPQSTA+4,C' '                                                  
         BH    *+10                                                             
         MVC   STAPQSTA+4(1),QMED                                               
*                                                                               
         CLC   =C'YCKD',STAPQSTA                                                
         BNE   SETST2                                                           
         MVI   MEDTYPE,C'Z'        STOP ANY CHANGES TO THIS FUCKER              
         MVC   NEWBMKST,=X'270ED6B5A4'                                          
         B     SETSTX                                                           
*                                                                               
SETST2   MVI   STAPACT,C'P'                                                     
         L     RE,ADBUY                                                         
         MVC   STAPAGY,20(RE)      SFC ! (YOU TOO, EVAN)                        
         MVC   STAPMED,QMED                                                     
         MVI   STAPCTRY,C'C'       WE'RE ONLY DOING CANADA                      
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BNE   SETSERR                                                          
         MVC   NEWBMKST,STAPMKST                                                
SETSTX   B     YES     !                                                        
*                                                                               
*SETSERR  CLC   LASTMKST,STAPMKST                                               
*         BE    SETSERR1                                                        
*                                                                               
*                                                                               
SETSERR  DS    0H                                                               
         MVC   LASTMKST,STAPMKST                                                
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,SPBUFF,P+5,13,=C'TOG',0                              
         MVC   P+35(4),STAPQMKT                                                 
         MVC   P+40(5),STAPQSTA                                                 
         MVC   P+50(22),=CL22'NOT FOUND IN NEW FILE'                            
         GOTO1 REPORT                                                           
         MVC   P,SAVEPRT                                                        
*                                                                               
SETSERR1 AP    NFOUND,=P'1'                                                     
         B     NO                  EXIT WITH CC NEQ                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
*                                                                               
SPTRACE  NTR1                                                                   
         CLI   QOPT5,C'Y'                                                       
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(5),=CL5'TRACE'                                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,SPBUFF,P+10,13,=C'TOG',0                             
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R6,SPBUFF+24                                                     
         MVI   ELCODE,0                                                         
*                                                                               
SPT10    ZIC   R3,1(R6)                                                         
         LTR   R3,R3                                                            
         BZ    SPT20                                                            
         GOTO1 HEXOUT,DMCB,(R6),P+10,(R3),=C'TOG',0                             
         GOTO1 REPORT                                                           
         BAS   RE,NEXTEL                                                        
         BE    SPT10                                                            
*                                                                               
SPT20    MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,EODAD=SPEND             
*                                                                               
         DS    0D                                                               
         DC    CL8'*AGYTAB*'                                                    
AGYTAB   DS    16CL3                                                            
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
RECTAB   DS    0CL20                                                            
REGCHAN  DC    PL4'0',CL16'REG CONTACTS CHG'                                    
GASCHAN  DC    PL4'0',CL16'GRADE ASSIGN CHG'                                    
LOGCHAN  DC    PL4'0',CL16'LOG ENTRIES CHG'                                     
SYNCHG   DC    PL4'0',CL16'SYNDICATION'                                         
INVCHAN  DC    PL4'0',CL16'INVOICES CHG'                                        
ORDCHAN  DC    PL4'0',CL16'ORD ENTRIES CHG'                                     
DUPLREC  DC    PL4'0',CL16'DUP RECORDS'                                         
BUYCHG   DC    PL4'0',CL16'BUY '                                                
INVCHG   DC    PL4'0',CL16'INVOICE'                                             
NSICHG   DC    PL4'0',CL16'NSID '                                               
DEMCHG   DC    PL4'0',CL16'DEMO OVERRIDE'                                       
SIDCHG   DC    PL4'0',CL16'SID '                                                
SLKCHG   DC    PL4'0',CL16'STATION LOCKIN'                                      
CLRCHG   DC    PL4'0',CL16'CLEARANCE STAT'                                      
UPLCHG   DC    PL4'0',CL16'UPLOAD'                                              
INFCHG   DC    PL4'0',CL16'INFOMERCIAL'                                         
PWCHG    DC    PL4'0',CL16'PROFIT WITHIN'                                       
DBLCHG   DC    PL4'0',CL16'DOUBLE BOOKING'                                      
SBKCHG   DC    PL4'0',CL16'STAT BILL BUCKET'                                    
SINCHG   DC    PL4'0',CL16'STATION INVOICE'                                     
STACHG   DC    PL4'0',CL16'STATUS'                                              
PATCHG   DC    PL4'0',CL16'PATTERN '                                            
INRCHG   DC    PL4'0',CL16'INST RECAP '                                         
SHPCHG   DC    PL4'0',CL16'SHIP RECAP '                                         
STADCHG  DC    PL4'0',CL16'STAT ADDR '                                          
BUYACHG  DC    PL4'0',CL16'BUY ACT/ESTM'                                        
LABLCHG  DC    PL4'0',CL16'LABEL LIST '                                         
STALCHG  DC    PL4'0',CL16'STATION LIST '                                       
TBUYCHG  DC    PL4'0',CL16'TRAFFIC BUY '                                        
TSTACHG  DC    PL4'0',CL16'STAT ADDRESS'                                        
NFOUND   DC    PL4'0',CL16'STAT NOT FOUND'                                      
TOTREC   DC    PL4'0',CL16'TOTAL # RECORDS'                                     
NRECTAB  EQU   ((*-RECTAB)/L'RECTAB)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* DATA                                                                          
*                                                                               
SPSCR7   DC    5F'0'               SAVE AREA FOR R7                             
*                                                                               
         DS    0H                                                               
TMKST    DS    0CL5                                                             
TMKT     DS    H                   PACKED MKT/STA                               
TSTA     DS    XL3                                                              
*                                                                               
TRACECDE DS    CL1                 TRACE CODE                                   
ELCODE   DS    CL1                                                              
M        DS    CL1                                                              
*                                                                               
NEWBMKST DS    0CL5                                                             
NEWBMKT  DS    H                   PACKED MKT/STA                               
NEWBSTA  DS    XL3                                                              
*                                                                               
MEDTYPE  DS    CL1                                                              
SVNETBTS DS    CL1                                                              
LASTMKST DS    XL5                                                              
*                                                                               
SAVEPRT  DS    CL132                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*RECORD*'                                                    
         DS    F                                                                
SPBUFF   DS    2000C                                                            
         EJECT                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLELDSP  DS    CL10                                                             
         EJECT                                                                  
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWBS                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPFX02S05/01/02'                                      
         END                                                                    
